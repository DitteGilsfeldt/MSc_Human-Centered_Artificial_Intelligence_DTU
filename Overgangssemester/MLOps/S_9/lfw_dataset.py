import argparse
import time
from pathlib import Path

import torch
from torch.utils.data import Dataset, DataLoader
from torchvision import transforms
from torchvision.utils import make_grid
from PIL import Image


class LFWDataset(Dataset):
    def __init__(self, root_dir: str, transform=None):
        self.root_dir = Path(root_dir)
        self.transform = transform

        self.img_paths = sorted(self.root_dir.rglob("*.jpg"))
        if len(self.img_paths) == 0:
            raise RuntimeError(f"No .jpg files found under {self.root_dir}")

    def __len__(self) -> int:
        return len(self.img_paths)

    def __getitem__(self, idx: int) -> torch.Tensor:
        img_path = self.img_paths[idx]
        img = Image.open(img_path).convert("RGB")

        if self.transform is not None:
            img = self.transform(img)

        if not isinstance(img, torch.Tensor):
            raise TypeError("Transform must return torch.Tensor (did you forget ToTensor?)")

        return img


def get_lfw_transforms(heavy_aug: bool = False):
    if not heavy_aug:
        return transforms.Compose([
            transforms.Resize((128, 128)),
            transforms.ToTensor(),
        ])

    return transforms.Compose([
        transforms.RandomAffine(5, (0.1, 0.1), (0.5, 2.0)),
        transforms.ColorJitter(brightness=0.2, contrast=0.2, saturation=0.2),
        transforms.RandomHorizontalFlip(p=0.5),
        transforms.Resize((128, 128)),
        transforms.ToTensor(),
    ])


def visualize_one_batch(dataloader: DataLoader):
    import matplotlib.pyplot as plt

    batch = next(iter(dataloader))          # [B, C, H, W]
    grid = make_grid(batch, nrow=4)         # [C, H_grid, W_grid]
    grid = grid.permute(1, 2, 0).cpu().numpy()

    plt.figure()
    plt.imshow(grid)
    plt.axis("off")
    plt.title("One batch from LFW")
    plt.show()


def _iterate_n_batches(dataloader: DataLoader, n_batches: int):
    n = 0
    for _ in dataloader:
        n += 1
        if n >= n_batches:
            break


def timing_experiment(
    dataset: Dataset,
    batch_size: int,
    num_workers: int,
    batches_to_check: int,
    repeats: int,
    mp_context: str | None,
    prefetch_factor: int,
    persistent_workers: bool,
    pin_memory: bool,
    warmup_batches: int,
):
    times = []

    for _ in range(repeats):
        dataloader = DataLoader(
            dataset,
            batch_size=batch_size,
            shuffle=False,
            num_workers=num_workers,
            multiprocessing_context=mp_context if num_workers > 0 else None,
            prefetch_factor=prefetch_factor if num_workers > 0 else None,
            persistent_workers=persistent_workers if num_workers > 0 else False,
            pin_memory=pin_memory,
        )

        # Warm-up to reduce one-off overhead in timing
        if warmup_batches > 0:
            _iterate_n_batches(dataloader, warmup_batches)

        start = time.perf_counter()
        _iterate_n_batches(dataloader, batches_to_check)
        end = time.perf_counter()

        times.append(end - start)

    t = torch.tensor(times, dtype=torch.float32)
    mean = float(t.mean())
    std = float(t.std(unbiased=False))
    return mean, std, times


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--data_root", type=str, default="lfw-deepfunneled")
    parser.add_argument("--batch_size", type=int, default=8)
    parser.add_argument("--num_workers", type=int, default=0)
    parser.add_argument("--batches_to_check", type=int, default=100)
    parser.add_argument("--repeats", type=int, default=5)

    parser.add_argument("--visualize_batch", action="store_true")
    parser.add_argument("--get_timing", action="store_true")
    parser.add_argument("--heavy_aug", action="store_true")

    # macOS multiprocessing
    parser.add_argument("--mp_context", type=str, default="fork", choices=["fork", "spawn"])

    # DataLoader performance knobs
    parser.add_argument("--prefetch_factor", type=int, default=2)
    parser.add_argument("--persistent_workers", action="store_true")
    parser.add_argument("--pin_memory", action="store_true")

    # Timing hygiene
    parser.add_argument("--warmup_batches", type=int, default=10)

    args = parser.parse_args()

    transform = get_lfw_transforms(heavy_aug=args.heavy_aug)
    dataset = LFWDataset(root_dir=args.data_root, transform=transform)

    print(f"Dataset size: {len(dataset)} images")

    dataloader = DataLoader(
        dataset,
        batch_size=args.batch_size,
        shuffle=False,
        num_workers=args.num_workers,
        multiprocessing_context=args.mp_context if args.num_workers > 0 else None,
        prefetch_factor=args.prefetch_factor if args.num_workers > 0 else None,
        persistent_workers=args.persistent_workers if args.num_workers > 0 else False,
        pin_memory=args.pin_memory,
    )

    # Sanity check
    batch = next(iter(dataloader))
    print("Sanity check batch shape:", tuple(batch.shape))

    if args.visualize_batch:
        visualize_one_batch(dataloader)

    if args.get_timing:
        mean, std, raw = timing_experiment(
            dataset=dataset,
            batch_size=args.batch_size,
            num_workers=args.num_workers,
            batches_to_check=args.batches_to_check,
            repeats=args.repeats,
            mp_context=args.mp_context,
            prefetch_factor=args.prefetch_factor,
            persistent_workers=args.persistent_workers,
            pin_memory=args.pin_memory,
            warmup_batches=args.warmup_batches,
        )
        print(f"Timing ({args.repeats} runs, {args.batches_to_check} batches each):")
        print("  raw:", [round(x, 4) for x in raw])
        print(f"  mean={mean:.4f}s, std={std:.4f}s")


if __name__ == "__main__":
    main()
