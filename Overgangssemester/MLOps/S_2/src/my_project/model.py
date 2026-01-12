import pytorch_lightning as pl
import torch
from torch import nn
from torch.utils.data import DataLoader, TensorDataset
from pytorch_lightning import Trainer
from pytorch_lightning.callbacks import ModelCheckpoint, EarlyStopping

from my_project.data import corrupt_mnist


class MyAwesomeModel(pl.LightningModule):
    """My awesome model."""

    def __init__(self) -> None:
        super().__init__()
        self.conv1 = nn.Conv2d(1, 32, 3, 1)
        self.conv2 = nn.Conv2d(32, 64, 3, 1)
        self.conv3 = nn.Conv2d(64, 128, 3, 1)
        self.dropout = nn.Dropout(0.5)
        self.fc1 = nn.Linear(128, 10)

        self.loss_fn = nn.CrossEntropyLoss()

    def forward(self, x: torch.Tensor) -> torch.Tensor:
         # Check: must be 4D: [N, C, H, W]
        if x.ndim != 4:
            raise ValueError('Expected input to a 4D tensor')
        if x.shape[1] != 1 or x.shape[2] != 28 or x.shape[3] != 28:
            raise ValueError('Expected each sample to have shape [1, 28, 28]')
        
        """Forward pass."""
        x = torch.relu(self.conv1(x))
        x = torch.max_pool2d(x, 2, 2)
        x = torch.relu(self.conv2(x))
        x = torch.max_pool2d(x, 2, 2)
        x = torch.relu(self.conv3(x))
        x = torch.max_pool2d(x, 2, 2)
        x = torch.flatten(x, 1)
        x = self.dropout(x)        
        return self.fc1(x)

    def training_step(self, batch):
        """Training step."""
        img, target = batch
        y_pred = self(img)
        return self.loss_fn(y_pred, target)

    def configure_optimizers(self):
        """Configure optimizer."""
        return torch.optim.Adam(self.parameters(), lr=1e-3)        

train_set, test_set = corrupt_mnist()

train_loader = DataLoader(train_set, batch_size=32, shuffle=True, num_workers=4)
test_loader = DataLoader(test_set, batch_size=32, shuffle=False, num_workers=4)


if __name__ == "__main__":
    model = MyAwesomeModel()

    print(f"Model architecture: {model}")
    print(f"Number of parameters: {sum(p.numel() for p in model.parameters())}")

    dummy_input = torch.randn(1, 1, 28, 28)
    output = model(dummy_input)
    print(f"Output shape: {output.shape}")

    checkpoint_callbacks = ModelCheckpoint(dirpath="./models", monitor="val_loss", mode="min")
    # early_stopping_callback = EarlyStopping(monitor="val_loss", patience=3, verbose=True, mode="min")

    trainer = Trainer(callbacks=[checkpoint_callbacks], accelerator = "gpu", max_epochs=5, limit_train_batches=0.2)
    trainer.fit(model, train_loader, test_loader)