from torch.utils.data import Dataset
from my_project.data import corrupt_mnist
from my_project.model import MyAwesomeModel
import torch

device = torch.device("mps" if torch.mps.is_available() else "cpu")

def test_training():
    model = MyAwesomeModel().to(device)
    # See if loss for last epoch is lower than first epoch
    train_set, _ = corrupt_mnist()
    train_dataloader = torch.utils.data.DataLoader(train_set, batch_size=32)
    loss_fn = torch.nn.CrossEntropyLoss()
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)
    losses = []
    for _ in range(2):
        model.train()
        epoch_loss = 0
        for i, (img, target) in enumerate(train_dataloader):
            img, target = img.to(device), target.to(device)
            optimizer.zero_grad()
            y_pred = model(img)
            loss = loss_fn(y_pred, target)
            loss.backward()
            optimizer.step()
            epoch_loss += loss.item()
        losses.append(epoch_loss / len(train_dataloader))
    assert losses[1] < losses[0]