import matplotlib.pyplot as plt
import torch
import typer
import hydra
import logging
import wandb
from my_project.model import MyAwesomeModel
from sklearn.metrics import accuracy_score, f1_score, precision_score, recall_score, RocCurveDisplay

from my_project.data import corrupt_mnist

### wandb ###
wandb.login()

config = {
    "learning_rate": 1e-3,
    "batch_size": 32,
    "epochs": 10,
    "architecture": "MyAwesomeModel",
    "dataset": "corrupt_mnist",
} 
#############

# DEVICE = torch.device("mps" if torch.backends.mps.is_available() else "cpu")
DEVICE = torch.device("cpu")

def train(lr: float = 1e-3, batch_size: int = 32, epochs: int = 10) -> None:
    """Train a model on MNIST."""
    print("Training day and night")
    print(f"{lr=}, {batch_size=}, {epochs=}")

    run = wandb.init(project="my_project_lucia", config=config)

    model = MyAwesomeModel().to(DEVICE)
    train_set, _ = corrupt_mnist()

    train_dataloader = torch.utils.data.DataLoader(train_set, batch_size=batch_size)

    loss_fn = torch.nn.CrossEntropyLoss()
    optimizer = torch.optim.Adam(model.parameters(), lr=lr)

    statistics = {"train_loss": [], "train_accuracy": []}

    preds, targets = [], []

    for epoch in range(epochs):
        model.train()
        
        for i, (img, target) in enumerate(train_dataloader):
            img, target = img.to(DEVICE), target.to(DEVICE)
            optimizer.zero_grad()
            y_pred = model(img)
            loss = loss_fn(y_pred, target)
            loss.backward()
            optimizer.step()
            statistics["train_loss"].append(loss.item())

            accuracy = (y_pred.argmax(dim=1) == target).float().mean().item()
            statistics["train_accuracy"].append(accuracy)

            wandb.log({"train_loss": loss.item(), "train_accuracy": accuracy})

            if i % 100 == 0:
                print(f"Epoch {epoch}, iter {i}, loss: {loss.item()}")
            
            preds.append(y_pred.detach().cpu())
            targets.append(target.detach().cpu())

    preds = torch.cat(preds)
    targets = torch.cat(targets)

    final_accuracy = accuracy_score(targets, preds.argmax(1))
    final_precision = precision_score(targets, preds.argmax(1), average="weighted")
    final_recall = recall_score(targets, preds.argmax(1), average="weighted")
    final_f1 = f1_score(targets, preds.argmax(1), average="weighted")

    torch.save(model.state_dict(), "model.pth")

    artifact = wandb.Artifact(
        name="corrupt_mnist_model",
        type="model",
        metadata={
            "accuracy": final_accuracy,
            "precision": final_precision,
            "recall": final_recall,
            "f1": final_f1,
        },
    )
    artifact.add_file("model.pth")
    run.log_artifact(artifact)

    run.finish()
    
    print("Training complete")

    # ### PREVENT DOCKER PARENT DICT ISSUE ###
    # from pathlib import Path
    
    # Path("models").mkdir(parents=True, exist_ok=True)
    # Path("reports/figures").mkdir(parents=True, exist_ok=True)
    # ###

    # torch.save(model.state_dict(), "models/model.pth")
    # fig, axs = plt.subplots(1, 2, figsize=(15, 5))
    # axs[0].plot(statistics["train_loss"])
    # axs[0].set_title("Train loss")
    # axs[1].plot(statistics["train_accuracy"])
    # axs[1].set_title("Train accuracy")
    # fig.savefig("reports/figures/training_statistics.png")

if __name__ == "__main__":
    typer.run(train)

# ### ERROR BECAUSE ABOVE VERSION IS TYPER, BUT BELOW IS HYDRA ###

# # log = logging.getLogger(__name__)

# # @hydra.main(version_base=None, config_path="../config", config_name="config")
# # def main(config: DictConfig) -> None:
# #     lr = config.hyperparameters.lr
# #     batch_size = config.hyperparameters.batch_size
# #     epochs = config.hyperparameters.epochs

# #     log.info(f"{lr=}, {batch_size=}, {epochs=}")

# #     model = MyAwesomeModel().to(DEVICE)
# #     train_set, _ = corrupt_mnist()

# #     train_dataloader = torch.utils.data.DataLoader(
# #         train_set, batch_size=batch_size
# #     )

# #     loss_fn = torch.nn.CrossEntropyLoss()
# #     optimizer = torch.optim.Adam(model.parameters(), lr=lr)

# #     statistics = {"train_loss": [], "train_accuracy": []}

# #     for epoch in range(epochs):
# #         model.train()
# #         for i, (img, target) in enumerate(train_dataloader):
# #             img, target = img.to(DEVICE), target.to(DEVICE)

# #             optimizer.zero_grad()
# #             y_pred = model(img)
# #             loss = loss_fn(y_pred, target)
# #             loss.backward()
# #             optimizer.step()

# #             statistics["train_loss"].append(loss.item())
# #             accuracy = (y_pred.argmax(dim=1) == target).float().mean().item()
# #             statistics["train_accuracy"].append(accuracy)

# #             if i % 100 == 0:
# #                 log.info(f"Epoch {epoch}, iter {i}, loss: {loss.item()}")

# #     log.info("Training complete")

# #     # Hydra-safe output dirs (relative to run dir)
# #     Path("models").mkdir(parents=True, exist_ok=True)
# #     Path("reports/figures").mkdir(parents=True, exist_ok=True)

# #     torch.save(model.state_dict(), "models/model.pth")

# #     fig, axs = plt.subplots(1, 2, figsize=(15, 5))
# #     axs[0].plot(statistics["train_loss"])
# #     axs[0].set_title("Train loss")
# #     axs[1].plot(statistics["train_accuracy"])
# #     axs[1].set_title("Train accuracy")
# #     fig.savefig("reports/figures/training_statistics.png")


# # if __name__ == "__main__":
# #     main()
