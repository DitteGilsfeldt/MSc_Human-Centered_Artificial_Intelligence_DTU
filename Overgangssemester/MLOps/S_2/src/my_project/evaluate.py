import torch
import typer
from my_project.model import MyAwesomeModel

from my_project.data import corrupt_mnist

device = torch.device("mps" if torch.mps.is_available() else "cpu")


def evaluate(model_checkpoint: str = "model.pth", batch_size: int = 32) -> None:
    """Evaluate a trained model."""
    print("Evaluating like my life depends on it")
    print(model_checkpoint)

    # TODO: Implement evaluation logic here
    model = MyAwesomeModel().to(device)
    state_dict = torch.load(model_checkpoint, map_location=device)
    model.load_state_dict(state_dict)
    model.eval()

    _, test_set = corrupt_mnist()
    testloader = torch.utils.data.DataLoader(test_set, batch_size=batch_size, shuffle=False)

    correct = 0
    total = 0

    with torch.no_grad():
        for images, labels in testloader:
            images, labels = images.to(device), labels.to(device)
            outputs = model(images)
            _, predicted = torch.max(outputs.data, 1)
            total += labels.size(0)
            correct += (predicted == labels).sum().item()

    print(f"Accuracy of the model on the test set: {100 * correct / total} %")


if __name__ == "__main__":
    typer.run(evaluate)
