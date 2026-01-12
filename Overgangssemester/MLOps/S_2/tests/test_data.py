# from torch.utils.data import Dataset
from src.my_project.data import corrupt_mnist
import pytest
import os.path
@pytest.mark.skipif(not os.path.exists("src/my_project/data"), reason="Data files not found")

# def test_my_dataset():
#     """Test the MyDataset class."""
#     dataset = MyDataset("data/raw")
#     assert isinstance(dataset, Dataset)

def test_data():
    train_set, test_set = corrupt_mnist()
    # assert len(dataset) == N_train for training and N_test for test
    assert len(train_set) == 30000, "Dataset did not have the correct number of samples"
    assert len(test_set) == 5000
    # assert that each datapoint has shape [1,28,28] or [784] depending on how you choose to format
    for datapoint in [train_set, test_set]:
        assert all(datapoint[i][0].shape == (1, 28, 28) for i in range(len(datapoint)))
    # assert that all labels are represented
    train_targets = torch.unique(train.tensors[1])
    assert (train_targets == torch.arange(0,10)).all()
    test_targets = torch.unique(test.tensors[1])
    assert (test_targets == torch.arange(0,10)).all()