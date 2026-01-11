import matplotlib.pyplot as plt
from my_project.model import MyAwesomeModel
import pytorch_lightning as pl
from pytorch_lightning import Trainer

model = MyAwesomeModel()
trainer = Trainer()
trainer.fit(model)