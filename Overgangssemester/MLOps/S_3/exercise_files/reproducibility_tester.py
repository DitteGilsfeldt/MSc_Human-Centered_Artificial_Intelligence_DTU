import sys

import torch
from model import Decoder, Encoder, Model  # noqa: F401

if __name__ == "__main__":
    print(sys.argv)

    exp1 = sys.argv[1]
    exp2 = sys.argv[2]

    print(f"Comparing run {exp1} to {exp2}")

sd1 = torch.load(f"{exp1}/trained_model.pt")
sd2 = torch.load(f"{exp2}/trained_model.pt")

for k in sd1:
    if not torch.allclose(sd1[k], sd2[k]):
        raise RuntimeError(
            f"encountered a difference in parameter {k}, your script is not fully reproducible"
        )