import pickle
from datetime import datetime, timezone
from pathlib import Path
from contextlib import asynccontextmanager

from fastapi import BackgroundTasks, FastAPI

DB_PATH = Path(__file__).with_name("prediction_database.csv")
MODEL_PATH = Path(__file__).with_name("model.pkl")

classes = ["Iris-Setosa", "Iris-Versicolour", "Iris-Virginica"]

def add_to_database(
    now: str,
    sepal_length: float,
    sepal_width: float,
    petal_length: float,
    petal_width: float,
    prediction: int,
) -> None:
    with DB_PATH.open("a") as f:
        f.write(f"{now},{sepal_length},{sepal_width},{petal_length},{petal_width},{prediction}\n")

@asynccontextmanager
async def lifespan(app: FastAPI):
    global model
    with MODEL_PATH.open("rb") as f:
        model = pickle.load(f)

    # create db + header if it doesn't exist
    if not DB_PATH.exists():
        DB_PATH.write_text("time,sepal_length,sepal_width,petal_length,petal_width,prediction\n")

    yield

    del model

app = FastAPI(lifespan=lifespan)

@app.post("/predict")
async def iris_inference(
    sepal_length: float,
    sepal_width: float,
    petal_length: float,
    petal_width: float,
    background_tasks: BackgroundTasks,
):
    prediction = model.predict([[sepal_length, sepal_width, petal_length, petal_width]]).item()
    now = datetime.now(timezone.utc).isoformat()

    background_tasks.add_task(
        add_to_database,
        now,
        sepal_length,
        sepal_width,
        petal_length,
        petal_width,
        prediction,
    )
    return {"prediction": classes[prediction], "prediction_int": prediction}
