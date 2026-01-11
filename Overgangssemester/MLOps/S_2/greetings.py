import typer

app = typer.Typer()

@app.command()
def hello(count: int = 1, name: str = "Lucia"):
    for _ in range(count):
        typer.echo(f"Yo {name}!")

if __name__ == "__main__":
    app()