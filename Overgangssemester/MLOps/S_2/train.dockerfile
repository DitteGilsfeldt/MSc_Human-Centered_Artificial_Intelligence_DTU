# Base image
FROM ghcr.io/astral-sh/uv:python3.12-bookworm-slim

RUN apt update && \
    apt install --no-install-recommends -y build-essential gcc && \
    apt clean && rm -rf /var/lib/apt/lists/*

COPY pyproject.toml pyproject.toml
COPY src/ src/
COPY data/ data/
COPY uv.lock uv.lock

WORKDIR /
ENV UV_LINK_MODE=copy
RUN --mount=type=cache,target=/root/.cache/uv uv sync --locked


ENTRYPOINT ["uv", "run", "python", "-u", "src/my_project/train.py"]

# ENV UV_LINK_MODE=copy
# RUN --mount=type=cache,target=/root/.cache/uv uv syncs