FROM python:3.14.6-slim

ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1
ENV KERAS_BACKEND=torch

WORKDIR /app

# quadprog needs a C compiler on Linux
RUN apt-get update \
    && apt-get install -y --no-install-recommends build-essential \
    && rm -rf /var/lib/apt/lists/*

COPY requirements-deploy.txt .

# CPU-only PyTorch
RUN python -m pip install --no-cache-dir \
    torch==2.13.0+cpu \
    --index-url https://download.pytorch.org/whl/cpu

RUN python -m pip install --no-cache-dir -r requirements-deploy.txt

COPY model/main.py /app/model/main.py
COPY model/model_functions.py /app/model/model_functions.py
COPY model/portfolio_optimization_functions.py /app/model/portfolio_optimization_functions.py

COPY model/models/full_fit /app/model/models/full_fit
COPY model/ml_ready_data/fulldata /app/model/ml_ready_data/fulldata

WORKDIR /app/model

CMD ["sh", "-c", "uvicorn main:app --host 0.0.0.0 --port ${PORT:-8080}"]