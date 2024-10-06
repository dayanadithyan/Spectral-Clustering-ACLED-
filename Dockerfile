# Use the official Python 3.11 slim image
FROM python:3.11-slim

# Set environment variables to prevent Python from writing pyc files to disk
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    libpq-dev \
    build-essential \
    curl \
    xz-utils \
    gnupg2 \
    dirmngr \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Poetry for managing Python dependencies
RUN curl -sSL https://install.python-poetry.org | python3 -
ENV PATH="/root/.local/bin:$PATH"

# Set work directory
WORKDIR /src 

# Copy only pyproject.toml and poetry.lock to leverage Docker cache for Python dependencies
COPY pyproject.toml poetry.lock /src/

# Install project dependencies using Poetry
RUN poetry install --no-root --no-dev

# Copy the rest of the application code (Python only)
COPY . /src/

# Expose port if needed (optional)
# EXPOSE 8080

# Command to run Python application (replace with your script)
CMD ["poetry", "run", "python", "main.py"]