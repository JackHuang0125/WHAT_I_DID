# benchmarks/benchmark_streaming_local_regression.py

import gc
import time
import tracemalloc
from dataclasses import dataclass

import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression


@dataclass
class BenchmarkResult:
    method: str
    n_samples: int
    n_features: int
    batch_size: int | None
    runtime_sec: float
    peak_memory_mb: float
    r2: float


def measure_peak_memory_and_time(func, *args, **kwargs):
    gc.collect()
    tracemalloc.start()

    start = time.perf_counter()
    output = func(*args, **kwargs)
    runtime = time.perf_counter() - start

    current, peak = tracemalloc.get_traced_memory()
    tracemalloc.stop()

    peak_memory_mb = peak / 1024 / 1024

    return output, runtime, peak_memory_mb


def generate_batch(
    batch_size: int,
    n_features: int,
    beta: np.ndarray,
    rng: np.random.Generator,
    noise_std: float = 0.1,
):
    Xb = rng.normal(size=(batch_size, n_features)).astype(np.float64)
    yb = Xb @ beta + rng.normal(scale=noise_std, size=batch_size)

    return Xb, yb


def traditional_generate_all_then_fit(
    n_samples: int,
    n_features: int,
    beta: np.ndarray,
    random_state: int = 42,
):
    rng = np.random.default_rng(random_state)

    X = rng.normal(size=(n_samples, n_features)).astype(np.float64)
    y = X @ beta + rng.normal(scale=0.1, size=n_samples)

    model = LinearRegression()
    model.fit(X, y)

    y_pred = model.predict(X)
    sse = np.sum((y - y_pred) ** 2)
    sst = np.sum((y - y.mean()) ** 2)
    r2 = 1.0 - sse / sst

    return model.coef_, r2


def streaming_batch_fit(
    n_samples: int,
    n_features: int,
    beta: np.ndarray,
    batch_size: int = 10_000,
    random_state: int = 42,
    ridge_alpha: float = 1e-8,
):
    rng = np.random.default_rng(random_state)

    p = n_features + 1
    xtx = np.zeros((p, p), dtype=np.float64)
    xty = np.zeros(p, dtype=np.float64)
    yty = 0.0
    sum_y = 0.0
    n_seen = 0

    remaining = n_samples

    while remaining > 0:
        current_batch_size = min(batch_size, remaining)

        Xb, yb = generate_batch(
            batch_size=current_batch_size,
            n_features=n_features,
            beta=beta,
            rng=rng,
        )

        ones = np.ones((current_batch_size, 1), dtype=np.float64)
        X_design = np.hstack([ones, Xb])

        xtx += X_design.T @ X_design
        xty += X_design.T @ yb
        yty += float(yb.T @ yb)
        sum_y += float(yb.sum())
        n_seen += current_batch_size

        remaining -= current_batch_size

        del Xb, yb, X_design, ones

    penalty = ridge_alpha * np.eye(p)
    penalty[0, 0] = 0.0

    beta_all = np.linalg.solve(xtx + penalty, xty)

    sse = yty - 2.0 * beta_all.T @ xty + beta_all.T @ xtx @ beta_all
    y_mean = sum_y / n_seen
    sst = yty - n_seen * y_mean**2
    r2 = 1.0 - sse / sst

    coef = beta_all[1:]

    return coef, r2


def run_streaming_benchmark(
    n_samples: int = 2_000_000,
    n_features: int = 500,
    batch_size: int = 10_000,
    random_state: int = 42,
):
    rng = np.random.default_rng(random_state)
    true_beta = rng.normal(size=n_features).astype(np.float64)

    traditional_output, traditional_time, traditional_peak = measure_peak_memory_and_time(
        traditional_generate_all_then_fit,
        n_samples,
        n_features,
        true_beta,
        random_state,
    )

    batch_output, batch_time, batch_peak = measure_peak_memory_and_time(
        streaming_batch_fit,
        n_samples,
        n_features,
        true_beta,
        batch_size,
        random_state,
    )

    traditional_coef, traditional_r2 = traditional_output
    batch_coef, batch_r2 = batch_output

    df = pd.DataFrame(
        [
            {
                "method": "Traditional generate-all + OLS",
                "n_samples": n_samples,
                "n_features": n_features,
                "batch_size": None,
                "runtime_sec": traditional_time,
                "peak_memory_mb": traditional_peak,
                "r2": traditional_r2,
            },
            {
                "method": "Streaming batch sufficient statistics",
                "n_samples": n_samples,
                "n_features": n_features,
                "batch_size": batch_size,
                "runtime_sec": batch_time,
                "peak_memory_mb": batch_peak,
                "r2": batch_r2,
            },
        ]
    )

    memory_reduction = (1.0 - batch_peak / traditional_peak) * 100
    runtime_change = (batch_time / traditional_time - 1.0) * 100
    coef_l2_error = np.linalg.norm(batch_coef - traditional_coef)
    r2_diff = abs(batch_r2 - traditional_r2)

    print(df)
    print()
    print(f"Peak memory reduction: {memory_reduction:.2f}%")
    print(f"Runtime change: {runtime_change:.2f}%")
    print(f"Coefficient L2 error: {coef_l2_error:.6e}")
    print(f"R2 difference: {r2_diff:.6e}")

    return df


if __name__ == "__main__":
    run_streaming_benchmark(
        n_samples=2_000_000,
        n_features=500,
        batch_size=100_000,
    )
