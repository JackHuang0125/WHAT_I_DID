import json
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy import stats


ROOT = Path(__file__).resolve().parents[1]
REPO = ROOT.parents[2]
DATA_PATH = REPO / "archive" / "raw" / "secom.csv"
OUT = ROOT / "outputs"
OUT.mkdir(exist_ok=True)

VARIABLE = "x297"
ALPHA = 0.005
N_PERM = 399
RNG = np.random.default_rng(611211106)


def load_series():
    df = pd.read_csv(DATA_PATH, sep="\t", parse_dates=["Time"])
    raw = df[["Time", VARIABLE, "Pass/Fail"]].copy()
    raw = raw.rename(columns={VARIABLE: "x"})
    raw = raw.sort_values("Time").reset_index(drop=True)
    raw["t"] = np.arange(1, len(raw) + 1)

    n_raw = len(raw)
    m_raw = n_raw // 2
    phase_i_raw = raw.iloc[:m_raw].copy()
    phase_ii_raw = raw.iloc[m_raw:].copy()

    phase_i_no_missing = phase_i_raw.dropna(subset=["x"]).copy()
    q1, q3 = phase_i_no_missing["x"].quantile([0.25, 0.75])
    iqr = q3 - q1
    lower = q1 - 3.0 * iqr
    upper = q3 + 3.0 * iqr
    phase_i_clean = phase_i_no_missing[
        (phase_i_no_missing["x"] >= lower) & (phase_i_no_missing["x"] <= upper)
    ].copy()
    phase_ii_observed = phase_ii_raw.dropna(subset=["x"]).copy()

    summary = {
        "variable": VARIABLE,
        "raw_n": n_raw,
        "raw_phase_i_n": int(len(phase_i_raw)),
        "raw_phase_ii_n": int(len(phase_ii_raw)),
        "phase_i_missing_removed": int(phase_i_raw["x"].isna().sum()),
        "phase_ii_missing_count_not_preprocessed": int(phase_ii_raw["x"].isna().sum()),
        "outlier_rule": "Phase I only: 3 IQR filter after dropping Phase I missing values",
        "phase_i_outlier_lower": float(lower),
        "phase_i_outlier_upper": float(upper),
        "phase_i_outliers_removed": int(len(phase_i_no_missing) - len(phase_i_clean)),
        "phase_i_clean_n": int(len(phase_i_clean)),
        "phase_ii_observed_n_for_diagnostics": int(len(phase_ii_observed)),
        "phase_i_mean": float(phase_i_clean["x"].mean()),
        "phase_i_std": float(phase_i_clean["x"].std(ddof=1)),
        "phase_i_min": float(phase_i_clean["x"].min()),
        "phase_i_q1": float(phase_i_clean["x"].quantile(0.25)),
        "phase_i_median": float(phase_i_clean["x"].median()),
        "phase_i_q3": float(phase_i_clean["x"].quantile(0.75)),
        "phase_i_max": float(phase_i_clean["x"].max()),
        "phase_ii_raw_mean": float(phase_ii_observed["x"].mean()),
        "phase_ii_raw_std": float(phase_ii_observed["x"].std(ddof=1)),
        "phase_ii_raw_min": float(phase_ii_observed["x"].min()),
        "phase_ii_raw_max": float(phase_ii_observed["x"].max()),
    }
    return raw, phase_i_clean, phase_ii_raw, phase_ii_observed, summary


def robust_segment_stats(x):
    x = np.asarray(x, dtype=float)
    return np.mean(x), np.std(x, ddof=1), np.median(x), np.median(np.abs(x - np.median(x)))


def max_segment_stat(x, mode):
    x = np.asarray(x, dtype=float)
    n = len(x)
    if n < 12:
        return 0.0, 0
    cuts = np.arange(6, n - 5)
    csum = np.cumsum(x)
    csum2 = np.cumsum(x * x)
    total = csum[-1]
    total2 = csum2[-1]

    left_n = cuts
    right_n = n - cuts
    left_sum = csum[cuts - 1]
    right_sum = total - left_sum
    left_mean = left_sum / left_n
    right_mean = right_sum / right_n

    left_sse = csum2[cuts - 1] - (left_sum * left_sum / left_n)
    right_sse = (total2 - csum2[cuts - 1]) - (right_sum * right_sum / right_n)
    left_var = left_sse / (left_n - 1)
    right_var = right_sse / (right_n - 1)

    if mode == "level":
        scale = np.sqrt(left_var / left_n + right_var / right_n)
        vals = np.divide(
            np.abs(left_mean - right_mean),
            scale,
            out=np.zeros_like(scale),
            where=scale > 0,
        )
    else:
        var_min = np.minimum(left_var, right_var)
        var_max = np.maximum(left_var, right_var)
        vals = np.divide(var_max, var_min, out=np.zeros_like(var_max), where=var_min > 0)

    best_idx = int(np.argmax(vals))
    return float(vals[best_idx]), int(cuts[best_idx])


def rsp_permutation(values, seed=611211106):
    x = np.asarray(values, dtype=float)
    rng = np.random.default_rng(seed)
    level_obs, level_j = max_segment_stat(x, "level")
    scale_obs, scale_j = max_segment_stat(x, "scale")
    level_perm = np.empty(N_PERM)
    scale_perm = np.empty(N_PERM)
    for b in range(N_PERM):
        xp = rng.permutation(x)
        level_perm[b], _ = max_segment_stat(xp, "level")
        scale_perm[b], _ = max_segment_stat(xp, "scale")
    p_level = (1 + np.sum(level_perm >= level_obs)) / (N_PERM + 1)
    p_scale = (1 + np.sum(scale_perm >= scale_obs)) / (N_PERM + 1)
    return {
        "p_level": float(p_level),
        "p_scale": float(p_scale),
        "level_stat": float(level_obs),
        "scale_stat": float(scale_obs),
        "level_change_index": int(level_j),
        "scale_change_index": int(scale_j),
    }


def gamma_shewhart_limits(phase_i):
    x = np.asarray(phase_i, dtype=float)
    positive = x[x > 0]
    shape, loc, scale = stats.gamma.fit(positive, floc=0)
    ucl = float(stats.gamma.ppf(1 - ALPHA, shape, loc=loc, scale=scale))
    return {
        "distribution": "gamma",
        "shape": float(shape),
        "loc": float(loc),
        "scale": float(scale),
        "alpha": ALPHA,
        "lcl": 0.0,
        "ucl": ucl,
        "positive_fit_n": int(len(positive)),
        "zero_count_excluded_from_fit": int(np.sum(x == 0)),
    }


def gamma_shewhart(x2, limits):
    x = np.asarray(x2, dtype=float)
    signal = (x < limits["lcl"]) | (x > limits["ucl"])
    return signal


def ewma(x2, mu, sigma, lam=0.1, kind="mean"):
    z = np.empty(len(x2))
    z_prev = mu if kind == "mean" else sigma**2
    target = mu if kind == "mean" else sigma**2
    values = x2 if kind == "mean" else (x2 - mu) ** 2
    for i, val in enumerate(values):
        z_prev = lam * val + (1 - lam) * z_prev
        z[i] = z_prev
    width = 2.454 * sigma * np.sqrt(lam / (2 - lam))
    if kind == "variance":
        se = np.sqrt(2 * lam / (2 - lam))
        lcl = 1 - 1.580 * se
        ucl = 1 + 2.595 * se
        signal = (z < lcl) | (z > ucl)
        return z, signal, 1.0, (lcl, ucl)
    signal = np.abs(z - target) > width
    return z, signal, target, width


def eta1(e, lam, u):
    e = np.asarray(e, dtype=float)
    return np.where(e < -u, e + (1 - lam) * u, np.where(e > u, e - (1 - lam) * u, lam * e))


def aewma(x2, mu, sigma, lam=0.1813, u=2.5752, h=0.7874):
    z = (x2 - mu) / sigma
    a = np.zeros(len(z))
    prev = 0.0
    for i, val in enumerate(z):
        e = val - prev
        a[i] = prev + eta1(e, lam, u)
        prev = a[i]
    signal = np.abs(a) > h
    return a, signal, h


def cusum_mean(x2, mu, sigma, k=0.5, h=4.095):
    cpos = np.zeros(len(x2))
    cneg = np.zeros(len(x2))
    for i, val in enumerate((x2 - mu) / sigma):
        prev_pos = cpos[i - 1] if i else 0.0
        prev_neg = cneg[i - 1] if i else 0.0
        cpos[i] = max(0.0, prev_pos + val - k)
        cneg[i] = min(0.0, prev_neg + val + k)
    signal = (cpos > h) | (cneg < -h)
    return cpos, cneg, signal


def cusum_variance(x2, mu, sigma, k_up=1.848, h_up=7.416, k_down=0.462, h_down=-2.446):
    y = ((x2 - mu) / sigma) ** 2
    cpos = np.zeros(len(x2))
    cneg = np.zeros(len(x2))
    for i, val in enumerate(y):
        prev_pos = cpos[i - 1] if i else 0.0
        prev_neg = cneg[i - 1] if i else 0.0
        cpos[i] = max(0.0, prev_pos + val - k_up)
        cneg[i] = min(0.0, prev_neg + val - k_down)
    signal = (cpos > h_up) | (cneg < h_down)
    return cpos, cneg, signal


def h_mean(n, alpha=ALPHA):
    hstar = {
        0.05: 3.662,
        0.02: 4.371,
        0.01: 4.928,
        0.005: 5.511,
        0.002: 6.340,
        0.001: 7.023,
    }[alpha]
    return hstar * (0.677 + 0.019 * np.log(alpha) + (1 - 0.115 * np.log(alpha)) / (n - 6))


def h_var(n, alpha=ALPHA):
    if alpha == 0.05:
        return 5 + 0.066 * np.log(n - 9)
    return -1.38 - 2.241 * np.log(alpha) + (1.61 + 0.691 * np.log(alpha)) / np.sqrt(n - 9)


def h_joint(n, alpha=ALPHA):
    if alpha == 0.05:
        return 8.43 + 0.074 * np.log(n - 9)
    return 1.58 - 2.52 * np.log(alpha) + (0.094 + 0.33 * np.log(alpha)) / np.sqrt(n - 9)


def cpd_monitor(full_x, start_n):
    n_total = len(full_x)
    tmax = np.full(n_total, np.nan)
    bmax = np.full(n_total, np.nan)
    jmax = np.full(n_total, np.nan)
    h_t = np.full(n_total, np.nan)
    h_b = np.full(n_total, np.nan)
    h_j = np.full(n_total, np.nan)
    r_t = np.zeros(n_total, dtype=int)
    r_b = np.zeros(n_total, dtype=int)
    r_j = np.zeros(n_total, dtype=int)
    for n in range(max(11, start_n), n_total + 1):
        x = full_x[:n]
        vals_t, vals_b, vals_j = [], [], []
        cand_t, cand_b, cand_j = [], [], []
        for r in range(6, n - 5):
            left = x[:r]
            right = x[r:n]
            pooled_sse = np.sum((left - left.mean()) ** 2) + np.sum((right - right.mean()) ** 2)
            pooled = np.sqrt(pooled_sse / (n - 2))
            t_val = np.sqrt(r * (n - r) / n) * abs(left.mean() - right.mean()) / pooled if pooled > 0 else 0.0
            vals_t.append(t_val)
            cand_t.append(r)

            v0 = np.var(left, ddof=1)
            v1 = np.var(right, ddof=1)
            v = pooled_sse / (n - 2)
            crn = 1 + (1 / 3) * (1 / (r - 1) + 1 / (n - r - 1) - 1 / (n - 2))
            b_val = ((r - 1) * np.log(v / v0) + (n - r - 1) * np.log(v / v1)) / crn if v0 > 0 and v1 > 0 else 0.0
            vals_b.append(max(0.0, b_val))
            cand_b.append(r)

            total = np.sum((x - x.mean()) ** 2)
            cstar = 1 + (11 / 12) * (1 / r + 1 / (n - r) - 1 / n)
            if pooled_sse > 0 and total > 0 and r > 1 and n - r > 1:
                se0 = np.sum((left - left.mean()) ** 2)
                se1 = np.sum((right - right.mean()) ** 2)
                j_val = (
                    r * np.log((total / n) / (se0 / (r - 1)))
                    + (n - r) * np.log((total / n) / (se1 / (n - r - 1)))
                ) / cstar
            else:
                j_val = 0.0
            vals_j.append(max(0.0, j_val))
            cand_j.append(r)
        idx_t = int(np.argmax(vals_t))
        idx_b = int(np.argmax(vals_b))
        idx_j = int(np.argmax(vals_j))
        tmax[n - 1] = vals_t[idx_t]
        bmax[n - 1] = vals_b[idx_b]
        jmax[n - 1] = vals_j[idx_j]
        r_t[n - 1] = cand_t[idx_t]
        r_b[n - 1] = cand_b[idx_b]
        r_j[n - 1] = cand_j[idx_j]
        h_t[n - 1] = h_mean(n)
        h_b[n - 1] = h_var(n)
        h_j[n - 1] = h_joint(n)
    return tmax, bmax, jmax, h_t, h_b, h_j, r_t, r_b, r_j


def first_signal(signal, offset):
    idx = np.flatnonzero(signal)
    return None if len(idx) == 0 else int(idx[0] + 1 + offset)


def plot_series(raw, phase_i_clean, m_raw, limits):
    fig, ax = plt.subplots(figsize=(11, 4.5))
    phase_i_raw = raw.iloc[:m_raw]
    phase_ii_raw = raw.iloc[m_raw:]
    ax.scatter(phase_i_raw["t"], phase_i_raw["x"], s=9, color="#4c78a8", alpha=0.7, label="Phase I raw")
    ax.scatter(phase_ii_raw["t"], phase_ii_raw["x"], s=9, color="#f58518", alpha=0.7, label="Phase II raw")
    ax.scatter(
        phase_i_clean["t"],
        phase_i_clean["x"],
        s=12,
        facecolors="none",
        edgecolors="#1b4332",
        linewidths=0.6,
        label="Phase I used for limits",
    )
    ax.axvline(m_raw + 0.5, color="#9b2226", ls="--", lw=1.3, label=f"Raw phase split: {m_raw} / {m_raw + 1}")
    ax.axhline(limits["lcl"], color="#b23a48", ls=":", lw=1, label="Gamma LCL = 0")
    ax.axhline(limits["ucl"], color="#b23a48", ls=":", lw=1, label=f"Gamma UCL ({1 - ALPHA:.3f})")
    ax.set_title(f"SECOM {VARIABLE}: raw phase split and Phase I gamma limit")
    ax.set_xlabel("Raw time-ordered observation index")
    ax.set_ylabel(VARIABLE)
    ax.legend(loc="best", fontsize=8)
    fig.tight_layout()
    fig.savefig(OUT / "series_phase_split.png", dpi=180)
    plt.close(fig)


def plot_phase_ii_gamma(phase_ii_observed, limits, signal):
    fig, ax = plt.subplots(figsize=(11, 4.2))
    ax.plot(phase_ii_observed["t"], phase_ii_observed["x"], lw=0.8, color="#2f6f8f", label="Phase II observed")
    if np.any(signal):
        ax.scatter(
            phase_ii_observed.loc[signal, "t"],
            phase_ii_observed.loc[signal, "x"],
            s=24,
            color="#ae2012",
            label="Above gamma UCL",
            zorder=3,
        )
    ax.axhline(limits["lcl"], color="#b23a48", ls=":", lw=1, label="LCL = 0")
    ax.axhline(limits["ucl"], color="#b23a48", ls="--", lw=1, label="Gamma UCL")
    ax.set_title("Phase II observations against Phase I gamma Shewhart limit")
    ax.set_xlabel("Raw time-ordered observation index")
    ax.set_ylabel(VARIABLE)
    ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(OUT / "phaseii_gamma_shewhart.png", dpi=180)
    plt.close(fig)


def plot_phase_ii_rsp(phase_ii_observed, rsp):
    fig, ax = plt.subplots(figsize=(11, 4.2))
    ax.plot(phase_ii_observed["t"], phase_ii_observed["x"], lw=0.8, color="#2f6f8f")
    level_t = rsp.get("level_change_raw_index")
    scale_t = rsp.get("scale_change_raw_index")
    if level_t:
        ax.axvline(level_t, color="#9b2226", ls="--", lw=1.1, label=f"RS level split, p={rsp['p_level']:.4f}")
    if scale_t:
        ax.axvline(scale_t, color="#6a4c93", ls=":", lw=1.3, label=f"RS scale split, p={rsp['p_scale']:.4f}")
    ax.set_title("Whole Phase II RS/P diagnostic")
    ax.set_xlabel("Raw time-ordered observation index")
    ax.set_ylabel(VARIABLE)
    ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(OUT / "phaseii_rsp_diagnosis.png", dpi=180)
    plt.close(fig)


def plot_monitoring(cleaned, m, charts):
    fig, axes = plt.subplots(5, 1, figsize=(11, 11.5), sharex=True)
    t2 = cleaned["t"].iloc[m:].to_numpy()
    axes[0].plot(t2, charts["shewhart_z"], color="#005f73", lw=0.8)
    axes[0].axhline(3, color="#ae2012", ls="--", lw=1)
    axes[0].axhline(-3, color="#ae2012", ls="--", lw=1)
    axes[0].set_ylabel("Shewhart z")

    axes[1].plot(t2, charts["mean_ewma"], color="#0a9396", lw=0.8, label="EWMA")
    axes[1].axhline(charts["ewma_mean_target"] + charts["ewma_mean_width"], color="#ae2012", ls="--", lw=1)
    axes[1].axhline(charts["ewma_mean_target"] - charts["ewma_mean_width"], color="#ae2012", ls="--", lw=1)
    axes[1].set_ylabel("Mean EWMA")

    axes[2].plot(t2, charts["aewma"], color="#7b2cbf", lw=0.8, label="AEWMA")
    axes[2].axhline(charts["aewma_h"], color="#ae2012", ls="--", lw=1)
    axes[2].axhline(-charts["aewma_h"], color="#ae2012", ls="--", lw=1)
    axes[2].set_ylabel("AEWMA")

    axes[3].plot(t2, charts["mean_cusum_pos"], color="#0077b6", lw=0.8, label="C+")
    axes[3].plot(t2, charts["mean_cusum_neg"], color="#ca6702", lw=0.8, label="C-")
    axes[3].axhline(4.095, color="#ae2012", ls="--", lw=1)
    axes[3].axhline(-4.095, color="#ae2012", ls="--", lw=1)
    axes[3].set_ylabel("Mean CUSUM")
    axes[3].legend(fontsize=8)

    n = cleaned["t"].to_numpy()
    axes[4].plot(n, charts["tmax"], color="#005f73", lw=0.8, label="Tmax,n")
    axes[4].plot(n, charts["h_t"], color="#ae2012", ls="--", lw=0.9, label="h_n")
    axes[4].set_ylabel("CPD mean")
    axes[4].set_xlabel("Cleaned observation index")
    axes[4].legend(fontsize=8)

    fig.suptitle("Phase II mean-shift monitoring charts", y=0.995)
    fig.tight_layout()
    fig.savefig(OUT / "monitoring_mean_methods.png", dpi=180)
    plt.close(fig)

    mean_panels = [
        ("monitoring_mean_shewhart.png", "Shewhart z"),
        ("monitoring_mean_ewma.png", "Mean EWMA"),
        ("monitoring_mean_aewma.png", "AEWMA"),
        ("monitoring_mean_cusum.png", "Mean CUSUM"),
        ("monitoring_mean_cpd.png", "CPD mean"),
    ]
    for source_ax, (filename, ylabel) in zip(axes, mean_panels):
        single_fig, single_ax = plt.subplots(figsize=(10, 3.2))
        for line in source_ax.get_lines():
            xdata = line.get_xdata()
            ydata = line.get_ydata()
            if len(xdata) == 2 and np.allclose(xdata, [0, 1]) and np.allclose(ydata[0], ydata[1]):
                single_ax.axhline(
                    ydata[0],
                    color=line.get_color(),
                    lw=line.get_linewidth(),
                    ls=line.get_linestyle(),
                    label=line.get_label(),
                )
            else:
                single_ax.plot(
                    xdata,
                    ydata,
                    color=line.get_color(),
                    lw=line.get_linewidth(),
                    ls=line.get_linestyle(),
                    label=line.get_label(),
                )
        single_ax.set_xlabel("Cleaned observation index")
        single_ax.set_ylabel(ylabel)
        single_ax.set_xlim(source_ax.get_xlim())
        handles, labels = single_ax.get_legend_handles_labels()
        visible = [(h, label) for h, label in zip(handles, labels) if not label.startswith("_")]
        if visible:
            single_ax.legend(*zip(*visible), fontsize=8)
        single_fig.tight_layout()
        single_fig.savefig(OUT / filename, dpi=180)
        plt.close(single_fig)

    fig, axes = plt.subplots(3, 1, figsize=(11, 7.5), sharex=True)
    axes[0].plot(t2, charts["var_ewma"], color="#0a9396", lw=0.8)
    axes[0].axhline(charts["ewma_var_width"][1], color="#ae2012", ls="--", lw=1)
    axes[0].axhline(charts["ewma_var_width"][0], color="#ae2012", ls="--", lw=1)
    axes[0].set_ylabel("Variance EWMA")

    axes[1].plot(t2, charts["var_cusum_pos"], color="#0077b6", lw=0.8, label="C+")
    axes[1].plot(t2, charts["var_cusum_neg"], color="#ca6702", lw=0.8, label="C-")
    axes[1].axhline(7.416, color="#ae2012", ls="--", lw=1)
    axes[1].axhline(-2.446, color="#ae2012", ls="--", lw=1)
    axes[1].set_ylabel("Variance CUSUM")
    axes[1].legend(fontsize=8)

    n = cleaned["t"].to_numpy()
    axes[2].plot(n, charts["bmax"], color="#005f73", lw=0.8, label="Bmax,n")
    axes[2].plot(n, charts["h_b"], color="#ae2012", ls="--", lw=0.9, label="variance h_n")
    axes[2].plot(n, charts["jmax"], color="#6a4c93", lw=0.8, label="Jmax,n")
    axes[2].plot(n, charts["h_j"], color="#bb3e03", ls="--", lw=0.9, label="joint h_n")
    axes[2].set_ylabel("CPD B/J")
    axes[2].set_xlabel("Cleaned observation index")
    axes[2].legend(fontsize=8)

    fig.suptitle("Phase II variance and joint monitoring charts", y=0.995)
    fig.tight_layout()
    fig.savefig(OUT / "monitoring_variance_joint_methods.png", dpi=180)
    plt.close(fig)

    variance_panels = [
        ("monitoring_variance_ewma.png", "Variance EWMA"),
        ("monitoring_variance_cusum.png", "Variance CUSUM"),
        ("monitoring_cpd_variance_joint.png", "CPD B/J"),
    ]
    for source_ax, (filename, ylabel) in zip(axes, variance_panels):
        single_fig, single_ax = plt.subplots(figsize=(10, 3.2))
        for line in source_ax.get_lines():
            xdata = line.get_xdata()
            ydata = line.get_ydata()
            if len(xdata) == 2 and np.allclose(xdata, [0, 1]) and np.allclose(ydata[0], ydata[1]):
                single_ax.axhline(
                    ydata[0],
                    color=line.get_color(),
                    lw=line.get_linewidth(),
                    ls=line.get_linestyle(),
                    label=line.get_label(),
                )
            else:
                single_ax.plot(
                    xdata,
                    ydata,
                    color=line.get_color(),
                    lw=line.get_linewidth(),
                    ls=line.get_linestyle(),
                    label=line.get_label(),
                )
        single_ax.set_xlabel("Cleaned observation index")
        single_ax.set_ylabel(ylabel)
        single_ax.set_xlim(source_ax.get_xlim())
        handles, labels = single_ax.get_legend_handles_labels()
        visible = [(h, label) for h, label in zip(handles, labels) if not label.startswith("_")]
        if visible:
            single_ax.legend(*zip(*visible), fontsize=8)
        single_fig.tight_layout()
        single_fig.savefig(OUT / filename, dpi=180)
        plt.close(single_fig)

    cpd_split_panels = [
        ("monitoring_cpd_variance.png", "CPD variance", [charts["bmax"], charts["h_b"]], ["Bmax,n", "variance h_n"], ["#005f73", "#ae2012"], ["-", "--"], [0.8, 0.9]),
        ("monitoring_cpd_joint.png", "CPD joint", [charts["jmax"], charts["h_j"]], ["Jmax,n", "joint h_n"], ["#6a4c93", "#bb3e03"], ["-", "--"], [0.8, 0.9]),
    ]
    for filename, ylabel, series, labels, colors, linestyles, widths in cpd_split_panels:
        single_fig, single_ax = plt.subplots(figsize=(10, 3.2))
        for ydata, label, color, linestyle, width in zip(series, labels, colors, linestyles, widths):
            single_ax.plot(n, ydata, color=color, ls=linestyle, lw=width, label=label)
        single_ax.set_xlabel("Cleaned observation index")
        single_ax.set_ylabel(ylabel)
        single_ax.set_xlim(axes[2].get_xlim())
        single_ax.legend(fontsize=8)
        single_fig.tight_layout()
        single_fig.savefig(OUT / filename, dpi=180)
        plt.close(single_fig)


def main():
    raw, phase_i_clean, phase_ii_raw, phase_ii_observed, summary = load_series()
    m_raw = summary["raw_phase_i_n"]
    phase_i = phase_i_clean["x"].to_numpy()
    phase_ii = phase_ii_observed["x"].to_numpy()

    phase_i_rsp = rsp_permutation(phase_i, seed=611211106)
    phase_ii_rsp = rsp_permutation(phase_ii, seed=611211107)
    phase_ii_rsp["level_change_raw_index"] = int(
        phase_ii_observed["t"].iloc[phase_ii_rsp["level_change_index"] - 1]
    ) if phase_ii_rsp["level_change_index"] else None
    phase_ii_rsp["scale_change_raw_index"] = int(
        phase_ii_observed["t"].iloc[phase_ii_rsp["scale_change_index"] - 1]
    ) if phase_ii_rsp["scale_change_index"] else None

    limits = gamma_shewhart_limits(phase_i)
    sig_gamma = gamma_shewhart(phase_ii, limits)

    comparison = pd.DataFrame(
        [
            [
                "Individuals Shewhart",
                "upper tail",
                first_signal(sig_gamma, m_raw),
                int(sig_gamma.sum()),
                f"Gamma UCL={limits['ucl']:.4f}; LCL=0; Phase I-only 3 IQR preprocessing",
            ],
            [
                "Whole Phase II RS/P",
                "level",
                "",
                "",
                f"p_level={phase_ii_rsp['p_level']:.4f}; split raw index={phase_ii_rsp['level_change_raw_index']}",
            ],
            [
                "Whole Phase II RS/P",
                "scale",
                "",
                "",
                f"p_scale={phase_ii_rsp['p_scale']:.4f}; split raw index={phase_ii_rsp['scale_change_raw_index']}",
            ],
            [
                "EWMA/CUSUM/CPD",
                "not used",
                "",
                "",
                "Excluded for Phase II limits because the variable is strongly right-skewed and violates the normality-based assumptions used here",
            ],
        ],
        columns=["method", "target", "first_signal_index", "signal_count", "setting_or_diagnosis"],
    )
    comparison.to_csv(OUT / "signal_comparison.csv", index=False)

    results = {
        "summary": summary,
        "gamma_shewhart_limits": limits,
        "phase_i_rsp": phase_i_rsp,
        "phase_ii_rsp": phase_ii_rsp,
        "gamma_shewhart_first_signal_raw_index": first_signal(sig_gamma, m_raw),
        "gamma_shewhart_signal_count": int(sig_gamma.sum()),
        "excluded_phase_ii_methods": ["EWMA", "CUSUM", "CPD"],
        "exclusion_reason": (
            "The selected variable is strongly right-skewed, so normality-based EWMA, CUSUM, "
            "and CPD control limits are not used for Phase II monitoring in this revision."
        ),
        "first_signal_table": comparison.where(pd.notna(comparison), "").to_dict(orient="records"),
    }
    (OUT / "analysis_summary.json").write_text(json.dumps(results, indent=2), encoding="utf-8")

    plot_series(raw, phase_i_clean, m_raw, limits)
    plot_phase_ii_gamma(phase_ii_observed, limits, sig_gamma)
    plot_phase_ii_rsp(phase_ii_observed, phase_ii_rsp)
    print(json.dumps(results, indent=2))


if __name__ == "__main__":
    main()
