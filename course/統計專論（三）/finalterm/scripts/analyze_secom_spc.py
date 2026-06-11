import json
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


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
    n_raw = len(raw)
    n_missing = int(raw["x"].isna().sum())
    raw = raw.dropna(subset=["x"]).sort_values("Time").reset_index(drop=True)

    q1, q3 = raw["x"].quantile([0.25, 0.75])
    iqr = q3 - q1
    lower = q1 - 1.5 * iqr
    upper = q3 + 1.5 * iqr
    cleaned = raw[(raw["x"] >= lower) & (raw["x"] <= upper)].reset_index(drop=True)
    cleaned["t"] = np.arange(1, len(cleaned) + 1)

    summary = {
        "variable": VARIABLE,
        "raw_n": n_raw,
        "missing_removed": n_missing,
        "outlier_rule": "global 1.5 IQR filter after dropping missing values",
        "outliers_removed": int(len(raw) - len(cleaned)),
        "clean_n": int(len(cleaned)),
        "mean": float(cleaned["x"].mean()),
        "std": float(cleaned["x"].std(ddof=1)),
        "min": float(cleaned["x"].min()),
        "q1": float(cleaned["x"].quantile(0.25)),
        "median": float(cleaned["x"].median()),
        "q3": float(cleaned["x"].quantile(0.75)),
        "max": float(cleaned["x"].max()),
    }
    return cleaned, summary


def robust_segment_stats(x):
    x = np.asarray(x, dtype=float)
    return np.mean(x), np.std(x, ddof=1), np.median(x), np.median(np.abs(x - np.median(x)))


def max_segment_stat(x, mode):
    x = np.asarray(x, dtype=float)
    n = len(x)
    if n < 12:
        return 0.0, 0
    best = -np.inf
    best_j = 0
    for j in range(6, n - 5):
        left = x[:j]
        right = x[j:]
        if mode == "level":
            scale = np.sqrt(np.var(left, ddof=1) / len(left) + np.var(right, ddof=1) / len(right))
            val = abs(np.mean(left) - np.mean(right)) / scale if scale > 0 else 0.0
        else:
            v0 = np.var(left, ddof=1)
            v1 = np.var(right, ddof=1)
            val = max(v0, v1) / min(v0, v1) if min(v0, v1) > 0 else 0.0
        if val > best:
            best = val
            best_j = j
    return float(best), int(best_j)


def rsp_permutation(phase_i):
    x = np.asarray(phase_i, dtype=float)
    level_obs, level_j = max_segment_stat(x, "level")
    scale_obs, scale_j = max_segment_stat(x, "scale")
    level_perm = np.empty(N_PERM)
    scale_perm = np.empty(N_PERM)
    for b in range(N_PERM):
        xp = RNG.permutation(x)
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


def shewhart(x2, mu, sigma):
    z = (x2 - mu) / sigma
    signal = np.abs(z) > 3
    return z, signal


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


def plot_series(cleaned, m, mu, sigma):
    fig, ax = plt.subplots(figsize=(11, 4.5))
    ax.plot(cleaned["t"], cleaned["x"], lw=0.8, color="#2f6f8f")
    ax.axvline(m, color="#9b2226", ls="--", lw=1.3, label=f"Phase split: {m} / {m + 1}")
    ax.axhline(mu, color="#222222", lw=1, label="Phase I mean")
    ax.axhline(mu + 3 * sigma, color="#b23a48", ls=":", lw=1, label="3-sigma limits")
    ax.axhline(mu - 3 * sigma, color="#b23a48", ls=":", lw=1)
    ax.set_title(f"SECOM {VARIABLE}: cleaned observations and Phase I limits")
    ax.set_xlabel("Cleaned observation index")
    ax.set_ylabel(VARIABLE)
    ax.legend(loc="best", fontsize=8)
    fig.tight_layout()
    fig.savefig(OUT / "series_phase_split.png", dpi=180)
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
    cleaned, summary = load_series()
    x = cleaned["x"].to_numpy()
    n = len(x)
    m = n // 2
    phase_i = x[:m]
    phase_ii = x[m:]
    mu = float(np.mean(phase_i))
    sigma = float(np.std(phase_i, ddof=1))
    rsp = rsp_permutation(phase_i)

    z, sig_s = shewhart(phase_ii, mu, sigma)
    mean_ewma, sig_me, ewma_mean_target, ewma_mean_width = ewma(phase_ii, mu, sigma, kind="mean")
    var_ewma, sig_ve, ewma_var_target, ewma_var_width = ewma(phase_ii, mu, sigma, kind="variance")
    aewma_stat, sig_ae, aewma_h = aewma(phase_ii, mu, sigma)
    cpos, cneg, sig_cm = cusum_mean(phase_ii, mu, sigma)
    vcpos, vcneg, sig_cv = cusum_variance(phase_ii, mu, sigma)
    tmax, bmax, jmax, h_t, h_b, h_j, r_t, r_b, r_j = cpd_monitor(x, start_n=m + 1)
    sig_ct = tmax > h_t
    sig_cb = bmax > h_b
    sig_cj = jmax > h_j

    charts = {
        "shewhart_z": z,
        "mean_ewma": mean_ewma,
        "var_ewma": var_ewma,
        "aewma": aewma_stat,
        "aewma_h": aewma_h,
        "mean_cusum_pos": cpos,
        "mean_cusum_neg": cneg,
        "var_cusum_pos": vcpos,
        "var_cusum_neg": vcneg,
        "tmax": tmax,
        "bmax": bmax,
        "jmax": jmax,
        "h_t": h_t,
        "h_b": h_b,
        "h_j": h_j,
        "ewma_mean_target": ewma_mean_target,
        "ewma_mean_width": ewma_mean_width,
        "ewma_var_target": ewma_var_target,
        "ewma_var_width": ewma_var_width,
    }

    comparison = pd.DataFrame(
        [
            ["Individuals Shewhart", "mean", first_signal(sig_s, m), int(sig_s.sum()), ""],
            ["Mean EWMA", "mean", first_signal(sig_me, m), int(sig_me.sum()), "lambda=0.1, rho=2.454 (ARL0 approx 200)"],
            ["Variance EWMA", "variance", first_signal(sig_ve, m), int(sig_ve.sum()), "lambda=0.1, rhoU=2.595, rhoL=1.580"],
            ["Adaptive EWMA", "mean", first_signal(sig_ae, m), int(sig_ae.sum()), "eta1 case (ii): lambda=0.1813, u=2.5752, h=0.7874"],
            ["Mean CUSUM", "mean", first_signal(sig_cm, m), int(sig_cm.sum()), "standardized k=0.5, h=4.095"],
            ["Variance CUSUM", "variance", first_signal(sig_cv, m), int(sig_cv.sum()), "k_up=1.848, h_up=7.416; k_down=0.462, h_down=-2.446"],
            ["CPD mean chart", "mean", first_signal(sig_ct[m:], m), int(np.nansum(sig_ct[m:])), f"estimated r={r_t[np.nanargmax(np.where(sig_ct, tmax, np.nan))] if np.any(sig_ct) else ''}"],
            ["CPD variance chart", "variance", first_signal(sig_cb[m:], m), int(np.nansum(sig_cb[m:])), f"estimated r={r_b[np.nanargmax(np.where(sig_cb, bmax, np.nan))] if np.any(sig_cb) else ''}"],
            ["CPD joint chart", "mean/variance", first_signal(sig_cj[m:], m), int(np.nansum(sig_cj[m:])), f"estimated r={r_j[np.nanargmax(np.where(sig_cj, jmax, np.nan))] if np.any(sig_cj) else ''}"],
        ],
        columns=["method", "target", "first_signal_index", "signal_count", "setting_or_diagnosis"],
    )
    comparison.to_csv(OUT / "signal_comparison.csv", index=False)

    results = {
        "summary": summary,
        "phase_i_n": int(m),
        "phase_ii_n": int(n - m),
        "phase_i_mean": mu,
        "phase_i_std": sigma,
        "phase_i_min": float(np.min(phase_i)),
        "phase_i_max": float(np.max(phase_i)),
        "phase_ii_mean": float(np.mean(phase_ii)),
        "phase_ii_std": float(np.std(phase_ii, ddof=1)),
        "rsp": rsp,
        "first_signal_table": comparison.where(pd.notna(comparison), "").to_dict(orient="records"),
    }
    (OUT / "analysis_summary.json").write_text(json.dumps(results, indent=2), encoding="utf-8")

    plot_series(cleaned, m, mu, sigma)
    plot_monitoring(cleaned, m, charts)
    print(json.dumps(results, indent=2))


if __name__ == "__main__":
    main()
