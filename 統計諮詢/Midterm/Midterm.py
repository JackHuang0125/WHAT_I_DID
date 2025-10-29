# %% 匯入模組
from pathlib import Path
notebook_dir = Path().resolve()
data_path = notebook_dir.parent / 'therapy_recovery_clean.csv'
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import statsmodels.formula.api as smf
from scipy import stats
from sklearn.model_selection import train_test_split
from statsmodels.stats.anova import anova_lm

# %% 匯入資料
data = pd.read_csv(data_path).drop(columns=['patient_id', 'discharge_date', 'discharge_date_fix'])

# %% 繪圖
data["admit_date"] = pd.to_datetime(data['admit_date'], errors="coerce")
plot_df = data.loc[:, ["admit_date", 'los_final_days', 'therapy']]
plt.figure(figsize=(6, 4))
sns.scatterplot(data=plot_df, x="admit_date", y='los_final_days', hue='therapy', hue_order=['std', 'new'])
plt.xlabel("Admit date")
plt.ylabel("LOS (days)")
plt.grid(axis="y", alpha=0.4)
plt.axhline(y=15, color='r', linestyle='--', label='Target: 15days')
plt.legend()
plt.tight_layout()
plt.show()


# %% 敘述統計
data.groupby('therapy')['los_final_days'].describe()

# %% 結果一 新舊療法 T-test
lfd_new = data.loc[data['therapy']=='new', 'los_final_days']
lfd_std = data.loc[data['therapy']=='std', 'los_final_days']

t_stat, p_val = stats.ttest_ind(lfd_new, lfd_std, equal_var=False)

mean_new, mean_std = lfd_new.mean(), lfd_std.mean()
diff = mean_new-mean_std

s1, s2 = lfd_new.std(ddof=1), lfd_std.std(ddof=1)
n1, n2 = len(lfd_new), len(lfd_std)
se_diff = np.sqrt((s1**2/n1)+(s2**2/n2))
df_welch = ((s1**2/n1+s2**2/n2)**2)/((s1**4/(n1**2*(n1-1)))+(s2**4/(n2**2*(n2-1))))
t_crit = stats.t.ppf(0.975, df_welch)
ci_low = diff-t_crit*se_diff
ci_up = diff+t_crit*se_diff
print(f"Mean(std) = {mean_std:.4f} days")
print(f"Mean(new) = {mean_new:.4f} days")
print(f"平均差異 Δ = {diff:.4f} 天 (new − std)")
print(f"95% CI = [{ci_low:.4f}, {ci_up:.4f}]")
print(f"t = {t_stat:.4f}, data = {df_welch:.1f}, p = {p_val:.4f}")

summary = (data.groupby('therapy')['los_final_days'].agg(n="count", mean="mean", sd=lambda s: s.std(ddof=1)).reset_index())

# t臨界值與CI半寬
summary["se"] = summary["sd"] / np.sqrt(summary["n"])
summary["tcrit"] = summary.apply(lambda r: stats.t.ppf(0.975, df=int(r["n"]-1)), axis=1)
summary["ci_half"] = summary["tcrit"] * summary["se"]
summary["ci_low"] = summary["mean"] - summary["ci_half"]
summary["ci_up"]  = summary["mean"] + summary["ci_half"]

order = ["std", "new"]
plot_df = summary.set_index('therapy').loc[order].reset_index()

m_new, m_std = plot_df.loc[plot_df['therapy']=="new","mean"].item(), plot_df.loc[plot_df['therapy']=="std","mean"].item()
s_new, s_std = plot_df.loc[plot_df['therapy']=="new","sd"].item(),   plot_df.loc[plot_df['therapy']=="std","sd"].item()
n_new, n_std = plot_df.loc[plot_df['therapy']=="new","n"].item(),    plot_df.loc[plot_df['therapy']=="std","n"].item()

fig, ax = plt.subplots(figsize=(5, 3))
x = np.arange(len(order))
y = plot_df["mean"].values
yerr = plot_df["ci_half"].values

ax.errorbar(x, y, yerr=yerr, fmt='o')
ax.set_xticks(x)
ax.set_xticklabels(["Standard", "New"])
ax.set_ylabel("Mean LOS (days)")
ax.grid(axis='y')

for xi, m, lo, up in zip(x, plot_df["mean"], plot_df["ci_low"], plot_df["ci_up"]):
    label = f"Mean={m:.2f}\n95% CI [{lo:.2f}, {up:.2f}]"
    ax.annotate(label,
                (xi, m),
                textcoords="offset points",
                xytext=(15, -10),  # 文字相對於點往上 12px
                ha="left")
    
plt.tight_layout()
plt.show()

# %% 結果 II ANOVA

data['age_band'] = data['age_band'].replace({'40-64 歲':'middle', '65 歲以上':'old', '<40 歲':'young'})

df = data.loc[:, ['therapy', 'comorbidity', 'los_final_days', 'age_band']].dropna().copy()
df['therapy'] = pd.Categorical(df['therapy'], categories=['std', 'new'])
df['comorbidity'] = pd.Categorical(df['comorbidity'], categories=['none', 'mild', 'severe'])
df['age_band'] = pd.Categorical(df['age_band'], categories=['young', 'middle', 'old'])

# 含交互作用的二因子 ANOVA
mod_full = smf.ols('los_final_days ~ C(therapy) * C(comorbidity) * C(age_band)', data=df).fit()
anova_full = anova_lm(mod_full, typ=2)   # typ=2 常用；你也可改 typ=3
print(anova_full)

g = df.groupby(["age_band", "comorbidity"])["los_final_days"]
summary = g.agg(mean="mean", sd="std", n="count").reset_index()
summary["se"] = summary["sd"] / np.sqrt(summary["n"].replace(0, np.nan))
summary["ci95"] = 1.96 * summary["se"]
summary["lower95"] = summary["mean"] - summary["ci95"]
summary["upper95"] = summary["mean"] + summary["ci95"]

age_levels = ['young', 'middle', 'old']
cb_levels = ['none', 'mild', 'severe']

x = np.arange(len(cb_levels))

fig1, ax1 = plt.subplots(figsize=(6, 4))
for j, age in enumerate(age_levels):
    sub = summary[summary["age_band"] == age]
    means = []
    errs = []
    for cb in cb_levels:
        row = sub[sub["comorbidity"] == cb]
        means.append(float(row["mean"].iloc[0]))
    positions = x + (j - (len(age_levels)-1)/2) * 0.26
    ax1.bar(x=positions, height=means, width=0.25, label=age, capsize=4)
    
ax1.set_xlabel("Comorbidity")
ax1.set_ylabel("Mean LOS (days)")
ax1.set_xticks(x)
ax1.set_xticklabels(cb_levels)
ax1.legend(title="Age Band")
ax1.grid(axis="y", alpha=0.3)
fig1.tight_layout()


