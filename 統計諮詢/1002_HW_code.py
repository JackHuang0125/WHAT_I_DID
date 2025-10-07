# package
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import statsmodels.formula.api as smf
from scipy import stats
from sklearn.model_selection import train_test_split
from statsmodels.stats.anova import anova_lm

# read data
data = pd.read_csv(r"C:\Users\No\Documents\R\data\processed\therapy_recovery_clean.csv").drop(columns=['patient_id', 'admit_date', 'discharge_date', 'discharge_date_fix'])
data.head()

# check data status
data.info()

# plot
plt.bar(x=data['therapy'].value_counts().index, height=data['therapy'].value_counts().values)


# plot
sns.kdeplot(data=data, x='los_final_days')
plt.show()
sns.kdeplot(data=data, x='age_years')
plt.show()
sns.boxplot(data=data, x='age_years', hue='therapy')
plt.show()

sns.boxplot(data=data, x='los_final_days', hue='therapy')
plt.show()

sns.boxplot(data=data, x='los_final_days', hue='sex')
plt.show()

sns.boxplot(data=data, x='los_final_days', hue='comorbidity')
plt.show()

sns.scatterplot(data=data, x='age_years', y='los_final_days', hue='therapy')
plt.show()

sns.scatterplot(data=data, x='age_years', y='los_final_days', hue='sex')
plt.show()

sns.scatterplot(data=data, x='age_years', y='los_final_days', hue='comorbidity')
plt.show()

# comorbidity boxplot
plt.figure()
order = ['none','mild','severe'] if set(['none','mild','severe']).issubset(set(data['comorbidity'].unique())) else sorted(data['comorbidity'].unique())
groups = [data.loc[data['comorbidity']==g, 'los_final_days'].values for g in order]
plt.boxplot(groups, labels=order, showmeans=True)
plt.xlabel("comorbidity")
plt.ylabel("los_final_days")
plt.tight_layout()
plt.show()

# ANOVA
# 變異數檢定
# lev_stat, lev_p = stats.levene(*groups, center='median')
# print(f"\nLevene test: stat={lev_stat:.4f}, p={lev_p:.4f}")

# 單因子 ANOVA
model = smf.ols("los_final_days ~ C(comorbidity)", data=data).fit()
anova_tbl = anova_lm(model, typ=2)
print("\n=== One-way ANOVA ===")
print(anova_tbl)

SS_between = anova_tbl.loc['C(comorbidity)', 'sum_sq']
SS_resid   = anova_tbl.loc['Residual', 'sum_sq']
eta_sq = SS_between / (SS_between + SS_resid)
print(f"eta^2 = {eta_sq:.3f}")

# Ordinary least square
formula = "los_final_days ~ C(sex, Treatment('M'))+C(therapy, Treatment('std'))+C(comorbidity, Treatment('none'))"
ols = smf.ols(formula=formula, data=data).fit(cov_type='nonrobust')
print(ols.summary())


