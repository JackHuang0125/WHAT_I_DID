import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

np.random.seed(42)

data = np.random.rand(200, 5)
df = pd.DataFrame(data, columns=['F1', 'F2', 'F3', 'F4', 'F5'])

scaler = StandardScaler()
data_scaled = scaler.fit_transform(df)

pca = PCA()
data_pca = pca.fit_transform(data_scaled)

explained_variance_ratio = pca.explained_variance_ratio_
print('各主成分解釋的變異比例: ')
print(explained_variance_ratio)

cumulative_variance = np.cumsum(explained_variance_ratio)
print('累積解釋變異比例: ')
print(cumulative_variance)

plt.figure(figsize=(8, 6))
plt.plot(range(1, len(cumulative_variance) + 1), cumulative_variance, 'o', linestyle = '--')
plt.xlabel('主成分數量')
plt.ylabel('累積解釋變異比例')
plt.title('PCA 累積解釋變異比例')
plt.axhline(y = 0.95, color = 'r', linestyle = '--', label = '95%解釋變異')
plt.legend()
plt.grid()
plt.show()

n_component_95 = np.argmax(cumulative_variance >= 0.95) + 1
print(f'保留至少95%變異需要的主成分數量:{n_component_95}')

pca_95 = PCA(n_components=n_component_95)
data_pca_95 = pca.fit_transform(data_scaled)
print('降維後的數據形狀:', data_pca_95)