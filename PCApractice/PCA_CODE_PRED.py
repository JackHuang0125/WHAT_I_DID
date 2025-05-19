import pandas as pd
data = pd.read_csv("Housing Prices_train.csv")
data.head()
data.drop(columns=['Id'], inplace=True)

# 處理缺失值（填充數值型變數的中位數，類別變數填充 'None'）
for col in data.columns:
    if data[col].dtype == "object":
        data[col].fillna("None", inplace=True)
    else:
        data[col].fillna(data[col].median(), inplace=True)

# one-hot coding 以及將布林值轉為0/1
data_encoded = pd.get_dummies(data, drop_first=True)
bool_cols = data_encoded.select_dtypes(include=['bool']).columns
data_encoded[bool_cols] = data_encoded[bool_cols].astype(int)

import numpy as np
from sklearn.preprocessing import StandardScaler
# 進行標準化（不包含目標變數 SalePrice）
scaler = StandardScaler()
data_scaled = scaler.fit_transform(data_encoded.drop(columns=["SalePrice"]))
y = data_encoded["SalePrice"]  # 目標變數

# 進行PCA並計算影響Y的變異
from sklearn.decomposition import PCA
pca = PCA()
principal_components = pca.fit_transform(data_scaled)
explained_variance = pca.explained_variance_ratio_
cumulative_variance = np.cumsum(explained_variance)

# 視覺化解釋變異比例
import matplotlib.pyplot as plt
plt.figure(figsize=(10, 6))
plt.plot(range(1, len(explained_variance) + 1), explained_variance, marker='o', linestyle='--', label="Individual Explained Variance")
plt.plot(range(1, len(cumulative_variance) + 1), cumulative_variance, marker='s', linestyle='-', label="Cumulative Explained Variance")
plt.axhline(y=0.95, color='r', linestyle='--', label="95% Variance Explained")
plt.xlabel('Number of Principal Components')
plt.ylabel('Explained Variance Ratio')
plt.title('PCA Explained Variance')
plt.legend()
plt.grid()
plt.show()

# 篩選出累積變異比例達95%的主成分數量
num_components = np.argmax(cumulative_variance >= 0.95) + 1

# 取得主要的主成分負荷量
principal_components = pca.components_[:num_components]
pc_df = pd.DataFrame(principal_components.T, 
                            index=data_encoded.drop(columns=["SalePrice"]).columns, 
                            columns=[f'PC{i+1}' for i in range(num_components)])

import seaborn as sns

# 計算每個變數對於前幾個主要主成分的影響程度（取絕對值最高的特徵）
top_pcs = pc_df.iloc[:, :10]  # 取前10個主要主成分

# 找出每個主成分影響最大的前10個特徵
top_features_dict = {pc: top_pcs[pc].abs().nlargest(10).index.tolist() for pc in top_pcs.columns}

# 轉換為DataFrame
top_features_df = pd.DataFrame(list(top_features_dict.items()), columns=["Principal Component", "Top Features"])

# 視覺化主成分與特徵的關係
plt.figure(figsize=(12, 8))
sns.heatmap(top_pcs, cmap="coolwarm", annot=False, linewidths=0.5)
plt.title("Feature Loadings for Top 10 Principal Components")
plt.xlabel("Principal Components")
plt.ylabel("Features")
plt.show()

from collections import Counter

# 展平所有主成分的前 10 大特徵
all_top_features = [feature for features in top_features_dict.values() for feature in features]

# 統計出現頻率
feature_counts = Counter(all_top_features)

# 篩選出現次數最多的前 20 個變數
most_influential_features = pd.DataFrame(feature_counts.most_common(20), columns=["Feature", "Count"])
# 提取原始數據中的最具影響力的特徵
influential_features_data = data_encoded[most_influential_features["Feature"]]

# 可視化這些特徵的分佈（箱型圖）
plt.figure(figsize=(14, 8))
sns.boxplot(data=influential_features_data, orient="h", showfliers=False)
plt.title("Distribution of Most Influential Features")
plt.xlabel("Feature Values")
plt.show()

from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score

# 定義特徵和目標變數
X = data_encoded[most_influential_features["Feature"]]
y = data_encoded["SalePrice"]

# 切分訓練集與測試集（80% 訓練，20% 測試）
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# 隨機森林回歸模型
rf_model = RandomForestRegressor(n_estimators=100, random_state=42)
rf_model.fit(X_train, y_train)
y_pred_rf = rf_model.predict(X_test)

# 計算模型評估指標
def evaluate_model(y_true, y_pred, model_name):
    mae = mean_absolute_error(y_true, y_pred)
    mse = mean_squared_error(y_true, y_pred)
    rmse = np.sqrt(mse)
    r2 = r2_score(y_true, y_pred)
    return pd.Series({"Model": model_name, "MAE": mae, "MSE": mse, "RMSE": rmse, "R²": r2})

# 匯總結果
results = pd.DataFrame([
    evaluate_model(y_test, y_pred_rf, "Random Forest")
])

# 讀取測試數據
test_data = pd.read_csv('Housing Prices_test.csv')

# 保留 Id 供最終輸出
test_ids = test_data['Id'] 
# 移除 Id 欄位（若存在）
test_data.drop(columns=['Id'], inplace=True)

# 處理缺失值（填充數值型變數的中位數，類別變數填充 'None'）
for col in test_data.columns:
    if test_data[col].dtype == "object":
        test_data[col].fillna("None", inplace=True)
    else:
        test_data[col].fillna(test_data[col].median(), inplace=True)

# one-hot coding 以及將布林值轉為0/1
test_data_encoded = pd.get_dummies(test_data, drop_first=True)
bool_cols = test_data_encoded.select_dtypes(include=['bool']).columns
test_data_encoded[bool_cols] = test_data_encoded[bool_cols].astype(int)

# 確保測試數據的欄位與訓練數據匹配（若有缺少的欄位則補0）
missing_cols = set(X.columns) - set(test_data_encoded.columns)
for col in missing_cols:
    test_data_encoded[col] = 0

# 確保欄位順序一致
test_data_encoded = test_data_encoded[X.columns]

# 進行預測
test_predictions = rf_model.predict(test_data_encoded)

# 建立提交結果
submission = pd.DataFrame({"Id": test_ids, "SalePrice": test_predictions})
submission.set_index("Id", inplace=True)

print(submission)

submission.to_csv('HousingPricePred3.csv')
