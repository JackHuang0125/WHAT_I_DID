# 1. 載入必要的套件
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from collections import Counter

from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score

# 2. 讀取與前處理訓練資料
data = pd.read_csv("Housing Prices_train.csv")
data.drop(columns=['Id'], inplace=True)

# 處理缺失值：數值型變數以中位數填補，類別變數填入 'None'
for col in data.columns:
    if data[col].dtype == "object":
        data[col].fillna("None", inplace=True)
    else:
        data[col].fillna(data[col].median(), inplace=True)

# one-hot encoding（drop_first=True 避免共線性）及布林值轉換
data_encoded = pd.get_dummies(data, drop_first=True)
bool_cols = data_encoded.select_dtypes(include=['bool']).columns
data_encoded[bool_cols] = data_encoded[bool_cols].astype(int)

# 分離目標變數與特徵
y = data_encoded["SalePrice"]
X = data_encoded.drop(columns=["SalePrice"])

# 3. 對特徵進行標準化
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# 4. 執行 PCA 分析
pca = PCA()
X_pca_transformed = pca.fit_transform(X_scaled)
explained_variance = pca.explained_variance_ratio_
cumulative_variance = np.cumsum(explained_variance)

# 繪製單一主成分解釋變異與累積變異比例
plt.figure(figsize=(10, 6))
plt.plot(range(1, len(explained_variance)+1), explained_variance, marker='o', linestyle='--', label="Individual Explained Variance")
plt.plot(range(1, len(cumulative_variance)+1), cumulative_variance, marker='s', linestyle='-', label="Cumulative Explained Variance")
plt.axhline(y=0.95, color='r', linestyle='--', label="95% Variance Explained")
plt.xlabel('Number of Principal Components')
plt.ylabel('Explained Variance Ratio')
plt.title('PCA Explained Variance')
plt.legend()
plt.grid()
plt.show()

# 根據累積變異選取主成分數量（至少解釋 95% 變異）
num_components = np.argmax(cumulative_variance >= 0.95) + 1
print(f"選取 {num_components} 個主成分以解釋至少 95% 的變異。")

# 取得 PCA 的主成分負荷量，並將變數命名清楚
# pca.components_ 為 (主成分數, 原始變數數) 陣列，因此需轉置
pca_loadings = pd.DataFrame(pca.components_.T, 
                            index=X.columns,
                            columns=[f'PC{i+1}' for i in range(pca.components_.shape[0])])

# 僅取前 num_components 的主成分負荷量
selected_loadings = pca_loadings.iloc[:, :num_components]

# 篩選出累積變異比例達95%的主成分數量
num_components = np.argmax(cumulative_variance >= 0.95) + 1

# 取得主要的主成分負荷量
principal_components = pca.components_[:num_components]
pc_df = pd.DataFrame(principal_components.T, 
                            index=data_encoded.drop(columns=["SalePrice"]).columns, 
                            columns=[f'PC{i+1}' for i in range(num_components)])

# 5. 依據各主成分中絕對值最大的前 10 個特徵進行特徵選取
top_features_dict = {pc: selected_loadings[pc].abs().nlargest(10).index.tolist() 
                     for pc in selected_loadings.columns}

# 將各主成分的前 10 大特徵展平，並統計各變數出現頻率
all_top_features = [feature for features in top_features_dict.values() for feature in features]
feature_counts = Counter(all_top_features)

# 選出出現頻率最高的前 20 個變數
most_influential_features = pd.DataFrame(feature_counts.most_common(20), columns=["Feature", "Count"])
selected_features = most_influential_features["Feature"].tolist()
print("選出的最具影響力的特徵：")
print(most_influential_features)

# 可視化這些特徵在各主成分負荷量中的分佈（以 heatmap 呈現）
plt.figure(figsize=(12, 8))
sns.heatmap(selected_loadings.loc[selected_features], cmap="coolwarm", annot=True, linewidths=0.5)
plt.title("Selected Features Loadings for Principal Components")
plt.xlabel("Principal Components")
plt.ylabel("Features")
plt.show()

# 從原始特徵中取出最具影響力的變數資料
X_selected = X[selected_features]

# 6. 切分訓練集與測試集，並建立隨機森林回歸模型
X_train, X_test, y_train, y_test = train_test_split(X_selected, y, test_size=0.2, random_state=42)

rf_model = RandomForestRegressor(n_estimators=100, random_state=42)
rf_model.fit(X_train, y_train)
y_pred_rf = rf_model.predict(X_test)

# 定義模型評估函數
def evaluate_model(y_true, y_pred, model_name):
    mae = mean_absolute_error(y_true, y_pred)
    mse = mean_squared_error(y_true, y_pred)
    rmse = np.sqrt(mse)
    r2 = r2_score(y_true, y_pred)
    return pd.Series({"Model": model_name, "MAE": mae, "MSE": mse, "RMSE": rmse, "R²": r2})

results = pd.DataFrame([evaluate_model(y_test, y_pred_rf, "Random Forest")])
print("模型評估結果：")
print(results)

# 7. 處理測試資料
test_data = pd.read_csv("Housing Prices_test.csv")
test_ids = test_data['Id']
test_data.drop(columns=['Id'], inplace=True)

for col in test_data.columns:
    if test_data[col].dtype == "object":
        test_data[col].fillna("None", inplace=True)
    else:
        test_data[col].fillna(test_data[col].median(), inplace=True)

test_data_encoded = pd.get_dummies(test_data, drop_first=True)
bool_cols_test = test_data_encoded.select_dtypes(include=['bool']).columns
test_data_encoded[bool_cols_test] = test_data_encoded[bool_cols_test].astype(int)

# 補齊測試資料中缺少的欄位，使其與訓練資料 X 保持一致
missing_cols = set(X.columns) - set(test_data_encoded.columns)
for col in missing_cols:
    test_data_encoded[col] = 0

# 重新排列欄位順序
test_data_encoded = test_data_encoded[X.columns]

# 依據選取的特徵進行預測
X_test_final = test_data_encoded[selected_features]
test_predictions = rf_model.predict(X_test_final)

# 建立提交結果檔案
submission = pd.DataFrame({"Id": test_ids, "SalePrice": test_predictions})
submission.set_index("Id", inplace=True)
print(submission)

submission.to_csv('HousingPricePred2.csv')
