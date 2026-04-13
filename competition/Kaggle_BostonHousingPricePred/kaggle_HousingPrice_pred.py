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

data = pd.read_csv("Housing Prices_train.csv")
data.head()
data.drop(columns=['Id'], inplace=True)

for col in data.columns:
    if data[col].dtype == "object":
        data[col].fillna("None", inplace=True)
    else:
        data[col].fillna(0, inplace=True)

data_encoded = pd.get_dummies(data, drop_first=True)
bool_cols = data_encoded.select_dtypes(include=['bool']).columns
data_encoded[bool_cols] = data_encoded[bool_cols].astype(int)

X = data_encoded
y = data_encoded['SalePrice']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)

pca = PCA(n_components=0.95)
X_train_pca = pca.fit_transform(X_train_scaled)
X_test_pca = pca.transform(X_test_scaled)

explained_variance = pca.explained_variance_ratio_
cumulative_variance = np.cumsum(explained_variance)

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

rf_model = RandomForestRegressor(n_estimators=100, random_state=42, max_depth=10, min_samples_leaf=10, min_samples_split=10)
rf_model.fit(X_train_pca, y_train)
y_pred = rf_model.predict(X_test_pca)

mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

print("Results：")
print(f"MSE: {mse}")
print(f"R²: {r2}")

plt.figure(figsize=(10,6))
plt.scatter(y_test, y_pred, color='b', marker='.', alpha=0.5)
plt.xlabel('Actual')
plt.ylabel('Predicted')
plt.title('HousePrice Actual vs Predicted')
plt.grid()
plt.legend()
plt.show()

test_data = pd.read_csv('Housing Prices_test.csv')

test_ids = test_data['Id'] 

test_data.drop(columns=['Id'], inplace=True)

for col in test_data.columns:
    if test_data[col].dtype == "object":
        test_data[col].fillna("None", inplace=True)
    else:
        test_data[col].fillna(0, inplace=True)

test_data_encoded = pd.get_dummies(test_data, drop_first=True)

test_data_encoded = test_data_encoded.reindex(columns=X_train.columns, fill_value=0)

bool_cols = test_data_encoded.select_dtypes(include=['bool']).columns
test_data_encoded[bool_cols] = test_data_encoded[bool_cols].astype(int)

test_data_scaled = scaler.transform(test_data_encoded)
test_data_pca = pca.transform(test_data_scaled)
test_predictions = rf_model.predict(test_data_pca)

submission = pd.DataFrame({"Id": test_ids, "SalePrice": test_predictions})
submission.set_index("Id", inplace=True)

print(submission)

submission.to_csv('HousingPricePred_PCA&RF.csv')
