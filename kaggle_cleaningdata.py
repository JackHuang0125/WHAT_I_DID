import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_absolute_error
from sklearn.ensemble import RandomForestRegressor
from sklearn.impute import SimpleImputer

x_data = pd.read_csv('melb_data.csv')
# cols = data.columns
# print(cols)
# print(x_data)
y = x_data.Price

melb_predictors = x_data.drop(['Price'], axis=1)
X = melb_predictors.select_dtypes(exclude=['object'])

X_train, X_valid, y_train, y_valid = train_test_split(X, y, train_size=0.8, test_size=0.2, random_state=0)

def score_dataset(X_train, X_valid, y_train, y_valid):
    model = RandomForestRegressor(n_estimators=10, random_state=0)
    model.fit(X_train, y_train)
    preds = model.predict(X_valid)
    return mean_absolute_error(preds, y_valid)

# 方法1 去掉含有NaN的欄位 進行分析
cols_with_missing = [col for col in X_train.columns if X_train[col].isnull().any()]
reduced_X_train = X_train.drop(cols_with_missing, axis=1)
reduced_X_valid = X_valid.drop(cols_with_missing, axis=1)
print('MAE from approach 1 (Drop columns with missing values):')
print(score_dataset(reduced_X_train, reduced_X_valid, y_train, y_valid))

# 方法2 保留含有NaN的欄位，並將含有NaN的格子中的值替換為預設的平均值
my_imputer = SimpleImputer(strategy='mean')
imputed_X_train = pd.DataFrame(my_imputer.fit_transform(X_train))
imputed_X_valid = pd.DataFrame(my_imputer.transform(X_valid))
imputed_X_train.columns = X_train.columns
imputed_X_valid.columns = X_valid.columns
print('MAE from approach 2 (Drop columns with missing values):')
print(score_dataset(imputed_X_train, imputed_X_valid, y_train, y_valid))

# 方法3 將修改後含有NaN的欄位增加到原本的欄位上
X_train_plus = X_train.copy()
X_valid_plus = X_valid.copy()

for col in cols_with_missing:
    X_train_plus[col + '_was_missing'] = X_train_plus[col].isnull()
    X_valid_plus[col + '_was_missing'] = X_valid_plus[col].isnull()
my_imputer = SimpleImputer()
imputed_X_train_plus = pd.DataFrame(my_imputer.fit_transform(X_train_plus))
imputed_X_valid_plus = pd.DataFrame(my_imputer.fit_transform(X_valid_plus))
print('MAE from approach 3 (An Extension to Imputation):')
print(score_dataset(imputed_X_train_plus, imputed_X_valid_plus, y_train, y_valid))