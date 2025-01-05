import requests as req
from bs4 import BeautifulSoup as B

# 伏特加 蘭姆 琴 龍舌蘭 威士忌(蘇格蘭、波本) 白蘭地
base_urls = [
    'https://www.thecocktaildb.com/ingredient/1-Vodka',
    'https://www.thecocktaildb.com/ingredient/2-Gin',
    'https://www.thecocktaildb.com/ingredient/3-Rum',
    'https://www.thecocktaildb.com/ingredient/4-Tequila',
    'https://www.thecocktaildb.com/ingredient/5-Scotch',
    'https://www.thecocktaildb.com/ingredient/71-Bourbonc',
    'https://www.thecocktaildb.com/ingredient/74-Brandy'
    ]
# 初始化空列表存儲名稱與URL
wine_names = []
wine_urls = []

for base_url in base_urls:
    base_resp = req.get(base_url)
    base_soup = B(base_resp.text, 'html.parser')

    for base_wine_urls in base_soup.find_all('td', style="vertical-align:top"):
        a_tag = base_wine_urls.find('a')
        if a_tag:
            base_wine_url = 'https://www.thecocktaildb.com'+ a_tag['href']
            wine_urls.append(base_wine_url)

    for base_wines in base_soup.find_all('figcaption'):
        if base_wines:
            base_wine = base_wines.text.strip()
            wine_names.append(base_wine)

import pandas as pd
df = pd.DataFrame({'Name': wine_names, 'URL': wine_urls})

Ingredients_list = []

for wine_url in df['URL']:
    url = wine_url
    resp = req.get(url)
    soup = B(resp.text, 'html.parser')

    ingredients_list = []

    for ingredients in soup.find_all('figcaption'):
        ingredient_name = ingredients.text.strip()
        ingredients_list.append(ingredient_name)
    
    Ingredients_list.append(','.join(ingredients_list))

df['Ingredients'] = Ingredients_list

import re  # 匯入正則表達式模組
# 逐步清理 Materials 欄位

for index in df.index:
    # 提取原始的 Materials 值
    x = df.loc[index, "Ingredients"]
    
    # 移除數字和單位 (正則匹配並替換)
    cleaned = re.sub(r'\d+(/\d+)?\s*(oz|cl|ml|tsp|cup|dash|shot|slice|part|g|kg|tablespoon|teaspoon)?\s*', '', x, flags=re.IGNORECASE)
    
    # 移除多餘的逗號和空格
    cleaned = ", ".join([item.strip() for item in cleaned.split(",") if item.strip()])
    

    # 更新 DataFrame 中的值
    df.loc[index, "Ingredients"] = cleaned

print(df)
