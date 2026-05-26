import requests as req
import pandas as pd
url = 'https://rate.bot.com.tw/xrt/flcsv/0/day'
resp = req.get(url)
resp.encoding='utf-8'
data = resp.text
data = data.split('\n')
headers = data[0].split(',')
rows = []


for i in data[1:]:
    if i.strip() == '':
        continue
    try:
        a = i.split(',')
        # 如果欄位數量不等於標題數量，則填充缺失的欄位
        if len(a) != len(headers):
            # 如果欄位數量多於標題數量，則刪除多餘的欄位
            a = a[:len(headers)]
            a.extend([''] * (len(headers) - len(a)))
        rows.append(a)
        # print(a)
        # print(a[0] +':'+ a[2])
    except Exception as e :
        print(f'資料出現即時錯誤: {e}')
        continue
df = pd.DataFrame(rows, columns=headers)
with open('TaiwanRate.csv', 'w' ,encoding='utf-8' ,newline='') as f:
    df.to_csv(f, index=False)
    print('寫入檔案成功')
# print(df)

print('----------------------')