import pandas as pd
import matplotlib.pyplot as plt
import yfinance as yf
import json as js
import requests as req

apple = yf.Ticker('AAPL')
url ='https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-PY0220EN-SkillsNetwork/data/apple.json'
resp = req.get(url)
apple_info = resp.json()
# with open('C:/Users/USER/PythonTraining/apple_info.json' ,'w') as js_file:
    # js.dump(apple_info, js_file)
with open('C:/Users/USER/PythonTraining/apple_info.json') as js_file:
    apple_data = js.load(js_file)
# print(apple_data)
# print(apple_info['country'])
apple_share_price_data = apple.history(period="ytd")
# print(apple_share_price_data)
# print(apple_share_price_data.head())
apple_share_price_data.reset_index(inplace=True)
# apple_share_price_data.plot(x="Date", y="Open")
apple_share_price_data = apple_share_price_data.loc['1990-01-01':]
# print(apple_share_price_data)
plt.figure(figsize=(30 ,7))
plt.plot(apple_share_price_data["Date"] ,apple_share_price_data["Open"] ,color='k')
plt.xlabel('Date')
plt.ylabel('Opne Price')
plt.title('Apple Stock Price')
plt.show()