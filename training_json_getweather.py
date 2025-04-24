import requests as req
import pandas as pd
url = 'https://opendata.cwa.gov.tw/fileapi/v1/opendataapi/F-C0032-001?Authorization=CWA-3D112B04-1126-44EA-85AD-7B29668D3DB5&downloadType=WEB&format=JSON'
resp = req.get(url)
data = resp.json()

# print(resp.text)

# with open('weather.txt' , 'w' , encoding='utf-8') as f :
    # f.write(resp.text)
eight_hours_weather_data = []
lo = data['cwaopendata']['dataset']['location'] #取得位於[][]中的location的資料

for i in lo :
    city = i['locationName']
    wx8 = i['weatherElement'][0]['time'][0]['parameter']['parameterName']
    maxT8 = i['weatherElement'][1]['time'][0]['parameter']['parameterName']
    minT8 = i['weatherElement'][2]['time'][0]['parameter']['parameterName']
    pop8 = i['weatherElement'][4]['time'][0]['parameter']['parameterName']
    # eighth_hour_later_weather_data = f'{city}未來8小時{wx8}，最高溫度{maxT8}，最低溫度{minT8}，降雨機率{pop8}'
    # f.write(eighth_hour_later_weather_data +'\n')
    rows = {'城市': city, 
            '天氣概況': wx8, 
            '最高溫度': maxT8, 
            '最低溫度': minT8, 
            '降雨機率': pop8
            }
    eight_hours_weather_data.append(rows)
df = pd.DataFrame(eight_hours_weather_data, columns=['城市', '天氣概況', '最高溫度', '最低溫度', '降雨機率'])
df.to_csv('Taiwan_8hours_weather.csv', index=False, encoding='utf-8')
print('寫入成功')

