import requests as req
earth_web = 'https://opendata.cwa.gov.tw/api/v1/rest/datastore/E-A0016-001?Authorization=CWA-3D112B04-1126-44EA-85AD-7B29668D3DB5&format=JSON'
earth_resp = req.get(earth_web)
earth_json = earth_resp.json()
earth_dic = earth_json['records']['Earthquake']

for i in earth_dic:
    earth_depth = i['EarthquakeInfo']['FocalDepth']
    earth_time = i['EarthquakeInfo']['OriginTime']
    earth_lo = i['EarthquakeInfo']['Epicenter']['Location']
    earth_val = i['EarthquakeInfo']['EarthquakeMagnitude']['MagnitudeValue']
    earth_url = i['ReportImageURI']
    earth_mes = f'{earth_lo} , 芮氏規模{earth_val} , 深度{earth_depth}公里 , 發生時間{earth_time}'
    url = 'https://notify-api.line.me/api/notify'
    token = 'QeBgBohTDk8dV70HHdWZPknFAKQZ6yCvYAUMTMyuNHe'
    headers = { 'Authorization':'Bearer '+token}
    data ={'Message': earth_mes , 'imageThumbnail':earth_url , 'imageFullsize':earth_url}
    data = req.post(url , data=data , headers=headers)
    break # 強制for執行一次後停止程序，即只發送最新訊息