import requests as req
url = 'https://data.moenv.gov.tw/api/v2/aqx_p_432?api_key=e8dd42e6-9b8b-43f8-991e-b3dee723a52d&limit=1000&sort=ImportDate%20desc&format=JSON'
resp = req.get(url)
data = resp.json()
for x in data['records']:
    print('城市名稱: '+ x['county'] + x['sitename'] , end='')
    print(' , AQI: ' + x['aqi'] , end='')
    print(' , 空氣品質'+x['status'])