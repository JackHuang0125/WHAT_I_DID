import requests as req
from bs4 import BeautifulSoup as B
import time
url = 'https://tip.railway.gov.tw/tra-tip-web/tip'

dpart = '花蓮'
arrive = '臺北'

staDic = {}
today = time.strftime('%Y/%m/%d')
stime = '06:00'
etime = '18:00'

def getTirp():
    resp = req.get(url)
    if resp.status_code != 200 :
        print('Error status_code')
        return
    
    soup = B(resp.text ,'html.parser')
    stations = soup.find(id = 'cityHot').find('ul').find_all('li')
    for station in stations:
        stationName = station.find('button').text
        stationId = station.find('button')['title']
        staDic[stationName] = stationId

    csrf = soup.find(id = 'queryForm').find('input' ,{'name':'_csrf'})['value']
    formData = {
        'trainTypeList':'ALL',
        'transfer':'ONE',
        'startOrEndTime':'true',
        'startStation':staDic[dpart],
        'endStation':staDic[arrive],
        'rideDate':today,
        'startTime':stime,
        'endTime':etime,
        '_csrf':csrf
    }
    queryUrl = soup.find(id ='queryForm')['action']
    qResp = req.post('https://tip.railway.gov.tw' +queryUrl ,data=formData)
    qSoup = B(qResp.text ,'html.parser')
    trs = qSoup.find_all('tr' ,'trip-column')
    for tr in trs:
        td = tr.find_all('td')
        print('%s : %s, %s' % (td[0].ul.li.a.text, td[1].text, td[2].text)) 

getTirp()
