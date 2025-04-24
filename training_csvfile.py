import csv
import requests as req

# csvfile = open('001.csv' , encoding='utf-8') # 含中文故用encoding解碼
# read = csv.reader(csvfile)
# for row in list(read):
#     print(row)

# csvfile2 = open('csv-demo.csv') # 無中文不用解碼
# r = csv.reader(csvfile2)
# for rr in list(r):
    # print(rr)

# with open('001.csv', newline='' , encoding='utf-8') as csvfile:
#     r = csv.reader(csvfile, delimiter=' ', quotechar=' ')
#     for row in r:
#         print(', '.join(row))

csvfile = open('csv-demo.csv' , 'a+') # open使用a+可以讀取檔案並寫入資料在最後一行，但若最後一行不為空，則會寫入在資料後方；故會多寫一空行，使資料可以寫入在新的最後一行
r = csv.writer(csvfile)
# r.writerow('')
# r.writerow(['999' , '99' , '99999'])
# r.writerow('')
# r.writerow(['banana' , 5 , 'yellow' , 99])

# data = [['banana' , 6 , 'yellow' ,99],['guava' , 7 , 'green' , 100]]
# r.writerows(data)

csvfile = open('Taiwanair.csv' , 'w')
csvfile_r = csv.writer(csvfile)

url ='https://data.moenv.gov.tw/api/v2/aqx_p_432?api_key=e8dd42e6-9b8b-43f8-991e-b3dee723a52d&limit=1000&sort=ImportDate desc&format=JSON'
resp = req.get(url)
data = resp.json()
title = [['country' , 'sitename' , 'aqi' , '空氣品質']]
for i in data['records']:
    title.append([i['county'],i['sitename'],i['aqi'],i['status']])
print(title)
csvfile_r.writerows(title)