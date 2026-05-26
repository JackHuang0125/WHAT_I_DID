import requests as req
from bs4 import BeautifulSoup as B
url = 'https://invoice.etax.nat.gov.tw/index.html'
resp = req.get(url)
resp.encoding='utf-8'

if resp.status_code != 200:
    print('Error status_code!!')

soup = B(resp.text, 'html.parser')
# 找出特別獎、頭獎號碼
n = soup.find(class_="etw-table-bgbox etw-tbig").find_all(class_="font-weight-bold etw-color-red")
ns = n[0].text
n1 = n[1].text
# 找出頭獎號碼 並且用list包起來
n2s = soup.find(class_="etw-table-bgbox etw-tbig").find_all(class_="etw-tbiggest mb-md-4")
n2 = [ n2s[0].text[-8:], n2s[1].text[-8:], n2s[2].text[-8:] ]

while True:
    try:
        num = input('請輸入發票號碼: ')
        if not int(num) or len(num) != 8:
            print('請輸入正確的發票號碼（8位數字）')
            continue

        if num == ns:
            print('特別獎!對中1000萬!')
            continue
        if num == n1:
            print('特獎!對中200萬!')
            continue
        for i in n2:
            if num == i:
                print('頭獎!對中20萬!')
                break
            elif num[-7:] == i[-7:]:
                print('二獎!對中4萬!')
                break
            elif num[-6:] == i[-6:]:
                print('三獎!對中1萬!')
                break
            elif num[-5:] == i[-5:]:
                print('四獎!對中4千!')
                break
            elif num[-4:] == i[-4:]:
                print('五獎!對中1千!')
                break
            elif num[-3:] == i[-3:]:
                print('六獎!對中2百!')
                break
        else:
            print('很遺憾未中獎')
    except ValueError: # 輸入整數以外的數字則執行except的程序
        print('輸入錯誤，請輸入數字')
print('---------------------------------')