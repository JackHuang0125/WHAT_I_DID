import requests as req
from bs4 import BeautifulSoup as B

url = 'https://www.ptt.cc/bbs/Beauty/M.1723594224.A.E9B.html'
resp = req.get(url , cookies = {'over18':'1'})
soup = B(resp.text , 'html.parser')

# 將html內容以text格式寫入檔案
# with open('pttbeauty' , 'w' , encoding = 'utf-8') as f:
#     f.write(resp.text)

pics = soup.find_all('img')
name = 0

for pic in pics:
    print(pic['src'])
    jpg = req.get(pic['src'])

    with open (f'C:/Users/USER/Documents/beauty/井口裕香_{name}.jpg' , 'wb') as f:
        f.write(jpg.content)
    name += 1