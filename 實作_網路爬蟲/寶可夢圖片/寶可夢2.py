import requests as req
from bs4 import BeautifulSoup as B

url = 'https://tw.portal-pokemon.com/play/pokedex/1006'
resp = req.get(url)
soup = B(resp.text , 'html.parser')

pic = soup.select('meta[property ="og:image"]') # select只搜索第一個且唯一的標籤，標籤後的屬性以[]搜索

print(pic[0]['content'])
data = req.get(pic[0]['content']) # 重新向該網址索取html內容，最後再轉為content並用write寫入

with open ('C:/Users/USER/Documents/beauty/pokemonpic_test0.png' , 'wb') as f: # 以wb二進制數據寫入文件 
    f.write(data.content)
print('-----------------------')