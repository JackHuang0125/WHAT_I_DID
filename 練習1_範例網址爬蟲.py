import requests as req
from bs4 import BeautifulSoup as B
url = 'https://www.iana.org/domains'
resp = req.get(url)
resp.encoding='utf-8'
soup = B(resp.text ,'html.parser')
print(soup)
