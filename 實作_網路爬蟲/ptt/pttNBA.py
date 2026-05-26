import requests as req
from bs4 import BeautifulSoup as B
import time
from datetime import datetime ,timedelta
# url = 'https://code-gym.github.io/spider_demo/'
# resp = req.get(url)
# soup = B(resp.text ,"html.parser")
# for web_url in soup.find_all("div" ,"nav-logo"):
    # print(web_url.a)
# h3_tags = soup.find_all('h3')

# for h3 in h3_tags:
    # h3_a = h3.a
    # print(h3_a.text)

# nav = soup.find(id = "nav")
# header = nav.parent
# print(header)

# javascript = soup.find("li" ,"cat-2")
# print(javascript)
# print(javascript.previous_sibling)
# print(javascript.next_sibling)

# ul = soup.find('ul')
# for li in ul.children:
    # print(li.find('a').text)

today = time.strftime("%m/%d").lstrip('0')
yesterday = (datetime.now() - timedelta(1)).strftime("%m/%d").lstrip('0')

def pttNBA(url):
    resp = req.get(url)

    if resp.status_code != 200:
        print("url is error: " + url)
        return

    soup = B(resp.text ,"html.parser")

    finishs =[]
    page = soup.find('div' ,'btn-group btn-group-paging').find_all('a')[1]['href']

    rents = soup.find_all('div' ,'r-ent')
    for rent in rents:
        title = rent.find('div' ,'title').text.strip()
        date = rent.find('div' ,'meta').find('div' ,'date').text.strip()
        finish = f"{date}: {title}"
        
        if yesterday == date:
            finishs.append(finish)

    if len(finishs) != 0:
        for finish in finishs:
            print(finish)
        pttNBA("https://www.ptt.cc" + page)
    else:
        return

pttNBA('https://www.ptt.cc/bbs/NBA/index.html')