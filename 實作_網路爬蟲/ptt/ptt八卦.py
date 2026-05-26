import requests as req
from bs4 import BeautifulSoup as B

for i in range(0,4):
    print(f'-----第{i}頁-----')
    url = f'https://www.ptt.cc/bbs/Gossiping/index3889{i}.html'
    resp = req.get(url , cookies={'over18':'1'})

    soup = B(resp.text , 'html.parser') # 利用Beautiful將resp以純文字的方式轉為標籤樹，因此才要對抓取的resp以.text方式轉為原始碼（及純文字）

    titles = soup.find_all('div', 'title') # 取得div標籤中，內容的class為title的文字
    
    for title in titles:
        a_tags = title.find('a')
        href_ = title.find('a')['href']
        if a_tags != None:
            print(a_tags.text + ': ' + 'https://www.ptt.cc/' + href_)

# for x in titles:
#     print(x.find('a').get_text()) #輸出a tag的內容
#     print('https://www.ptt.cc' + x.find('a')['href']) #輸出a tag的內容

# 搜索標題+網址，並寫入ptt.txt中
# with open ('ptt.txt' , 'w' , encoding = 'utf-8') as f:
#     for x in titles:
#         f.write(x.find('a').get_text() +': ' + 'https://www.ptt.cc' + x.find('a')['href'] +'\n')


print('-----------------')
