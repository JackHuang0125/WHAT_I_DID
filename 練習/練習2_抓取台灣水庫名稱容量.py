import requests as req
url = 'https://water.taiwanstat.com/'
resp = req.get(url)

from bs4 import BeautifulSoup as B
soup = B(resp.text , 'html.parser') # 利用Beautifulsoup將文字檔轉為標籤樹，後續利用Beautifulsoup模組抓取標籤

# reserviors = soup.find_all('div' , class_='reservoir') 
# # reserviors = soup.select('.reservoir') # 同上，只是更簡潔的寫法

# for name in reserviors:
#     print(name.find('div' , class_ = 'name' ).get_text() , end = '' ) # print會自動換行，多一個end使其不會自動換行
#     print(name.find('div' , class_ = 'volumn' ).get_text()  , end = '')
#     print(name.find('h5').get_text())
#    print() #多加一個print() 會使迴圈自動換行

# select , find_all 用法一樣，但回應的是列表，故要用迴圈遍歷列表以印出我們想要的資料
# dam = soup.select('div[class="name"]')
# for name in dam:
    # print(name.text)
# dam = soup.find_all('div' , class_='name')
# for name in dam:
    # print(name.text)

# dam = soup.select('div[class="volumn"]')
# for volumn in dam:
    # print(volumn.text)

dam = soup.select('div[class="reservoir"]')
with open('dam.txt' , 'w' , encoding='utf-8') as f:
    for name_volumn in dam:
        f.write('水庫名稱: '+ name_volumn.find('div' ,class_='name').text +', ' + "水庫容量: " + name_volumn.find('div' , class_='volumn').text + '\n')    
    # print('水庫名稱: '+ name_volumn.find('div' ,class_='name').text +', ' + "水庫容量: " + name_volumn.find('div' , class_='volumn').text)
    print('寫入檔案成功!')