import requests as req
from bs4 import BeautifulSoup as B
from concurrent.futures import ThreadPoolExecutor

stock_urls = ['https://tw.stock.yahoo.com/quote/2330.TW' , 'https://tw.stock.yahoo.com/quote/2303.TW' , 'https://tw.stock.yahoo.com/quote/2449.TW'
              ,'https://tw.stock.yahoo.com/quote/2454.TW' , 'https://tw.stock.yahoo.com/quote/2369.TW'
              ] 

# url = 'https://tw.stock.yahoo.com/quote/2330'

def getstockdata(url):
    resp = req.get(url)
    soup = B(resp.text , 'html.parser')
    title = soup.find('h1' , class_='C($c-link-text)') # 正常的網頁裡，id 和 h1 都只會存在一個 ( 不會有兩個重複的 id 或重複的 h1 )
    price = soup.select('.Fz\(32px\)')[0]
    range = soup.select('.Mend\(4px\)')[0]
    status = ''
    
    #若照下面的程式碼設定status，只會判定漲跌，平盤不會顯示
    # try:
    #     if soup.select('#main-0-QuoteHeader-Proxy')[0].select('.C\(\$c-trend-up\)')[0]: # class中有特殊符號，要用\轉譯
    #         status = '+'
    # except:
    #     if soup.select('#main-0-QuoteHeader-Proxy')[0].select('.C\(\$c-trend-down\)')[0]:
    #         status = '-'

    try:
        if soup.select('#main-0-QuoteHeader-Proxy')[0].select('.C\(\$c-trend-up\)')[0]:
            status = '+'
    except:
        try:
            if soup.select('#main-0-QuoteHeader-Proxy')[0].select('.C\(\$c-trend-down\)')[0]:
                status = '-'
        except:
            status = ''

    print(f'股票名稱: {title.text} , 價格: {price.text} , 漲幅度: {status}{range.text}')
executor = ThreadPoolExecutor()
with ThreadPoolExecutor() as executor:
    executor.map(getstockdata , stock_urls)
