import requests as req
from selenium import webdriver
from selenium.webdriver.common.by import By
from time import sleep

options = webdriver.EdgeOptions()
driver = webdriver.Edge(options=options)
driver.get('https://www.pinterest.com/pgoma2016/feng-shui-basics/')

piclist = []
scroll = 0

def getPic():
    global piclist , scroll
    scroll = scroll + 500
    driver.execute_script(f'window.scrollTo(0,{scroll})')
    sleep(0.5)
    pics = driver.find_elements(By.CSS_SELECTOR , 'div[data-test-id="pin"]')
    for i in pics:
        try:
            url = i.get_attribute('src')
            if url not in piclist:
                piclist.append(url)
        except:
            continue
while True:
    getPic()

print(piclist)