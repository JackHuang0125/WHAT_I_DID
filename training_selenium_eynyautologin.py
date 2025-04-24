from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.select import Select   # 使用 Select 對應下拉選單
# from selenium.webdriver.common.action_chains import ActionChains
from time import sleep

user_agent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 Edg/124.0.0.0'
options = webdriver.EdgeOptions()
options.add_argument('--user-agent=%s' % user_agent)
driver = webdriver.Edge(options=options)
driver.get('https://www71.eyny.com/index.php')

loginbutton = driver.find_element(By.LINK_TEXT , '登錄')
loginbutton.click()
sleep(1)

username_block = driver.find_element(By.NAME , 'username')
username_block.send_keys('boy850125')
sleep(2)
password_block = driver.find_element(By.NAME , 'password')
password_block.send_keys('D5r5e8a6m89012')
sleep(2)
secure_button = driver.find_element(By.NAME , 'questionid')
secure_button.click()
sleep(2)
# secure_question_select = driver.find_element(By.NAME , 'questionid') # 視NAME為questionid的屬性為一'按鈕'，不會將此視為一個'選單'
secure_question_select = Select(driver.find_element(By.NAME , 'questionid')) # 利用select取得'對應的值'的'選單'
secure_question_select.select_by_visible_text('母親的名字')
sleep(2)
secure_answer_block = driver.find_element(By.NAME , 'answer')
secure_answer_block.send_keys('廖秀鳳')
sleep(2)
login_button = driver.find_element(By.NAME , 'loginsubmit')
login_button.click()
sleep(5)

# actions = ActionChains(driver)
# check_box = driver.find_element(By.CSS_SELECTOR , 'cckbox')
# check_box.click()