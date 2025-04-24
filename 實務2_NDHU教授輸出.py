import requests as req
from bs4 import BeautifulSoup as B
url = 'https://am.ndhu.edu.tw/p/412-1038-2920.php?Lang=zh-tw'
resp = req.get(url)
soup = B(resp.text , 'html.parser')

# list = soup.select('div[id="pageptlist"]') # 使用select選擇'id'屬性為'pageptlist'的<div>的標籤，並存入 list
# with open ('NDHU_pt.txt' , 'w' ,encoding='utf-8') as f:
#     for pt_list in list:
#         pt_name_list = pt_list.find_all('div' , class_='meditor') # 利用迴圈搜尋所有'class'屬性為'meditor'的<div>標籤，並存入 pt_name_list
#         for pt_name in pt_name_list:
#             h3_tag = pt_name.find('h3') # 利用迴圈搭配find再搜尋當中含有<h3>標籤
#             ul_tag = pt_name.find('ul') # 同上找到<ul>標籤
#             if h3_tag: # 確認是否為<h3>
#                 h3_pt_name = h3_tag.get_text() # 是，則將text存入h3_name
#                 if ul_tag:
#                     li_tags = ul_tag.find_all('li') # 找到所有的<li>標籤
#                     pt_pro = li_tags[1] # 取得第二個<li>標籤
#                     pt_email = li_tags[3]
#                     print(h3_pt_name + ' , ' + pt_pro.text + ' , '+ pt_email.text)
#                     f.write(h3_pt_name + ' , ' + pt_pro.text + ' , '+ pt_email.text + '\n')

# a = soup.select('#pageptlist')[0].select('.meditor') # id前面# class前面.
# for b in a:
#     b_name = b.select('h3')[0]
#     b_pro = b.select('li')[1]
#     b_emil = b.select('li')[3]
#     print(f'教授姓名: {b_name.text} ,{b_pro.text} ,{b_emil.text}')     
profList = soup.find(id = 'pageptlist').find_all(class_='row listBS boxSD')


with open('C:/Users/USER/PythonTraining/NDHU_pt.txt' ,'w', encoding = 'utf-8') as f:
    for profinfos in profList:
        profinfo = profinfos.find_all('li')
        profEdu = profinfo[0].text
        profPro = profinfo[1].text
        profEmail = profinfo[3].text
        profName = profinfos.find('h3').text
        print(f'姓名:{profName} ,{profEdu} ,{profPro} ,{profEmail}')
        f.write(f'姓名:{profName} ,學歷:{profEdu} ,專長:{profPro} ,信箱:{profEmail}'+'\n')


print('----------------')