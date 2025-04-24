import requests as req
line_url = 'https://today.line.me/tw/v2/comment/article/WBYB1pE'
line_data = req.get(line_url)
article_id = line_data.text.split('<script>')[1].split('id:"article:')[1].split(':')[0] # 擷取文章的articleId
print(article_id)

# 將articleid替換成 article_id (記得f-string) 方便日後不同文章的擷取
comment_url = f'https://today.line.me/webapi/interaction/comment/list?articleId={article_id}&country=TW&postId={article_id}&postType=Article&sort=POPULAR&direction=DESC&limit=60&pivot=0'
comment_data = req.get(comment_url)
comment_json = comment_data.json()
num = int(comment_json['count']) # 將文章留言數由字串轉為整數數字
print(num)

for i in comment_json['comments']:
    try:
        name = i['displayName']
    except:
        name = '???'
    print('留言者'+ '<' + name  + '>'+  '說: ' + i['contents'][0]['extData']['content'])