import requests as req
from bs4 import BeautifulSoup as B
import threading as thr
import os

# def downloadpokemonpic(num):
#     try:
#         url = f'https://tw.portal-pokemon.com/play/pokedex/{num}'
#         resp = req.get(url)
#         soup = B(resp.text, 'html.parser')
#         pics = soup.find_all('img' , class_='pokemon-img__front')

#         for pic in pics:
#             print('https://tw.portal-pokemon.com' + pic['src'])
#             jpg = req.get('https://tw.portal-pokemon.com' + pic['src'])
#         with open('C:/Users/USER/Documents/beauty/pokenmon{num}.jpg' , 'wb') as f:
#             f.write(jpg.content)
#     except:
#         print('worng')
#         pass

# for i in range(1,100):
#     n = f'{i:04d}'
#     thr.Thread(target=downloadpokemonpic ,args=(n,)).start()

def downloadpokemonpics(num):
    try:
        url = f'https://tw.portal-pokemon.com/play/pokedex/{num}'
        resp = req.get(url)
        soup = B(resp.text, 'html.parser')
        pics = soup.select('meta[property="og:image"]')
        pic = pics[0]['content']
        picpng = req.get(pic)
        with open(f'C:/Users/USER/Documents/Pokemonpictures/pokenmon{num}.png' , 'wb') as f:
            f.write(picpng.content)

    except:
        print(f'{num}wrong')
        pass

for i in range(0,1026):
    num = f'{i:04d}'
    thr.Thread(target=downloadpokemonpics , args=(num,)).start()


print('---------------')