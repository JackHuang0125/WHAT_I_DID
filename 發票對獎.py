win = open('中獎發票.txt').read().split()
mine = open('我的發票.txt').read().split()
sn1 = win[0]
sn2 = win[1]
hns = [win[2], win[3], win[4]]
price = {
    7:('二獎', '40000'),
    6:('三獎', '10000'),
    5:('四獎', '4000'),
    4:('五獎', '1000'),
    3:('六獎', '200')
}
total_prize = 0

for i, num in enumerate(mine):
    if num == sn1:
        print('第', i+1, '張發票中特別獎! 獎金1000萬元!', num, '!')
        total_prize += 10000000
        continue
    elif num == sn2:
        print('第', i+1, '張發票中特獎! 獎金200萬元!', num, '!')
        total_prize += 2000000
        continue
    for hn in hns:
            if num == hn:
                print(f'第 {i+1} 張發票中頭獎! 獎金20萬元! {num}!')
                total_prize += 200000
                break
            for length, (price_Name, price_Amount) in price.items():
                if num[-length:] == hn[-length:]:
                    print(f'第 {i+1} 張發票中{price_Name}! 獎金{price_Amount}元! {num} !')
                    total_prize += int(price_Amount)
                    break
            else:
                continue

            break
# print('中獎總金額為: ' + str(total_prize) + '元')
# print(f'中獎總金額為: {total_prize}元')
            

        # elif num[-7:] == hn[-7:]: 
        #     print('第', i+1, '張發票中二獎! 獎金4萬元!', num, '!')
        # elif num[-6:] == hn[-6:]:
        #     print('第', i+1, '張發票中三獎! 獎金1萬元!', num, '!')
        # elif num[-5:] == hn[-5:]:
        #     print('第', i+1, '張發票中四獎! 獎金4千元!', num, '!')
        # elif num[-4:] == hn[-4:]:
        #     print('第', i+1, '張發票中五獎! 獎金1千元!', num, '!') 
