# %%
import random
import numpy as np
import pandas as pd

# %%
# 正常抽獎
def normal_draw(if_win=False):
    r = random.random() # 0 ~ 1

    if if_win == False:
        first_p = 0.05
        seceond_p = 0.10
    else:
        first_p = 0.03
        seceond_p = 0.06

    if r < first_p:
        return '頭獎'
    elif r < first_p + seceond_p:
        return '貳獎'
    else:
        return '未中獎'

# %%
# 保底抽獎
def guaranteed_draw():
    r = random.random()
    if r < 1/3:
        return '頭獎' # 0.33 中頭獎
    else:
        return '貳獎' # 0.66 中貳獎

# %%
# 帳號狀態清單 可更動 {編號: 是否為新手期, 新手期已抽次數, 新手期是否已中獎, 非新手期連續未中獎次數, 上次是否中獎}
accounts = {
    '1':{'is_new_player': True, "new_draws": 0, "new_has_won": False, "lose_conti": 0, "last_win": False},
    '2':{'is_new_player': True, "new_draws": 2, "new_has_won": False, "lose_conti": 0, "last_win": False},
    '3':{'is_new_player': True, "new_draws": 3, "new_has_won": True, "lose_conti": 0, "last_win": False},
    '4':{'is_new_player': True, "new_draws": 2, "new_has_won": True, "lose_conti": 0, "last_win": False},
}

# 遊戲機制
def play_game(account, times): # (帳號, 遊玩次數)
    
    if account not in accounts:
        # 新手期：前 5 期至少中 1 次
        accounts[account] = {
            'is_new_player': True,
            'new_draws': 0, # 新手期已抽幾次
            'new_has_won': False, # 新手期是否已中過
            'lose_conti': 0, # 非新手期連續未中次數
            'last_win': False
        }
        print(f'帳號：{account}，新建立玩家資料')
    else:
        print(f'帳號：{account}，已存在玩家資料')

    player = accounts[account]
    
    if player['new_draws'] >= 5:
        player['is_new_player'] = False

    # 帳號狀態確認
    if player['is_new_player']:
        if player['new_has_won']:
            print('目前狀態：新手期，但您已中獎，不會再觸發新手保底')
        else:
            print('目前狀態：新手期，尚未中獎，仍具有新手保底')
    else:
        print('目前狀態：非新手期，您不具有新手保底')

    print(f'開始遊玩{times}次')

    jackpot_nums = 0
    second_nums = 0
    rewards = [] # 紀錄每次獎金
    
    for i in range(1, times+1):
        trigger = '正常抽獎'
        mes = False
        
        # 新手期 前 5 抽至少中 1
        if player['is_new_player'] and player['new_draws'] < 5:
            if player['new_draws'] == 4 and player['new_has_won'] == False:
                result = guaranteed_draw()
                trigger = '新手保底'
            else:
                result = normal_draw(if_win=player['last_win'])

            player['new_draws'] = player['new_draws'] + 1

            if result != '未中獎':
                if player['new_has_won'] == False:
                    mes = True
                player['new_has_won'] = True
                player['is_new_player'] = False

            elif player['new_draws'] == 5:
                player['is_new_player'] = False
        
        # 非新手期 20 抽至少中 1
        else:
            if player['lose_conti'] >= 19:
                result = guaranteed_draw()
                trigger = '20抽大保'
            else:
                result = normal_draw(if_win=player['last_win'])

            if result == '未中獎':
                player['lose_conti'] = player['lose_conti'] + 1
                player['last_win'] = False
            else:
                player['lose_conti'] = 0
                player['last_win'] = True

        if result == '頭獎':
            reward = 500
            text = '頭獎 500 元'
            jackpot_nums = jackpot_nums + 1
        elif result == '貳獎':
            reward = 200
            text = '貳獎 200元'
            second_nums = second_nums + 1
        else:
            reward = 0
            text = '未中獎 再接再厲'

        rewards.append(reward)

        print(f'第{i}次：{text}，觸發{trigger}')

        if mes:
            print('您已不是新手期，之後不具有新手保底')
            
    print('遊玩結束後玩家狀態：')
    print(player)
    
    n = len(rewards)
    z = 1.96

    # 獎金期望值估計
    rewards = np.array(rewards)
    x_bar = rewards.mean()
    s = rewards.std(ddof=1)
    se = s / (n**(1/2))
    REWARD_CI_UPPER = x_bar + z * se
    REWARD_CI_LOWER = x_bar - z * se
    
    # 頭獎比例估計
    jackpot_p = jackpot_nums / n
    jackpot_se = ((jackpot_p*(1-jackpot_p))/n)**(1/2)
    JACKPOT_CI_UPPER = jackpot_p + z * jackpot_se
    JACKPOT_CI_LOWER = jackpot_p - z * jackpot_se

    # 貳獎比例估計
    second_p = second_nums / n
    second_se = ((second_p*(1-second_p))/n)**(1/2)
    SECOND_CI_UPPER = second_p + z * second_se
    SECOND_CI_LOWER = second_p - z * second_se

    summary = pd.DataFrame(
        {
            '帳號': [account],
            '樣本數': [n],
            '獎金期望值估計':[f'{x_bar:.2f}'],
            '獎金CI下界': [round(REWARD_CI_LOWER, 2)],
            '獎金CI上界': [round(REWARD_CI_UPPER, 2)],

            '頭獎比例': [round(jackpot_p, 2)],
            '頭獎CI下界': [round(JACKPOT_CI_LOWER, 2)],
            '頭獎CI上界': [round(JACKPOT_CI_UPPER, 2)],

            '貳獎比例': [round(second_p, 2)],
            '貳獎CI下界': [round(SECOND_CI_LOWER, 2)],
            '貳獎CI上界': [round(SECOND_CI_UPPER, 2)],
        }
    )
    return summary

# %%
while True:
    account = input('請輸入帳號（輸入 /c 離開）：')
    
    if account == '/c':
        print('程式結束')
        break

    summary = play_game(account, times=5000)
    print(summary)


