# 檢查git當前版本
git --version 

# git 能夠儲存創作者的資訊
git config --global user.name "作者姓名"
git config --global user.email "作者信箱"

# 當前目錄建立.git 用來儲存檔案變更歷史 絕對不能刪除!!!
git init


## git 是檢查檔案的狀態 不是檔案本身
# 檢查檔案狀態
git status
# U Untracked 代表檔案為追蹤
# 多個檔案用空白鍵隔開
git add 檔案名稱
# U 改為 A : Tracked 已追蹤

## 每次已追蹤或已修改為放入暫存區的檔案都要經過commit提交
git commit -m "你對此次檔案變更的版本主旨"
# A 不見 : Commited 已提交

## 修改檔案內容
# 多了 M : 已修改 但是未放入暫存區

# 將副檔名為 md 的文件的所有變更放入暫存區
git add *.md

# 將當前目錄所有的變更放入暫存區
git add .

# 顯示詳細的提交歷史
git log
# 顯示簡化的提交歷史
git log --oneline