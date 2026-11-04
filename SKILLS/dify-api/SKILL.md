---
name: dify-api
description: 用 dify_api.py 命令列工具讀取、修改、測試 Dify 工作流程草稿（列節點、取出或改寫提示詞與 code、草稿執行、單節點試跑、讀運行紀錄），不開瀏覽器。任何要看或改 Dify 節點、跑 Dify 測試的任務都先讀這份。
---

# Dify API 命令列工具

工具位置：`C:\Users\ur81260844\Documents\dify-tests\dify_api.py`（只用 Python 標準函式庫）。
工作區通常是各助手的子資料夾，所以**一律用完整路徑執行**：
`python C:\Users\ur81260844\Documents\dify-tests\dify_api.py <指令> ...`（下表省略路徑）

## 為什麼不用瀏覽器

Dify 畫布的頁面內容與截圖每次都有數萬 tokens，而且會留在對話裡讓之後每一輪變慢；
點擊畫布也常失敗重試。這個工具只回傳需要的欄位，一次修改通常只用幾千 tokens。
**不要開 Dify 網頁、不要截圖、不要用 run_playwright_code 呼叫 console API。**
**也不要自己寫腳本或 `python -c` 直接呼叫 Dify API、讀取 cookie.txt；** 工具做不到時停下來告訴使用者。

## 指令

| 指令 | 用途 |
|---|---|
| `apps [關鍵字]` | 用名稱搜尋應用，列出 app_id、類型、名稱 |
| `copy <app> <新名稱>` | 複製應用（建立 TEST 版本；只在使用者要求時使用） |
| `nodes <app>` | 列出節點 id、類型、標題（先用這個找 id；標題可能重複，一律用 id 指認） |
| `edges <app>` | 列出連線，含分支 handle |
| `node <app> <node_id> [--raw]` | 看單一節點設定；`--raw` 顯示完整節點（含 position、parentId），可當新增節點的範本 |
| `get <app> <node_id> <路徑> -o <檔>` | 取出欄位存成檔案。路徑例：`prompt_template.1.text`、`code`、`model.completion_params` |
| `set <app> <node_id> <路徑> <檔>` | 用檔案內容取代欄位並寫回草稿（`.json` 檔視為 JSON，其他視為文字） |
| `set-node <app> <node_id> <JSON 檔>` | 用 JSON 合併取代整個節點 data |
| `add-node <app> <JSON 檔> [--after ID]` | 新增節點。JSON 可為完整節點或只有 data（需 type、title）；沒給 id 自動產生；`--after` 放在該節點右邊 |
| `connect <app> <來源> <目標> [--handle H]` | 新增連線；if-else／問題分類器要指定分支 handle（不給會列出可用的） |
| `disconnect <app> <來源> <目標> [--handle H]` | 刪除連線 |
| `delete-node <app> <node_id> [--force]` | 刪除節點與其連線；有其他節點引用它的變數時拒絕 |

改結構的四個指令都可加 `--dry-run`，只顯示會做什麼、不寫回。
| `run <app> "<問題>" [--inputs JSON] [--conv ID]` | 草稿執行（預覽），印每個節點摘要、回答、排隊時間。workflow 類型且只有一個輸入變數時，問題會自動填入 |
| `run-node <app> <node_id> --inputs JSON` | 單節點試跑（只呼叫 1 次 LLM） |
| `trace <app> <run_id> [--node ID]` | 讀某次運行的節點紀錄；加 `--node` 看該節點的 inputs/outputs |

輸出過長會截斷，真的需要全文才加 `--full`。

## 修改流程

1. 從工作區的 `CLAUDE.md` / `README.md` 取得 app_id（沒有就用 `apps <關鍵字>` 找），`nodes` 找到節點 id。
2. `get ... -o %TEMP%\dify-edit\<名稱>.txt` 取出要改的欄位（暫存檔不要放進助手資料夾）。
3. 用編輯工具改那個檔案（只改需要的段落）。
4. 先向使用者列出要改的節點、改什麼、怎麼測，**等同意後**才 `set` 寫回。
5. 改流程結構時：`node --raw` 看類似節點當範本 → 寫 JSON 檔 → `add-node` / `connect` / `disconnect`
   先加 `--dry-run` 確認，再正式執行；最後用 `edges` 檢查連線。
6. 測試：只驗證單一提示詞或 code 時用 `run-node`；需要看路由時才用 `run`。

## 規則

- 只改 TEST 應用的草稿；正式版只讀。該助手資料夾若有 `CLAUDE.md`，先讀並遵守。
- 測試遵守 `dify-test-load-control` 技能：一次一題、一批最多 5 題、先用「你好」探測、逾時不重送。
  工具本身已用鎖檔防止同時執行兩個測試。
- `run-node` 會覆蓋該節點的「上次運行」紀錄，要在報告註明。
- 出現「登入已失效」：停下來，請使用者用無痕視窗登入 Dify、複製 Cookie 更新到
  `%USERPROFILE%\.dify\cookie.txt` 後關閉無痕視窗（不要登出）。不要改用瀏覽器，也不要自己去讀取瀏覽器的 cookie。
