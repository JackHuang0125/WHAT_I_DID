# Claude Skills

這個資料夾存放我自己的 Claude Code Skills，用 Git 在多台電腦之間同步。

每個 Skill 一個資料夾，裡面至少有一個 `SKILL.md`：

```
SKILLS/
├── task-orchestrator/
│   └── SKILL.md
└── setup.ps1
```

## 新電腦設定（只需做一次）

```powershell
git clone https://github.com/JackHuang0125/WHAT_I_DID.git $env:USERPROFILE\WHAT_I_DID
powershell -ExecutionPolicy Bypass -File $env:USERPROFILE\WHAT_I_DID\SKILLS\setup.ps1
```

`setup.ps1` 會把 `%USERPROFILE%\.claude\skills` 建成指向本資料夾的 Junction，
Claude Code 會直接讀這裡的 Skills。

## 日常同步

```powershell
cd $env:USERPROFILE\WHAT_I_DID
git pull
```

pull 完重新開一個 Claude 對話就會載入最新的 Skills。

## 新增／修改 Skill

在任一台電腦請 Claude 新增或修改 Skill，Claude 會先列出變更請我確認，
同意後才 commit 並 push 到這個 repo。
