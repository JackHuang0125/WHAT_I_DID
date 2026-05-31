# 專案名稱：NDHU Applied Math RAG Assistant

## 專案目的：
建立一個基於 RAG 的知識文件問答系統，讓使用者可以根據指定文件內容進行問答。

## 技術架構：
- Streamlit：網頁介面
- LangChain：RAG 流程串接
- ChromaDB：向量資料庫
- HuggingFace Embedding：文字向量化
- Gemini API：語言模型回答生成

## 主要功能：
讀取本地文字文件
自動切分文件段落
建立向量資料庫
根據問題檢索相關段落
使用 Gemini 依據檢索內容回答
顯示參考段落