import streamlit as st
from dotenv import load_dotenv

from langchain.chat_models import init_chat_model
from langchain_community.document_loaders import TextLoader
from langchain_community.document_loaders import PyPDFLoader # 讀取 pdf 文字檔案
from langchain_text_splitters import RecursiveCharacterTextSplitter
from langchain_chroma import Chroma
from langchain_huggingface import HuggingFaceEmbeddings
from langchain_core.prompts import ChatPromptTemplate

load_dotenv()

DATA_PATH = "C:/Users/USER/Documents/GitHub/WHAT_I_DID/實作_RAG/RAG/data/semiconductor_inf.txt"
DB_DIR = "chroma_db"

@st.cache_resource
def build_vector_db():
    loader = TextLoader(DATA_PATH, encoding="utf-8")
    docs = loader.load()

    splitter = RecursiveCharacterTextSplitter(
        chunk_size=300,
        chunk_overlap=50
    )
    chunks = splitter.split_documents(docs)

    embeddings = HuggingFaceEmbeddings(
        model_name="sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2" # 選擇文字轉向量的模型
    )

    vector_db = Chroma.from_documents(
        documents=chunks,
        embedding=embeddings,
        persist_directory=DB_DIR
    )

    return vector_db


@st.cache_resource
def load_llm():
    model = init_chat_model(
        "gemini-3.5-flash",
        model_provider="google_genai",
        temperature=0
    )
    return model

def extract_text(response):
    content = response.content

    if isinstance(content, str):
        return content

    if isinstance(content, list):
        return "\n".join(
            block.get("text", "")
            for block in content
            if isinstance(block, dict) and block.get("type") == "text"
        )

    return str(content)

def rag_answer(question: str, vector_db, llm):
    retriever = vector_db.as_retriever(
        search_kwargs={"k": 3}
    )

    related_docs = retriever.invoke(question)

    context = "\n\n".join(
        doc.page_content for doc in related_docs
    )

    prompt = ChatPromptTemplate.from_template("""
你是一個半導體資訊問答助手。

請只能根據「參考資料」回答問題。
如果參考資料不足，請回答「目前資料不足，無法判斷」。

回答時請遵守：
1. 使用繁體中文。
2. 回答要精簡、具體。
3. 不確定的內容不要猜測。
                                              
參考資料：
{context}

問題：
{question}

""")

    chain = prompt | llm

    response = chain.invoke({
        "context": context,
        "question": question
    })

    answer = extract_text(response)

    return answer, related_docs


st.set_page_config(
    page_title="半導體資訊問答助手 Demo",
    layout="centered"
)

st.title("半導體資訊問答助手")
st.caption("LangChain + ChromaDB + Gemini + Streamlit")

vector_db = build_vector_db()
llm = load_llm()

if "messages" not in st.session_state:
    st.session_state.messages = []

for msg in st.session_state.messages:
    with st.chat_message(msg["role"]):
        st.write(msg["content"])

user_question = st.chat_input("請輸入問題，例如：什麼是半導體？")

if user_question:
    st.session_state.messages.append({
        "role": "user",
        "content": user_question
    })

    with st.chat_message("user"):
        st.write(user_question)

    answer, source_docs = rag_answer(user_question, vector_db, llm)

    with st.chat_message("assistant"):
        st.write(answer)

        with st.expander("檢索到的參考資料"):
            for i, doc in enumerate(source_docs, start=1):
                st.markdown(f"**段落 {i}**")
                st.write(doc.page_content)

    st.session_state.messages.append({
        "role": "assistant",
        "content": answer
    })