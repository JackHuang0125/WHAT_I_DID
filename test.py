import pandas as pd
from jinja2 import Template

# 假設 jaccard_similarity_df 是您的相似度矩陣
# 範例相似度矩陣
data = {
    "Mojito": {"Mojito": 1.0, "Old Cuban": 0.85, "Caipirinha": 0.78, "Mai Tai": 0.65},
    "Old Cuban": {"Mojito": 0.85, "Old Cuban": 1.0, "Caipirinha": 0.82, "Mai Tai": 0.70},
    "Caipirinha": {"Mojito": 0.78, "Old Cuban": 0.82, "Caipirinha": 1.0, "Mai Tai": 0.68},
    "Mai Tai": {"Mojito": 0.65, "Old Cuban": 0.70, "Caipirinha": 0.68, "Mai Tai": 1.0},
}
jaccard_similarity_df = pd.DataFrame(data)

# 推薦邏輯
def recommend_similar_html(cocktail_name, similarity_matrix, top_n=3):
    """
    返回推薦結果的 HTML 格式
    """
    recommendations = (
        similarity_matrix.loc[cocktail_name]
        .drop(cocktail_name)
        .sort_values(ascending=False)
        .head(top_n)
    )
    return recommendations.reset_index().values.tolist()  # 返回列表

# 生成 HTML 文件
template = Template("""
<!DOCTYPE html>
<html>
<head>
    <title>Cocktail Recommendation</title>
    <style>
        body { font-family: Arial, sans-serif; padding: 20px; }
        .container { max-width: 600px; margin: 0 auto; }
        select, button { font-size: 16px; padding: 10px; margin: 5px 0; width: 100%; }
        table { width: 100%; border-collapse: collapse; margin-top: 20px; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
    </style>
    <script>
        function showRecommendations() {
            const cocktailName = document.getElementById('cocktailSelect').value;
            const recommendations = JSON.parse(document.getElementById('data').textContent)[cocktailName];
            const tableBody = document.getElementById('recommendationsBody');
            tableBody.innerHTML = '';
            recommendations.forEach(row => {
                const tr = document.createElement('tr');
                row.forEach(cell => {
                    const td = document.createElement('td');
                    td.textContent = cell;
                    tr.appendChild(td);
                });
                tableBody.appendChild(tr);
            });
        }
    </script>
</head>
<body>
    <div class="container">
        <h1>Cocktail Recommendation System</h1>
        <label for="cocktailSelect">Select a Cocktail:</label>
        <select id="cocktailSelect" onchange="showRecommendations()">
            {% for cocktail in cocktails %}
            <option value="{{ cocktail }}">{{ cocktail }}</option>
            {% endfor %}
        </select>
        <table>
            <thead>
                <tr>
                    <th>Cocktail</th>
                    <th>Similarity Score</th>
                </tr>
            </thead>
            <tbody id="recommendationsBody"></tbody>
        </table>
        <div style="display:none;" id="data">{{ data_json }}</div>
    </div>
</body>
</html>
""")

# 准備推薦數據
data_json = {
    cocktail: recommend_similar_html(cocktail, jaccard_similarity_df)
    for cocktail in jaccard_similarity_df.index
}

# 渲染 HTML
html_content = template.render(
    cocktails=jaccard_similarity_df.index.tolist(), data_json=data_json
)

# 將 HTML 保存為文件
with open("cocktail_recommendation.html", "w", encoding="utf-8") as f:
    f.write(html_content)

print("HTML 文件已生成，名稱為 cocktail_recommendation.html")

