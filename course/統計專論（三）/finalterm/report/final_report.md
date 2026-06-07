# SECOM 半導體製程資料之 Phase I/II 統計製程監控分析

作者：黃丞暘 611211106  
課程：統計專論 (III) Final Report  
日期：2026/06/16

## 摘要

本報告依據期末專案要求，使用 UCI SECOM 半導體製程資料集，選取單一製程變數建立完整的統計製程監控流程。本文選用變數 `x297`，先移除缺失值並以 1.5 IQR 規則刪除離群值，再將清理後資料依 `m = floor(0.5n)` 分為 Phase I 與 Phase II。Phase I 以 RS/P recursive segmentation + permutation 方法檢查 level 與 scale 穩定性；Phase II 則比較 Shewhart、EWMA、CUSUM 與 CPD 管制圖。

結果顯示 Phase I 未呈現明顯不穩定；Phase II 最早由 variance EWMA 於 cleaned index 719 發出訊號，而 mean-sensitive charts 則多於 index 743 發出訊號。CPD charts 進一步指出後段可能存在結構性變化，主要估計變化位置約在 index 1212 至 1229 附近。

## 1. 研究目的

本期末報告的目標是針對 SECOM 半導體製程資料建立完整的 Phase I/II 統計製程監控流程：

Phase I diagnosis -> control chart design -> Phase II monitoring。

分析內容包含：

- 選擇一個可用的 SECOM 製程變數。
- 移除缺失值與離群值。
- 依 `m = floor(0.5n)` 將資料切分為 Phase I 與 Phase II。
- 於 Phase I 使用 RS/P 方法檢查 level 與 scale 穩定性。
- 於 Phase II 建立 Shewhart、EWMA、CUSUM 與 CPD 管制圖。
- 比較各管制圖的 first signal time 與 signal count。
- 判斷製程型態較可能是 mean shift、variance shift 或 structural change。

## 2. 資料來源與變數選擇

本研究使用 UCI SECOM semiconductor manufacturing dataset。資料共有 1567 筆觀測值與 590 個製程變數，另包含時間欄位與 pass/fail 標記。

本文選用變數 `x297`。選擇原因如下：

- 缺失比例低，只有 2 筆缺失值。
- 有足夠多的不同觀測值，不屬於近似常數欄位。
- 標準差足夠大，適合比較 mean-sensitive 與 variance-sensitive 管制圖。
- 作為 individual observations 的單變量製程變數，可直接用於 Shewhart、EWMA、CUSUM 與 CPD 分析。

## 3. 資料前處理與描述統計

資料前處理分為兩步：

1. 刪除 `x297` 的缺失值。
2. 使用整體資料的 1.5 IQR 規則刪除離群值。

前處理摘要：

| 項目 | 數值 |
|---|---:|
| 原始觀測值數 | 1567 |
| 移除缺失值數 | 2 |
| 離群值規則 | 全資料 1.5 IQR |
| 移除離群值數 | 128 |
| 清理後觀測值數 | 1437 |

清理後 `x297` 的描述統計：

| 平均數 | 標準差 | 最小值 | Q1 | 中位數 | Q3 | 最大值 |
|---:|---:|---:|---:|---:|---:|---:|
| 1410.91 | 1126.51 | 0.00 | 556.99 | 1061.44 | 1995.39 | 4945.92 |

## 4. Phase I 與 Phase II 切分

依照 Chapter 6 期末專案規定：

`m = floor(0.5n)`。

清理後資料共有 `n = 1437` 筆，因此：

`m = floor(0.5 * 1437) = 718`。

前 718 筆觀測值作為 Phase I 參考資料，後 719 筆觀測值作為 Phase II 監控資料。

| 階段 | 觀測值數 | 平均數 | 標準差 |
|---|---:|---:|---:|
| Phase I | 718 | 1403.64 | 1105.53 |
| Phase II | 719 | 1418.18 | 1147.80 |

## 5. Phase I RS/P 穩定性診斷

Phase I 的目的在於建立 in-control reference sample，因此必須先檢查 Phase I 是否具有明顯的 level shift 或 scale shift。本文使用 RS/P recursive segmentation + permutation 方法進行診斷，並採用固定亂數種子 `611211106`。

RS/P 診斷結果：

- `p_level = 0.8675`
- `p_scale = 0.8325`

兩個 p-value 皆遠大於 0.05，因此沒有足夠證據拒絕 Phase I level 或 scale 穩定的假設。換言之，在本文所採用的前處理規則下，Phase I 可視為合理的 IC reference sample。

## 6. Phase II 管制圖設計

Phase II 監控使用 Phase I 估計的 `mu0 = 1403.64` 與 `sigma0 = 1105.53` 作為 IC 參數。本文比較的管制圖如下：

| 方法 | 監控目標 | 主要設定 |
|---|---|---|
| Individuals Shewhart | mean | 3-sigma limits |
| Mean EWMA | mean | `lambda = 0.1`, `rho = 2.454` |
| Variance EWMA | variance | `lambda = 0.1`, `rhoU = 2.595`, `rhoL = 1.580` |
| Adaptive EWMA | mean | eta1 case (ii), `lambda = 0.1813`, `u = 2.5752`, `h = 0.7874` |
| Mean CUSUM | mean | `k = 0.5`, `h = 4.095` |
| Variance CUSUM | variance | `k_up = 1.848`, `h_up = 7.416`; `k_down = 0.462`, `h_down = -2.446` |
| CPD mean chart | mean | `alpha = 0.005` |
| CPD variance chart | variance | `alpha = 0.005` |
| CPD joint chart | mean/variance | `alpha = 0.005` |

EWMA 與 CUSUM 參數主要參考課堂 InClass R code；CPD charts 則依照 Chapter 6 的公式與控制界限近似。

## 7. 訊號比較

各管制圖的 first signal index 與 signal count 如下。此處 index 是清理後資料的整體觀測順序，因此 Phase II 的第一個觀測點為 index 719。

| 方法 | 監控目標 | First signal index | Signal count |
|---|---|---:|---:|
| Variance EWMA | variance | 719 | 719 |
| Mean EWMA | mean | 743 | 17 |
| Adaptive EWMA | mean | 743 | 32 |
| Mean CUSUM | mean | 743 | 13 |
| CPD mean chart | mean | 749 | 118 |
| CPD variance chart | variance | 761 | 78 |
| CPD joint chart | mean/variance | 762 | 71 |
| Individuals Shewhart | mean | 773 | 10 |
| Variance CUSUM | variance | 773 | 59 |

最早發出訊號的方法是 variance EWMA，其 first signal index 為 719，也就是 Phase II 的第一筆觀測值。若只比較 mean-sensitive methods，mean EWMA、adaptive EWMA 與 mean CUSUM 都在 index 743 發出第一個訊號，早於 Individuals Shewhart 的 index 773。

這表示變化可能不是單一極端點，而是具有持續性，因此 EWMA 與 CUSUM 這類會累積歷史資訊的方法較早偵測到異常。

## 8. CPD 結果與變化點解釋

CPD charts 的優點是除了發出訊號外，也能估計最可能的變化點位置。本文結果如下：

- CPD mean chart first signal index 為 749。
- CPD variance chart first signal index 為 761。
- CPD joint chart first signal index 為 762。
- CPD mean chart 最強訊號對應的估計變化點約為 `r_hat = 1212`。
- CPD variance 與 joint charts 最強訊號對應的估計變化點約為 `r_hat = 1229`。

此結果表示 Phase II 中可能不只存在單一早期 shift。前段 mean-sensitive charts 很快發現平均水準的偏離，而 CPD variance 與 joint charts 則指出後段約 index 1229 附近可能有更明顯的變異或結構性改變。

## 9. 綜合討論

整體而言，本文的分析結果可分為三個層次解釋。

第一，Phase I RS/P 結果並未顯示明顯 level 或 scale 不穩定，因此將 Phase I 當作 IC reference 是合理的。

第二，Phase II 的 mean-sensitive charts 皆顯示某種程度的平均水準偏移。Mean EWMA、adaptive EWMA 與 mean CUSUM 都在 index 743 發出訊號，而 Shewhart 到 index 773 才發出訊號。這代表變化可能具有持續性，而不只是單一極端點。

第三，variance-sensitive charts 的結果顯示變異結構也可能改變。Variance EWMA 從 Phase II 第一點即發出訊號，說明 Phase II 的標準化平方偏差相對於 Phase I 參考分布有明顯差異。不過，由於 variance EWMA 過於敏感，解釋時應搭配 variance CUSUM 與 CPD variance chart。CPD variance 與 joint charts 將較強變化位置指向 index 1229 附近，暗示後段可能有結構性改變。

## 10. 結論

本報告針對 SECOM 資料中的 `x297` 變數建立完整的 Phase I/II SPC 分析流程。資料經缺失值移除與 1.5 IQR 離群值過濾後，共保留 1437 筆觀測值。依照期末專案要求，前 718 筆作為 Phase I，後 719 筆作為 Phase II。

Phase I RS/P 診斷得到 `p_level = 0.8675` 與 `p_scale = 0.8325`，顯示 Phase I 沒有明顯 level 或 scale 不穩定，可作為 IC reference。Phase II 監控中，variance EWMA 最早於 index 719 發出訊號；mean-sensitive methods 中，mean EWMA、adaptive EWMA 與 mean CUSUM 皆於 index 743 發出訊號；CPD charts 則於稍後發出訊號，但提供了重要的變化點定位資訊。

綜合各方法結果，本文認為 `x297` 的 Phase II 型態不宜解釋為單純 mean shift。較合理的判斷是：Phase II 同時存在平均水準不穩定與變異或結構性變化，其中後段約 index 1212 至 1229 附近可能是主要變化區域。
