# SECOM 半導體製程資料 Phase I/II 修正版分析

學號：611211106  
課程：統計專論（三）Final Report  
日期：2026/06/17

## 摘要

本次修正依照報告後老師的建議，將分析流程改為先將原始時間序列資料切分為 Phase I 與 Phase II，再只對 Phase I 做資料前處理。Phase I 先刪除缺失值，再使用 3 倍 IQR 規則移除離群值；Phase II 則視為新取得資料，不做離群值刪除。

由於選用變數 `x297` 呈現嚴重右偏，後續不再使用以常態假設為基礎的 EWMA、CUSUM 與 CPD 管制界線作為 Phase II 偵測工具。Individuals Shewhart chart 改以 Phase I 配適 Gamma 分配建立上管制界線，下管制界線設為 0。Phase II 的主要診斷則改採整段 Phase II 的無母數 RS/P 診斷。

主要結果為：原始資料共 1567 筆，先切成 Phase I 783 筆與 Phase II 784 筆。Phase I 清理後保留 757 筆，用於估計 Gamma 管制界線；Gamma 上管制界線為 7338.84，下界為 0。Phase II 整段 RS/P 診斷得到 `p_level = 0.0575`、`p_scale = 0.1000`，在 0.05 顯著水準下尚未拒絕整段 Phase II 無 level 或 scale 結構改變的假設，但 level 的 p-value 接近 0.05，後續可考慮分段抽樣再做多次 RS/P 診斷。

## 1. 修正後分析流程

原本流程是先對整筆資料刪除缺失值與離群值，再切分 Phase I 與 Phase II。這會讓 Phase II 的資料被事先清理，與「新取得資料」的監控概念不一致。

修正後流程如下：

1. 依時間排序原始 SECOM 資料。
2. 先將原始資料切分為 Phase I 與 Phase II。
3. 只在 Phase I 進行前處理：刪除缺失值，並使用 3 倍 IQR 移除離群值。
4. 使用清理後 Phase I 建立管制界線。
5. Phase II 不做離群值刪除，作為新資料進行診斷。
6. 因資料嚴重右偏，Phase II 不再繪製 EWMA、CUSUM 與 CPD 管制圖。
7. Phase II 改以整段 RS/P 無母數診斷檢查 level 與 scale 是否有結構變化。

## 2. 資料與切分

本分析使用 SECOM 資料中的單一製程變數 `x297`。原始資料共 1567 筆，依時間排序後以 `m = floor(n / 2)` 切分：

| 階段 | 原始筆數 | 說明 |
|---|---:|---|
| Phase I | 783 | 用於建立參考樣本與管制界線 |
| Phase II | 784 | 視為新取得資料，不做離群值刪除 |

Phase I 前處理結果如下：

| 項目 | 數值 |
|---|---:|
| Phase I 原始筆數 | 783 |
| Phase I 缺失值刪除 | 2 |
| Phase I 3 IQR 離群值刪除 | 24 |
| Phase I 清理後筆數 | 757 |
| 3 IQR 下界 | -4376.27 |
| 3 IQR 上界 | 7265.77 |

Phase II 不做離群值刪除。本資料的 Phase II 中 `x297` 無缺失值，因此整段 784 筆皆可用於 RS/P 診斷。

## 3. Phase I 描述統計

清理後 Phase I 的 `x297` 摘要如下：

| 平均數 | 標準差 | 最小值 | Q1 | 中位數 | Q3 | 最大值 |
|---:|---:|---:|---:|---:|---:|---:|
| 1629.34 | 1478.01 | 0.00 | 598.12 | 1141.48 | 2109.16 | 7189.73 |

此變數明顯為非負且右偏，平均數大於中位數，最大值也遠高於 Q3。因此若直接使用常態分配下的對稱三倍標準差界線，會不符合資料型態。

## 4. Phase I RS/P 穩定性診斷

Phase I 用於建立 in-control reference sample，因此仍先以 RS/P 方法檢查清理後 Phase I 是否有明顯 level 或 scale 變化。

| 診斷 | p-value | 最大統計量 | 估計切點（Phase I 清理後序列） |
|---|---:|---:|---:|
| Level | 0.9650 | 1.5446 | 82 |
| Scale | 0.7450 | 2.7315 | 34 |

兩個 p-value 皆大於 0.05，因此沒有足夠證據認為清理後 Phase I 有明顯 level 或 scale 不穩定。Phase I 可作為後續建立管制界線的參考資料。

## 5. Gamma Individuals Shewhart 管制界線

考量 `x297` 為非負且右偏，本次不採用常態 3-sigma limits。Individuals Shewhart chart 改以 Gamma 分配估計上管制界線，下管制界線固定為 0。

Gamma 分配配適時，Phase I 中有 1 筆為 0，因此配適 shape 與 scale 時使用正值樣本，並保留下界為 0 的管制圖解釋。

| 參數 | 數值 |
|---|---:|
| Gamma shape | 1.3607 |
| Gamma scale | 1199.0099 |
| Gamma location | 0 |
| 上管制界線 UCL | 7338.8437 |
| 下管制界線 LCL | 0 |
| Phase II 超過 UCL 筆數 | 23 |
| Phase II 第一個超過 UCL 的原始 index | 842 |

這張圖可作為單點極端值偵測工具，但由於老師指出 Phase II 後續不適合繪製 EWMA、CUSUM 與 CPD，本報告不再把 Gamma Shewhart 與那些方法做 first signal 比較。

## 6. Phase II 整段 RS/P 診斷

Phase II 不做離群值刪除，直接使用整段 784 筆資料進行 RS/P 診斷。結果如下：

| 診斷 | p-value | 最大統計量 | 估計切點（Phase II 序列） | 對應原始 index |
|---|---:|---:|---:|---:|
| Level | 0.0575 | 6.9002 | 776 | 1559 |
| Scale | 0.1000 | 17.5997 | 776 | 1559 |

在 0.05 顯著水準下，整段 Phase II 的 level 與 scale 診斷皆尚未達顯著。不過 `p_level = 0.0575` 已接近 0.05，且 level 與 scale 的估計切點都落在 Phase II 後段，對應原始 index 1559。這表示 Phase II 後段可能有需要進一步檢查的結構變化。

## 7. 不再使用 EWMA、CUSUM 與 CPD 的理由

EWMA、CUSUM 與原本使用的 CPD 管制界線主要依賴常態或近似常態下的統計量校準。`x297` 的分布嚴重右偏且非負，若仍使用常態型上下界，容易產生不合理的訊號解釋，尤其是下界可能落在不符合資料支撐範圍的位置。

因此本次修正採用下列原則：

- Phase I 前處理只影響管制界線估計，不改動 Phase II。
- Individuals Shewhart 使用 Gamma 上界與 0 下界。
- Phase II 主要以無母數 RS/P 診斷檢查整段資料是否存在 level 或 scale 改變。
- EWMA、CUSUM 與 CPD 不再作為 Phase II 管制圖結果呈現。

## 8. 結論與後續建議

本次修正後，分析流程更符合 Phase I/Phase II 監控架構。Phase I 清理後可視為穩定參考樣本，並以 Gamma 分配建立非負右偏資料較合理的 Individuals Shewhart 管制界線。Phase II 保持原始狀態，整段 RS/P 診斷在 0.05 水準下尚未顯著，但 level 診斷接近顯著，且估計切點落在 Phase II 後段。

後續建議是針對 Phase II 進一步做分段 RS/P 診斷，例如以滑動視窗或分批抽樣方式檢查後段是否穩定。若多個區段都顯示後段 level 或 scale p-value 偏小，則可更有根據地指出 Phase II 後段存在製程結構變化。
