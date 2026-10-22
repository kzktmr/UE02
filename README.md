# 都市経済学【R演習】実証トピックス1

都市経済学の【実証トピックス1】用のリポジトリです。

## 起動方法

下のリンクをクリックしてください（Orthrosアカウントでログイン）：

[![Binder](https://binder.cs.rcos.nii.ac.jp/badge_logo.svg)](https://binder.cs.rcos.nii.ac.jp/v2/gh/kzktmr/UE01/main)

## 含まれるパッケージ

- tidyverse（データ処理・可視化）
- readxl（Excelファイルの読み込み）

## データ

`data/` フォルダに含まれるデータは以下のとおりです。

| ファイル | 内容 |
|---|---|
| `a001.xls` | 基礎データ A 人口・世帯（総務省統計局） |
| `kenbetsu-vaccination_data3.xlsx` | 接種回数の都道府県別実績（厚生労働省） |
| `confirmed_cases_cumulative_daily.csv` | 都道府県別累積陽性者数の日次推移（厚生労働省） |

データの出所：

- [総務省統計局「社会生活統計指標－都道府県の指標－2024」](https://www.e-stat.go.jp/stat-search/files?stat_infid=000040133615)
- [厚生労働省「特例臨時接種期間における新型コロナワクチンの接種回数について」](https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/kenkou_iryou/kenkou/kekkaku-kansenshou/yobou-sesshu/syukeihou_00002.html)
- [厚生労働省「データからわかる－新型コロナウイルス感染症情報－」](https://covid19.mhlw.go.jp)
