# COVID-19の感染率と人口密度の関係
#
# 使い方：1行ずつ（またはひとまとまりずつ）Ctrl+Enter（Macは Cmd+Enter）で実行する。
# 途中でうまくいかなくなったら、各セクション冒頭の「再開用」の行から実行し直す。

# ライブラリの読み込み ====

library(tidyverse)
library(readxl)
library(ggrepel) # 散布図でラベルが重ならないようにするためのライブラリ


# データの読み込み ====

# 感染者数のデータを読み込む ----
# 2022年10月30日時点の累積感染者数（厚生労働省）

covid_data <- read_csv("data/covid_cases.csv")
View(covid_data)


# 人口のデータを読み込む ----
# 社会生活統計指標（総務省統計局）のExcelファイルから、必要な範囲だけを読み込む

pop_data <-
  read_excel(
    path = "data/a001.xls",  # Excelファイル名
    range = "I13:N59",       # 読み込む範囲
    col_names = c("都道府県", "Prefecture", "Population"), # 列名を指定
    col_types = c("text", "text", "skip", "skip", "skip", "numeric")
    # 列の型を指定。K〜M列は読み飛ばし、N列（2022年の人口）を読む
  )
View(pop_data)

pop_data <- pop_data |>
  mutate(
    Prefecture = str_remove(Prefecture, "-ken$|-to$|-fu$"), # 末尾の「-ken」「-to」「-fu」を削除
    Prefecture = if_else(Prefecture == "Gumma", "Gunma", Prefecture) # 群馬県の表記を修正（ヘボン式→訓令式）
  )
View(pop_data)


# DID（人口集中地区）のデータを読み込む ----
# 同じExcelファイルの別の範囲から、DID人口とDID面積を読み込む

did_data <-
  read_excel(
    path = "data/a001.xls",  # Excelファイル名
    range = "CZ13:DC59",     # 読み込む範囲
    col_names = c("DID_Population", "DID_Area"), # 列名を指定
    col_types = c("numeric", "skip", "skip", "numeric") # 列の型を指定（不要な列はスキップ）
  )
View(did_data)

did_data <- did_data |>
  mutate(DID_Density = DID_Population / DID_Area) |> # DID人口密度（人／km²）を計算
  select(DID_Density) # DID人口密度の列だけを残す
View(did_data)


# ワクチン接種率のデータを読み込む ----
# 3回目接種率（%）（厚生労働省）

vaccination_data <- read_csv("data/vaccination.csv")
View(vaccination_data)


# データの結合 ====
# 人口とDIDのデータを並べた後、都道府県名（Prefecture）をキーにして感染者数と接種率をつなぐ

dat <- pop_data |>
  bind_cols(did_data) |> # 人口とDIDのデータを横に並べる（どちらも同じ都道府県の順番）
  left_join(covid_data, by = "Prefecture") |>
  left_join(vaccination_data, by = "Prefecture")
View(dat)

dat <- dat |>
  mutate(Confirmed_Cases_per_Capita = Confirmed_Cases / Population * 100) # 感染率（%）を計算
View(dat)

# 確認ポイント：dat が 47行 × 7列 で、空欄（NA）がなければOK


# 分析 ====

# 【再開用】ここまでがうまくいかなかった人は、次の行を実行してから先に進む
# （うまくいった人が実行しても、同じデータが読み込まれるだけなので問題ない）
dat <- read_csv("data/dat.csv")


# 人口密度と感染率の散布図 ----

ggplot(dat, aes(x = DID_Density, y = Confirmed_Cases_per_Capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + # 回帰直線を描く
  geom_text_repel(aes(label = 都道府県), size = 3, max.overlaps = Inf) + # 都道府県名をラベルとして表示（全県）
  labs(
    title = "DID人口密度とCOVID-19の感染率",
    x = "DID人口密度（人／km²）",
    y = "感染率（%）"
  ) +
  theme_bw()


# 単回帰分析 ----
# 感染率 = a + b × DID人口密度

model1 <- lm(Confirmed_Cases_per_Capita ~ DID_Density, data = dat)
summary(model1)


# ワクチン接種率と感染率の散布図 ----

ggplot(dat, aes(x = Vaccination_Rate, y = Confirmed_Cases_per_Capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(aes(label = 都道府県), size = 3, max.overlaps = Inf) +
  labs(
    title = "ワクチン接種率とCOVID-19の感染率",
    x = "ワクチン接種率（%）",
    y = "感染率（%）"
  ) +
  theme_bw()


# 重回帰分析 ----
# 感染率 = a + b1 × DID人口密度 + b2 × ワクチン接種率

model2 <- lm(Confirmed_Cases_per_Capita ~ DID_Density + Vaccination_Rate, data = dat)
summary(model2)

# 確認ポイント：DID_Density の係数（Estimate）が 0.000557 前後になっていればOK
# 考えてみよう：model1 と model2 で、DID_Density の係数はどう変わったか？ それはなぜか？


# 課題：説明変数を1つ追加した重回帰分析 ====
# ★ の付いた行を、自分が選んだ変数に合わせて書き換える。
# 以下は例として「高齢化率（65歳以上人口の割合）」を追加している。
# 課題では、この例とは別の変数を選ぶこと。

# 追加する変数を読み込む ----

add_data <-
  read_excel(
    path = "data/a001.xls",
    range = "AX13:AX59",    # ★ 読み込む列（行は13〜59のまま、列の記号だけ変える）
    col_names = "Elderly",  # ★ 列名（自由に決めてよい。英数字と _ だけを使う）
    col_types = "numeric"
  )
View(add_data)

# 2020年の人口（2020年の値しかない変数を、人口で割るときに使う）
pop2020_data <-
  read_excel(
    path = "data/a001.xls",
    range = "M13:M59",
    col_names = "Population_2020",
    col_types = "numeric"
  )

# 人口に対する割合（%）を計算する ----
# 2022年や2021年の変数は Population で、2020年の変数は Population_2020 で割る

dat2 <- dat |>
  bind_cols(add_data, pop2020_data) |> # dat と同じ都道府県の順番なので、横に並べられる
  mutate(Elderly_Rate = Elderly / Population * 100) # ★ 新しい変数名 = 計算式
View(dat2)

# 追加した変数と感染率の散布図 ----

ggplot(dat2, aes(x = Elderly_Rate, y = Confirmed_Cases_per_Capita)) + # ★ x = 新しい変数名
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(aes(label = 都道府県), size = 3, max.overlaps = Inf) +
  labs(
    title = "高齢化率とCOVID-19の感染率", # ★ グラフのタイトル
    x = "高齢化率（%）",                   # ★ 横軸のラベル
    y = "感染率（%）"
  ) +
  theme_bw()

# 重回帰分析 ----

model3 <- lm(
  Confirmed_Cases_per_Capita ~ DID_Density + Vaccination_Rate + Elderly_Rate, # ★ 最後を新しい変数名に
  data = dat2
)
summary(model3)

# 考えてみよう：model2 と比べて、DID_Density の係数はどう変わったか？ 追加した変数の係数はどう解釈できるか？