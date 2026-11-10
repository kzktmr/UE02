# 地価関数の推定：中心地からの距離と地価の関係
#
# 使い方：1行ずつ（またはひとまとまりずつ）Ctrl+Enter（Macは Cmd+Enter）で実行する。
# 途中でうまくいかなくなったら、「回帰分析」のセクションの冒頭の行から実行し直す。

# ライブラリの読み込み ====

library(tidyverse)
library(sf)
library(tmap)

# データの読み込み ====

# 地価のデータを読み込む ----
# 都道府県地価調査（2025年7月1日時点）、国土交通省「国土数値情報」
# 実行すると「replacing crs does not reproject data」という警告が出るが、問題ない
Ishikawa_land_price <- 
  read_sf("data/L02-25_17.geojson") |> 
  st_set_crs(6668) |>     # 座標系を指定（国土数値情報の仕様に従い、JGD2011の緯度経度）
  st_transform(6675) |>   # 距離を計算するため、平面直角座標系VII系に投影変換
  rename(                 # わかりやすい変数名にしておく
    land_price = L02_006, # 地価（円／m²）
    city_code = L02_020,  # 行政区域コード
    land_use = L02_001    # 用途区分（土地利用）
  ) |> 
  mutate(
    land_price = land_price / 10000, # 地価を万円／m²に変換
    land_use = factor(land_use,      # 用途区分を因子型に 
      levels = c("000", "003", "005", "009", "020"), 
      labels = c("住宅地", "宅地見込地", "商業地", "工業地", "林地")
    ),
    land_use = fct_collapse(land_use, 住宅地 = c("住宅地", "宅地見込地")) # 「宅地見込地」を「住宅地」に統合
  )

Ishikawa_land_price |> st_drop_geometry() |> count(land_use) # 用途区分ごとの地点数
tm_shape(Ishikawa_land_price) + tm_symbols()                 # 地図で確認

# 金沢都市圏の基準地を取り出す ----
# 都市雇用圏（2020年基準、中心市：金沢市）による https://www.csis.u-tokyo.ac.jp/UEA/
Kanazawa_city_code <- c(
  "17201", # 金沢市
  "17209", # かほく市
  "17210", # 白山市
  "17212", # 野々市市
  "17324", # 能美郡川北町
  "17361", # 河北郡津幡町
  "17365", # 河北郡内灘町
  "17386"  # 羽咋郡宝達志水町
)

Kanazawa_land_price <- 
  Ishikawa_land_price |> 
  filter(
    city_code %in% Kanazawa_city_code, # 行政区域コードが金沢都市圏に含まれる基準地
    L02_048 != "都計外"                 # ただし、都市計画区域外の基準地は除く
  )

Kanazawa_land_price |> st_drop_geometry() |> count(land_use) # 用途区分ごとの地点数
tm_shape(Kanazawa_land_price) + tm_symbols(fill = "land_price") # 地価の地図

# 中心地と距離 ====

# 最も地価が高い地点を中心地とする ----
center <-
  Kanazawa_land_price |>
  slice_max(land_price, n = 1, with_ties = FALSE) # 地価が最大の1地点を選び出す

center |> st_drop_geometry() |> select(L02_022, land_use, land_price) # 中心地の所在地と地価

tm_shape(Kanazawa_land_price) + tm_symbols(fill = "land_price") +
  tm_shape(center) + tm_symbols(fill = "red") # 赤い点が中心地

# 中心地から各地点までの距離を計算し、distance列に格納 ----
Kanazawa_land_price <- 
  Kanazawa_land_price |> 
  mutate(distance = as.numeric(st_distance(geometry, center)[, 1]) / 1000) # 距離（km）を計算する

Kanazawa_land_price |> select(distance, land_price) |> View() # 確認
# Kanazawa_land_price |> write_rds("data/kanazawa_land_price.rds") # データを保存しておく

# 回帰分析 ====

# ここまででうまくいかなかった場合は、次の行を実行してから先に進む（同じ手順で作ったデータが読み込まれる）
Kanazawa_land_price <- read_rds("data/kanazawa_land_price.rds")

# 散布図 ----
ggplot(Kanazawa_land_price, aes(x = distance, y = land_price)) +
  geom_point(aes(color = land_use)) +
  labs(
    x = "中心地からの距離（km）",
    y = "地価（万円／m²）",
    color = "用途区分"
  ) +
  theme_bw()

# 二次関数のモデル ----
model1 <- lm(land_price ~ distance + I(distance^2), data = Kanazawa_land_price)
summary(model1)

# 散布図と回帰曲線
ggplot(Kanazawa_land_price, aes(x = distance, y = land_price)) +
  geom_point(aes(color = land_use)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) + # 二次の回帰曲線
  labs(
    x = "中心地からの距離（km）",
    y = "地価（万円／m²）",
    color = "用途区分"
  ) +
  theme_bw()

# 距離による地価の変化（曲線の傾き）
b <- coef(model1)
b["distance"] + 2 * b["I(distance^2)"] * c(1, 5, 10) # 中心地から1km、5km、10kmの地点での傾き

# 頂点の距離
-b["distance"] / (2 * b["I(distance^2)"]) # 頂点の距離（km）
max(Kanazawa_land_price$distance)          # データの中で最も遠い基準地の距離（km）

# 片対数モデル ----
model2 <- lm(log(land_price) ~ distance, data = Kanazawa_land_price)
summary(model2)

# 距離が1km離れたときの地価の変化率（%）
b <- coef(model2)
(exp(b["distance"]) - 1) * 100

# 散布図と回帰直線（縦軸は対数目盛）
ggplot(Kanazawa_land_price, aes(x = distance, y = land_price)) +
  geom_point(aes(color = land_use)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + # 片対数の回帰直線
  scale_y_log10() + # 縦軸を対数目盛にする
  labs(
    x = "中心地からの距離（km）",
    y = "地価（万円／m²、対数目盛）",
    color = "用途区分"
  ) +
  theme_bw()

# 二次関数のモデルと片対数モデルの比較（元の単位に戻して描く）
pred <- tibble(distance = seq(0, max(Kanazawa_land_price$distance), length.out = 200))
pred <- pred |>
  mutate(
    quadratic = predict(model1, newdata = pred),     # 二次関数のモデルの予測値
    semilog = exp(predict(model2, newdata = pred))  # 片対数モデルの予測値（exp() で元の単位に戻す）
  ) |>
  pivot_longer(c(quadratic, semilog), names_to = "model", values_to = "land_price")

ggplot(Kanazawa_land_price, aes(x = distance, y = land_price)) +
  geom_point(color = "gray50") +
  geom_line(data = pred, aes(color = model), linewidth = 1) +
  scale_color_discrete(labels = c("二次関数のモデル", "片対数モデル")) +
  labs(
    x = "中心地からの距離（km）",
    y = "地価（万円／m²）",
    color = "モデル"
  ) +
  theme_bw()

# 用途区分を加えた重回帰分析（片対数モデル） ----
model3 <- lm(log(land_price) ~ distance + land_use, data = Kanazawa_land_price)
summary(model3)

# 変化率（%）に換算
b <- coef(model3)
(exp(b[c("distance", "land_use商業地", "land_use工業地")]) - 1) * 100

# 課題：説明変数を1つ追加した分析 ====
# ★ の付いた行を、自分が選んだ変数に合わせて書き換える。
# 例はガスの供給の有無（L02_030）。課題では、この例とは別の変数を選ぶこと。

# 追加する変数を作る
dat2 <- Kanazawa_land_price |>
  mutate(Gas = L02_030) # ★ 新しい変数名 = 元の列（必要なら計算式）

dat2 |> st_drop_geometry() |> select(Gas, land_price) |> View() # ★ 新しい変数の値を確認

# 散布図（縦軸は対数目盛）
ggplot(dat2, aes(x = Gas, y = land_price)) + # ★ x = 新しい変数名
  geom_point(aes(color = land_use)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  scale_y_log10() +
  labs(
    x = "ガスの供給（1：あり、0：なし）", # ★ 横軸のラベル
    y = "地価（万円／m²、対数目盛）",
    color = "用途区分"
  ) +
  theme_bw()

# 重回帰分析
model4 <- lm(
  log(land_price) ~ distance + land_use + Gas, # ★ 最後を新しい変数名に
  data = dat2
)
summary(model4)
