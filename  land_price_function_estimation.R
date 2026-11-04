# 地価関数の推定：中心地からの距離と地価の関係
#
# 使い方：1行ずつ（またはひとまとまりずつ）Ctrl+Enter（Macは Cmd+Enter）で実行する。
# 途中でうまくいかなくなったら、各セクション冒頭の「再開用」の行から実行し直す。

# ライブラリの読み込み ====

library(tidyverse)
library(sf)
library(tmap)

# データの読み込み ====

# 地価のデータを読み込む ----
# 2022年10月30日時点の累積感染者数（厚生労働省）

Ishikawa_land_price <- 
  read_sf("data/L02-25_17.geojson") |> 
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
    )
  ) 

Ishikawa_land_price
tm_shape(Ishikawa_land_price) + tm_symbols()

# 金沢都市圏の選別
Kanazawa_city_code <- c(
  "17201", "17209", "17210", "17212", 
  "17324", "17361", "17365", "17386"
)

Kanazawa_land_price <- 
  Ishikawa_land_price |> 
  filter(city_code %in% Kanazawa_city_code) # filter関数で、自治体コード（L02_020）が金沢都市圏に含まれるデータだけ取り出す

tm_shape(Kanazawa_land_price) + tm_symbols(fill = "L02_006")

# 最も地価が高い地点を中心地とする----
center <-
  Kanazawa_land_price |>
  slice_max(land_price, n = 1, with_ties = FALSE) # 地価が最大の1地点を選び出す。

tm_shape(Ishikawa_land_price) + tm_symbols(fill = "land_price") +
  tm_shape(center) + tm_symbols(fill = "red")

# 中心地から各地点までの距離を計算し、distance列に格納 ----
Kanazawa_land_price <- 
  Kanazawa_land_price |> 
  mutate(distance = as.numeric(st_distance(geometry, center)[, 1]) / 1000) # 距離（km）を計算する。

Kanazawa_land_price |> select(distance, land_price) |> View() # 確認

# 回帰分析 ----
model0 <- lm(land_price ~ distance + I(distance^2), data = Kanazawa_land_price)

summary(mode0)

# 散布図と回帰曲線
ggplot(Kanazawa_land_price, aes(x = distance, y = land_price, color = land_use)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) + # 二次の回帰曲線
  labs(
    x = "中心地からの距離（km）",
    y = "地価（万円／m²）"
  ) +
  theme_bw()

ggsave("KanazawaLandPriceFunction.png")

model1 <- lm(log(land_price) ~ distance, data = Kanazawa_land_price)

summary(model1)

# 散布図と回帰曲線
ggplot(Kanazawa_land_price, aes(x = distance, y = land_price, color = land_use)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + # 片対数の回帰曲線
  scale_y_log10() +
  labs(
    x = "中心地からの距離（km）",
    y = "地価（万円／m²）"
  ) +
  theme_bw()

ggsave("KanazawaLandPriceFunction.png")

# 片対数モデルの予測値（元の単位に戻す）
pred <- tibble(distance = seq(0, max(Kanazawa_land_price$distance), length.out = 200))
pred <- pred |> mutate(land_price = exp(predict(model1, newdata = pred)))

ggplot(Kanazawa_land_price, aes(x = distance, y = land_price, color = land_use)) +
geom_point() +
geom_line(data = pred, color = "blue", linewidth = 1) + # 片対数モデルの回帰曲線
labs(
x = "中心地からの距離（km）",
y = "地価（万円／m²）"
) +
theme_bw()

ggsave("KanazawaLandPriceFunction.png")
