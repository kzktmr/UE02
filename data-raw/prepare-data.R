# 授業用データの作成（教員用）
#
# data-raw/ の元データを整形し、学生用の CSV を data/ に書き出す。
# プロジェクト直下（UE01/）を作業ディレクトリにして実行すること。
#
# 入力：
#   data-raw/confirmed_cases_cumulative_daily.csv  累積陽性者数（厚生労働省）
#   data-raw/kenbetsu-vaccination_data3.xlsx       都道府県別接種実績（厚生労働省）
#   data/a001.xls                                  社会生活統計指標（総務省統計局）
# 出力：
#   data/covid_cases.csv   Prefecture, Confirmed_Cases
#   data/vaccination.csv   Prefecture, Vaccination_Rate
#   data/dat.csv           結合済みデータ（授業中の救済用）

library(tidyverse)
library(readxl)

target_date <- "2022/10/30" # 感染者数を取り出す日付
# 人口は2022年10月1日現在の推計人口（a001.xls の N列）を使う


# 都道府県名の対応表（日本語 ⇔ ローマ字） ====
# 学生用スクリプトと同じ手順でローマ字表記を作る

pref_table <-
  read_excel(
    path = "data/a001.xls",
    range = "I13:J59",
    col_names = c("都道府県", "Prefecture"),
    col_types = c("text", "text")
  ) |>
  mutate(
    Prefecture = str_remove(Prefecture, "-ken$|-to$|-fu$"),
    Prefecture = if_else(Prefecture == "Gumma", "Gunma", Prefecture)
  )

stopifnot(nrow(pref_table) == 47, !anyDuplicated(pref_table$Prefecture))


# 感染者数 ====

covid_cases <-
  read_csv("data-raw/confirmed_cases_cumulative_daily.csv", show_col_types = FALSE) |>
  filter(Date == target_date) |>
  select(-c(Date, ALL)) |>
  pivot_longer(
    cols = everything(),
    names_to = "Prefecture",
    values_to = "Confirmed_Cases"
  )

stopifnot(
  nrow(covid_cases) == 47,
  setequal(covid_cases$Prefecture, pref_table$Prefecture)
)

write_csv(covid_cases, "data/covid_cases.csv")


# 接種率 ====
# シート「総接種回数」の3回目接種率（死亡者分を除外した接種回数 ÷ 人口）を使う。
# 列の並び：1 都道府県名, 2 総接種回数, 3-5 1回目, 6-8 2回目, 9-11 3回目,
#           （各回：接種回数, 除外する回数, 接種率）
# 2回目接種率（8列目）とは相関が0.97と高く、どちらを使っても結論は同じ。

vaccination <-
  read_excel(
    path = "data-raw/kenbetsu-vaccination_data3.xlsx",
    sheet = "総接種回数",
    col_names = FALSE,
    .name_repair = "minimal"
  ) |>
  select(name = 1, rate_3rd = 11) |>
  filter(str_detect(name, "^\\d{2} ")) |> # 「01 北海道」形式の行だけ残す
  mutate(
    都道府県 = str_remove(name, "^\\d{2} "),
    Vaccination_Rate = as.numeric(rate_3rd) * 100 # %表示
  ) |>
  inner_join(pref_table, by = "都道府県") |>
  select(Prefecture, Vaccination_Rate)

stopifnot(nrow(vaccination) == 47, !anyNA(vaccination))

write_csv(vaccination, "data/vaccination.csv")


# 結合済みデータ（救済用） ====
# 学生用スクリプトの手順をそのまま再現する

population_data <-
  bind_cols(
    read_excel(
      path = "data/a001.xls",
      range = "I13:N59",
      col_names = c("都道府県", "Prefecture", "Population"),
      col_types = c("text", "text", "skip", "skip", "skip", "numeric") # 2022年
    ),
    read_excel(
      path = "data/a001.xls",
      range = "CZ13:DC59",
      col_names = c("DID_Population", "DID_Area"),
      col_types = c("numeric", "skip", "skip", "numeric")
    )
  ) |>
  mutate(
    Prefecture = str_remove(Prefecture, "-ken$|-to$|-fu$"),
    Prefecture = if_else(Prefecture == "Gumma", "Gunma", Prefecture),
    DID_Density = DID_Population / DID_Area
  ) |>
  select(-c(DID_Population, DID_Area))

dat <- population_data |>
  left_join(covid_cases, by = "Prefecture") |>
  left_join(vaccination, by = "Prefecture") |>
  mutate(Confirmed_Cases_per_Capita = Confirmed_Cases / Population * 100)

stopifnot(nrow(dat) == 47, !anyNA(dat))

write_csv(dat, "data/dat.csv")
