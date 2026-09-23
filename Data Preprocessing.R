library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

df <- read_csv("Datasets/Daily Air Quality Index (AQI) in Jakarta from 2010 - 2023/ispu_dki_all.csv")
df <- df %>% select(-pm25)

df <- df %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

df <- df %>%
  mutate(tahun = year(ymd(tanggal)))

agg_stasiun_tahun <- df %>%
  group_by(tahun, stasiun) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')

agg_tahun <- agg_stasiun_tahun %>%
  select(-stasiun) %>%
  group_by(tahun) %>%
  summarise(across(everything(), mean, na.rm = TRUE))


agg_tahun <- agg_tahun %>%
  rowwise() %>%
  mutate(max = max(c_across(pm10:no2))) %>%
  ungroup()

agg_tahun

write.csv(agg_tahun, "hasil_aggregasi.csv", row.names = FALSE)

### 2021-2025 PM10
df <- read_csv("Datasets/Daily Air Quality Index (AQI) in Jakarta from 2010 - 2023/ispu_dki_all.csv")
df

df$tanggal <- as.Date(df$tanggal)

# Filter for years 2021 to 2025 and PM10 not NA
data_2021_2025_pm10 <- df %>%
  filter(year(tanggal) >= 2021 & year(tanggal) <= 2025) %>%
  select(stasiun, pm10) %>%
  drop_na(pm10)

write.csv(data_2021_2025_pm10, "2021-2025 pm10.csv", row.names = FALSE)

### Lubang Buaya
df <- read_csv("Datasets/Daily Air Quality Index (AQI) in Jakarta from 2010 - 2023/ispu_dki_all.csv")
df

data_lubangbuaya_monthly <- df %>%
  filter(stasiun == "DKI4 (Lubang Buaya)") %>%
  filter(!is.na(pm10)) %>%
  filter(year(tanggal) >= 2021) %>%  # filter untuk tahun lebih dari 2021
  mutate(year_month = floor_date(tanggal, "month")) %>%
  group_by(year_month) %>%
  summarise(pm10_avg = mean(pm10, na.rm = TRUE)) %>%
  ungroup()

data_lubangbuaya_monthly
# Plot line chart
overall_avg <- mean(data_lubangbuaya_monthly$pm10_avg, na.rm = TRUE)

write.csv(data_lubangbuaya_monthly, "Data Lubang Buaya Monthly.csv", row.names = FALSE)
