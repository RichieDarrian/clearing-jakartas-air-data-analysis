library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library('tidyverse')
library(scales)

### 1. Pollutant Trend in Jakarta (2010–2025)
df1 = read.csv('Datasets/Daily Air Quality Index (AQI) in Jakarta from 2010 - 2023/ispu_dki_all (aggregated).csv')
df1

agg_long$polutan <- factor(agg_long$polutan, levels = c("o3", "pm10", "so2", "co", "no2"))

ggplot(agg_long, aes(x = tahun, y = konsentrasi, color = polutan)) +
  geom_line(size = 1) +
  annotate("rect", xmin = 2019, xmax = 2023, ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = 0.2) +
  annotate("text", x = 2021, y = max(agg_long$konsentrasi, na.rm = TRUE) * 0.95,
           label = "COVID-19", color = "black", size = 4) +
  scale_x_continuous(breaks = 2010:2025) +
  scale_color_manual(values = c(
    o3   = "#E41A1C",
    pm10 = "#FFD700",
    so2  = "#1B5E20",
    co   = "#66BB6A",
    no2  = "#C8E6C9"
  )) +
  labs(
    title = "Pollutant Trend in Jakarta (2010–2025)",
    x = "Year",
    y = "Concentration (µg/m³)",
    color = "Pollutant",
    caption = "Data source: https://www.kaggle.com/datasets/senadu34/air-quality-index-in-jakarta-2010-2021"
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

### 2. Recorded PM10 Distribution by Station in Jakarta (2021-2025)
df1.5 = read.csv('Datasets/Daily Air Quality Index (AQI) in Jakarta from 2010 - 2023/2021-2025 pm10.csv')
df1.5

df1.5_clean <- df1.5 %>%
  filter(!is.na(stasiun))

other_stations <- setdiff(unique(df1.5_clean$stasiun), "DKI4 (Lubang Buaya)")

yellow_shades <- scales::seq_gradient_pal("#FFFFCC", "#997A00", "Lab")(seq(0, 1, length.out = length(other_stations)))

color_map <- c("DKI4 (Lubang Buaya)" = "red")
color_map[other_stations] <- yellow_shades
avg_pm10 <- mean(df1.5_clean$pm10, na.rm = TRUE)

# Plot
ggplot(df1.5_clean, aes(x = stasiun, y = pm10, fill = stasiun)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  scale_fill_manual(values = color_map) +
  geom_hline(yintercept = avg_pm10, linetype = "dashed", color = "blue", size = 1) +
  annotate("text", x = 7, y = avg_pm10, label = paste0("Avg: ", round(avg_pm10, 1), " µg/m³"),
           hjust = 1.1, vjust = -0.5, color = "blue", size = 4) +
  labs(title = "Recorded PM10 Distribution by Station in Jakarta (2021-2025)",
       x = "Station",
       y = "Concentration (µg/m³)",
       fill = "Station",
       caption = "Data source: https://www.kaggle.com/datasets/senadu34/air-quality-index-in-jakarta-2010-2021"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### 3. Average PM10 Concentration in Lubang Buaya Station Per Month
df1.75 = read.csv('Datasets/Daily Air Quality Index (AQI) in Jakarta from 2010 - 2023/Data Lubang Buaya Monthly.csv')
df1.75
overall_avg <- mean(df1.75$pm10_avg, na.rm = TRUE)
df1.75$year_month <- as.Date(df1.75$year_month)  

ggplot(df1.75, aes(x = year_month, y = pm10_avg)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = overall_avg, linetype = "dashed", color = "red", size = 1) +
  annotate("text", 
           x = max(df1.75$year_month), 
           y = overall_avg, 
           label = paste0("Average\nOverall: ", round(overall_avg, 2)),
           color = "red", 
           hjust = -0.1,   # sedikit keluar ke kanan
           vjust = -0.5,   # sedikit ke atas garis
           size = 4) +
  labs(title = "Average PM10 Concentration in Lubang Buaya Station Per Month",
       x = "Month-Year",
       y = "Concentration (µg/m³)") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months", expand = expansion(mult = c(0.05, 0.2))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 4. Proportion of Motor Vehicles in Jakarta (2016–2022)
df2 = read.csv('Datasets/Jumlah Kendaraan Bermotor Menurut Jenis Kendaraan (unit) di Provinsi DKI Jakarta, 2016-2022.csv')
df2

data_kendaraan <- tribble(
  ~Jenis,          ~`2016`, ~`2017`, ~`2018`, ~`2019`, ~`2020`, ~`2021`, ~`2022`,
  "Mobil Penumpang", 2570433, 2827399, 3082616, 3310426, 3365467, 3544491, 3766059,
  "Bus",               29336,   31593,   33419,   34905,   35266,   36339,   37180,
  "Truk",             553320,  587860,  631156,  669724,  679708,  713059,  748395,
  "Sepeda Motor",   13338593,14137126,15037359,15868191,16141380,16711638,17304447
)

data_persen <- data_kendaraan %>%
  pivot_longer(-Jenis, names_to = "Tahun", values_to = "Jumlah") %>%
  mutate(Tahun = as.integer(Tahun)) %>%
  group_by(Tahun) %>%
  mutate(Persentase = Jumlah / sum(Jumlah)) %>%
  ungroup()

warna_manual <- c(
  "Bus" = "#98FB98",         
  "Truk" = "#32CD32",            
  "Mobil Penumpang" = "#228B22", 
  "Sepeda Motor" = "brown"    
)

data_persen$Jenis <- factor(data_persen$Jenis, levels = c("Bus", "Truk", "Mobil Penumpang", "Sepeda Motor"))

ggplot(data_persen, aes(x = Tahun, y = Persentase, fill = Jenis)) +
  geom_area(alpha = 0.95, color = "white") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2016:2022) +
  scale_fill_manual(values = warna_manual) +
  labs(
    title = "Proportion of Motor Vehicles in Jakarta (2016–2022)",
    y = "Percentage",
    x = "Year",
    fill = "Motor Vehicle Type",
    caption = "Data source: https://jakarta.bps.go.id/id/statistics-table/2/Nzg2IzI=/jumlah-kendaraan-bermotor-menurut-jenis-kendaraan-unit-di-provinsi-dki-jakarta.html"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

### Crude Oil Production in Indonesia (1996–2023)
df3 = read.csv('Datasets/Produksi Minyak Bumi dan Gas Alam, 1996-2023.csv')
df3

data_produksi <- tribble(
  ~Tahun, ~Minyak, ~Gas,
  1996, 548648.3, 3164016.2,
  1997, 543752.6, 3166034.9,
  1998, 534892.0, 2978851.9,
  1999, 494643.0, 3068349.1,
  2000, 484393.3, 2845532.9,
  2001, 480116.1, 3762828.5,
  2002, 397308.5, 2279373.9,
  2003, 383700.0, 2142605.0,
  2004, 404992.9, 3026069.3,
  2005, 387653.5, 2985341.0,
  2006, 357477.4, 2948021.6,
  2007, 348348.0, 2805540.3,
  2008, 358718.7, 2790988.0,
  2009, 346313.0, 2887892.2,
  2010, 344888.0, 3407592.3,
  2011, 329249.3, 3256378.9,
  2012, 314665.9, 2982753.5,
  2013, 301191.9, 2969210.8,
  2014, 287902.2, 2999524.4,
  2015, 286814.2, 2948365.8,
  2017, 292373.8, 2781154.0,
  2018, 281826.6, 2833783.5,
  2019, 273494.8, 2647985.9,
  2020, 259246.8, 2442830.7,
  2021, 240324.5, 2433364.0,
  2022, 223532.5, 1962929.0,
  2023, 221088.9, 2420059.5
)

data_long <- data_produksi %>%
  pivot_longer(cols = c(Minyak, Gas), names_to = "Jenis", values_to = "Produksi")

data_minyak <- subset(data_long, Jenis == "Minyak")
data_gas <- subset(data_long, Jenis == "Gas")

ggplot(data_minyak, aes(x = Tahun, y = Produksi)) +
  geom_line(color = "#FF4500", size = 1.2) +
  geom_point(color = "#FF4500", size = 2) +
  scale_x_continuous(breaks = seq(1996, 2023, by = 2)) +
  scale_y_continuous(
    breaks = seq(0, max(data_minyak$Produksi, na.rm = TRUE), by = 50000),
    labels = comma
  ) +
  labs(
    title = "Crude Oil Production in Indonesia (1996–2023)",
    x = "Year",
    y = "Production (in thousand barrels)"
  ) +
  theme_minimal()
