######################## LIBRARIES

library(vdemdata) #data
library(dplyr) #data wrangling
library(ggplot2) #plots
library(plotly) #animated plots
library(forcats) #time series analysis
library(janitor) #clean names
library(colorBlindness) #test color blind friendly
library(htmlwidgets) #create html files for animated plots

######################## GET DATA

dem_data = vdem |>
  select(
    country = country_name,
    vDemCtryId = country_id,
    year,
    region = e_regionpol_6C,
    parDem = v2x_partipdem,
    eduPlus15 = e_peaveduc,
    eduUnequal = e_peedgini,
    pop = e_pop
  ) |>
  mutate(region = case_match(region,
    1 ~ "Eastern Europe",
    2 ~ "Latin America",
    3 ~ "Middle East",
    4 ~ "Africa",
    5 ~ "The West",
    6 ~ "Asia")
  )

######################## SCATTERPLOT 1: 1895-1905

demDataSummary1 = dem_data |>
  filter(year %in% 1895:1905) |>
  group_by(country, region) |>
  summarize(
    parDem = mean(parDem, na.rm = TRUE),
    eduUnequal = mean(eduUnequal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(parDem))

nineteenth = ggplot(demDataSummary1, aes(x = eduUnequal, y = parDem, color = region, label = country)) +
  geom_point() +
  geom_smooth(method = "loess", linewidth = 1, color = "black") +
  geom_hline(yintercept = 0.1607, linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = 73, y = 0.19, label = "Average Participatory Democracy Score", fontface = 2) +
  labs(
    x = "Education Inequality",
    y = "Participatory Democracy",
    caption = "Source: V-Dem Institute",
    color = "Region"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis_d() +
  theme_classic() +
  theme(legend.title = element_blank())

nineteenth_interactive = ggplotly(nineteenth, tooltip = "all")
saveWidget(nineteenth_interactive, "education_democracy_1895_1905.html", selfcontained = TRUE)

######################## SCATTERPLOT 2: 1995-2005

demDataSummary2 = dem_data |>
  filter(year %in% 1995:2005) |>
  group_by(country, region) |>
  summarize(
    parDem = mean(parDem, na.rm = TRUE),
    eduUnequal = mean(eduUnequal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(parDem))

twentieth = ggplot(demDataSummary2, aes(x = eduUnequal, y = parDem, color = region, label = country)) +
  geom_point() +
  geom_smooth(method = "loess", linewidth = 1, color = "black") +
  geom_hline(yintercept = 0.1607, linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = 68, y = 0.29, label = "Average Participatory Democracy Score", fontface = 2) +
  labs(
    x = "Education Inequality",
    y = "Participatory Democracy",
    caption = "Source: V-Dem Institute",
    color = "Region"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis_d() +
  theme_classic() +
  theme(legend.title = element_blank())

twentieth_interactive = ggplotly(twentieth, tooltip = "all")
saveWidget(twentieth_interactive, "education_democracy_1995_2005.html", selfcontained = TRUE)

######################## TIME SERIES: AFFLUENT COUNTRIES

countries = c("United States of America", "Germany", "Mexico", "Brazil")

time_series = dem_data |>
  filter(country %in% countries) |>
  ggplot(aes(x = year, y = parDem, color = country)) +
  geom_line(linewidth = 1) +
  stat_smooth(color = "black", fill = "black", method = "loess") +
  labs(
    x = "Year",
    y = "Participatory Democracy Index",
    caption = "Source: V-Dem Institute",
    color = "Country"
  ) +
  scale_color_viridis_d() +
  theme_classic()

time_series_interactive = ggplotly(time_series, tooltip = "all")
saveWidget(time_series_interactive, "democracy_time_series_affluent.html", selfcontained = TRUE)

######################## BAR CHART DATA

bar_chart_data = dem_data |>
  filter(year >= 2000) |>
  group_by(region) |>
  summarize(
    parDem = mean(parDem, na.rm = TRUE),
    eduPlus15 = mean(eduPlus15, na.rm = TRUE),
    eduUnequal = mean(eduUnequal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(parDem)

######################## BAR CHART: PARTICIPATORY DEMOCRACY BY REGION

bar_pardem = ggplot(bar_chart_data, aes(x = fct_reorder(region, parDem), y = parDem)) +
  geom_col(fill = "#005699") +
  labs(
    x = "Region",
    y = "Participatory Democracy",
    caption = "Source: V-Dem Institute"
  ) +
  theme_classic()

ggsave("bar_democracy_by_region.png", plot = bar_pardem, width = 7, height = 5, dpi = 150)

######################## BAR CHART: EDUCATION INEQUALITY BY REGION

bar_eduineq = ggplot(bar_chart_data, aes(x = fct_reorder(region, eduUnequal), y = eduUnequal)) +
  geom_col(fill = "#005699") +
  labs(
    x = "Region",
    y = "Education Inequality (population 15+)",
    caption = "Source: V-Dem Institute"
  ) +
  theme_classic()

ggsave("bar_education_inequality_by_region.png", plot = bar_eduineq, width = 7, height = 5, dpi = 150)

######################## BAR CHART: AVERAGE YEARS OF EDUCATION BY REGION

bar_eduyrs = ggplot(bar_chart_data, aes(x = fct_reorder(region, eduPlus15), y = eduPlus15)) +
  geom_col(fill = "#005699") +
  labs(
    x = "Region",
    y = "Average Years of Education (population 15+)",
    caption = "Source: V-Dem Institute"
  ) +
  theme_classic()

ggsave("bar_education_years_by_region.png", plot = bar_eduyrs, width = 7, height = 5, dpi = 150)
