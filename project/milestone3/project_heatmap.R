
library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork)

df = read_csv("https://github.com/zaynez4/stat436/raw/refs/heads/main/project/milestone2/tidy_global_inflation_with_NAs.csv")

to_plot = df %>% drop_na() %>% filter(year %in% c(2019, 2020, 2021, 2022), Country %in% c("India", "China", "United States", "Indonesia", "Pakistan", "Nigeria", "Brazil", "Bangladesh", "Russian Federation", "Ethiopia", "Mexico", "Japan", "Egypt, Arab Rep.", "Philippines", "Congo, Dem. Rep.", "Vietnam", "Iran, Islamic Rep.", "Turkey", "Germany", "Thailand")) %>% group_by(Country, year) %>% summarise(avg_infl = mean(rate))


to_plot %>% ggplot() + geom_tile(aes(x=year, y=Country, fill=avg_infl)) + scale_fill_gradient(low="white", high = "black", limits = c(-1, 85)) + labs(fill="average inflation")

p = list()

make_plot = function(df, given_type){
  

  
filtered_data = df %>% drop_na() %>% filter(year %in% c(2019, 2020, 2021, 2022), type == given_type, Country %in% c("India", "China", "United States", "Indonesia", "Pakistan", "Nigeria", "Brazil", "Bangladesh", "Russian Federation", "Ethiopia", "Mexico", "Japan", "Egypt, Arab Rep.", "Philippines", "Congo, Dem. Rep.", "Vietnam", "Iran, Islamic Rep.", "Turkey", "Germany", "Thailand"))

filtered_data <- filtered_data %>%
  mutate(Country = factor(Country, levels = names(sort(tapply(rate, Country, mean)))))


ggplot(filtered_data) + geom_tile(aes(x=year, y=Country, fill=rate)) + scale_fill_gradient(low="white", high = "red", limits = c(-10, 100)) + labs(fill=given_type)
}

p[["p_hcpi"]] = make_plot(df, "Headline Consumer Price Inflation")

p[["p_ecpi"]] = make_plot(df, "Energy Consumer Price Inflation")

p[["p_fcpi"]] = make_plot(df, "Food Consumer Price Inflation")

p[["p_occpi"]] = make_plot(df, "Official Core Consumer Price Inflation")

p[["p_ppi"]] = make_plot(df, "Producer Price Inflation")



p[["p_avg_all_types"]] = df %>% drop_na() %>% filter(year %in% c(2019, 2020, 2021, 2022), Country %in% c("India", "China", "United States", "Indonesia", "Pakistan", "Nigeria", "Brazil", "Bangladesh", "Russian Federation", "Ethiopia", "Mexico", "Japan", "Egypt, Arab Rep.", "Philippines", "Congo, Dem. Rep.", "Vietnam", "Iran, Islamic Rep.", "Turkey", "Germany", "Thailand")) %>% group_by(Country, year) %>% summarise(avg_infl = mean(rate)) %>% ggplot() + geom_tile(aes(x=year, y=Country, fill=avg_infl)) + scale_fill_gradient(low="white", high = "red", limits = c(-10, 100)) + labs(fill="Average Inflation (all types)")

p[["p_hcpi"]] + p[["p_ecpi"]] + p[["p_fcpi"]] + p[["p_occpi"]] + p[["p_ppi"]] + p[["p_avg_all_types"]]
