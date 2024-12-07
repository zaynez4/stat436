library(tidyverse)
library(ggridges)
library(RColorBrewer)
theme_set(theme_bw())

global_inflation = read_csv("https://github.com/zaynez4/stat436/raw/refs/heads/main/project/milestone2/tidy_global_inflation.csv")


options(warn=-1)
ggplot(global_inflation, aes(log(rate), type, fill=type)) +
  geom_density_ridges(alpha=0.5) +
  scale_x_log10(oob = scales::squish_infinite) +
  scale_fill_manual(values = rev(brewer.pal(5, "RdBu"))) +
  guides(fill = "none") +
  labs(title = "Density Ridge Plot for Inflation Type Rates",
       x = "Log-scaled Rate",
       y = "Density")


ggplot(global_inflation, aes(log(rate), fill=type)) +
  geom_density(alpha=0.5, bw=0.12) +
  scale_fill_manual(values = c("#4C00FF", "#0880FF", "#00E5FF", "#00FF4D", "#FFE0B3")) +
  guides(fill = "none") +
  labs(title = "Density Plot for Inflation Type Rates",
       x = "Log-scaled Rate",
       y = "Density")


options(warn=0)
