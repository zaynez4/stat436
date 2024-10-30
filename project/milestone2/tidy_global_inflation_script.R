library(tidyverse)
library(dplyr)

global_inflation = read_csv("https://github.com/zaynez4/stat436/raw/refs/heads/main/project/milestone2/Global%20Dataset%20of%20Inflation.csv")

tidy_global_inflation = 
  as.data.frame(global_inflation %>% 
                  pivot_longer('1970':'2022', names_to = "year", values_to = "rate",
                               values_drop_na=T)) %>% 
  rename(CountryCode = 'Country Code',
         IMF = 'IMF Country Code',
         type = 'Series Name') %>% 
  select(CountryCode, IMF, Country, type, year, rate) %>% 
  arrange(Country)

head(tidy_global_inflation)