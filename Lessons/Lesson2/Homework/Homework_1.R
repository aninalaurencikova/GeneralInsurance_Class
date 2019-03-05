# Find out, which __year__ was the __most terrific__ for portfolio you have identified as __most profitable__ during the lesson and 
# show it on the chart using `ggplot2` package. Write an explanation about your findings into the code as comment.
# __Commit__ it to your repository into `Lessons/Lesson2/Homework`.

## Code
library(dplyr)
library(ggplot2)

#dt_KPI_raw <- read.csv("./Data/lesson2_KPI.csv")
dt_KPI <- read.csv("C:/Users/laure/Desktop/Anina_github/GeneralInsurance_Class/Data/lesson2_KPI.csv")

dt_KPI_raw %>% 
  mutate(Premium = ifelse(Premium < 0, 0, Premium))

# najprofitabilnejsie portfolio 7
dt_KPI_raw %>%  mutate(UWR = Premium - Expenses - Losses) %>% 
  group_by(Unit) %>% 
  summarize(UWR = sum(UWR, na.rm = TRUE)) %>% 
  arrange(UWR)

dt_KPI_raw %>%  mutate(UWR = Premium - Expenses - Losses) %>% 
  filter(Unit == "Unit7") %>%
  group_by(Year) %>% 
  summarize(UWR = sum(UWR, na.rm = TRUE)) %>% 
  arrange(UWR)

dt_KPI_raw %>% 
  mutate(UWR = Premium - Expenses - Losses) %>% 
  filter(Unit == "Unit7") %>%
  group_by(Year) %>% 
  summarize(UWR = sum(UWR, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(Year, UWR), y = UWR)) + 
  geom_col()

# Your Explanation about analysis:
# Najprv sme dáta očistili od záporného poistného. 
# Potom sme zistili, ktoré portfólo bolo najprofitabilnejšie (Unit 7).
# Vyfiltrovali sme si UWR pre Unit 7, groupli sme ich podľa rokov. 
# Najlepším rokov podľa UWR bol 2013, najhorším 2014. 

# Podľa analýzy: Najhorším rokom bol 2015, pretože má najmenšiu hodnotu UWR.