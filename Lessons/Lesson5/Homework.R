library(dplyr)
dt_Policy <- read.csv("./Data/lesson5_PolicyHistory.csv")
dt_Policy <- dt_Policy %>% distinct(NrPolicy, NrObject, .keep_all = TRUE) 
dt_Claims <- read.csv("./Data/lesson5_Claims.csv") 
dt_Claims <- dt_Claims %>% distinct(NrClaim, .keep_all = TRUE)

dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject"))

library(lubridate)
dt_pol_w_claims <- dt_pol_w_claims %>% 
  mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start),
         Ult_Loss = Paid + Reserves,
         Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure)))

#Construct_year
library(ggplot2)
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_jitter()

# Najviac bodov a zaroven najvacsie hodnoty sa nadobudaju pri vozidlach vyrobenych 
# v rokoch 2005 - 2015. Dovodom je jednak to, ze je v poistovni najviac takychto vozidiel
#poistenych, ale aj to, ze vozidla casom stracaju hodnotu a teda castokrat im poistovna
#v pripade skody vyplati velmi male odskodne.

dt_pol_w_claims %>% 
  filter(Burning_Cost != 0) %>% 
  group_by(Construct_year) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
#Najvyssi priemer aj median BC maju vozidla vyrobene v roku 1997. Toto by sme ale nemali
#brat do uvahy, pretoze mame len 2 data, takze to nie je relevantne.
#Ako som spomenula vyssie, najviac dat a zaroven najvacsie BC dosiahli vozidla 
#vyrobene v rokoch 2005 - 2015. Do uvahy zoberieme ale data od roku 2000 (ako relevantne).
#najvyssi priemer BC - auta vyrobene v roku 2014
#najvyssi median - auta vyrobene v roku 2004

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_boxplot() + coord_flip()
  ylim(0, 100)
  
#vidime, ze v datach je vela outlierov, takze toto nam moze kazit nasu analyzu a predikciu

#Veh_type1  
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Veh_type1)) + 
  geom_jitter() + coord_flip()

# Najviac bodov a zaroven najvacsie hodnoty sa nadobudaju pri private car. 
# Toto je logicke, kedze pravdepodobne najviac takychto aut je poistenych. Vela hodnot
#sa nachadza aj pri commercial car - toto je tiez logicke, kedze su to auta pouzivane vo 
#firmach, jazdi sa na nich casto a castokrat si ludia s firemnym autom davaju menej pozor. 
#Logicky, v grafe je vidno BC aj pri autach z autoskoly.

dt_pol_w_claims %>% 
  filter(Burning_Cost != 0) %>% 
  group_by(Veh_type1) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
#Najvyssi priemer maju tow car - teda odtahove vozidla. 
#za nimi v priemere nasleduju motorcycle - dovodom je zrejme to, ze ked motorkar nabura,
#vacsinou je to totalka. Pri private car je priemer aj median dost nizky napriek tomu,
#ze tych skod je tam najviac. Dovodm je to, ze ludia hlasili aj skrabance, a teda 
#je skod vela, ale nie tak velkych.

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Veh_type1)) + 
  geom_boxplot() + coord_flip()
  ylim(0, 100)
  
#velmi vela outlierov, takze nase data nam mozu kazit nasu analyzu
  
# GLM model
GLMmodel <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
                formula = Burning_Cost ~ Construct_year + Veh_type1,
                family = Gamma())
summary(GLMmodel)

#no zda sa, ze s modelom spokojna nie som, kedze pri tejto kombinacii faktorov nie su
#construct year vobec vyznamne faktory. 