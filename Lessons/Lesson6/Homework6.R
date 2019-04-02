library(dplyr)
dt_pol_w_claims <- readRDS("./Data/lesson6_dt_pol_w_claims.rds")
set.seed(58742)
ind <- sample(2, nrow(dt_pol_w_claims), replace=TRUE, prob=c(0.80, 0.20)) # generate random indicator to split by
dt_pol_w_claims <- mutate(dt_pol_w_claims,
                          data_status = ifelse(ind == 1, 
                                               "Training",
                                               ifelse(ind == 2, 
                                                      "Validation", 
                                                      "Unseen")
                          )
)
train <- dt_pol_w_claims %>% filter(data_status == "Training")
val <- dt_pol_w_claims %>% filter(data_status == "Validation")
mse <- function(prediction, actual){
  return(sum((prediction-actual)^2, na.rm = TRUE)/length(prediction))
}

#chceme vylepsit tento model
GLMmodel1 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
                formula = Burning_Cost ~ Construct_year + Veh_type1,
                family = Gamma())
summary(GLMmodel1)


#vyskusame tento model na training datach
GLMmodel2 <- glm(data = train,
                       formula = Burning_Cost ~ Construct_year + Veh_type1,
                       family = Gamma())
summary(GLMmodel2)

prediction2a <- predict(GLMmodel2, train, type = "response")
mse(prediction2a, train$Burning_Cost) #191.80
prediction2b <- predict(GLMmodel2, val, type = "response")
mse(prediction2b, val$Burning_Cost) #288.67

#pridame novu premennu D_age, kedze aj skusenosti vodica mozu mat vplyv na jeho skodovost
GLMmodel3 <- glm(data = train,
                 formula = Burning_Cost ~ D_age + Construct_year + Veh_type1,
                 family = Gamma())
summary(GLMmodel3)

prediction3a <- predict(GLMmodel3, train, type = "response")
mse(prediction3a, train$Burning_Cost) #190.67
prediction3b <- predict(GLMmodel3, val, type = "response")
mse(prediction3b, val$Burning_Cost) #283.55
#mse pri train data sa takmer vobec neznizilo
#mse pri validacnych datach sa mierne znizilo, takze mozeme povedat, ze to nas model
#jemne vylepsilo

#capping strategy
#kedze premenne Construct_year aj D_age obsahuju kategorie s malym poctom dat
#ktore mozu kazit nase odhady, rozhodli sme sa pouzit tuto techniku
#pokusime sa identifikovat tieto kategorie a zgrupnut ich dokopy
#budeme analyzovat kazdu premennu zvlast
#premennu Veh_type1 v tejto casti neanalyzujeme, pretoze to nie je ciselna, ale 
#kategorialna premenna
source("./Lessons/Lesson6/Support/emb_chart.R")

#podla Construct_year
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(GLMmodel3, train, type = "response"))),
  x_var =  "Construct_year",
  target = "Burning_Cost",
  prediction =  "pred"
)
#fitovany priemer je najmenej presny v datach cca do roku 2006, zvysne data to fitlo
#velmi pekne
#zgrupneme teda tieto data
train <- train %>% mutate(Construct_year = ifelse(Construct_year <= 2006, 2006, Construct_year))

#podla D_age
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(GLMmodel3, train, type = "response"))),
  x_var =  "D_age",
  target = "Burning_Cost",
  prediction =  "pred"
)
#priemer nebol spravne fitnuty takmer nikde, najviac nam to ale kazia data cca od veku 60
#a do veku cca 30
#zgrupneme ich teda dokopy
train <- train %>% mutate(D_age = ifelse(D_age <= 30, 30, D_age))
train <- train %>% mutate(D_age = ifelse(D_age >= 60, 60, D_age)) 

val <- val %>% mutate(D_age = ifelse(D_age <= 30, 30, D_age))
val <- val %>% mutate(D_age = ifelse(D_age >= 60, 60, D_age)) 

GLMmodel4 <- glm(data = train,
                 formula = Burning_Cost ~ D_age + Construct_year + Veh_type1,
                 family = Gamma())
summary(GLMmodel4)
prediction4a <- predict(GLMmodel4, train, type = "response")
mse(prediction4a, train$Burning_Cost) #190.72
prediction4b <- predict(GLMmodel4, val, type = "response")
mse(prediction4b, val$Burning_Cost) #284.35
#mse pri treningovych aj validacnych datach sa zvysilo
#tento krok nam teda nepomohol zlepsit nas model

#category grouping
#v casti vyssie sme zgrupovali premenne, ktore mali ciselny charakter
#v tejto casti budeme zgrupovat kategorialnu premmennu Veh_type1
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(GLMmodel4, train, type = "response"))),
  x_var = "Veh_type1",
  target = "Burning_Cost",
  prediction = "pred" )
#najhorsie je preimer fitovany pri datach, ktore maju skutocny priemer menej ako 1 a tak
#som ich zgrupla dokopy
train <- train %>% mutate(Veh_type1 = ifelse(as.character(Veh_type1) == 'articulated vehicle' 
                                             | as.character(Veh_type1) == 'motorcycle' 
                                             | as.character(Veh_type1) == 'private truck'
                                             | as.character(Veh_type1) == 'taxi'
                                             | as.character(Veh_type1) == 'tow car'
                                             | as.character(Veh_type1) == 'truck 10 to 20 tn'
                                             | as.character(Veh_type1) == 'truck over 20 tn'
                                             | as.character(Veh_type1) == 'truck up to 10 tn',
                                            'OTHER', as.character(Veh_type1)))
val <- val %>% mutate(Veh_type1 = ifelse(as.character(Veh_type1) == 'articulated vehicle' 
                                             | as.character(Veh_type1) == 'motorcycle' 
                                             | as.character(Veh_type1) == 'private truck'
                                             | as.character(Veh_type1) == 'taxi'
                                             | as.character(Veh_type1) == 'tow car'
                                             | as.character(Veh_type1) == 'truck 10 to 20 tn'
                                             | as.character(Veh_type1) == 'truck over 20 tn'
                                             | as.character(Veh_type1) == 'truck up to 10 tn',
                                             'OTHER', as.character(Veh_type1)))

GLMmodel5 <- glm(data = train,
                 formula = Burning_Cost ~ D_age + Construct_year + Veh_type1,
                 family = Gamma())
prediction5a <- predict(GLMmodel5, train, type = "response")
mse(prediction5a, train$Burning_Cost) #193.41
prediction5b <- predict(GLMmodel5, val, type = "response")
mse(prediction5b, val$Burning_Cost) #287.37
#nemozeme povedat, ze by cele toto co sme urobili nas model nejako zlepsilo.
#mse sa nijako radikalne nemenilo
#ak by som ale mala povedat, ktory krok zlepsil model najviac, tak to bolo pridanie
#premennej
