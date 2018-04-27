rm(list=ls())
library(dplyr)
library(ggplot2)
raw_data <- read.csv("Documents/Honors Thesis/compas-scores-two-years.csv")
nrow(raw_data)

#All Races
df <- dplyr::select(raw_data, age, c_charge_degree, c_charge_desc, race, age_cat, score_text, sex, juv_fel_count, juv_misd_count, juv_other_count, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
        filter(days_b_screening_arrest <= 30) %>%
        filter(days_b_screening_arrest >= -30) %>%
        filter(is_recid != -1) %>%
        filter(c_charge_degree != "O") %>%
        filter(score_text != 'N/A')
nrow(df)
#6172

#Only Black and White
df_race<- dplyr::select(raw_data, age, c_charge_degree, c_charge_desc, race, age_cat, score_text, sex, juv_fel_count, juv_misd_count, juv_other_count, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
        filter(days_b_screening_arrest <= 30) %>%
        filter(days_b_screening_arrest >= -30) %>%
        filter(is_recid != -1) %>%
        filter(c_charge_degree != "O") %>%
        filter(score_text != 'N/A') %>%
        filter(race %in% c("African-American","Caucasian"))
nrow(df_race)
#5278

#Number of Juvenile Charges
df_race$juv_fel_misd <- as.numeric(df_race$juv_fel_count) + as.numeric(df_race$juv_misd_count)

#Violent Crimes
violent = c("Agg Assault Law Enforc Officer", "Agg Assault W/int Com Fel Dome", 
        "Agg Batt W/Arm S/B/I 25 Min/Ma", "Agg Battery Bod Hrm-Deadly Weap", "Agg Battery Grt/Bod/Harm", 
        "Agg Battery Law Enforc Officer", "Agg Flee/Eluding (Injury/Prop Damage)", "Agg Fleeing and Eluding", 
        "Agg Fleeing/Eluding High Speed", "Aggrav Battery w/Deadly Weapon", "Aggrav Child Abuse-Agg Battery", 
        "Aggrav Child Abuse-Causes Harm", "Aggravated Assault", "Battery")

df_race <- mutate(df_race, violent_charge = factor(c_charge_desc != violent, labels = c("Violent","NonViolent")))

df_race$violent_charge <- "NonViolent"
  df_race$violent_charge[df_race$c_charge_desc == "Agg Assault Law Enforc Officer"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Agg Assault W/int Com Fel Dome"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Agg Batt W/Arm S/B/I 25 Min/Ma"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Agg Battery Bod Hrm-Deadly Weap"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Agg Battery Grt/Bod/Harm"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Agg Battery Law Enforc Officer"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Agg Flee/Eluding (Injury/Prop Damage)"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Agg Fleeing and Eluding"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Agg Fleeing/Eluding High Speed"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aggrav Battery w/Deadly Weapon"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aggrav Child Abuse-Agg Battery"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aggrav Child Abuse-Causes Harm"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aggravated Assault"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aggravated Assault W/Dead Weap"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aggravated Assault W/dead Weap"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aggravated Assault w/Firearm"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aggravated Battery"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aggravated Battery / Pregnant"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aggravated Battery On 65/Older"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Aid/Abet Burglary Assault/Batt"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Armed Carjacking"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Armed False Imprisonment"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Armed Kidnapping"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Armed Sex Batt/vict 12 Yrs +"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Arson II (Vehicle)"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Arson in the Second Degree"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Assault"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Assault Law Enforcement Officer"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Assault On Law Enforc Officer"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Att Robbery Sudd Snatch No Weap"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Attempt Burglary (Struct)"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Attempt Felony Murder"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Attempt Murder in the First Degree"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Attempted Robbery  No Weapon"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Attempted Robbery Firearm"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Battery"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Battery Emergency Care Provide"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Battery on a Person Over 65"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Battery on Law Enforc Officer"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Battery Spouse Or Girlfriend"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Battery Upon Detainee"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Burglary Conveyance Assault/Bat"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Burglary Dwelling Armed"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Burglary Dwelling Assault/Batt"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Burglary With Assault/battery"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Carjacking"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Child Abuse"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Cruelty To Animals"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "D.U.I. Serious Bodily Injury"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Discharge Firearm in Public/Res"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "DOC/Engage In Fighting"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "DOC/Fighting/Threatening Words"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Felony Batt(Great Bodily Harm)"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Felony Battery"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Felony Battery (Dom Strang)"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Felony Battery w/Prior Convict"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Home Invasion Robbery"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Kidnapping (Facilitate Felony)"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Kidnapping / Domestic Violence"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Manslaughter with Weapon"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Murder in 2nd Degree"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Murder in the First Degree"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Neglect Child / Bodily Harm"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Robbery"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Robbery / No Weapon"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Robbery / Weapon"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Robbery Sudd Snatch No Weapon"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Robbery Sudd Snatch w/Weapon"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Robbery W/Deadly Weapon"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Robbery W/Firearm"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Robbery-Strong Arm W/mask"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Sex Batt Faml/Cust Vict 12-17Y"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Sex Battery Deft 18+/Vict 11-"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Sexual Battery / Vict 12 Yrs +"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Shoot In Occupied Building"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Shoot/Throw Into Vehicle"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Stalking (Aggravated)"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Strong Armed  Robbery"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Threat Public Servant"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Threaten Throw Destruct Device"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Throw Deadly Missile Into Veh"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Throw In Occupied Dwell"] = "Violent"
  df_race$violent_charge[df_race$c_charge_desc == "Vehicular Homicide"] = "Violent"

#Variable into Factors
df_race <- mutate(df_race, crime_factor = factor(c_charge_degree)) %>%
      mutate(race_factor = factor(race)) %>%
      within(race_factor <- relevel(race_factor, ref = 2)) %>%
      mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
      within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
      mutate(score_factor = factor(score_text != "Low", labels = c("LowScore","HighScore")))

summary(df_race$race)
#African American
#3175 
#Caucasian
#2103

#Confusion Matrix
library(caret)

#Test/Training Samples
set.seed(1)
n <- nrow(df_race)
n.train = sample(1:n, n*.5)
n.test = (-n.train)
train = df_race[n.train,]
test = df_race[n.test,]
library(pROC)
less25_subset = test[which(test$age_cat=="Less than 25") ,]
nrow(less25_subset)
#565
from25to45_subset = test[which(test$age_cat=="25 - 45") ,]
nrow(from25to45_subset)
#1510
greater45_subset = test[which(test$age_cat=="Greater than 45") ,]
nrow(greater45_subset)
#564

#MULTIPLE LINEAR REGRESSION
#Neither
#Less than 25
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, less25_subset, type="response")
lm.pred = rep(0,565)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  86  59
#          1 157 263
                                          
#                Accuracy : 0.6177          
#                  95% CI : (0.5762, 0.6579)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.01187         
                                          
#                   Kappa : 0.1796          
#  Mcnemar's Test P-Value : 4.111e-11       
                                          
#             Sensitivity : 0.3539          
#             Specificity : 0.8168*          
#          Pos Pred Value : 0.5931          
#          Neg Pred Value : 0.6262          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1522          
#    Detection Prevalence : 0.2566          
#       Balanced Accuracy : 0.5853  

#25 - 45
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, from25to45_subset, type="response")
lm.pred = rep(0,1510)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 575 272
#          1 215 448
                                         
#                Accuracy : 0.6775         
#                  95% CI : (0.6532, 0.701)
#     No Information Rate : 0.5232         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3513         
#  Mcnemar's Test P-Value : 0.01116        
                                         
#             Sensitivity : 0.7278         
#             Specificity : 0.6222         
#          Pos Pred Value : 0.6789         
#          Neg Pred Value : 0.6757         
#              Prevalence : 0.5232         
#          Detection Rate : 0.3808         
#    Detection Prevalence : 0.5609         
#       Balanced Accuracy : 0.6750         
                                   
#Greater Than 45
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, greater45_subset, type="response")
lm.pred = rep(0,564)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 349 130
#          1  27  58
                                          
#                Accuracy : 0.7216          
#                  95% CI : (0.6827, 0.7583)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.002887        
                                          
#                   Kappa : 0.2743          
#  Mcnemar's Test P-Value : 3.937e-16       
                                          
#             Sensitivity : 0.9282**          
#             Specificity : 0.3085          
#          Pos Pred Value : 0.7286          
#          Neg Pred Value : 0.6824          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6188          
#    Detection Prevalence : 0.8493          
#       Balanced Accuracy : 0.6184     

#Race
#Less than 25
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, less25_subset, type="response")
lm_race.pred = rep(0,565)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  81  59
#          1 162 263
                                          
#                Accuracy : 0.6088          
#                  95% CI : (0.5672, 0.6493)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.03345         
                                          
#                   Kappa : 0.1583          
#  Mcnemar's Test P-Value : 6.825e-12       
                                          
#             Sensitivity : 0.3333          
#             Specificity : 0.8168*          
#          Pos Pred Value : 0.5786          
#          Neg Pred Value : 0.6188          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1434          
#    Detection Prevalence : 0.2478          
#       Balanced Accuracy : 0.5751          
                                   
#25 - 45
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, from25to45_subset, type="response")
lm_race.pred = rep(0,1510)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 573 274
#          1 217 446
                                          
#                Accuracy : 0.6748          
#                  95% CI : (0.6506, 0.6984)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.346           
#  Mcnemar's Test P-Value : 0.0115          
                                          
#             Sensitivity : 0.7253          
#             Specificity : 0.6194          
#          Pos Pred Value : 0.6765          
#          Neg Pred Value : 0.6727          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3795          
#    Detection Prevalence : 0.5609          
#       Balanced Accuracy : 0.6724    

#Greater than 45
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, greater45_subset, type="response")
lm_race.pred = rep(0,564)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 348 130
#          1  28  58
                                          
#                Accuracy : 0.7199          
#                  95% CI : (0.6808, 0.7566)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.003813        
                                          
#                   Kappa : 0.2708          
#  Mcnemar's Test P-Value : 9.348e-16       
                                          
#             Sensitivity : 0.9255**          
#             Specificity : 0.3085          
#          Pos Pred Value : 0.7280          
#          Neg Pred Value : 0.6744          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6170          
#    Detection Prevalence : 0.8475          
#       Balanced Accuracy : 0.6170   

#Gender**
#Less than 25
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, less25_subset, type="response")
lm_sex.pred = rep(0,565)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  76  44
#          1 167 278
                                          
#                Accuracy : 0.6265          
#                  95% CI : (0.5852, 0.6666)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.003555        
                                          
#                   Kappa : 0.1878          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.3128          
#             Specificity : 0.8634**          
#          Pos Pred Value : 0.6333          
#          Neg Pred Value : 0.6247          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1345          
#    Detection Prevalence : 0.2124          
#       Balanced Accuracy : 0.5881          
                                 
#25 - 45
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, from25to45_subset, type="response")
lm_sex.pred = rep(0,1510)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 521 248
#          1 269 472
                                          
#                Accuracy : 0.6576          
#                  95% CI : (0.6331, 0.6816)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3146          
#  Mcnemar's Test P-Value : 0.3791          
                                          
#             Sensitivity : 0.6595          
#             Specificity : 0.6556          
#          Pos Pred Value : 0.6775          
#          Neg Pred Value : 0.6370          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3450          
#    Detection Prevalence : 0.5093          
#       Balanced Accuracy : 0.6575          

#Greater than 45
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, greater45_subset, type="response")
lm_sex.pred = rep(0,564)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 346 126
#          1  30  62
                                        
#                Accuracy : 0.7234        
#                  95% CI : (0.6845, 0.76)
#     No Information Rate : 0.6667        
#     P-Value [Acc > NIR] : 0.002168      
                                        
#                   Kappa : 0.2866        
#  Mcnemar's Test P-Value : 2.825e-14     
                                        
#             Sensitivity : 0.9202**        
#             Specificity : 0.3298        
#          Pos Pred Value : 0.7331        
#          Neg Pred Value : 0.6739        
#              Prevalence : 0.6667        
#          Detection Rate : 0.6135        
#    Detection Prevalence : 0.8369        
#       Balanced Accuracy : 0.6250  
                                 
#Both
#Less than 25
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, less25_subset, type="response")
lm_both.pred = rep(0,565)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  98  71
#          1 145 251
                                          
#                Accuracy : 0.6177          
#                  95% CI : (0.5762, 0.6579)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.01187         
                                          
#                   Kappa : 0.1899          
#  Mcnemar's Test P-Value : 6.799e-07       
                                          
#             Sensitivity : 0.4033          
#             Specificity : 0.7795*          
#          Pos Pred Value : 0.5799          
#          Neg Pred Value : 0.6338          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1735          
#    Detection Prevalence : 0.2991          
#       Balanced Accuracy : 0.5914            
                                  
#25 - 45 
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, from25to45_subset, type="response")
lm_both.pred = rep(0,1510)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 582 287
#          1 208 433
                                          
#                Accuracy : 0.6722          
#                  95% CI : (0.6479, 0.6958)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3397          
#  Mcnemar's Test P-Value : 0.0004552       
                                          
#             Sensitivity : 0.7367          
#             Specificity : 0.6014          
#          Pos Pred Value : 0.6697          
#          Neg Pred Value : 0.6755          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3854          
#    Detection Prevalence : 0.5755          
#       Balanced Accuracy : 0.6690

#Greater than 45
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, greater45_subset, type="response")
lm_both.pred = rep(0,564)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 348 130
#          1  28  58
                                          
#                Accuracy : 0.7199          
#                  95% CI : (0.6808, 0.7566)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.003813        
                                          
#                   Kappa : 0.2708          
#  Mcnemar's Test P-Value : 9.348e-16       
                                          
#             Sensitivity : 0.9255**          
#             Specificity : 0.3085          
#          Pos Pred Value : 0.7280          
#          Neg Pred Value : 0.6744          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6170          
#    Detection Prevalence : 0.8475          
#       Balanced Accuracy : 0.6170          
                                                                  
#Logistic
library(ISLR)
#Neither**
#Less than 25
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, less25_subset, type="response")
glm.pred = rep(0,565)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 104  68
#          1 139 254
                                          
#                Accuracy : 0.6336          
#                  95% CI : (0.5924, 0.6735)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.001196        
                                          
#                   Kappa : 0.2249          
#  Mcnemar's Test P-Value : 1.143e-06       
                                          
#             Sensitivity : 0.4280          
#             Specificity : 0.7888*          
#          Pos Pred Value : 0.6047          
#          Neg Pred Value : 0.6463          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1841          
#    Detection Prevalence : 0.3044          
#       Balanced Accuracy : 0.6084 

#25 - 45
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, from25to45_subset, type="response")
glm.pred = rep(0,1510)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 568 263
#          1 222 457
                                          
#                Accuracy : 0.6788          
#                  95% CI : (0.6546, 0.7023)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.3546          
#  Mcnemar's Test P-Value : 0.06932         
                                          
#             Sensitivity : 0.7190          
#             Specificity : 0.6347          
#          Pos Pred Value : 0.6835          
#          Neg Pred Value : 0.6730          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3762          
#    Detection Prevalence : 0.5503          
#       Balanced Accuracy : 0.6769  
            
#Greater than 45
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, greater45_subset, type="response")
glm.pred = rep(0,564)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 345 127
#          1  31  61
                                          
#                Accuracy : 0.7199          
#                  95% CI : (0.6808, 0.7566)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.003813        
                                          
#                   Kappa : 0.2774          
#  Mcnemar's Test P-Value : 4.1e-14         
                                          
#             Sensitivity : 0.9176**          
#             Specificity : 0.3245          
#          Pos Pred Value : 0.7309          
#          Neg Pred Value : 0.6630          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6117          
#    Detection Prevalence : 0.8369          
#       Balanced Accuracy : 0.6210

#Race
#Less than 25
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, less25_subset, type="response")
glm_race.pred = rep(0,565)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  74  49
#          1 169 273
                                          
#                Accuracy : 0.6142          
#                  95% CI : (0.5726, 0.6545)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.01833         
                                          
#                   Kappa : 0.1622          
#  Mcnemar's Test P-Value : 7.648e-16       
                                          
#             Sensitivity : 0.3045          
#             Specificity : 0.8478*          
#          Pos Pred Value : 0.6016          
#          Neg Pred Value : 0.6176          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1310          
#    Detection Prevalence : 0.2177          
#       Balanced Accuracy : 0.5762          
                                   
#25 - 45
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, from25to45_subset, type="response")
glm_race.pred = rep(0,1510)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 553 249
#          1 237 471
                                          
#                Accuracy : 0.6781          
#                  95% CI : (0.6539, 0.7017)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3544          
#  Mcnemar's Test P-Value : 0.6178          
                                          
#             Sensitivity : 0.7000          
#             Specificity : 0.6542          
#          Pos Pred Value : 0.6895          
#          Neg Pred Value : 0.6653          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3662          
#    Detection Prevalence : 0.5311          
#       Balanced Accuracy : 0.6771          
                                    
#Greater than 45
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, greater45_subset, type="response")
glm_race.pred = rep(0,564)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 344 125
#          1  32  63
                                          
#                Accuracy : 0.7216          
#                  95% CI : (0.6827, 0.7583)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.002887        
                                          
#                   Kappa : 0.2853          
#  Mcnemar's Test P-Value : 2.098e-13       
                                          
#             Sensitivity : 0.9149**          
#             Specificity : 0.3351          
#          Pos Pred Value : 0.7335          
#          Neg Pred Value : 0.6632          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6099          
#    Detection Prevalence : 0.8316          
#       Balanced Accuracy : 0.6250                                     

#Gender**
#Less than 25
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, less25_subset, type="response")
glm_sex.pred = rep(0,565)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 104  70
#          1 139 252
                                        
#                Accuracy : 0.6301        
#                  95% CI : (0.5888, 0.67)
#     No Information Rate : 0.5699        
#     P-Value [Acc > NIR] : 0.002091      
                                        
#                   Kappa : 0.2182        
#  Mcnemar's Test P-Value : 2.555e-06     
                                        
#             Sensitivity : 0.4280        
#             Specificity : 0.7826*        
#          Pos Pred Value : 0.5977        
#          Neg Pred Value : 0.6445        
#              Prevalence : 0.4301        
#          Detection Rate : 0.1841        
#    Detection Prevalence : 0.3080        
#       Balanced Accuracy : 0.6053 

#25 - 45
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, from25to45_subset, type="response")
glm_sex.pred = rep(0,1510)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 580 277
#          1 210 443
                                         
#                Accuracy : 0.6775         
#                  95% CI : (0.6532, 0.701)
#     No Information Rate : 0.5232         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.3509         
#  Mcnemar's Test P-Value : 0.002783       
                                         
#             Sensitivity : 0.7342         
#             Specificity : 0.6153         
#          Pos Pred Value : 0.6768         
#          Neg Pred Value : 0.6784         
#              Prevalence : 0.5232         
#          Detection Rate : 0.3841         
#    Detection Prevalence : 0.5675         
#       Balanced Accuracy : 0.6747   

#Greater than 45
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, greater45_subset, type="response")
glm_sex.pred = rep(0,564)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 346 128
#          1  30  60
                                          
#                Accuracy : 0.7199          
#                  95% CI : (0.6808, 0.7566)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.003813        
                                          
#                   Kappa : 0.2752          
#  Mcnemar's Test P-Value : 1.192e-14       
                                          
#             Sensitivity : 0.9202**          
#             Specificity : 0.3191          
#          Pos Pred Value : 0.7300          
#          Neg Pred Value : 0.6667          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6135          
#    Detection Prevalence : 0.8404          
#       Balanced Accuracy : 0.6197 

#Both
#Less than 25
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, less25_subset, type="response")
glm_both.pred = rep(0,565)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 102  72
#          1 141 250
                                          
#                Accuracy : 0.623           
#                  95% CI : (0.5816, 0.6631)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.005878        
                                          
#                   Kappa : 0.2032          
#  Mcnemar's Test P-Value : 3.173e-06       
                                          
#             Sensitivity : 0.4198          
#             Specificity : 0.7764*          
#          Pos Pred Value : 0.5862          
#          Neg Pred Value : 0.6394          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1805          
#    Detection Prevalence : 0.3080          
#       Balanced Accuracy : 0.5981          
                                   
#25 - 45
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, from25to45_subset, type="response")
glm_both.pred = rep(0,1510)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 576 276
#          1 214 444
                                          
#                Accuracy : 0.6755          
#                  95% CI : (0.6512, 0.6991)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3471          
#  Mcnemar's Test P-Value : 0.005857        
                                          
#             Sensitivity : 0.7291          
#             Specificity : 0.6167          
#          Pos Pred Value : 0.6761          
#          Neg Pred Value : 0.6748          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3815          
#    Detection Prevalence : 0.5642          
#       Balanced Accuracy : 0.6729 

#Greater than 45
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, greater45_subset, type="response")
glm_both.pred = rep(0,564)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 346 129
#          1  30  59
                                         
#                Accuracy : 0.7181         
#                  95% CI : (0.679, 0.7549)
#     No Information Rate : 0.6667         
#     P-Value [Acc > NIR] : 0.004995       
                                         
#                   Kappa : 0.2695         
#  Mcnemar's Test P-Value : 7.731e-15      
                                         
#             Sensitivity : 0.9202**         
#             Specificity : 0.3138         
#          Pos Pred Value : 0.7284         
#          Neg Pred Value : 0.6629         
#              Prevalence : 0.6667         
#          Detection Rate : 0.6135         
#    Detection Prevalence : 0.8422         
#       Balanced Accuracy : 0.6170 

#LDA
library(MASS)
#Neither
#Less than 25
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, less25_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,565)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 110  79
#          1 133 243
                                          
#                Accuracy : 0.6248          
#                  95% CI : (0.5834, 0.6648)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.0045869       
                                          
#                   Kappa : 0.2131          
#  Mcnemar's Test P-Value : 0.0002726       
                                          
#             Sensitivity : 0.4527          
#             Specificity : 0.7547*          
#          Pos Pred Value : 0.5820          
#          Neg Pred Value : 0.6463          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1947          
#    Detection Prevalence : 0.3345          
#       Balanced Accuracy : 0.6037

#25 - 45
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, from25to45_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,1510)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 594 299
#          1 196 421
                                          
#                Accuracy : 0.6722          
#                  95% CI : (0.6479, 0.6958)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3388          
#  Mcnemar's Test P-Value : 4.549e-06       
                                          
#             Sensitivity : 0.7519*          
#             Specificity : 0.5847          
#          Pos Pred Value : 0.6652          
#          Neg Pred Value : 0.6823          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3934          
#    Detection Prevalence : 0.5914          
#       Balanced Accuracy : 0.6683   

#Greater than 45
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, greater45_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,564)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 351 131
#          1  25  57
                                        
#                Accuracy : 0.7234        
#                  95% CI : (0.6845, 0.76)
#     No Information Rate : 0.6667        
#     P-Value [Acc > NIR] : 0.002168      
                                        
#                   Kappa : 0.2755        
#  Mcnemar's Test P-Value : < 2.2e-16     
                                        
#             Sensitivity : 0.9335**        
#             Specificity : 0.3032        
#          Pos Pred Value : 0.7282        
#          Neg Pred Value : 0.6951        
#              Prevalence : 0.6667        
#          Detection Rate : 0.6223        
#    Detection Prevalence : 0.8546        
#       Balanced Accuracy : 0.6184 

#Race
#Less than 25
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, less25_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,565)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 110  78
#          1 133 244
                                          
#                Accuracy : 0.6265          
#                  95% CI : (0.5852, 0.6666)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.0035550       
                                          
#                   Kappa : 0.2164          
#  Mcnemar's Test P-Value : 0.0002012       
                                          
#             Sensitivity : 0.4527          
#             Specificity : 0.7578*          
#          Pos Pred Value : 0.5851          
#          Neg Pred Value : 0.6472          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1947          
#    Detection Prevalence : 0.3327          
#       Balanced Accuracy : 0.6052

#25 - 45 
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, from25to45_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,1510)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 594 301
#          1 196 419
                                          
#                Accuracy : 0.6709          
#                  95% CI : (0.6465, 0.6945)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.336           
#  Mcnemar's Test P-Value : 3.086e-06       
                                          
#             Sensitivity : 0.7519*          
#             Specificity : 0.5819          
#          Pos Pred Value : 0.6637          
#          Neg Pred Value : 0.6813          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3934          
#    Detection Prevalence : 0.5927          
#       Balanced Accuracy : 0.6669

#Greater than 45
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, greater45_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,564)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 350 131
#          1  26  57
                                          
#                Accuracy : 0.7216          
#                  95% CI : (0.6827, 0.7583)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.002887        
                                          
#                   Kappa : 0.272           
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.9309**          
#             Specificity : 0.3032          
#          Pos Pred Value : 0.7277          
#          Neg Pred Value : 0.6867          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6206          
#    Detection Prevalence : 0.8528          
#       Balanced Accuracy : 0.6170  

#Gender**
#Less than 25
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, less25_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,565)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  92  70
#          1 151 252
                                          
#                Accuracy : 0.6088          
#                  95% CI : (0.5672, 0.6493)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.03345         
                                          
#                   Kappa : 0.1681          
#  Mcnemar's Test P-Value : 7.392e-08       
                                          
#             Sensitivity : 0.3786          
#             Specificity : 0.7826*          
#          Pos Pred Value : 0.5679          
#          Neg Pred Value : 0.6253          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1628          
#    Detection Prevalence : 0.2867          
#       Balanced Accuracy : 0.5806

#25 - 45
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, from25to45_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,1510)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 581 288
#          1 209 432
                                          
#                Accuracy : 0.6709          
#                  95% CI : (0.6465, 0.6945)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3371          
#  Mcnemar's Test P-Value : 0.0004674       
                                          
#             Sensitivity : 0.7354          
#             Specificity : 0.6000          
#          Pos Pred Value : 0.6686          
#          Neg Pred Value : 0.6739          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3848          
#    Detection Prevalence : 0.5755          
#       Balanced Accuracy : 0.6677 

#Greater than 45
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, greater45_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,564)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 348 130
#          1  28  58
                                          
#                Accuracy : 0.7199          
#                  95% CI : (0.6808, 0.7566)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.003813        
                                          
#                   Kappa : 0.2708          
#  Mcnemar's Test P-Value : 9.348e-16       
                                          
#             Sensitivity : 0.9255**          
#             Specificity : 0.3085          
#          Pos Pred Value : 0.7280          
#          Neg Pred Value : 0.6744          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6170          
#    Detection Prevalence : 0.8475          
#       Balanced Accuracy : 0.6170          
                                  
#Both
#Less than 25
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, less25_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,565)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  97  70
#          1 146 252
                                          
#                Accuracy : 0.6177          
#                  95% CI : (0.5762, 0.6579)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.01187         
                                          
#                   Kappa : 0.189           
#  Mcnemar's Test P-Value : 3.341e-07       
                                          
#             Sensitivity : 0.3992          
#             Specificity : 0.7826*          
#          Pos Pred Value : 0.5808          
#          Neg Pred Value : 0.6332          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1717          
#    Detection Prevalence : 0.2956          
#       Balanced Accuracy : 0.5909         

#25 - 45
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, from25to45_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,1510)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 582 287
#          1 208 433
                                          
#                Accuracy : 0.6722          
#                  95% CI : (0.6479, 0.6958)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3397          
#  Mcnemar's Test P-Value : 0.0004552       
                                          
#             Sensitivity : 0.7367          
#             Specificity : 0.6014          
#          Pos Pred Value : 0.6697          
#          Neg Pred Value : 0.6755          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3854          
#    Detection Prevalence : 0.5755          
#       Balanced Accuracy : 0.6690  

#Greater than 45
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, greater45_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,564)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 348 130
#          1  28  58
                                          
#                Accuracy : 0.7199          
#                  95% CI : (0.6808, 0.7566)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.003813        
                                          
#                   Kappa : 0.2708          
#  Mcnemar's Test P-Value : 9.348e-16       
                                          
#             Sensitivity : 0.9255**          
#             Specificity : 0.3085          
#          Pos Pred Value : 0.7280          
#          Neg Pred Value : 0.6744          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6170          
#    Detection Prevalence : 0.8475          
#       Balanced Accuracy : 0.6170                  

#QDA
#Neither
#Less than 25
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, less25_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,565)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 218 251
#          1  25  71
                                          
#                Accuracy : 0.5115          
#                  95% CI : (0.4694, 0.5535)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.9977          
                                          
#                   Kappa : 0.1056          
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.8971**          
#             Specificity : 0.2205*          
#          Pos Pred Value : 0.4648          
#          Neg Pred Value : 0.7396          
#              Prevalence : 0.4301          
#          Detection Rate : 0.3858          
#    Detection Prevalence : 0.8301          
#       Balanced Accuracy : 0.5588        
                                
#25 - 45
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, from25to45_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,1510)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 695 450
#          1  95 270
                                          
#                Accuracy : 0.6391          
#                  95% CI : (0.6143, 0.6633)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2604          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.8797**          
#             Specificity : 0.3750          
#          Pos Pred Value : 0.6070          
#          Neg Pred Value : 0.7397          
#              Prevalence : 0.5232          
#          Detection Rate : 0.4603          
#    Detection Prevalence : 0.7583          
#       Balanced Accuracy : 0.6274

#Greater than 45
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, greater45_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,564)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 348 133
#          1  28  55
                                          
#                Accuracy : 0.7145          
#                  95% CI : (0.6753, 0.7515)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.008368        
                                          
#                   Kappa : 0.2535          
#  Mcnemar's Test P-Value : 2.478e-16       
                                          
#             Sensitivity : 0.9255**          
#             Specificity : 0.2926*          
#          Pos Pred Value : 0.7235          
#          Neg Pred Value : 0.6627          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6170          
#    Detection Prevalence : 0.8528          
#       Balanced Accuracy : 0.6090          

#Race
#Less than 25
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, less25_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,565)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 218 251
#          1  25  71
                                          
#                Accuracy : 0.5115          
#                  95% CI : (0.4694, 0.5535)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.9977          
                                          
#                   Kappa : 0.1056          
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.8971**          
#             Specificity : 0.2205*          
#          Pos Pred Value : 0.4648          
#          Neg Pred Value : 0.7396          
#              Prevalence : 0.4301          
#          Detection Rate : 0.3858          
#    Detection Prevalence : 0.8301          
#       Balanced Accuracy : 0.5588  

#25 - 45
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, from25to45_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,1510)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 695 451
#          1  95 269
                                          
#                Accuracy : 0.6384          
#                  95% CI : (0.6136, 0.6627)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.259           
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.8797**          
#             Specificity : 0.3736          
#          Pos Pred Value : 0.6065          
#          Neg Pred Value : 0.7390          
#              Prevalence : 0.5232          
#          Detection Rate : 0.4603          
#    Detection Prevalence : 0.7589          
#       Balanced Accuracy : 0.6267             

#Greater than 45
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, greater45_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,564)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 347 131
#          1  29  57
                                          
#                Accuracy : 0.7163          
#                  95% CI : (0.6772, 0.7532)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.006491        
                                          
#                   Kappa : 0.2615          
#  Mcnemar's Test P-Value : 1.408e-15       
                                          
#             Sensitivity : 0.9229**          
#             Specificity : 0.3032          
#          Pos Pred Value : 0.7259          
#          Neg Pred Value : 0.6628          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6152          
#    Detection Prevalence : 0.8475          
#       Balanced Accuracy : 0.6130

#Gender
#Less than 25
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, less25_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,565)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 218 245
#          1  25  77
                                       
#                Accuracy : 0.5221       
#                  95% CI : (0.48, 0.564)
#     No Information Rate : 0.5699       
#     P-Value [Acc > NIR] : 0.9901       
                                       
#                   Kappa : 0.1226       
#  Mcnemar's Test P-Value : <2e-16       
                                       
#             Sensitivity : 0.8971**       
#             Specificity : 0.2391*       
#          Pos Pred Value : 0.4708       
#          Neg Pred Value : 0.7549       
#              Prevalence : 0.4301       
#          Detection Rate : 0.3858       
#    Detection Prevalence : 0.8195       
#       Balanced Accuracy : 0.5681 

#25 - 45
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, from25to45_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,1510)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 694 447
#          1  96 273
                                          
#                Accuracy : 0.6404          
#                  95% CI : (0.6156, 0.6646)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2633          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.8785**          
#             Specificity : 0.3792          
#          Pos Pred Value : 0.6082          
#          Neg Pred Value : 0.7398          
#              Prevalence : 0.5232          
#          Detection Rate : 0.4596          
#    Detection Prevalence : 0.7556          
#       Balanced Accuracy : 0.6288 

#Greater than 45
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, greater45_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,564)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 348 130
#          1  28  58
                                          
#                Accuracy : 0.7199          
#                  95% CI : (0.6808, 0.7566)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.003813        
                                          
#                   Kappa : 0.2708          
#  Mcnemar's Test P-Value : 9.348e-16       
                                          
#             Sensitivity : 0.9255**          
#             Specificity : 0.3085          
#          Pos Pred Value : 0.7280          
#          Neg Pred Value : 0.6744          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6170          
#    Detection Prevalence : 0.8475          
#       Balanced Accuracy : 0.6170         
                                 
#Both
#Less than 25
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, less25_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,565)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 218 245
#          1  25  77
                                       
#                Accuracy : 0.5221       
#                  95% CI : (0.48, 0.564)
#     No Information Rate : 0.5699       
#     P-Value [Acc > NIR] : 0.9901       
                                       
#                   Kappa : 0.1226       
#  Mcnemar's Test P-Value : <2e-16       
                                       
#             Sensitivity : 0.8971**       
#             Specificity : 0.2391*       
#          Pos Pred Value : 0.4708       
#          Neg Pred Value : 0.7549       
#              Prevalence : 0.4301       
#          Detection Rate : 0.3858       
#    Detection Prevalence : 0.8195       
#       Balanced Accuracy : 0.5681         

#25 - 45
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, from25to45_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,1510)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 694 442
#          1  96 278
                                         
#                Accuracy : 0.6437         
#                  95% CI : (0.619, 0.6679)
#     No Information Rate : 0.5232         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2703         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
#             Sensitivity : 0.8785**         
#             Specificity : 0.3861         
#          Pos Pred Value : 0.6109         
#          Neg Pred Value : 0.7433         
#              Prevalence : 0.5232         
#          Detection Rate : 0.4596         
#    Detection Prevalence : 0.7523         
#       Balanced Accuracy : 0.6323 

#Greater than 45
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, greater45_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,564)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 348 130
#          1  28  58
                                          
#                Accuracy : 0.7199          
#                  95% CI : (0.6808, 0.7566)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.003813        
                                          
#                   Kappa : 0.2708          
#  Mcnemar's Test P-Value : 9.348e-16       
                                          
#             Sensitivity : 0.9255**          
#             Specificity : 0.3085          
#          Pos Pred Value : 0.7280          
#          Neg Pred Value : 0.6744          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6170          
#    Detection Prevalence : 0.8475          
#       Balanced Accuracy : 0.6170          
                                 
#KNN
library(class)
attach(df_race)
#Neither
#Less than 25
train.y = two_year_recid[n.train]
test.y = less25_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(less25_subset$age, less25_subset$juv_fel_misd, less25_subset$priors_count, less25_subset$crime_factor, less25_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(less25_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 201 267
#          1  42  55
                                          
#                Accuracy : 0.4531          
#                  95% CI : (0.4115, 0.4952)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 1               
                                          
#                   Kappa : -0.0018         
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.8272*          
#             Specificity : 0.1708**          
#          Pos Pred Value : 0.4295          
#          Neg Pred Value : 0.5670          
#              Prevalence : 0.4301          
#          Detection Rate : 0.3558          
#    Detection Prevalence : 0.8283          
#       Balanced Accuracy : 0.4990  

#25 - 45
train.y = two_year_recid[n.train]
test.y = from25to45_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(from25to45_subset$age, from25to45_subset$juv_fel_misd, from25to45_subset$priors_count, from25to45_subset$crime_factor, from25to45_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(from25to45_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 654 598
#          1 136 122
                                          
#                Accuracy : 0.5139          
#                  95% CI : (0.4884, 0.5394)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : 0.7725          
                                          
#                   Kappa : -0.0028         
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.8278*          
#             Specificity : 0.1694**          
#          Pos Pred Value : 0.5224          
#          Neg Pred Value : 0.4729          
#              Prevalence : 0.5232          
#          Detection Rate : 0.4331          
#    Detection Prevalence : 0.8291          
#       Balanced Accuracy : 0.4986    

#Greater than 45
train.y = two_year_recid[n.train]
test.y = greater45_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(greater45_subset$age, greater45_subset$juv_fel_misd, greater45_subset$priors_count, greater45_subset$crime_factor, greater45_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(greater45_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 316 151
#          1  60  37
                                         
#                Accuracy : 0.6259         
#                  95% CI : (0.5845, 0.666)
#     No Information Rate : 0.6667         
#     P-Value [Acc > NIR] : 0.9814         
                                         
#                   Kappa : 0.0424         
#  Mcnemar's Test P-Value : 5.797e-10      
                                         
#             Sensitivity : 0.8404*         
#             Specificity : 0.1968**         
#          Pos Pred Value : 0.6767         
#          Neg Pred Value : 0.3814         
#              Prevalence : 0.6667         
#          Detection Rate : 0.5603         
#    Detection Prevalence : 0.8280         
#       Balanced Accuracy : 0.5186  

#Race
#Less than 25
train.y = two_year_recid[n.train]
test.y = less25_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(less25_subset$age, less25_subset$juv_fel_misd, less25_subset$priors_count, less25_subset$crime_factor, less25_subset$violent_charge, less25_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(less25_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 201 267
#          1  42  55
                                          
#                Accuracy : 0.4531          
#                  95% CI : (0.4115, 0.4952)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 1               
                                          
#                   Kappa : -0.0018         
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.8272*          
#             Specificity : 0.1708**          
#          Pos Pred Value : 0.4295          
#          Neg Pred Value : 0.5670          
#              Prevalence : 0.4301          
#          Detection Rate : 0.3558          
#    Detection Prevalence : 0.8283          
#       Balanced Accuracy : 0.4990 

#25 - 45
train.y = two_year_recid[n.train]
test.y = from25to45_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(from25to45_subset$age, from25to45_subset$juv_fel_misd, from25to45_subset$priors_count, from25to45_subset$crime_factor, from25to45_subset$violent_charge, from25to45_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(from25to45_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 654 598
#          1 136 122
                                          
#                Accuracy : 0.5139          
#                  95% CI : (0.4884, 0.5394)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : 0.7725          
                                          
#                   Kappa : -0.0028         
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.8278*          
#             Specificity : 0.1694**          
#          Pos Pred Value : 0.5224          
#          Neg Pred Value : 0.4729          
#              Prevalence : 0.5232          
#          Detection Rate : 0.4331          
#    Detection Prevalence : 0.8291          
#       Balanced Accuracy : 0.4986 

#Greater than 45
train.y = two_year_recid[n.train]
test.y = greater45_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(greater45_subset$age, greater45_subset$juv_fel_misd, greater45_subset$priors_count, greater45_subset$crime_factor, greater45_subset$violent_charge, greater45_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(greater45_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 316 151
#          1  60  37
                                         
#                Accuracy : 0.6259         
#                  95% CI : (0.5845, 0.666)
#     No Information Rate : 0.6667         
#     P-Value [Acc > NIR] : 0.9814         
                                         
#                   Kappa : 0.0424         
#  Mcnemar's Test P-Value : 5.797e-10      
                                         
#             Sensitivity : 0.8404*         
#             Specificity : 0.1968**         
#          Pos Pred Value : 0.6767         
#          Neg Pred Value : 0.3814         
#              Prevalence : 0.6667         
#          Detection Rate : 0.5603         
#    Detection Prevalence : 0.8280         
#       Balanced Accuracy : 0.5186         

#Gender
#Less than 25
train.y = two_year_recid[n.train]
test.y = less25_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(less25_subset$age, less25_subset$juv_fel_misd, less25_subset$priors_count, less25_subset$crime_factor, less25_subset$violent_charge, less25_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(less25_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 201 267
#          1  42  55
                                          
#                Accuracy : 0.4531          
#                  95% CI : (0.4115, 0.4952)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 1               
                                          
#                   Kappa : -0.0018         
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.8272*          
#             Specificity : 0.1708**          
#          Pos Pred Value : 0.4295          
#          Neg Pred Value : 0.5670          
#              Prevalence : 0.4301          
#          Detection Rate : 0.3558          
#    Detection Prevalence : 0.8283          
#       Balanced Accuracy : 0.4990   

#25 - 45
train.y = two_year_recid[n.train]
test.y = from25to45_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(from25to45_subset$age, from25to45_subset$juv_fel_misd, from25to45_subset$priors_count, from25to45_subset$crime_factor, from25to45_subset$violent_charge, from25to45_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(from25to45_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 654 598
#          1 136 122
                                          
#                Accuracy : 0.5139          
#                  95% CI : (0.4884, 0.5394)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : 0.7725          
                                          
#                   Kappa : -0.0028         
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.8278*          
#             Specificity : 0.1694**          
#          Pos Pred Value : 0.5224          
#          Neg Pred Value : 0.4729          
#              Prevalence : 0.5232          
#          Detection Rate : 0.4331          
#    Detection Prevalence : 0.8291          
#       Balanced Accuracy : 0.4986          
                                  

#Greater than 45
train.y = two_year_recid[n.train]
test.y = greater45_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(greater45_subset$age, greater45_subset$juv_fel_misd, greater45_subset$priors_count, greater45_subset$crime_factor, greater45_subset$violent_charge, greater45_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(greater45_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 316 151
#          1  60  37
                                         
#                Accuracy : 0.6259         
#                  95% CI : (0.5845, 0.666)
#     No Information Rate : 0.6667         
#     P-Value [Acc > NIR] : 0.9814         
                                         
#                   Kappa : 0.0424         
#  Mcnemar's Test P-Value : 5.797e-10      
                                         
#             Sensitivity : 0.8404*         
#             Specificity : 0.1968**         
#          Pos Pred Value : 0.6767         
#          Neg Pred Value : 0.3814         
#              Prevalence : 0.6667         
#          Detection Rate : 0.5603         
#    Detection Prevalence : 0.8280         
#       Balanced Accuracy : 0.5186       

#Both**
#Less than 25
train.y = two_year_recid[n.train]
test.y = less25_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(less25_subset$age, less25_subset$juv_fel_misd, less25_subset$priors_count, less25_subset$crime_factor, less25_subset$violent_charge, less25_subset$race_factor, less25_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(less25_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 201 267
#          1  42  55
                                          
#                Accuracy : 0.4531          
#                  95% CI : (0.4115, 0.4952)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 1               
                                          
#                   Kappa : -0.0018         
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.8272*          
#             Specificity : 0.1708**          
#          Pos Pred Value : 0.4295          
#          Neg Pred Value : 0.5670          
#              Prevalence : 0.4301          
#          Detection Rate : 0.3558          
#    Detection Prevalence : 0.8283          
#       Balanced Accuracy : 0.4990       

#25 - 45
train.y = two_year_recid[n.train]
test.y = from25to45_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(from25to45_subset$age, from25to45_subset$juv_fel_misd, from25to45_subset$priors_count, from25to45_subset$crime_factor, from25to45_subset$violent_charge, from25to45_subset$race_factor, from25to45_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(from25to45_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 654 598
#          1 136 122
                                          
#                Accuracy : 0.5139          
#                  95% CI : (0.4884, 0.5394)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : 0.7725          
                                          
#                   Kappa : -0.0028         
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.8278*          
#             Specificity : 0.1694**          
#          Pos Pred Value : 0.5224          
#          Neg Pred Value : 0.4729          
#              Prevalence : 0.5232          
#          Detection Rate : 0.4331          
#    Detection Prevalence : 0.8291          
#       Balanced Accuracy : 0.4986 

#Greater than 45
train.y = two_year_recid[n.train]
test.y = greater45_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(greater45_subset$age, greater45_subset$juv_fel_misd, greater45_subset$priors_count, greater45_subset$crime_factor, greater45_subset$violent_charge, greater45_subset$race_factor, greater45_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(greater45_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 316 151
#          1  60  37
                                         
#                Accuracy : 0.6259         
#                  95% CI : (0.5845, 0.666)
#     No Information Rate : 0.6667         
#     P-Value [Acc > NIR] : 0.9814         
                                         
#                   Kappa : 0.0424         
#  Mcnemar's Test P-Value : 5.797e-10      
                                         
#             Sensitivity : 0.8404*         
#             Specificity : 0.1968**         
#          Pos Pred Value : 0.6767         
#          Neg Pred Value : 0.3814         
#              Prevalence : 0.6667         
#          Detection Rate : 0.5603         
#    Detection Prevalence : 0.8280         
#       Balanced Accuracy : 0.5186 

#Tree
library(tree)
set.seed(1)
n <- nrow(df_race)
n.train = sample(1:n, n*.5)
n.test = (-n.train)
train = df_race[n.train,]
test = df_race[n.test,]
less25_subset = test[which(test$age_cat=="Less than 25") ,]
from25to45_subset = test[which(test$age_cat=="25 - 45") ,]
greater45_subset = test[which(test$age_cat=="Greater than 45") ,]

#Neither
#Less than 25
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
tree.probs = predict(tree, newdata=less25_subset)

tree.pred = rep(0,565)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0   0   0
          1 243 322
                                          
#                Accuracy : 0.5699          
#                  95% CI : (0.5279, 0.6112)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.5177          
                                          
#                   Kappa : 0               
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.0000**          
#             Specificity : 1.0000**          
#          Pos Pred Value :    NaN          
#          Neg Pred Value : 0.5699          
#              Prevalence : 0.4301          
#          Detection Rate : 0.0000          
#    Detection Prevalence : 0.0000          
#       Balanced Accuracy : 0.5000

#25 - 45
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
tree.probs = predict(tree, newdata=from25to45_subset)

tree.pred = rep(0,1510)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 496 246
#          1 294 474
                                          
#                Accuracy : 0.6424          
#                  95% CI : (0.6176, 0.6666)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.2853          
#  Mcnemar's Test P-Value : 0.04312         
                                          
#             Sensitivity : 0.6278          
#             Specificity : 0.6583          
#          Pos Pred Value : 0.6685          
#          Neg Pred Value : 0.6172          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3285          
#    Detection Prevalence : 0.4914          
#       Balanced Accuracy : 0.6431 

#Greater than 45
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
tree.probs = predict(tree, newdata=greater45_subset)

tree.pred = rep(0,564)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 264  68
#          1 112 120
                                          
#                Accuracy : 0.6809          
#                  95% CI : (0.6406, 0.7192)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.25234         
                                          
#                   Kappa : 0.3216          
#  Mcnemar's Test P-Value : 0.00135         
                                          
#             Sensitivity : 0.7021          
#             Specificity : 0.6383          
#          Pos Pred Value : 0.7952          
#          Neg Pred Value : 0.5172          
#              Prevalence : 0.6667          
#          Detection Rate : 0.4681          
#    Detection Prevalence : 0.5887          
#       Balanced Accuracy : 0.6702

#Pruning
#Less than 25
set.seed(1)
cv.tree = cv.tree(tree)
tree.min = cv.tree$size[which.min(cv.tree$dev)]
prune = prune.tree(tree, best=5)
summary(prune)
prune.probs = predict(prune, newdata=less25_subset)

prune.pred = rep(0,nrow(less25_subset))
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0   0   0
          1 243 322
                                          
#                Accuracy : 0.5699          
#                  95% CI : (0.5279, 0.6112)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.5177          
                                          
#                   Kappa : 0               
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.0000**          
#             Specificity : 1.0000**          
#          Pos Pred Value :    NaN          
#          Neg Pred Value : 0.5699          
#              Prevalence : 0.4301          
#          Detection Rate : 0.0000          
#    Detection Prevalence : 0.0000          
#       Balanced Accuracy : 0.5000         

#25 - 45                                   
set.seed(1)
cv.tree = cv.tree(tree)
tree.min = cv.tree$size[which.min(cv.tree$dev)]
prune = prune.tree(tree, best=5)
summary(prune)
prune.probs = predict(prune, newdata=from25to45_subset)

prune.pred = rep(0, 1510)
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 496 246
#          1 294 474
                                          
#                Accuracy : 0.6424          
#                  95% CI : (0.6176, 0.6666)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.2853          
#  Mcnemar's Test P-Value : 0.04312         
                                          
#             Sensitivity : 0.6278          
#             Specificity : 0.6583          
#          Pos Pred Value : 0.6685          
#          Neg Pred Value : 0.6172          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3285          
#    Detection Prevalence : 0.4914          
#       Balanced Accuracy : 0.6431

#Greater than 45
set.seed(1)
cv.tree = cv.tree(tree)
tree.min = cv.tree$size[which.min(cv.tree$dev)]
prune = prune.tree(tree, best=5)
summary(prune)
prune.probs = predict(prune, newdata=greater45_subset)

prune.pred = rep(0,nrow(greater45_subset))
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 264  68
#          1 112 120
                                          
#                Accuracy : 0.6809          
#                  95% CI : (0.6406, 0.7192)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.25234         
                                          
#                   Kappa : 0.3216          
#  Mcnemar's Test P-Value : 0.00135         
                                          
#             Sensitivity : 0.7021          
#             Specificity : 0.6383          
#          Pos Pred Value : 0.7952          
#          Neg Pred Value : 0.5172          
#              Prevalence : 0.6667          
#          Detection Rate : 0.4681          
#    Detection Prevalence : 0.5887          
#       Balanced Accuracy : 0.6702                

#Random Forest
#Less than 25
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=less25_subset)

rf.pred = rep(0,565)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 181 229
#          1  62  93
                                        
#                Accuracy : 0.485         
#                  95% CI : (0.443, 0.527)
#     No Information Rate : 0.5699        
#     P-Value [Acc > NIR] : 1             
                                        
#                   Kappa : 0.0311        
#  Mcnemar's Test P-Value : <2e-16        
                                        
#             Sensitivity : 0.7449        
#             Specificity : 0.2888*        
#          Pos Pred Value : 0.4415        
#          Neg Pred Value : 0.6000        
#              Prevalence : 0.4301        
#          Detection Rate : 0.3204        
#    Detection Prevalence : 0.7257        
#       Balanced Accuracy : 0.5168 

#25 - 45
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=from25to45_subset)

rf.pred = rep(0,1510)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 579 519
#          1 211 201
                                        
#                Accuracy : 0.5166        
#                  95% CI : (0.491, 0.542)
#     No Information Rate : 0.5232        
#     P-Value [Acc > NIR] : 0.7058        
                                        
#                   Kappa : 0.0123        
#  Mcnemar's Test P-Value : <2e-16        
                                        
#             Sensitivity : 0.7329        
#             Specificity : 0.2792*        
#          Pos Pred Value : 0.5273        
#          Neg Pred Value : 0.4879        
#              Prevalence : 0.5232        
#          Detection Rate : 0.3834        
#    Detection Prevalence : 0.7272        
#       Balanced Accuracy : 0.5060 

#Greater than 45
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=greater45_subset)

rf.pred = rep(0,564)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 282 127
#          1  94  61
                                          
#                Accuracy : 0.6082          
#                  95% CI : (0.5665, 0.6487)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.99845         
                                          
#                   Kappa : 0.0779          
#  Mcnemar's Test P-Value : 0.03135         
                                          
#             Sensitivity : 0.7500*          
#             Specificity : 0.3245          
#          Pos Pred Value : 0.6895          
#          Neg Pred Value : 0.3935          
#              Prevalence : 0.6667          
#          Detection Rate : 0.5000          
#    Detection Prevalence : 0.7252          
#       Balanced Accuracy : 0.5372

#Race
#Less than 25
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
tree_race.probs = predict(tree_race, newdata=less25_subset)

tree_race.pred = rep(0,565)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0   0   0
          1 243 322
                                          
#                Accuracy : 0.5699          
#                  95% CI : (0.5279, 0.6112)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.5177          
                                          
#                   Kappa : 0               
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.0000**          
#             Specificity : 1.0000**          
#          Pos Pred Value :    NaN          
#          Neg Pred Value : 0.5699          
#              Prevalence : 0.4301          
#          Detection Rate : 0.0000          
#    Detection Prevalence : 0.0000          
#       Balanced Accuracy : 0.5000        

#25 - 45
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
tree_race.probs = predict(tree_race, newdata=from25to45_subset)

tree_race.pred = rep(0,1510)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 496 246
#          1 294 474
                                          
#                Accuracy : 0.6424          
#                  95% CI : (0.6176, 0.6666)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.2853          
#  Mcnemar's Test P-Value : 0.04312         
                                          
#             Sensitivity : 0.6278          
#             Specificity : 0.6583          
#          Pos Pred Value : 0.6685          
#          Neg Pred Value : 0.6172          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3285          
#    Detection Prevalence : 0.4914          
#       Balanced Accuracy : 0.6431 

#Greater than 45
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
tree_race.probs = predict(tree_race, newdata=greater45_subset)

tree_race.pred = rep(0,564)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 264  68
#          1 112 120
                                          
#                Accuracy : 0.6809          
#                  95% CI : (0.6406, 0.7192)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.25234         
                                          
#                   Kappa : 0.3216          
#  Mcnemar's Test P-Value : 0.00135         
                                          
#             Sensitivity : 0.7021          
#             Specificity : 0.6383          
#          Pos Pred Value : 0.7952          
#          Neg Pred Value : 0.5172          
#              Prevalence : 0.6667          
#          Detection Rate : 0.4681          
#    Detection Prevalence : 0.5887          
#       Balanced Accuracy : 0.6702          

#Pruning
#Less than 25
set.seed(1)
cv_race.tree = cv.tree(tree_race)
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
prune_race.probs = predict(prune_race, newdata=less25_subset)

prune_race.pred = rep(0,565)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0   0   0
#          1 243 322
                                          
#                Accuracy : 0.5699          
#                  95% CI : (0.5279, 0.6112)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.5177          
                                          
#                   Kappa : 0               
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.0000**          
#             Specificity : 1.0000**          
#          Pos Pred Value :    NaN          
#          Neg Pred Value : 0.5699          
#              Prevalence : 0.4301          
#          Detection Rate : 0.0000          
#    Detection Prevalence : 0.0000          
#       Balanced Accuracy : 0.5000 

#25 - 45
set.seed(1)
cv_race.tree = cv.tree(tree_race)
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
prune_race.probs = predict(prune_race, newdata=from25to45_subset)

prune_race.pred = rep(0,1510)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 496 246
#          1 294 474
                                          
#                Accuracy : 0.6424          
#                  95% CI : (0.6176, 0.6666)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.2853          
#  Mcnemar's Test P-Value : 0.04312         
                                          
#             Sensitivity : 0.6278          
#             Specificity : 0.6583          
#          Pos Pred Value : 0.6685          
#          Neg Pred Value : 0.6172          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3285          
#    Detection Prevalence : 0.4914          
#       Balanced Accuracy : 0.6431

#Greater than 45
set.seed(1)
cv_race.tree = cv.tree(tree_race)
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
prune_race.probs = predict(prune_race, newdata=greater45_subset)

prune_race.pred = rep(0,564)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 264  68
#          1 112 120
                                          
#                Accuracy : 0.6809          
#                  95% CI : (0.6406, 0.7192)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.25234         
                                          
#                   Kappa : 0.3216          
#  Mcnemar's Test P-Value : 0.00135         
                                          
#             Sensitivity : 0.7021          
#             Specificity : 0.6383          
#          Pos Pred Value : 0.7952          
#          Neg Pred Value : 0.5172          
#              Prevalence : 0.6667          
#          Detection Rate : 0.4681          
#    Detection Prevalence : 0.5887          
#       Balanced Accuracy : 0.6702                                         

#Random Forest
#Less than 25
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=less25_subset)

rf_race.pred = rep(0,565)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 172 232
#          1  71  90
                                         
#                Accuracy : 0.4637         
#                  95% CI : (0.422, 0.5058)
#     No Information Rate : 0.5699         
#     P-Value [Acc > NIR] : 1              
                                         
#                   Kappa : -0.0117        
#  Mcnemar's Test P-Value : <2e-16         
                                         
#             Sensitivity : 0.7078         
#             Specificity : 0.2795**         
#          Pos Pred Value : 0.4257         
#          Neg Pred Value : 0.5590         
#              Prevalence : 0.4301         
#          Detection Rate : 0.3044         
#    Detection Prevalence : 0.7150         
#       Balanced Accuracy : 0.4937         
                                 
#25 - 45
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=from25to45_subset)

rf_race.pred = rep(0,1510)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 575 509
#          1 215 211
                                        
#                Accuracy : 0.5205        
#                  95% CI : (0.495, 0.546)
#     No Information Rate : 0.5232        
#     P-Value [Acc > NIR] : 0.5918        
                                        
#                   Kappa : 0.0213        
#  Mcnemar's Test P-Value : <2e-16        
                                        
#             Sensitivity : 0.7278        
#             Specificity : 0.2931*        
#          Pos Pred Value : 0.5304        
#          Neg Pred Value : 0.4953        
#              Prevalence : 0.5232        
#          Detection Rate : 0.3808        
#    Detection Prevalence : 0.7179        
#       Balanced Accuracy : 0.5105        
                                   
#Greater than 45
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=greater45_subset)

rf_race.pred = rep(0,564)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 273 130
#          1 103  58
                                         
#                Accuracy : 0.5869         
#                  95% CI : (0.545, 0.6279)
#     No Information Rate : 0.6667         
#     P-Value [Acc > NIR] : 0.99997        
                                         
#                   Kappa : 0.0359         
#  Mcnemar's Test P-Value : 0.08851        
                                         
#             Sensitivity : 0.7261         
#             Specificity : 0.3085         
#          Pos Pred Value : 0.6774         
#          Neg Pred Value : 0.3602         
#              Prevalence : 0.6667         
#          Detection Rate : 0.4840         
#    Detection Prevalence : 0.7145         
#       Balanced Accuracy : 0.5173       
                                 
#Gender
#Less than 25
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
tree_sex.probs = predict(tree_sex, newdata=less25_subset)

tree_sex.pred = rep(0,565)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0   0   0
          1 243 322
                                          
#                Accuracy : 0.5699          
#                  95% CI : (0.5279, 0.6112)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.5177          
                                          
#                   Kappa : 0               
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.0000**          
#             Specificity : 1.0000**          
#          Pos Pred Value :    NaN          
#          Neg Pred Value : 0.5699          
#              Prevalence : 0.4301          
#          Detection Rate : 0.0000          
#    Detection Prevalence : 0.0000          
#       Balanced Accuracy : 0.5000   

#25 - 45
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
tree_sex.probs = predict(tree_sex, newdata=from25to45_subset)

tree_sex.pred = rep(0,1510)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 496 246
#          1 294 474
                                          
#                Accuracy : 0.6424          
#                  95% CI : (0.6176, 0.6666)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.2853          
#  Mcnemar's Test P-Value : 0.04312         
                                          
#             Sensitivity : 0.6278          
#             Specificity : 0.6583          
#          Pos Pred Value : 0.6685          
#          Neg Pred Value : 0.6172          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3285          
#    Detection Prevalence : 0.4914          
#       Balanced Accuracy : 0.6431  

#Greater than 45
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
tree_sex.probs = predict(tree_sex, newdata=greater45_subset)

tree_sex.pred = rep(0,564)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 264  68
#          1 112 120
                                          
#                Accuracy : 0.6809          
#                  95% CI : (0.6406, 0.7192)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.25234         
                                          
#                   Kappa : 0.3216          
#  Mcnemar's Test P-Value : 0.00135         
                                          
#             Sensitivity : 0.7021          
#             Specificity : 0.6383          
#          Pos Pred Value : 0.7952          
#          Neg Pred Value : 0.5172          
#              Prevalence : 0.6667          
#          Detection Rate : 0.4681          
#    Detection Prevalence : 0.5887          
#       Balanced Accuracy : 0.6702  

#Pruning
#Less than 25
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
prune_sex.probs = predict(prune_sex, newdata=less25_subset)

prune_sex.pred = rep(0,565)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0   0   0
          1 243 322
                                          
#                Accuracy : 0.5699          
#                  95% CI : (0.5279, 0.6112)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.5177          
                                          
#                   Kappa : 0               
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.0000**          
#             Specificity : 1.0000**          
#          Pos Pred Value :    NaN          
#          Neg Pred Value : 0.5699          
#              Prevalence : 0.4301          
#          Detection Rate : 0.0000          
#    Detection Prevalence : 0.0000          
#       Balanced Accuracy : 0.5000

#25 - 45
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
prune_sex.probs = predict(prune_sex, newdata=from25to45_subset)

prune_sex.pred = rep(0,1510)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 496 246
#          1 294 474
                                          
#                Accuracy : 0.6424          
#                  95% CI : (0.6176, 0.6666)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.2853          
#  Mcnemar's Test P-Value : 0.04312         
                                          
#             Sensitivity : 0.6278          
#             Specificity : 0.6583          
#          Pos Pred Value : 0.6685          
#          Neg Pred Value : 0.6172          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3285          
#    Detection Prevalence : 0.4914          
#       Balanced Accuracy : 0.6431 

#Greater than 45
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
prune_sex.probs = predict(prune_sex, newdata=greater45_subset)

prune_sex.pred = rep(0,564)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 264  68
#          1 112 120
                                          
#                Accuracy : 0.6809          
#                  95% CI : (0.6406, 0.7192)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.25234         
                                          
#                   Kappa : 0.3216          
#  Mcnemar's Test P-Value : 0.00135         
                                          
#             Sensitivity : 0.7021          
#             Specificity : 0.6383          
#          Pos Pred Value : 0.7952          
#          Neg Pred Value : 0.5172          
#              Prevalence : 0.6667          
#          Detection Rate : 0.4681          
#    Detection Prevalence : 0.5887          
#       Balanced Accuracy : 0.6702          
          
#Random Forest
#Less than 25
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=less25_subset)

rf_sex.pred = rep(0,565)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 194 249
#          1  49  73
                                          
#                Accuracy : 0.4726          
#                  95% CI : (0.4308, 0.5147)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 1               
                                          
#                   Kappa : 0.0228          
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.7984*          
#             Specificity : 0.2267*          
#          Pos Pred Value : 0.4379          
#          Neg Pred Value : 0.5984          
#              Prevalence : 0.4301          
#          Detection Rate : 0.3434          
#    Detection Prevalence : 0.7841          
#       Balanced Accuracy : 0.5125   

#25 - 45
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=from25to45_subset)

rf_sex.pred = rep(0,1510)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 623 558
#          1 167 162
                                          
#                Accuracy : 0.5199          
#                  95% CI : (0.4943, 0.5453)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : 0.6117          
                                          
#                   Kappa : 0.0139          
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.7886*          
#             Specificity : 0.2250*          
#          Pos Pred Value : 0.5275          
#          Neg Pred Value : 0.4924          
#              Prevalence : 0.5232          
#          Detection Rate : 0.4126          
#    Detection Prevalence : 0.7821          
#       Balanced Accuracy : 0.5068

#Greater than 45
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=greater45_subset)

rf_sex.pred = rep(0,564)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 301 141
#          1  75  47
                                          
#                Accuracy : 0.617           
#                  95% CI : (0.5755, 0.6573)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.9942          
                                          
#                   Kappa : 0.0554          
#  Mcnemar's Test P-Value : 9.748e-06       
                                          
#             Sensitivity : 0.8005*          
#             Specificity : 0.2500*          
#          Pos Pred Value : 0.6810          
#          Neg Pred Value : 0.3852          
#              Prevalence : 0.6667          
#          Detection Rate : 0.5337          
#    Detection Prevalence : 0.7837          
#       Balanced Accuracy : 0.5253          

#Both
#Less than 25
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
tree_both.probs = predict(tree_both, newdata=less25_subset)

tree_both.pred = rep(0,565)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0   0   0
#          1 243 322
                                          
#                Accuracy : 0.5699          
#                  95% CI : (0.5279, 0.6112)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.5177          
                                          
#                   Kappa : 0               
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.0000**          
#             Specificity : 1.0000**          
#          Pos Pred Value :    NaN          
#          Neg Pred Value : 0.5699          
#              Prevalence : 0.4301          
#          Detection Rate : 0.0000          
#    Detection Prevalence : 0.0000          
#       Balanced Accuracy : 0.5000   

#25 - 45
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
tree_both.probs = predict(tree_both, newdata=from25to45_subset)

tree_both.pred = rep(0,1510)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 496 246
#          1 294 474
                                          
#                Accuracy : 0.6424          
#                  95% CI : (0.6176, 0.6666)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.2853          
#  Mcnemar's Test P-Value : 0.04312         
                                          
#             Sensitivity : 0.6278          
#             Specificity : 0.6583          
#          Pos Pred Value : 0.6685          
#          Neg Pred Value : 0.6172          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3285          
#    Detection Prevalence : 0.4914          
#       Balanced Accuracy : 0.6431 

#Greater than 45
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
tree_both.probs = predict(tree_both, newdata=greater45_subset)

tree_both.pred = rep(0,564)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 264  68
#          1 112 120
                                          
#                Accuracy : 0.6809          
#                  95% CI : (0.6406, 0.7192)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.25234         
                                          
#                   Kappa : 0.3216          
#  Mcnemar's Test P-Value : 0.00135         
                                          
#             Sensitivity : 0.7021          
#             Specificity : 0.6383          
#          Pos Pred Value : 0.7952          
#          Neg Pred Value : 0.5172          
#              Prevalence : 0.6667          
#          Detection Rate : 0.4681          
#    Detection Prevalence : 0.5887          
#       Balanced Accuracy : 0.6702 

#Pruning
#Less than 25
set.seed(1)
cv_both.tree = cv.tree(tree_both)
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
prune_both.probs = predict(prune_both, newdata=less25_subset)

prune_both.pred = rep(0,565)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0   0   0
          1 243 322
                                          
#                Accuracy : 0.5699          
#                  95% CI : (0.5279, 0.6112)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.5177          
                                          
#                   Kappa : 0               
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.0000**          
#             Specificity : 1.0000**          
#          Pos Pred Value :    NaN          
#          Neg Pred Value : 0.5699          
#              Prevalence : 0.4301          
#          Detection Rate : 0.0000          
#    Detection Prevalence : 0.0000          
#       Balanced Accuracy : 0.5000          

#25 - 45
set.seed(1)
cv_both.tree = cv.tree(tree_both)
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
prune_both.probs = predict(prune_both, newdata=from25to45_subset)

prune_both.pred = rep(0,1510)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 496 246
#          1 294 474
                                          
#                Accuracy : 0.6424          
#                  95% CI : (0.6176, 0.6666)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.2853          
#  Mcnemar's Test P-Value : 0.04312         
                                          
#             Sensitivity : 0.6278          
#             Specificity : 0.6583          
#          Pos Pred Value : 0.6685          
#          Neg Pred Value : 0.6172          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3285          
#    Detection Prevalence : 0.4914          
#       Balanced Accuracy : 0.6431

#Greater than 45
set.seed(1)
cv_both.tree = cv.tree(tree_both)
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
prune_both.probs = predict(prune_both, newdata=greater45_subset)

prune_both.pred = rep(0,564)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 264  68
#          1 112 120
                                          
#                Accuracy : 0.6809          
#                  95% CI : (0.6406, 0.7192)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.25234         
                                          
#                   Kappa : 0.3216          
#  Mcnemar's Test P-Value : 0.00135         
                                          
#             Sensitivity : 0.7021          
#             Specificity : 0.6383          
#          Pos Pred Value : 0.7952          
#          Neg Pred Value : 0.5172          
#              Prevalence : 0.6667          
#          Detection Rate : 0.4681          
#    Detection Prevalence : 0.5887          
#       Balanced Accuracy : 0.6702         

#Random Forest
#Less than 25
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=less25_subset)

rf_both.pred = rep(0,565)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 188 248
#          1  55  74
                                         
#                Accuracy : 0.4637         
#                  95% CI : (0.422, 0.5058)
#     No Information Rate : 0.5699         
#     P-Value [Acc > NIR] : 1              
                                         
#                   Kappa : 0.0032         
#  Mcnemar's Test P-Value : <2e-16         
                                         
#             Sensitivity : 0.7737*         
#             Specificity : 0.2298*         
#          Pos Pred Value : 0.4312         
#          Neg Pred Value : 0.5736         
#              Prevalence : 0.4301         
#          Detection Rate : 0.3327         
#    Detection Prevalence : 0.7717         
#       Balanced Accuracy : 0.5017  

#25 - 45
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=from25to45_subset)

rf_both.pred = rep(0,1510)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 612 551
#          1 178 169
                                          
#                Accuracy : 0.5172          
#                  95% CI : (0.4917, 0.5427)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : 0.6878          
                                          
#                   Kappa : 0.0096          
#  Mcnemar's Test P-Value : <2e-16          
                                          
#             Sensitivity : 0.7747*          
#             Specificity : 0.2347*          
#          Pos Pred Value : 0.5262          
#          Neg Pred Value : 0.4870          
#              Prevalence : 0.5232          
#          Detection Rate : 0.4053          
#    Detection Prevalence : 0.7702          
#       Balanced Accuracy : 0.5047         
   
#Greater than 45
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=greater45_subset)

rf_both.pred = rep(0,564)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 297 138
#          1  79  50
                                          
#                Accuracy : 0.6152          
#                  95% CI : (0.5737, 0.6556)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.9955          
                                          
#                   Kappa : 0.0606          
#  Mcnemar's Test P-Value : 8.24e-05        
                                          
#             Sensitivity : 0.7899*          
#             Specificity : 0.2660*          
#          Pos Pred Value : 0.6828          
#          Neg Pred Value : 0.3876          
#              Prevalence : 0.6667          
#          Detection Rate : 0.5266          
#    Detection Prevalence : 0.7713          
#       Balanced Accuracy : 0.5279

#GAM
library(gam)
library(gamsel)

set.seed(1)
n <- nrow(df_race)
n.train = sample(1:n, n*.5)
n.test = (-n.train)
train = df_race[n.train,]
test = df_race[n.test,]
library(pROC)

#Neither
#Less than 25
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, less25_subset, type="response")

gam.pred = rep(0,565)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  86  59
#          1 157 263
                                          
#                Accuracy : 0.6177          
#                  95% CI : (0.5762, 0.6579)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.01187         
                                          
#                   Kappa : 0.1796          
#  Mcnemar's Test P-Value : 4.111e-11       
                                          
#             Sensitivity : 0.3539          
#             Specificity : 0.8168*          
#          Pos Pred Value : 0.5931          
#          Neg Pred Value : 0.6262          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1522          
#    Detection Prevalence : 0.2566          
#       Balanced Accuracy : 0.5853  

#25 - 45
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, from25to45_subset, type="response")

gam.pred = rep(0,1510)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 575 272
#          1 215 448
                                         
#                Accuracy : 0.6775         
#                  95% CI : (0.6532, 0.701)
#     No Information Rate : 0.5232         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3513         
#  Mcnemar's Test P-Value : 0.01116        
                                         
#             Sensitivity : 0.7278         
#             Specificity : 0.6222         
#          Pos Pred Value : 0.6789         
#          Neg Pred Value : 0.6757         
#              Prevalence : 0.5232         
#          Detection Rate : 0.3808         
#    Detection Prevalence : 0.5609         
#       Balanced Accuracy : 0.6750

#Greater than 45
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, greater45_subset, type="response")

gam.pred = rep(0,564)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 349 130
#          1  27  58
                                          
#                Accuracy : 0.7216          
#                  95% CI : (0.6827, 0.7583)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.002887        
                                          
#                   Kappa : 0.2743          
#  Mcnemar's Test P-Value : 3.937e-16       
                                          
#             Sensitivity : 0.9282**          
#             Specificity : 0.3085          
#          Pos Pred Value : 0.7286          
#          Neg Pred Value : 0.6824          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6188          
#    Detection Prevalence : 0.8493          
#       Balanced Accuracy : 0.6184       
                                           
#Race
#Less than 25
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, less25_subset, type="response")

gam_race.pred = rep(0,565)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  81  59
#          1 162 263
                                          
#                Accuracy : 0.6088          
#                  95% CI : (0.5672, 0.6493)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.03345         
                                          
#                   Kappa : 0.1583          
#  Mcnemar's Test P-Value : 6.825e-12       
                                          
#             Sensitivity : 0.3333          
#             Specificity : 0.8168*          
#          Pos Pred Value : 0.5786          
#          Neg Pred Value : 0.6188          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1434          
#    Detection Prevalence : 0.2478          
#       Balanced Accuracy : 0.5751    

#25 - 45
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, from25to45_subset, type="response")

gam_race.pred = rep(0,1510)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 573 274
#          1 217 446
                                          
#                Accuracy : 0.6748          
#                  95% CI : (0.6506, 0.6984)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.346           
#  Mcnemar's Test P-Value : 0.0115          
                                          
#             Sensitivity : 0.7253          
#             Specificity : 0.6194          
#          Pos Pred Value : 0.6765          
#          Neg Pred Value : 0.6727          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3795          
#    Detection Prevalence : 0.5609          
#       Balanced Accuracy : 0.6724  

#Greater than 45
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, greater45_subset, type="response")

gam_race.pred = rep(0,564)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 348 130
#          1  28  58
                                          
#                Accuracy : 0.7199          
#                  95% CI : (0.6808, 0.7566)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.003813        
                                          
#                   Kappa : 0.2708          
#  Mcnemar's Test P-Value : 9.348e-16       
                                          
#             Sensitivity : 0.9255**          
#             Specificity : 0.3085          
#          Pos Pred Value : 0.7280          
#          Neg Pred Value : 0.6744          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6170          
#    Detection Prevalence : 0.8475          
#       Balanced Accuracy : 0.6170   

#Gender
#Less than 25
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, less25_subset, type="response")

gam_sex.pred = rep(0,565)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  76  44
#          1 167 278
                                          
#                Accuracy : 0.6265          
#                  95% CI : (0.5852, 0.6666)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.003555        
                                          
#                   Kappa : 0.1878          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.3128          
#             Specificity : 0.8634**          
#          Pos Pred Value : 0.6333          
#          Neg Pred Value : 0.6247          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1345          
#    Detection Prevalence : 0.2124          
#       Balanced Accuracy : 0.5881          

#25 - 45
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, from25to45_subset, type="response")

gam_sex.pred = rep(0,1510)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 521 248
#          1 269 472
                                          
#                Accuracy : 0.6576          
#                  95% CI : (0.6331, 0.6816)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3146          
#  Mcnemar's Test P-Value : 0.3791          
                                          
#             Sensitivity : 0.6595          
#             Specificity : 0.6556          
#          Pos Pred Value : 0.6775          
#          Neg Pred Value : 0.6370          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3450          
#    Detection Prevalence : 0.5093          
#       Balanced Accuracy : 0.6575         

#Greater than 45
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, greater45_subset, type="response")

gam_sex.pred = rep(0,564)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 346 126
#          1  30  62
                                        
#                Accuracy : 0.7234        
#                  95% CI : (0.6845, 0.76)
#     No Information Rate : 0.6667        
#     P-Value [Acc > NIR] : 0.002168      
                                        
#                   Kappa : 0.2866        
#  Mcnemar's Test P-Value : 2.825e-14     
                                        
#             Sensitivity : 0.9202**        
#             Specificity : 0.3298        
#          Pos Pred Value : 0.7331        
#          Neg Pred Value : 0.6739        
#              Prevalence : 0.6667        
#          Detection Rate : 0.6135        
#    Detection Prevalence : 0.8369        
#       Balanced Accuracy : 0.6250           

#Both
#Less than 25
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, less25_subset, type="response")

gam_both.pred = rep(0,565)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=less25_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0  98  71
#          1 145 251
                                          
#                Accuracy : 0.6177          
#                  95% CI : (0.5762, 0.6579)
#     No Information Rate : 0.5699          
#     P-Value [Acc > NIR] : 0.01187         
                                          
#                   Kappa : 0.1899          
#  Mcnemar's Test P-Value : 6.799e-07       
                                          
#             Sensitivity : 0.4033          
#             Specificity : 0.7795*          
#          Pos Pred Value : 0.5799          
#          Neg Pred Value : 0.6338          
#              Prevalence : 0.4301          
#          Detection Rate : 0.1735          
#    Detection Prevalence : 0.2991          
#       Balanced Accuracy : 0.5914         

#25 - 45
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, from25to45_subset, type="response")

gam_both.pred = rep(0,1510)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=from25to45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 582 287
#          1 208 433
                                          
#                Accuracy : 0.6722          
#                  95% CI : (0.6479, 0.6958)
#     No Information Rate : 0.5232          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3397          
#  Mcnemar's Test P-Value : 0.0004552       
                                          
#             Sensitivity : 0.7367          
#             Specificity : 0.6014          
#          Pos Pred Value : 0.6697          
#          Neg Pred Value : 0.6755          
#              Prevalence : 0.5232          
#          Detection Rate : 0.3854          
#    Detection Prevalence : 0.5755          
#       Balanced Accuracy : 0.6690   

#Greater than 45
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, greater45_subset, type="response")

gam_both.pred = rep(0,564)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=greater45_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 348 130
#          1  28  58
                                          
#                Accuracy : 0.7199          
#                  95% CI : (0.6808, 0.7566)
#     No Information Rate : 0.6667          
#     P-Value [Acc > NIR] : 0.003813        
                                          
#                   Kappa : 0.2708          
#  Mcnemar's Test P-Value : 9.348e-16       
                                          
#             Sensitivity : 0.9255**          
#             Specificity : 0.3085          
#          Pos Pred Value : 0.7280          
#          Neg Pred Value : 0.6744          
#              Prevalence : 0.6667          
#          Detection Rate : 0.6170          
#    Detection Prevalence : 0.8475          
#       Balanced Accuracy : 0.6170            
