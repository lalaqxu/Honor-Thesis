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
am_subset = test[which(test$race_factor=="African-American" & test$gender_factor=="Male") ,]
nrow(am_subset)
#1304
cm_subset = test[which(test$race_factor=="Caucasian" & test$gender_factor=="Male") ,]
nrow(cm_subset)
#831
af_subset = test[which(test$race_factor=="African-American" & test$gender_factor=="Female") ,]
nrow(af_subset)
#259
cf_subset = test[which(test$race_factor=="Caucasian" & test$gender_factor=="Female") ,]
nrow(cf_subset)
#245

#MULTIPLE LINEAR REGRESSION
#Neither
#AM
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, am_subset, type="response")
lm.pred = rep(0,1304)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=am_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 345 196
#          1 226 537
                                          
#                Accuracy : 0.6764          
#                  95% CI : (0.6502, 0.7017)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3388          
#  Mcnemar's Test P-Value : 0.158           
                                          
#             Sensitivity : 0.6042          
#             Specificity : 0.7326          
#          Pos Pred Value : 0.6377          
#          Neg Pred Value : 0.7038          
#              Prevalence : 0.4379          
#          Detection Rate : 0.2646          
#    Detection Prevalence : 0.4149          
#       Balanced Accuracy : 0.6684  

#CM
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, cm_subset, type="response")
lm.pred = rep(0,831)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=cm_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 409 175
#          1 103 144
                                          
#                Accuracy : 0.6655          
#                  95% CI : (0.6322, 0.6975)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.001797        
                                          
#                   Kappa : 0.2614          
#  Mcnemar's Test P-Value : 2.06e-05        
                                          
#             Sensitivity : 0.7988          
#             Specificity : 0.4514          
#          Pos Pred Value : 0.7003          
#          Neg Pred Value : 0.5830          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4922          
#    Detection Prevalence : 0.7028          
#       Balanced Accuracy : 0.6251

#AF
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, af_subset, type="response")
lm.pred = rep(0,259)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=af_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 122  37
#          1  44  56
                                         
#                Accuracy : 0.6873         
#                  95% CI : (0.627, 0.7432)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.06709        
                                         
#                   Kappa : 0.3316         
#  Mcnemar's Test P-Value : 0.50499        
                                         
#             Sensitivity : 0.7349         
#             Specificity : 0.6022         
#          Pos Pred Value : 0.7673         
#          Neg Pred Value : 0.5600         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4710         
#    Detection Prevalence : 0.6139         
#       Balanced Accuracy : 0.6685     

#CF
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, cf_subset, type="response")
lm.pred = rep(0,245)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=cf_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 134  53
#          1  26  32
                                          
#                Accuracy : 0.6776          
#                  95% CI : (0.6151, 0.7357)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.231179        
                                          
#                   Kappa : 0.2312          
#  Mcnemar's Test P-Value : 0.003442        
                                          
             Sensitivity : 0.8375          
#             Specificity : 0.3765          
#          Pos Pred Value : 0.7166          
#          Neg Pred Value : 0.5517          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5469          
#    Detection Prevalence : 0.7633          
#       Balanced Accuracy : 0.6070 

#Race
#AM
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, am_subset, type="response")
lm_race.pred = rep(0,1304)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 345 201
#          1 226 532
                                         
#                Accuracy : 0.6725         
#                  95% CI : (0.6463, 0.698)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 2.341e-16      
                                         
#                   Kappa : 0.3316         
#  Mcnemar's Test P-Value : 0.2455         
                                         
#             Sensitivity : 0.6042         
#             Specificity : 0.7258         
#          Pos Pred Value : 0.6319         
#          Neg Pred Value : 0.7018         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2646         
#    Detection Prevalence : 0.4187         
#       Balanced Accuracy : 0.6650   

#CM
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, cm_subset, type="response")
lm_race.pred = rep(0,831)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=cm_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 402 173
#          1 110 146
                                          
#                Accuracy : 0.6594          
#                  95% CI : (0.6261, 0.6917)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.0054034       
                                          
#                   Kappa : 0.2522          
#  Mcnemar's Test P-Value : 0.0002282       
                                          
#             Sensitivity : 0.7852          
#             Specificity : 0.4577          
#          Pos Pred Value : 0.6991          
#          Neg Pred Value : 0.5703          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4838          
#    Detection Prevalence : 0.6919          
#       Balanced Accuracy : 0.6214

#AF
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, af_subset, type="response")
lm_race.pred = rep(0,259)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 122  37
#          1  44  56
                                         
#                Accuracy : 0.6873         
#                  95% CI : (0.627, 0.7432)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.06709        
                                         
#                   Kappa : 0.3316         
#  Mcnemar's Test P-Value : 0.50499        
                                         
#             Sensitivity : 0.7349         
#             Specificity : 0.6022         
#          Pos Pred Value : 0.7673         
#          Neg Pred Value : 0.5600         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4710         
#    Detection Prevalence : 0.6139         
#       Balanced Accuracy : 0.6685 

#CF
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, cf_subset, type="response")
lm_race.pred = rep(0,245)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=cf_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 133  52
#          1  27  33
                                          
#                Accuracy : 0.6776          
#                  95% CI : (0.6151, 0.7357)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.23118         
                                          
#                   Kappa : 0.2357          
#  Mcnemar's Test P-Value : 0.00693         
                                          
             Sensitivity : 0.8313          
#             Specificity : 0.3882          
#          Pos Pred Value : 0.7189          
#          Neg Pred Value : 0.5500          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5429          
#    Detection Prevalence : 0.7551          
#       Balanced Accuracy : 0.6097  

#Gender**
#AM
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, am_subset, type="response")
lm_sex.pred = rep(0,1304)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 296 152
#          1 275 581
                                         
#                Accuracy : 0.6725         
#                  95% CI : (0.6463, 0.698)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 2.341e-16      
                                         
#                   Kappa : 0.3186         
#  Mcnemar's Test P-Value : 3.548e-09      
                                         
#             Sensitivity : 0.5184         
#             Specificity : 0.7926         
#          Pos Pred Value : 0.6607         
#          Neg Pred Value : 0.6787         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2270         
#    Detection Prevalence : 0.3436         
#       Balanced Accuracy : 0.6555

#CM
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, cm_subset, type="response")
lm_sex.pred = rep(0,831)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=cm_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 352 143
#          1 160 176
                                          
#                Accuracy : 0.6354          
#                  95% CI : (0.6016, 0.6682)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.1343          
                                          
#                   Kappa : 0.2368          
#  Mcnemar's Test P-Value : 0.3580          
                                          
#             Sensitivity : 0.6875          
#             Specificity : 0.5517          
#          Pos Pred Value : 0.7111          
#          Neg Pred Value : 0.5238          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4236          
#    Detection Prevalence : 0.5957          
#       Balanced Accuracy : 0.6196          

#AF
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, af_subset, type="response")
lm_sex.pred = rep(0,259)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 145  56
#          1  21  37
                                         
#                Accuracy : 0.7027         
#                  95% CI : (0.643, 0.7577)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.0212330      
                                         
#                   Kappa : 0.2958         
#  Mcnemar's Test P-Value : 0.0001068      
                                         
             Sensitivity : 0.8735         
#             Specificity : 0.3978         
#          Pos Pred Value : 0.7214         
#          Neg Pred Value : 0.6379         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5598         
#    Detection Prevalence : 0.7761         
#       Balanced Accuracy : 0.6357 

#CF
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, cf_subset, type="response")
lm_sex.pred = rep(0,245)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=cf_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 150  67
#          1  10  18
                                          
#                Accuracy : 0.6857          
#                  95% CI : (0.6235, 0.7433)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.157           
                                          
#                   Kappa : 0.1771          
#  Mcnemar's Test P-Value : 1.75e-10        
                                          
             Sensitivity : 0.9375          
             Specificity : 0.2118          
#          Pos Pred Value : 0.6912          
#          Neg Pred Value : 0.6429          
#              Prevalence : 0.6531          
#          Detection Rate : 0.6122          
#    Detection Prevalence : 0.8857          
#       Balanced Accuracy : 0.5746          
                                 
#Both
#AM
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, am_subset, type="response")
lm_both.pred = rep(0,1304)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 342 186
#          1 229 547
                                         
#                Accuracy : 0.6817         
#                  95% CI : (0.6557, 0.707)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3481         
#  Mcnemar's Test P-Value : 0.03924        
                                         
#             Sensitivity : 0.5989         
#             Specificity : 0.7462         
#          Pos Pred Value : 0.6477         
#          Neg Pred Value : 0.7049         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2623         
#    Detection Prevalence : 0.4049         
#       Balanced Accuracy : 0.6726         
                                  
#CM
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, cm_subset, type="response")
lm_both.pred = rep(0,831)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 385 166
#          1 127 153
                                          
#                Accuracy : 0.6474          
#                  95% CI : (0.6138, 0.6799)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.03394         
                                          
#                   Kappa : 0.237           
#  Mcnemar's Test P-Value : 0.02642         
                                          
#             Sensitivity : 0.7520          
#             Specificity : 0.4796          
#          Pos Pred Value : 0.6987          
#          Neg Pred Value : 0.5464          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4633          
#    Detection Prevalence : 0.6631          
#       Balanced Accuracy : 0.6158 

#AF
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, af_subset, type="response")
lm_both.pred = rep(0,259)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 149  68
#          1  17  25
                                         
#                Accuracy : 0.6718         
#                  95% CI : (0.611, 0.7287)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.1657         
                                         
#                   Kappa : 0.1892         
#  Mcnemar's Test P-Value : 5.852e-08      
                                         
             Sensitivity : 0.8976         
             Specificity : 0.2688         
#          Pos Pred Value : 0.6866         
#          Neg Pred Value : 0.5952         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5753         
#    Detection Prevalence : 0.8378         
#       Balanced Accuracy : 0.5832         
                                  
#CF
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, cf_subset, type="response")
lm_both.pred = rep(0,245)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 152  68
#          1   8  17
                                          
#                Accuracy : 0.6898          
#                  95% CI : (0.6278, 0.7471)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.1265          
                                          
#                   Kappa : 0.1797          
#  Mcnemar's Test P-Value : 1.308e-11       
                                          
             Sensitivity : 0.9500          
             Specificity : 0.2000          
#          Pos Pred Value : 0.6909          
#          Neg Pred Value : 0.6800          
#              Prevalence : 0.6531          
#          Detection Rate : 0.6204          
#    Detection Prevalence : 0.8980          
#       Balanced Accuracy : 0.5750          
                                   
#Logistic
library(ISLR)
#Neither**
#AM
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, am_subset, type="response")
glm.pred = rep(0,1304)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 348 197
#          1 223 536
                                          
#                Accuracy : 0.6779          
#                  95% CI : (0.6518, 0.7032)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3424          
#  Mcnemar's Test P-Value : 0.2225          
                                          
#             Sensitivity : 0.6095          
#             Specificity : 0.7312          
#          Pos Pred Value : 0.6385          
#          Neg Pred Value : 0.7062          
#              Prevalence : 0.4379          
#          Detection Rate : 0.2669          
#    Detection Prevalence : 0.4179          
#       Balanced Accuracy : 0.6703 

#CM
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, cm_subset, type="response")
glm.pred = rep(0,831)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 411 174
#          1 101 145
                                         
#                Accuracy : 0.6691         
#                  95% CI : (0.6359, 0.701)
#     No Information Rate : 0.6161         
#     P-Value [Acc > NIR] : 0.0008738      
                                         
#                   Kappa : 0.2689         
#  Mcnemar's Test P-Value : 1.413e-05      
                                         
             Sensitivity : 0.8027         
#             Specificity : 0.4545         
#          Pos Pred Value : 0.7026         
#          Neg Pred Value : 0.5894         
#              Prevalence : 0.6161         
#          Detection Rate : 0.4946         
#    Detection Prevalence : 0.7040         
#       Balanced Accuracy : 0.6286
            
#AF
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, af_subset, type="response")
glm.pred = rep(0,259)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 122  35
#          1  44  58
                                         
#                Accuracy : 0.695          
#                  95% CI : (0.635, 0.7505)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.03897        
                                         
#                   Kappa : 0.3511         
#  Mcnemar's Test P-Value : 0.36808        
                                         
#             Sensitivity : 0.7349         
#             Specificity : 0.6237         
#          Pos Pred Value : 0.7771         
#          Neg Pred Value : 0.5686         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4710         
#    Detection Prevalence : 0.6062         
#       Balanced Accuracy : 0.6793 

#CF
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, cf_subset, type="response")
glm.pred = rep(0,245)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 136  52
#          1  24  33
                                          
#                Accuracy : 0.6898          
#                  95% CI : (0.6278, 0.7471)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.126485        
                                          
#                   Kappa : 0.2582          
#  Mcnemar's Test P-Value : 0.001954        
                                          
             Sensitivity : 0.8500          
#             Specificity : 0.3882          
#          Pos Pred Value : 0.7234          
#          Neg Pred Value : 0.5789          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5551          
#    Detection Prevalence : 0.7673          
#       Balanced Accuracy : 0.6191 

#Race
#AM
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, am_subset, type="response")
glm_race.pred = rep(0,1304)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 332 182
#          1 239 551
                                         
#                Accuracy : 0.6771         
#                  95% CI : (0.651, 0.7025)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.3369         
#  Mcnemar's Test P-Value : 0.006347       
                                         
#             Sensitivity : 0.5814         
#             Specificity : 0.7517         
#          Pos Pred Value : 0.6459         
#          Neg Pred Value : 0.6975         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2546         
#    Detection Prevalence : 0.3942         
#       Balanced Accuracy : 0.6666                                      

#CM
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, cm_subset, type="response")
glm_race.pred = rep(0,831)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=cm_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 387 159
#          1 125 160
                                          
#                Accuracy : 0.6582          
#                  95% CI : (0.6249, 0.6905)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.006635        
                                          
#                   Kappa : 0.2627          
#  Mcnemar's Test P-Value : 0.050208        
                                          
#             Sensitivity : 0.7559          
#             Specificity : 0.5016          
#          Pos Pred Value : 0.7088          
#          Neg Pred Value : 0.5614          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4657          
#    Detection Prevalence : 0.6570          
#       Balanced Accuracy : 0.6287

#AF
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, af_subset, type="response")
glm_race.pred = rep(0,259)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 119  34
#          1  47  59
                                         
#                Accuracy : 0.6873         
#                  95% CI : (0.627, 0.7432)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.06709        
                                         
#                   Kappa : 0.3408         
#  Mcnemar's Test P-Value : 0.18242        
                                         
#             Sensitivity : 0.7169         
#             Specificity : 0.6344         
#          Pos Pred Value : 0.7778         
#          Neg Pred Value : 0.5566         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4595         
#    Detection Prevalence : 0.5907         
#       Balanced Accuracy : 0.6756                                      

#CF
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, cm_subset, type="response")
glm_race.pred = rep(0,245)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=cm_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 108  57
#          1 125 160
                                          
#                Accuracy : 0.5956          
#                  95% CI : (0.5486, 0.6413)
#     No Information Rate : 0.5178          
#     P-Value [Acc > NIR] : 0.0005453       
                                          
#                   Kappa : 0.1987          
#  Mcnemar's Test P-Value : 6.822e-07       
                                          
#             Sensitivity : 0.4635          
#             Specificity : 0.7373          
#          Pos Pred Value : 0.6545          
#          Neg Pred Value : 0.5614          
#              Prevalence : 0.5178          
#          Detection Rate : 0.2400          
#    Detection Prevalence : 0.3667          
#       Balanced Accuracy : 0.6004

#Gender**
#AM
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, am_subset, type="response")
glm_sex.pred = rep(0,1304)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=am_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 335 182
#          1 236 551
                                          
#                Accuracy : 0.6794          
#                  95% CI : (0.6534, 0.7047)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.342           
#  Mcnemar's Test P-Value : 0.009533        
                                          
#             Sensitivity : 0.5867          
#             Specificity : 0.7517          
#          Pos Pred Value : 0.6480          
#          Neg Pred Value : 0.7001          
#              Prevalence : 0.4379          
#          Detection Rate : 0.2569          
#    Detection Prevalence : 0.3965          
#       Balanced Accuracy : 0.6692 

#CM
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, cm_subset, type="response")
glm_sex.pred = rep(0,831)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=cm_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 395 165
#          1 117 154
                                          
#                Accuracy : 0.6606          
#                  95% CI : (0.6273, 0.6928)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.004378        
                                          
#                   Kappa : 0.2617          
#  Mcnemar's Test P-Value : 0.005129        
                                          
#             Sensitivity : 0.7715          
#             Specificity : 0.4828          
#          Pos Pred Value : 0.7054          
#          Neg Pred Value : 0.5683          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4753          
#    Detection Prevalence : 0.6739          
#       Balanced Accuracy : 0.6271  

#AF
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, af_subset, type="response")
glm_sex.pred = rep(0,259)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=af_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 148  60
#          1  18  33
                                         
#                Accuracy : 0.6988         
#                  95% CI : (0.639, 0.7541)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.029          
                                         
#                   Kappa : 0.2736         
#  Mcnemar's Test P-Value : 3.445e-06      
                                         
             Sensitivity : 0.8916         
#             Specificity : 0.3548         
#          Pos Pred Value : 0.7115         
#          Neg Pred Value : 0.6471         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5714         
#    Detection Prevalence : 0.8031         
#       Balanced Accuracy : 0.6232

#CF
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, cf_subset, type="response")
glm_sex.pred = rep(0,245)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=cf_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 152  68
#          1   8  17
                                          
#                Accuracy : 0.6898          
#                  95% CI : (0.6278, 0.7471)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.1265          
                                          
#                   Kappa : 0.1797          
#  Mcnemar's Test P-Value : 1.308e-11       
                                          
             Sensitivity : 0.9500          
             Specificity : 0.2000          
#          Pos Pred Value : 0.6909          
#          Neg Pred Value : 0.6800          
#              Prevalence : 0.6531          
#          Detection Rate : 0.6204          
#    Detection Prevalence : 0.8980          
#       Balanced Accuracy : 0.5750

#Both
#AM
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, am_subset, type="response")
glm_both.pred = rep(0,1304)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 336 186
#          1 235 547
                                         
#                Accuracy : 0.6771         
#                  95% CI : (0.651, 0.7025)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3379         
#  Mcnemar's Test P-Value : 0.01932        
                                         
#             Sensitivity : 0.5884         
#             Specificity : 0.7462         
#          Pos Pred Value : 0.6437         
#          Neg Pred Value : 0.6995         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2577         
#    Detection Prevalence : 0.4003         
#       Balanced Accuracy : 0.6673   

#CM
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, cm_subset, type="response")
glm_both.pred = rep(0,831)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 388 161
#          1 124 158
                                          
#                Accuracy : 0.657           
#                  95% CI : (0.6237, 0.6893)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.008109        
                                          
#                   Kappa : 0.2588          
#  Mcnemar's Test P-Value : 0.032969        
                                          
#             Sensitivity : 0.7578          
#             Specificity : 0.4953          
#          Pos Pred Value : 0.7067          
#          Neg Pred Value : 0.5603          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4669          
#    Detection Prevalence : 0.6606          
#       Balanced Accuracy : 0.6266 

#AF
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, af_subset, type="response")
glm_both.pred = rep(0,259)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 148  63
#          1  18  30
                                         
#                Accuracy : 0.6873         
#                  95% CI : (0.627, 0.7432)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.06709        
                                         
#                   Kappa : 0.2396         
#  Mcnemar's Test P-Value : 1.014e-06      
                                         
             Sensitivity : 0.8916         
#             Specificity : 0.3226         
#          Pos Pred Value : 0.7014         
#          Neg Pred Value : 0.6250         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5714         
#    Detection Prevalence : 0.8147         
#       Balanced Accuracy : 0.6071

#CF
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, cf_subset, type="response")
glm_both.pred = rep(0,245)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 152  67
#          1   8  18
                                        
#                Accuracy : 0.6939        
#                  95% CI : (0.632, 0.751)
#     No Information Rate : 0.6531        
#     P-Value [Acc > NIR] : 0.1003        
                                        
#                   Kappa : 0.1932        
#  Mcnemar's Test P-Value : 2.124e-11     
                                        
#             Sensitivity : 0.9500        
#             Specificity : 0.2118        
#          Pos Pred Value : 0.6941        
#          Neg Pred Value : 0.6923        
#              Prevalence : 0.6531        
#          Detection Rate : 0.6204        
#    Detection Prevalence : 0.8939        
#       Balanced Accuracy : 0.5809  

#LDA
library(MASS)
#Neither
#AM
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, am_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,1304)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 367 223
#          1 204 510
                                         
#                Accuracy : 0.6725         
#                  95% CI : (0.6463, 0.698)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 2.341e-16      
                                         
#                   Kappa : 0.3373         
#  Mcnemar's Test P-Value : 0.3837         
                                         
#             Sensitivity : 0.6427         
#             Specificity : 0.6958         
#          Pos Pred Value : 0.6220         
#          Neg Pred Value : 0.7143         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2814         
#    Detection Prevalence : 0.4525         
#       Balanced Accuracy : 0.6693 

#CM
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, cm_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,831)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=cm_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 421 188
#          1  91 131
                                         
#                Accuracy : 0.6643         
#                  95% CI : (0.631, 0.6963)
#     No Information Rate : 0.6161         
#     P-Value [Acc > NIR] : 0.002262       
                                         
#                   Kappa : 0.2471         
#  Mcnemar's Test P-Value : 9.064e-09      
                                         
#             Sensitivity : 0.8223         
#             Specificity : 0.4107         
#          Pos Pred Value : 0.6913         
#          Neg Pred Value : 0.5901         
#              Prevalence : 0.6161         
#          Detection Rate : 0.5066         
#    Detection Prevalence : 0.7329         
#       Balanced Accuracy : 0.6165    

#AF
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, af_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,259)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 128  42
#          1  38  51
                                         
#                Accuracy : 0.6911         
#                  95% CI : (0.631, 0.7468)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.05153        
                                         
#                   Kappa : 0.3225         
#  Mcnemar's Test P-Value : 0.73732        
                                         
#             Sensitivity : 0.7711         
#             Specificity : 0.5484         
#          Pos Pred Value : 0.7529         
#          Neg Pred Value : 0.5730         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4942         
#    Detection Prevalence : 0.6564         
#       Balanced Accuracy : 0.6597

#CF
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, cf_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,245)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=cf_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 139  56
#          1  21  29
                                          
#                Accuracy : 0.6857          
#                  95% CI : (0.6235, 0.7433)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.1570149       
                                          
#                   Kappa : 0.2323          
#  Mcnemar's Test P-Value : 0.0001068       
                                          
             Sensitivity : 0.8688          
#             Specificity : 0.3412          
#          Pos Pred Value : 0.7128          
#          Neg Pred Value : 0.5800          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5673          
#    Detection Prevalence : 0.7959          
#       Balanced Accuracy : 0.6050

#Race
#AM
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, am_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,1304)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 369 226
#          1 202 507
                                          
#                Accuracy : 0.6718          
#                  95% CI : (0.6455, 0.6972)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 3.757e-16       
                                          
#                   Kappa : 0.3364          
#  Mcnemar's Test P-Value : 0.2662          
                                          
#             Sensitivity : 0.6462          
#             Specificity : 0.6917          
#          Pos Pred Value : 0.6202          
#          Neg Pred Value : 0.7151          
#              Prevalence : 0.4379          
#          Detection Rate : 0.2830          
#    Detection Prevalence : 0.4563          
#       Balanced Accuracy : 0.6690 

#CM
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, cm_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,831)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=cm_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 418 187
#          1  94 132
                                         
#                Accuracy : 0.6619         
#                  95% CI : (0.6286, 0.694)
#     No Information Rate : 0.6161         
#     P-Value [Acc > NIR] : 0.003531       
                                         
#                   Kappa : 0.2436         
#  Mcnemar's Test P-Value : 4.059e-08      
                                         
             Sensitivity : 0.8164         
#             Specificity : 0.4138         
#          Pos Pred Value : 0.6909         
#          Neg Pred Value : 0.5841         
#              Prevalence : 0.6161         
#          Detection Rate : 0.5030         
#    Detection Prevalence : 0.7280         
#       Balanced Accuracy : 0.6151 

#AF
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, af_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,259)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 128  43
#          1  38  50
                                         
#                Accuracy : 0.6873         
#                  95% CI : (0.627, 0.7432)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.06709        
                                         
#                   Kappa : 0.3124         
#  Mcnemar's Test P-Value : 0.65672        
                                         
#             Sensitivity : 0.7711         
#             Specificity : 0.5376         
#          Pos Pred Value : 0.7485         
#          Neg Pred Value : 0.5682         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4942         
#    Detection Prevalence : 0.6602         
#       Balanced Accuracy : 0.6544 

#CF
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, cf_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,245)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 139  54
#          1  21  31
                                        
#                Accuracy : 0.6939        
#                  95% CI : (0.632, 0.751)
#     No Information Rate : 0.6531        
#     P-Value [Acc > NIR] : 0.1003014     
                                        
#                   Kappa : 0.2568        
#  Mcnemar's Test P-Value : 0.0002199     
                                        
             Sensitivity : 0.8688        
#             Specificity : 0.3647        
#          Pos Pred Value : 0.7202        
#          Neg Pred Value : 0.5962        
#              Prevalence : 0.6531        
#          Detection Rate : 0.5673        
#    Detection Prevalence : 0.7878        
#       Balanced Accuracy : 0.6167

#Gender**
#AM
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, am_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,1304)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 331 180
#          1 240 553
                                          
#                Accuracy : 0.6779          
#                  95% CI : (0.6518, 0.7032)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.338           
#  Mcnemar's Test P-Value : 0.003991        
                                          
#             Sensitivity : 0.5797          
#             Specificity : 0.7544          
#          Pos Pred Value : 0.6477          
#          Neg Pred Value : 0.6974          
#              Prevalence : 0.4379          
#          Detection Rate : 0.2538          
#    Detection Prevalence : 0.3919          
#       Balanced Accuracy : 0.6671    

#CM
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, cm_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,831)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=cm_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 389 171
#          1 123 148
                                          
#                Accuracy : 0.6462          
#                  95% CI : (0.6126, 0.6788)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.039740        
                                          
#                   Kappa : 0.2302          
#  Mcnemar's Test P-Value : 0.006123        
                                          
#             Sensitivity : 0.7598          
#             Specificity : 0.4639          
#          Pos Pred Value : 0.6946          
#          Neg Pred Value : 0.5461          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4681          
#    Detection Prevalence : 0.6739          
#       Balanced Accuracy : 0.6119 

#AF
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, af_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,259)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 149  67
#          1  17  26
                                         
#                Accuracy : 0.6757         
#                  95% CI : (0.615, 0.7323)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.1351         
                                         
#                   Kappa : 0.2009         
#  Mcnemar's Test P-Value : 8.975e-08      
                                         
             Sensitivity : 0.8976         
             Specificity : 0.2796         
#          Pos Pred Value : 0.6898         
#          Neg Pred Value : 0.6047         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5753         
#    Detection Prevalence : 0.8340         
#       Balanced Accuracy : 0.5886  

#CF
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, cf_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,245)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 152  70
#          1   8  15
                                          
#                Accuracy : 0.6816          
#                  95% CI : (0.6193, 0.7395)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.1919          
                                          
#                   Kappa : 0.1525          
#  Mcnemar's Test P-Value : 4.954e-12       
                                          
             Sensitivity : 0.9500          
             Specificity : 0.1765          
#          Pos Pred Value : 0.6847          
#          Neg Pred Value : 0.6522          
#              Prevalence : 0.6531          
#          Detection Rate : 0.6204          
#    Detection Prevalence : 0.9061          
#       Balanced Accuracy : 0.5632
#Both
#AM
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, am_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,1304)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=am_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 341 185
#          1 230 548
                                         
#                Accuracy : 0.6817         
#                  95% CI : (0.6557, 0.707)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3478         
#  Mcnemar's Test P-Value : 0.03078        
                                         
#             Sensitivity : 0.5972         
#             Specificity : 0.7476         
#          Pos Pred Value : 0.6483         
#          Neg Pred Value : 0.7044         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2615         
#    Detection Prevalence : 0.4034         
#       Balanced Accuracy : 0.6724          

#CM
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, cm_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,831)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 385 166
#          1 127 153
                                          
#                Accuracy : 0.6474          
#                  95% CI : (0.6138, 0.6799)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.03394         
                                          
#                   Kappa : 0.237           
#  Mcnemar's Test P-Value : 0.02642         
                                          
#             Sensitivity : 0.7520          
#             Specificity : 0.4796          
#          Pos Pred Value : 0.6987          
#          Neg Pred Value : 0.5464          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4633          
#    Detection Prevalence : 0.6631          
#       Balanced Accuracy : 0.6158 

#AF
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, af_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,259)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 149  68
#          1  17  25
                                         
#                Accuracy : 0.6718         
#                  95% CI : (0.611, 0.7287)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.1657         
                                         
#                   Kappa : 0.1892         
#  Mcnemar's Test P-Value : 5.852e-08      
                                         
             Sensitivity : 0.8976         
             Specificity : 0.2688         
#          Pos Pred Value : 0.6866         
#          Neg Pred Value : 0.5952         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5753         
#    Detection Prevalence : 0.8378         
#       Balanced Accuracy : 0.5832                   

#CF
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, cf_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,245)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 152  68
#          1   8  17
                                          
#                Accuracy : 0.6898          
#                  95% CI : (0.6278, 0.7471)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.1265          
                                          
#                   Kappa : 0.1797          
#  Mcnemar's Test P-Value : 1.308e-11       
                                          
             Sensitivity : 0.9500          
             Specificity : 0.2000          
#          Pos Pred Value : 0.6909          
#          Neg Pred Value : 0.6800          
#              Prevalence : 0.6531          
#          Detection Rate : 0.6204          
#    Detection Prevalence : 0.8980          
#       Balanced Accuracy : 0.5750 

#QDA
#Neither
#AM
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, am_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,1304)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 469 431
#          1 102 302
                                         
#                Accuracy : 0.5913         
#                  95% CI : (0.564, 0.6181)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 0.01797        
                                         
#                   Kappa : 0.2194         
#  Mcnemar's Test P-Value : < 2e-16        
                                         
             Sensitivity : 0.8214         
#             Specificity : 0.4120         
#          Pos Pred Value : 0.5211         
#          Neg Pred Value : 0.7475         
#              Prevalence : 0.4379         
#          Detection Rate : 0.3597         
#    Detection Prevalence : 0.6902         
#       Balanced Accuracy : 0.6167        
                                
#CM
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, cm_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,831)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 483 258
#          1  29  61
                                         
#                Accuracy : 0.6546         
#                  95% CI : (0.6212, 0.687)
#     No Information Rate : 0.6161         
#     P-Value [Acc > NIR] : 0.01193        
                                         
#                   Kappa : 0.1556         
#  Mcnemar's Test P-Value : < 2e-16        
                                         
             Sensitivity : 0.9434         
             Specificity : 0.1912         
#          Pos Pred Value : 0.6518         
#          Neg Pred Value : 0.6778         
#              Prevalence : 0.6161         
#          Detection Rate : 0.5812         
#    Detection Prevalence : 0.8917         
#       Balanced Accuracy : 0.5673 

#AF
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, af_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,259)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=af_subset$two_year_recid)
# #Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 154  73
#          1  12  20
                                         
#                Accuracy : 0.6718         
#                  95% CI : (0.611, 0.7287)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.1657         
                                         
#                   Kappa : 0.1668         
#  Mcnemar's Test P-Value : 7.62e-11       
                                         
             Sensitivity : 0.9277         
#             Specificity : 0.2151         
#          Pos Pred Value : 0.6784         
#          Neg Pred Value : 0.6250         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5946         
#    Detection Prevalence : 0.8764         
#       Balanced Accuracy : 0.5714         
                                
#CF
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, cf_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,245)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=cf_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 155  72
#          1   5  13
                                          
#                Accuracy : 0.6857          
#                  95% CI : (0.6235, 0.7433)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.157           
                                          
#                   Kappa : 0.1493          
#  Mcnemar's Test P-Value : 5.419e-14       
                                          
             Sensitivity : 0.9688          
             Specificity : 0.1529          
#          Pos Pred Value : 0.6828          
#          Neg Pred Value : 0.7222          
#              Prevalence : 0.6531          
#          Detection Rate : 0.6327          
#    Detection Prevalence : 0.9265          
#       Balanced Accuracy : 0.5608 

#Race
#AM
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, am_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,1304)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 470 434
#          1 101 299
                                          
#                Accuracy : 0.5897          
#                  95% CI : (0.5625, 0.6166)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 0.02356         
                                          
#                   Kappa : 0.217           
#  Mcnemar's Test P-Value : < 2e-16         
                                          
             Sensitivity : 0.8231          
#             Specificity : 0.4079          
#          Pos Pred Value : 0.5199          
#          Neg Pred Value : 0.7475          
#              Prevalence : 0.4379          
#          Detection Rate : 0.3604          
#    Detection Prevalence : 0.6933          
#       Balanced Accuracy : 0.6155 

#CM
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, cm_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,831)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 481 256
#          1  31  63
                                         
#                Accuracy : 0.6546         
#                  95% CI : (0.6212, 0.687)
#     No Information Rate : 0.6161         
#     P-Value [Acc > NIR] : 0.01193        
                                         
#                   Kappa : 0.1579         
#  Mcnemar's Test P-Value : < 2e-16        
                                         
             Sensitivity : 0.9395         
             Specificity : 0.1975         
#          Pos Pred Value : 0.6526         
#          Neg Pred Value : 0.6702         
#              Prevalence : 0.6161         
#          Detection Rate : 0.5788         
#    Detection Prevalence : 0.8869         
#       Balanced Accuracy : 0.5685           

#AF
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, af_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,259)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 154  73
#          1  12  20
                                         
#                Accuracy : 0.6718         
#                  95% CI : (0.611, 0.7287)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.1657         
                                         
#                   Kappa : 0.1668         
#  Mcnemar's Test P-Value : 7.62e-11       
                                         
             Sensitivity : 0.9277         
             Specificity : 0.2151         
#          Pos Pred Value : 0.6784         
#          Neg Pred Value : 0.6250         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5946         
#    Detection Prevalence : 0.8764         
#       Balanced Accuracy : 0.5714

#CF
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, cf_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,245)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 155  70
#          1   5  15
                                        
#                Accuracy : 0.6939        
#                  95% CI : (0.632, 0.751)
#     No Information Rate : 0.6531        
#     P-Value [Acc > NIR] : 0.1003        
                                        
#                   Kappa : 0.1769        
#  Mcnemar's Test P-Value : 1.467e-13     
                                        
             Sensitivity : 0.9688        
             Specificity : 0.1765        
#          Pos Pred Value : 0.6889        
#          Neg Pred Value : 0.7500        
#              Prevalence : 0.6531        
#          Detection Rate : 0.6327        
#    Detection Prevalence : 0.9184        
#       Balanced Accuracy : 0.5726 

#Gender
#AM
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, am_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,1304)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 464 413
#          1 107 320
                                          
#                Accuracy : 0.6012          
#                  95% CI : (0.5741, 0.6279)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 0.002341        
                                          
#                   Kappa : 0.2352          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8126          
#             Specificity : 0.4366          
#          Pos Pred Value : 0.5291          
#          Neg Pred Value : 0.7494          
#              Prevalence : 0.4379          
#          Detection Rate : 0.3558          
#    Detection Prevalence : 0.6725          
#       Balanced Accuracy : 0.6246

#CM
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, cm_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,831)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 481 252
#          1  31  67
                                          
#                Accuracy : 0.6594          
#                  95% CI : (0.6261, 0.6917)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.005403        
                                          
#                   Kappa : 0.1719          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.9395          
             Specificity : 0.2100          
#          Pos Pred Value : 0.6562          
#          Neg Pred Value : 0.6837          
#              Prevalence : 0.6161          
#          Detection Rate : 0.5788          
#    Detection Prevalence : 0.8821          
#       Balanced Accuracy : 0.5747

#AF
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, af_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,259)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 157  79
#          1   9  14
                                         
#                Accuracy : 0.6602         
#                  95% CI : (0.599, 0.7177)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.2814         
                                         
#                   Kappa : 0.1154         
#  Mcnemar's Test P-Value : 1.903e-13      
                                         
             Sensitivity : 0.9458         
             Specificity : 0.1505         
#          Pos Pred Value : 0.6653         
#          Neg Pred Value : 0.6087         
#              Prevalence : 0.6409         
#          Detection Rate : 0.6062         
#    Detection Prevalence : 0.9112         
#       Balanced Accuracy : 0.5482         
                                 
#CF
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, cf_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,245)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 158  78
#          1   2   7
                                          
#                Accuracy : 0.6735          
#                  95% CI : (0.6109, 0.7318)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.2744          
                                          
#                   Kappa : 0.0884          
#  Mcnemar's Test P-Value : <2e-16          
                                          
             Sensitivity : 0.98750         
             Specificity : 0.08235         
#          Pos Pred Value : 0.66949         
#          Neg Pred Value : 0.77778         
#              Prevalence : 0.65306         
#          Detection Rate : 0.64490         
#    Detection Prevalence : 0.96327         
#       Balanced Accuracy : 0.53493 

#Both
#AM
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, am_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,1304)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 465 410
#          1 106 323
                                         
#                Accuracy : 0.6043         
#                  95% CI : (0.5772, 0.631)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 0.001132       
                                         
#                   Kappa : 0.2408         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
             Sensitivity : 0.8144         
#             Specificity : 0.4407         
#          Pos Pred Value : 0.5314         
#          Neg Pred Value : 0.7529         
#              Prevalence : 0.4379         
#          Detection Rate : 0.3566         
#    Detection Prevalence : 0.6710         
#       Balanced Accuracy : 0.6275          

#CM
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, cm_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,831)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 480 252
#          1  32  67
                                          
#                Accuracy : 0.6582          
#                  95% CI : (0.6249, 0.6905)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.006635        
                                          
#                   Kappa : 0.1696          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.9375          
             Specificity : 0.2100          
#          Pos Pred Value : 0.6557          
#          Neg Pred Value : 0.6768          
#              Prevalence : 0.6161          
#          Detection Rate : 0.5776          
#    Detection Prevalence : 0.8809          
#       Balanced Accuracy : 0.5738 

#AF
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, af_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,259)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 157  79
#          1   9  14
                                         
#                Accuracy : 0.6602         
#                  95% CI : (0.599, 0.7177)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.2814         
                                         
#                   Kappa : 0.1154         
#  Mcnemar's Test P-Value : 1.903e-13      
                                         
             Sensitivity : 0.9458         
             Specificity : 0.1505         
#          Pos Pred Value : 0.6653         
#          Neg Pred Value : 0.6087         
#              Prevalence : 0.6409         
#          Detection Rate : 0.6062         
#    Detection Prevalence : 0.9112         
#       Balanced Accuracy : 0.5482          

#CF
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, cf_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,245)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 158  76
#          1   2   9
                                          
#                Accuracy : 0.6816          
#                  95% CI : (0.6193, 0.7395)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.1919          
                                          
#                   Kappa : 0.1173          
#  Mcnemar's Test P-Value : <2e-16          
                                          
             Sensitivity : 0.9875          
             Specificity : 0.1059          
#          Pos Pred Value : 0.6752          
#          Neg Pred Value : 0.8182          
#              Prevalence : 0.6531          
#          Detection Rate : 0.6449          
#    Detection Prevalence : 0.9551          
#       Balanced Accuracy : 0.5467  

#KNN
library(class)
attach(df_race)
#Neither
#AM
train.y = two_year_recid[n.train]
test.y = am_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(am_subset$age, am_subset$juv_fel_misd, am_subset$priors_count, am_subset$crime_factor, am_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(am_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 379 259
#          1 192 474
                                        
#                Accuracy : 0.6541        
#                  95% CI : (0.6276, 0.68)
#     No Information Rate : 0.5621        
#     P-Value [Acc > NIR] : 7.856e-12     
                                        
#                   Kappa : 0.3064        
#  Mcnemar's Test P-Value : 0.001885      
                                        
#             Sensitivity : 0.6637        
#             Specificity : 0.6467        
#          Pos Pred Value : 0.5940        
#          Neg Pred Value : 0.7117        
#              Prevalence : 0.4379        
#          Detection Rate : 0.2906        
#    Detection Prevalence : 0.4893        
#       Balanced Accuracy : 0.6552  

#CM
train.y = two_year_recid[n.train]
test.y = cm_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(cm_subset$age, cm_subset$juv_fel_misd, cm_subset$priors_count, cm_subset$crime_factor, cm_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(cm_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 432 194
#          1  80 125
                                          
#                Accuracy : 0.6703          
#                  95% CI : (0.6372, 0.7022)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.0006802       
                                          
#                   Kappa : 0.2526          
#  Mcnemar's Test P-Value : 8.696e-12       
                                          
             Sensitivity : 0.8438          
#             Specificity : 0.3918          
#          Pos Pred Value : 0.6901          
#          Neg Pred Value : 0.6098          
#              Prevalence : 0.6161          
#          Detection Rate : 0.5199          
#    Detection Prevalence : 0.7533          
#       Balanced Accuracy : 0.6178   

#AF
train.y = two_year_recid[n.train]
test.y = af_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(af_subset$age, af_subset$juv_fel_misd, af_subset$priors_count, af_subset$crime_factor, af_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(af_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 123  42
#          1  43  51
                                         
#                Accuracy : 0.6718         
#                  95% CI : (0.611, 0.7287)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.1657         
                                         
#                   Kappa : 0.2887         
#  Mcnemar's Test P-Value : 1.0000         
                                         
#             Sensitivity : 0.7410         
#             Specificity : 0.5484         
#          Pos Pred Value : 0.7455         
#          Neg Pred Value : 0.5426         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4749         
#    Detection Prevalence : 0.6371         
#       Balanced Accuracy : 0.6447 

#CF
train.y = two_year_recid[n.train]
test.y = cf_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(cf_subset$age, cf_subset$juv_fel_misd, cf_subset$priors_count, cf_subset$crime_factor, cf_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(cf_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 138  59
#          1  22  26
                                         
#                Accuracy : 0.6694         
#                  95% CI : (0.6067, 0.728)
#     No Information Rate : 0.6531         
#     P-Value [Acc > NIR] : 0.3212         
                                         
#                   Kappa : 0.1875         
#  Mcnemar's Test P-Value : 6.334e-05      
                                         
             Sensitivity : 0.8625         
#             Specificity : 0.3059         
#          Pos Pred Value : 0.7005         
#          Neg Pred Value : 0.5417         
#              Prevalence : 0.6531         
#          Detection Rate : 0.5633         
#    Detection Prevalence : 0.8041         
#       Balanced Accuracy : 0.5842  

#Race
#AM
train.y = two_year_recid[n.train]
test.y = am_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(am_subset$age, am_subset$juv_fel_misd, am_subset$priors_count, am_subset$crime_factor, am_subset$violent_charge, am_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(am_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 388 252
#          1 183 481
                                         
#                Accuracy : 0.6664         
#                  95% CI : (0.6401, 0.692)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 9.377e-15      
                                         
#                   Kappa : 0.3313         
#  Mcnemar's Test P-Value : 0.001113       
                                         
#             Sensitivity : 0.6795         
#             Specificity : 0.6562         
#          Pos Pred Value : 0.6063         
#          Neg Pred Value : 0.7244         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2975         
#    Detection Prevalence : 0.4908         
#       Balanced Accuracy : 0.6679 

#CM
train.y = two_year_recid[n.train]
test.y = cm_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(cm_subset$age, cm_subset$juv_fel_misd, cm_subset$priors_count, cm_subset$crime_factor, cm_subset$violent_charge, cm_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(cm_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 433 192
#          1  79 127
                                          
#                Accuracy : 0.6739          
#                  95% CI : (0.6408, 0.7057)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.0003112       
                                          
#                   Kappa : 0.2613          
#  Mcnemar's Test P-Value : 1.021e-11       
                                          
             Sensitivity : 0.8457          
#             Specificity : 0.3981          
#          Pos Pred Value : 0.6928          
#          Neg Pred Value : 0.6165          
#              Prevalence : 0.6161          
#          Detection Rate : 0.5211          
#    Detection Prevalence : 0.7521          
#       Balanced Accuracy : 0.6219 

#AF
train.y = two_year_recid[n.train]
test.y = af_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(af_subset$age, af_subset$juv_fel_misd, af_subset$priors_count, af_subset$crime_factor, af_subset$violent_charge, af_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(af_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 126  40
#          1  40  53
                                         
#                Accuracy : 0.6911         
#                  95% CI : (0.631, 0.7468)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.05153        
                                         
#                   Kappa : 0.3289         
#  Mcnemar's Test P-Value : 1.00000        
                                         
#             Sensitivity : 0.7590         
#             Specificity : 0.5699         
#          Pos Pred Value : 0.7590         
#          Neg Pred Value : 0.5699         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4865         
#    Detection Prevalence : 0.6409         
#       Balanced Accuracy : 0.6645         

#CF
train.y = two_year_recid[n.train]
test.y = cf_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(cf_subset$age, cf_subset$juv_fel_misd, cf_subset$priors_count, cf_subset$crime_factor, cf_subset$violent_charge, cf_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(cf_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 138  56
#          1  22  29
                                          
#                Accuracy : 0.6816          
#                  95% CI : (0.6193, 0.7395)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.1919481       
                                          
#                   Kappa : 0.2247          
#  Mcnemar's Test P-Value : 0.0001866       
                                          
             Sensitivity : 0.8625          
#             Specificity : 0.3412          
#          Pos Pred Value : 0.7113          
#          Neg Pred Value : 0.5686          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5633          
#    Detection Prevalence : 0.7918          
#       Balanced Accuracy : 0.6018 

#Gender
#AM
train.y = two_year_recid[n.train]
test.y = am_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(am_subset$age, am_subset$juv_fel_misd, am_subset$priors_count, am_subset$crime_factor, am_subset$violent_charge, am_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(am_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 380 244
#          1 191 489
                                         
#                Accuracy : 0.6664         
#                  95% CI : (0.6401, 0.692)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 9.377e-15      
                                         
#                   Kappa : 0.3292         
#  Mcnemar's Test P-Value : 0.01266        
                                         
#             Sensitivity : 0.6655         
#             Specificity : 0.6671         
#          Pos Pred Value : 0.6090         
#          Neg Pred Value : 0.7191         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2914         
#    Detection Prevalence : 0.4785         
#       Balanced Accuracy : 0.6663   

#CM
train.y = two_year_recid[n.train]
test.y = cm_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(cm_subset$age, cm_subset$juv_fel_misd, cm_subset$priors_count, cm_subset$crime_factor, cm_subset$violent_charge, cm_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(cm_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 431 190
#          1  81 129
                                          
#                Accuracy : 0.6739          
#                  95% CI : (0.6408, 0.7057)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.0003112       
                                          
#                   Kappa : 0.2631          
#  Mcnemar's Test P-Value : 5.362e-11       
                                          
             Sensitivity : 0.8418          
#             Specificity : 0.4044          
#          Pos Pred Value : 0.6940          
#          Neg Pred Value : 0.6143          
#              Prevalence : 0.6161          
#          Detection Rate : 0.5187          
#    Detection Prevalence : 0.7473          
#       Balanced Accuracy : 0.6231   

#AF
train.y = two_year_recid[n.train]
test.y = af_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(af_subset$age, af_subset$juv_fel_misd, af_subset$priors_count, af_subset$crime_factor, af_subset$violent_charge, af_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(af_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 129  43
#          1  37  50
                                         
#                Accuracy : 0.6911         
#                  95% CI : (0.631, 0.7468)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.05153        
                                         
#                   Kappa : 0.3193         
#  Mcnemar's Test P-Value : 0.57615        
                                         
#             Sensitivity : 0.7771         
#             Specificity : 0.5376         
#          Pos Pred Value : 0.7500         
#          Neg Pred Value : 0.5747         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4981         
#    Detection Prevalence : 0.6641         
#       Balanced Accuracy : 0.6574      

#CM
train.y = two_year_recid[n.train]
test.y = cf_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(cf_subset$age, cf_subset$juv_fel_misd, cf_subset$priors_count, cf_subset$crime_factor, cf_subset$violent_charge, cf_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(cf_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 140  58
#          1  20  27
                                          
#                Accuracy : 0.6816          
#                  95% CI : (0.6193, 0.7395)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.1919          
                                          
#                   Kappa : 0.2152          
#  Mcnemar's Test P-Value : 2.797e-05       
                                          
             Sensitivity : 0.8750          
#             Specificity : 0.3176          
#          Pos Pred Value : 0.7071          
#          Neg Pred Value : 0.5745          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5714          
#    Detection Prevalence : 0.8082          
#       Balanced Accuracy : 0.5963  

#Both**
#AM
train.y = two_year_recid[n.train]
test.y = am_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(am_subset$age, am_subset$juv_fel_misd, am_subset$priors_count, am_subset$crime_factor, am_subset$violent_charge, am_subset$race_factor, am_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(am_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 382 239
#          1 189 494
                                          
#                Accuracy : 0.6718          
#                  95% CI : (0.6455, 0.6972)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 3.757e-16       
                                          
#                   Kappa : 0.3397          
#  Mcnemar's Test P-Value : 0.01786         
                                          
#             Sensitivity : 0.6690          
#             Specificity : 0.6739          
#          Pos Pred Value : 0.6151          
#          Neg Pred Value : 0.7233          
#              Prevalence : 0.4379          
#          Detection Rate : 0.2929          
#    Detection Prevalence : 0.4762          
#       Balanced Accuracy : 0.6715

#CM
train.y = two_year_recid[n.train]
test.y = cm_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(cm_subset$age, cm_subset$juv_fel_misd, cm_subset$priors_count, cm_subset$crime_factor, cm_subset$violent_charge, cm_subset$race_factor, cm_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(cm_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 428 187
#          1  84 132
                                          
#                Accuracy : 0.6739          
#                  95% CI : (0.6408, 0.7057)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.0003112       
                                          
#                   Kappa : 0.2659          
#  Mcnemar's Test P-Value : 5.789e-10       
                                          
             Sensitivity : 0.8359          
#             Specificity : 0.4138          
#          Pos Pred Value : 0.6959          
#          Neg Pred Value : 0.6111          
#              Prevalence : 0.6161          
#          Detection Rate : 0.5150          
#    Detection Prevalence : 0.7401          
#       Balanced Accuracy : 0.6249  

#AF
train.y = two_year_recid[n.train]
test.y = af_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(af_subset$age, af_subset$juv_fel_misd, af_subset$priors_count, af_subset$crime_factor, af_subset$violent_charge, af_subset$race_factor, af_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(af_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 126  40
#          1  40  53
                                         
#                Accuracy : 0.6911         
#                  95% CI : (0.631, 0.7468)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.05153        
                                         
#                   Kappa : 0.3289         
#  Mcnemar's Test P-Value : 1.00000        
                                         
#             Sensitivity : 0.7590         
#             Specificity : 0.5699         
#          Pos Pred Value : 0.7590         
#          Neg Pred Value : 0.5699         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4865         
#    Detection Prevalence : 0.6409         
#       Balanced Accuracy : 0.6645 

#CF
train.y = two_year_recid[n.train]
test.y = cf_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(cf_subset$age, cf_subset$juv_fel_misd, cf_subset$priors_count, cf_subset$crime_factor, cf_subset$violent_charge, cf_subset$race_factor, cf_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(cf_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 145  58
#          1  15  27
                                          
#                Accuracy : 0.702           
#                  95% CI : (0.6405, 0.7586)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.06008         
                                          
#                   Kappa : 0.254           
#  Mcnemar's Test P-Value : 8.845e-07       
                                          
             Sensitivity : 0.9062          
#             Specificity : 0.3176          
#          Pos Pred Value : 0.7143          
#          Neg Pred Value : 0.6429          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5918          
#    Detection Prevalence : 0.8286          
#       Balanced Accuracy : 0.6119 

#Tree
library(tree)
set.seed(1)
n <- nrow(df_race)
n.train = sample(1:n, n*.5)
n.test = (-n.train)
train = df_race[n.train,]
test = df_race[n.test,]

#Neither
#AM
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
tree.probs = predict(tree, newdata=am_subset)

tree.pred = rep(0,1304)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 249 114
#          1 322 619
                                          
#                Accuracy : 0.6656          
#                  95% CI : (0.6393, 0.6912)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 1.465e-14       
                                          
#                   Kappa : 0.2923          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.4361          
             Specificity : 0.8445          
#          Pos Pred Value : 0.6860          
#          Neg Pred Value : 0.6578          
#              Prevalence : 0.4379          
#          Detection Rate : 0.1910          
#    Detection Prevalence : 0.2784          
#       Balanced Accuracy : 0.6403 

#CM
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
tree.probs = predict(tree, newdata=cm_subset)

tree.pred = rep(0,831)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 315 128
#          1 197 191
                                          
#                Accuracy : 0.6089          
#                  95% CI : (0.5748, 0.6422)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.679311        
                                          
#                   Kappa : 0.2056          
#  Mcnemar's Test P-Value : 0.000162        
                                          
#             Sensitivity : 0.6152          
#             Specificity : 0.5987          
#          Pos Pred Value : 0.7111          
#          Neg Pred Value : 0.4923          
#              Prevalence : 0.6161          
#          Detection Rate : 0.3791          
#    Detection Prevalence : 0.5331          
#       Balanced Accuracy : 0.6070 

#AF
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
tree.probs = predict(tree, newdata=af_subset)

tree.pred = rep(0,259)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 91 28
#          1 75 65
                                          
#                Accuracy : 0.6023          
#                  95% CI : (0.5399, 0.6624)
#     No Information Rate : 0.6409          
#     P-Value [Acc > NIR] : 0.9124          
                                          
#                   Kappa : 0.2224          
#  Mcnemar's Test P-Value : 5.829e-06       
                                          
#             Sensitivity : 0.5482          
#             Specificity : 0.6989          
#          Pos Pred Value : 0.7647          
#          Neg Pred Value : 0.4643          
#              Prevalence : 0.6409          
#          Detection Rate : 0.3514          
#    Detection Prevalence : 0.4595          
#       Balanced Accuracy : 0.6236

#CF
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
text(tree, pretty=0)
tree.probs = predict(tree, newdata=cf_subset)

tree.pred = rep(0,245)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 105  44
#          1  55  41
                                          
#                Accuracy : 0.5959          
#                  95% CI : (0.5316, 0.6579)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.9732          
                                          
#                   Kappa : 0.1345          
#  Mcnemar's Test P-Value : 0.3149          
                                          
#             Sensitivity : 0.6562          
#             Specificity : 0.4824          
#          Pos Pred Value : 0.7047          
#          Neg Pred Value : 0.4271          
#              Prevalence : 0.6531          
#          Detection Rate : 0.4286          
#    Detection Prevalence : 0.6082          
#       Balanced Accuracy : 0.5693 

#Pruning
#AM
set.seed(1)
cv.tree = cv.tree(tree)
tree.min = cv.tree$size[which.min(cv.tree$dev)]
prune = prune.tree(tree, best=5)
summary(prune)
prune.probs = predict(prune, newdata=am_subset)

prune.pred = rep(0,nrow(am_subset))
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 249 114
#          1 322 619
                                          
#                Accuracy : 0.6656          
#                  95% CI : (0.6393, 0.6912)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 1.465e-14       
                                          
#                   Kappa : 0.2923          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.4361          
             Specificity : 0.8445          
#          Pos Pred Value : 0.6860          
#          Neg Pred Value : 0.6578          
#              Prevalence : 0.4379          
#          Detection Rate : 0.1910          
#    Detection Prevalence : 0.2784          
#       Balanced Accuracy : 0.6403         

#CM                                    
set.seed(1)
cv.tree = cv.tree(tree)
tree.min = cv.tree$size[which.min(cv.tree$dev)]
prune = prune.tree(tree, best=5)
summary(prune)
prune.probs = predict(prune, newdata=cm_subset)

prune.pred = rep(0, 831)
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 315 128
#          1 197 191
                                          
#                Accuracy : 0.6089          
#                  95% CI : (0.5748, 0.6422)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.679311        
                                          
#                   Kappa : 0.2056          
#  Mcnemar's Test P-Value : 0.000162        
                                          
#             Sensitivity : 0.6152          
#             Specificity : 0.5987          
#          Pos Pred Value : 0.7111          
#          Neg Pred Value : 0.4923          
#              Prevalence : 0.6161          
#          Detection Rate : 0.3791          
#    Detection Prevalence : 0.5331          
#       Balanced Accuracy : 0.6070 

#AF
set.seed(1)
cv.tree = cv.tree(tree)
tree.min = cv.tree$size[which.min(cv.tree$dev)]
prune = prune.tree(tree, best=5)
summary(prune)
prune.probs = predict(prune, newdata=af_subset)

prune.pred = rep(0,nrow(af_subset))
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 91 28
#          1 75 65
                                          
#                Accuracy : 0.6023          
#                  95% CI : (0.5399, 0.6624)
#     No Information Rate : 0.6409          
#     P-Value [Acc > NIR] : 0.9124          
                                          
#                   Kappa : 0.2224          
#  Mcnemar's Test P-Value : 5.829e-06       
                                          
#             Sensitivity : 0.5482          
#             Specificity : 0.6989          
#          Pos Pred Value : 0.7647          
#          Neg Pred Value : 0.4643          
#              Prevalence : 0.6409          
#          Detection Rate : 0.3514          
#    Detection Prevalence : 0.4595          
#       Balanced Accuracy : 0.6236        

#CF                                    
set.seed(1)
cv.tree = cv.tree(tree)
tree.min = cv.tree$size[which.min(cv.tree$dev)]
prune = prune.tree(tree, best=5)
summary(prune)
prune.probs = predict(prune, newdata=cf_subset)

prune.pred = rep(0,nrow(cf_subset))
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 105  44
#          1  55  41
                                          
#                Accuracy : 0.5959          
#                  95% CI : (0.5316, 0.6579)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.9732          
                                          
#                   Kappa : 0.1345          
#  Mcnemar's Test P-Value : 0.3149          
                                          
#             Sensitivity : 0.6562          
#             Specificity : 0.4824          
#          Pos Pred Value : 0.7047          
#          Neg Pred Value : 0.4271          
#              Prevalence : 0.6531          
#          Detection Rate : 0.4286          
#    Detection Prevalence : 0.6082          
#       Balanced Accuracy : 0.5693

#Random Forest
#AM
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=am_subset)

rf.pred = rep(0,1304)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 332 194
#          1 239 539
                                          
#                Accuracy : 0.6679          
#                  95% CI : (0.6416, 0.6935)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 3.804e-15       
                                          
#                   Kappa : 0.3196          
#  Mcnemar's Test P-Value : 0.03447         
                                          
#             Sensitivity : 0.5814          
#             Specificity : 0.7353          
#          Pos Pred Value : 0.6312          
#          Neg Pred Value : 0.6928          
#              Prevalence : 0.4379          
#          Detection Rate : 0.2546          
#    Detection Prevalence : 0.4034          
#       Balanced Accuracy : 0.6584

#CM
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=cm_subset)

rf.pred = rep(0,831)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 387 160
#          1 125 159
                                          
#                Accuracy : 0.657           
#                  95% CI : (0.6237, 0.6893)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.008109        
                                          
#                   Kappa : 0.2597          
#  Mcnemar's Test P-Value : 0.044011        
                                          
#             Sensitivity : 0.7559          
#             Specificity : 0.4984          
#          Pos Pred Value : 0.7075          
#          Neg Pred Value : 0.5599          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4657          
#    Detection Prevalence : 0.6582          
#       Balanced Accuracy : 0.6271 

#AF
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=af_subset)

rf.pred = rep(0,259)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 118  38
#          1  48  55
                                        
#                Accuracy : 0.668         
#                  95% CI : (0.607, 0.725)
#     No Information Rate : 0.6409        
#     P-Value [Acc > NIR] : 0.2004        
                                        
#                   Kappa : 0.2953        
#  Mcnemar's Test P-Value : 0.3318        
                                        
#             Sensitivity : 0.7108        
#             Specificity : 0.5914        
#          Pos Pred Value : 0.7564        
#          Neg Pred Value : 0.5340        
#              Prevalence : 0.6409        
#          Detection Rate : 0.4556        
#    Detection Prevalence : 0.6023        
#       Balanced Accuracy : 0.6511 

#CF
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=cf_subset)

rf.pred = rep(0,245)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 133  45
#          1  27  40
                                          
#                Accuracy : 0.7061          
#                  95% CI : (0.6448, 0.7624)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.04534         
                                          
#                   Kappa : 0.3176          
#  Mcnemar's Test P-Value : 0.04513         
                                          
             Sensitivity : 0.8313          
#             Specificity : 0.4706          
#          Pos Pred Value : 0.7472          
#          Neg Pred Value : 0.5970          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5429          
#    Detection Prevalence : 0.7265          
#       Balanced Accuracy : 0.6509 

#Race
#AM
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
tree_race.probs = predict(tree_race, newdata=am_subset)

tree_race.pred = rep(0,1304)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 249 114
#          1 322 619
                                          
#                Accuracy : 0.6656          
#                  95% CI : (0.6393, 0.6912)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 1.465e-14       
                                          
#                   Kappa : 0.2923          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.4361          
             Specificity : 0.8445          
#          Pos Pred Value : 0.6860          
#          Neg Pred Value : 0.6578          
#              Prevalence : 0.4379          
#          Detection Rate : 0.1910          
#    Detection Prevalence : 0.2784          
#       Balanced Accuracy : 0.6403         

#CM
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
tree_race.probs = predict(tree_race, newdata=cm_subset)

tree_race.pred = rep(0,831)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 315 128
#          1 197 191
                                          
#                Accuracy : 0.6089          
#                  95% CI : (0.5748, 0.6422)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.679311        
                                          
#                   Kappa : 0.2056          
#  Mcnemar's Test P-Value : 0.000162        
                                          
#             Sensitivity : 0.6152          
#             Specificity : 0.5987          
#          Pos Pred Value : 0.7111          
#          Neg Pred Value : 0.4923          
#              Prevalence : 0.6161          
#          Detection Rate : 0.3791          
#    Detection Prevalence : 0.5331          
#       Balanced Accuracy : 0.6070 

#AF
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
tree_race.probs = predict(tree_race, newdata=af_subset)

tree_race.pred = rep(0,259)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 91 28
#          1 75 65
                                          
#                Accuracy : 0.6023          
#                  95% CI : (0.5399, 0.6624)
#     No Information Rate : 0.6409          
#     P-Value [Acc > NIR] : 0.9124          
                                          
#                   Kappa : 0.2224          
#  Mcnemar's Test P-Value : 5.829e-06       
                                          
#             Sensitivity : 0.5482          
#             Specificity : 0.6989          
#          Pos Pred Value : 0.7647          
#          Neg Pred Value : 0.4643          
#              Prevalence : 0.6409          
#          Detection Rate : 0.3514          
#    Detection Prevalence : 0.4595          
#       Balanced Accuracy : 0.6236        

#CM
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
tree_race.probs = predict(tree_race, newdata=cf_subset)

tree_race.pred = rep(0,245)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 105  44
#          1  55  41
                                          
#                Accuracy : 0.5959          
#                  95% CI : (0.5316, 0.6579)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.9732          
                                          
#                   Kappa : 0.1345          
#  Mcnemar's Test P-Value : 0.3149          
                                          
#             Sensitivity : 0.6562          
#             Specificity : 0.4824          
#          Pos Pred Value : 0.7047          
#          Neg Pred Value : 0.4271          
#              Prevalence : 0.6531          
#          Detection Rate : 0.4286          
#    Detection Prevalence : 0.6082          
#       Balanced Accuracy : 0.5693

#Pruning
#AM
set.seed(1)
cv_race.tree = cv.tree(tree_race)
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
prune_race.probs = predict(prune_race, newdata=am_subset)

prune_race.pred = rep(0,1304)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 249 114
#          1 322 619
                                          
#                Accuracy : 0.6656          
#                  95% CI : (0.6393, 0.6912)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 1.465e-14       
                                          
#                   Kappa : 0.2923          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.4361          
             Specificity : 0.8445          
#          Pos Pred Value : 0.6860          
#          Neg Pred Value : 0.6578          
#              Prevalence : 0.4379          
#          Detection Rate : 0.1910          
#    Detection Prevalence : 0.2784          
#       Balanced Accuracy : 0.6403 

#CM
set.seed(1)
cv_race.tree = cv.tree(tree_race)
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
prune_race.probs = predict(prune_race, newdata=cm_subset)

prune_race.pred = rep(0,831)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 315 128
#          1 197 191
                                          
#                Accuracy : 0.6089          
#                  95% CI : (0.5748, 0.6422)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.679311        
                                          
#                   Kappa : 0.2056          
#  Mcnemar's Test P-Value : 0.000162        
                                          
#             Sensitivity : 0.6152          
#             Specificity : 0.5987          
#          Pos Pred Value : 0.7111          
#          Neg Pred Value : 0.4923          
#              Prevalence : 0.6161          
#          Detection Rate : 0.3791          
#    Detection Prevalence : 0.5331          
#       Balanced Accuracy : 0.6070 

#AF
set.seed(1)
cv_race.tree = cv.tree(tree_race)
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
prune_race.probs = predict(prune_race, newdata=af_subset)

prune_race.pred = rep(0,259)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 91 28
#          1 75 65
                                          
#                Accuracy : 0.6023          
#                  95% CI : (0.5399, 0.6624)
#     No Information Rate : 0.6409          
#     P-Value [Acc > NIR] : 0.9124          
                                          
#                   Kappa : 0.2224          
#  Mcnemar's Test P-Value : 5.829e-06       
                                          
#             Sensitivity : 0.5482          
#             Specificity : 0.6989          
#          Pos Pred Value : 0.7647          
#          Neg Pred Value : 0.4643          
#              Prevalence : 0.6409          
#          Detection Rate : 0.3514          
#    Detection Prevalence : 0.4595          
#       Balanced Accuracy : 0.6236  

#CF
set.seed(1)
cv_race.tree = cv.tree(tree_race)
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
prune_race.probs = predict(prune_race, newdata=cf_subset)

prune_race.pred = rep(0,245)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 105  44
#          1  55  41
                                          
#                Accuracy : 0.5959          
#                  95% CI : (0.5316, 0.6579)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.9732          
                                          
#                   Kappa : 0.1345          
#  Mcnemar's Test P-Value : 0.3149          
                                          
#             Sensitivity : 0.6562          
#             Specificity : 0.4824          
#          Pos Pred Value : 0.7047          
#          Neg Pred Value : 0.4271          
#              Prevalence : 0.6531          
#          Detection Rate : 0.4286          
#    Detection Prevalence : 0.6082          
#       Balanced Accuracy : 0.5693 

#Random Forest
#AM
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=am_subset)

rf_race.pred = rep(0,1304)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 316 176
#          1 255 557
                                         
#                Accuracy : 0.6695         
#                  95% CI : (0.6432, 0.695)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 1.523e-15      
                                         
#                   Kappa : 0.3182         
#  Mcnemar's Test P-Value : 0.0001719      
                                         
#             Sensitivity : 0.5534         
#             Specificity : 0.7599         
#          Pos Pred Value : 0.6423         
#          Neg Pred Value : 0.6860         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2423         
#    Detection Prevalence : 0.3773         
#       Balanced Accuracy : 0.6567         
                                 
#CM
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=cm_subset)

rf_race.pred = rep(0,831)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 380 159
#          1 132 160
                                          
#                Accuracy : 0.6498          
#                  95% CI : (0.6163, 0.6823)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.02441         
                                          
#                   Kappa : 0.2477          
#  Mcnemar's Test P-Value : 0.12747         
                                          
#             Sensitivity : 0.7422          
#             Specificity : 0.5016          
#          Pos Pred Value : 0.7050          
#          Neg Pred Value : 0.5479          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4573          
#    Detection Prevalence : 0.6486          
#       Balanced Accuracy : 0.6219  

#AF
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=af_subset)

rf_race.pred = rep(0,259)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 115  35
#          1  51  58
                                        
#                Accuracy : 0.668         
#                  95% CI : (0.607, 0.725)
#     No Information Rate : 0.6409        
#     P-Value [Acc > NIR] : 0.2004        
                                        
#                   Kappa : 0.3049        
#  Mcnemar's Test P-Value : 0.1058        
                                        
#             Sensitivity : 0.6928        
#             Specificity : 0.6237        
#          Pos Pred Value : 0.7667        
#          Neg Pred Value : 0.5321        
#              Prevalence : 0.6409        
#          Detection Rate : 0.4440        
#    Detection Prevalence : 0.5792        
#       Balanced Accuracy : 0.6582       
                                 
#CF
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=cf_subset)

rf_race.pred = rep(0,245)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 128  48
#          1  32  37
                                          
#                Accuracy : 0.6735          
#                  95% CI : (0.6109, 0.7318)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.27442         
                                          
#                   Kappa : 0.2462          
#  Mcnemar's Test P-Value : 0.09353         
                                          
             Sensitivity : 0.8000          
#             Specificity : 0.4353          
#          Pos Pred Value : 0.7273          
#          Neg Pred Value : 0.5362          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5224          
#    Detection Prevalence : 0.7184          
#       Balanced Accuracy : 0.6176 

#Gender
#AM
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
tree_sex.probs = predict(tree_sex, newdata=am_subset)

tree_sex.pred = rep(0,1304)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 249 114
#          1 322 619
                                          
#                Accuracy : 0.6656          
#                  95% CI : (0.6393, 0.6912)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 1.465e-14       
                                          
#                   Kappa : 0.2923          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.4361          
             Specificity : 0.8445          
#          Pos Pred Value : 0.6860          
#          Neg Pred Value : 0.6578          
#              Prevalence : 0.4379          
#          Detection Rate : 0.1910          
#    Detection Prevalence : 0.2784          
#       Balanced Accuracy : 0.6403  

#CM
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
tree_sex.probs = predict(tree_sex, newdata=cm_subset)

tree_sex.pred = rep(0,831)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 315 128
#          1 197 191
                                          
#                Accuracy : 0.6089          
#                  95% CI : (0.5748, 0.6422)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.679311        
                                          
#                   Kappa : 0.2056          
#  Mcnemar's Test P-Value : 0.000162        
                                          
#             Sensitivity : 0.6152          
#             Specificity : 0.5987          
#          Pos Pred Value : 0.7111          
#          Neg Pred Value : 0.4923          
#              Prevalence : 0.6161          
#          Detection Rate : 0.3791          
#    Detection Prevalence : 0.5331          
#       Balanced Accuracy : 0.6070  

#AF
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
tree_sex.probs = predict(tree_sex, newdata=af_subset)

tree_sex.pred = rep(0,259)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 91 28
#          1 75 65
                                          
#                Accuracy : 0.6023          
#                  95% CI : (0.5399, 0.6624)
#     No Information Rate : 0.6409          
#     P-Value [Acc > NIR] : 0.9124          
                                          
#                   Kappa : 0.2224          
#  Mcnemar's Test P-Value : 5.829e-06       
                                          
#             Sensitivity : 0.5482          
#             Specificity : 0.6989          
#          Pos Pred Value : 0.7647          
#          Neg Pred Value : 0.4643          
#              Prevalence : 0.6409          
#          Detection Rate : 0.3514          
#    Detection Prevalence : 0.4595          
#       Balanced Accuracy : 0.6236  

#CF
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
tree_sex.probs = predict(tree_sex, newdata=cf_subset)

tree_sex.pred = rep(0,245)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 105  44
#          1  55  41
                                          
#                Accuracy : 0.5959          
#                  95% CI : (0.5316, 0.6579)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.9732          
                                          
#                   Kappa : 0.1345          
#  Mcnemar's Test P-Value : 0.3149          
                                          
#             Sensitivity : 0.6562          
#             Specificity : 0.4824          
#          Pos Pred Value : 0.7047          
#          Neg Pred Value : 0.4271          
#              Prevalence : 0.6531          
#          Detection Rate : 0.4286          
#    Detection Prevalence : 0.6082          
#       Balanced Accuracy : 0.5693  

#Pruning
#AM
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
prune_sex.probs = predict(prune_sex, newdata=am_subset)

prune_sex.pred = rep(0,1304)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 249 114
#          1 322 619
                                          
#                Accuracy : 0.6656          
#                  95% CI : (0.6393, 0.6912)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 1.465e-14       
                                          
#                   Kappa : 0.2923          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.4361          
             Specificity : 0.8445          
#          Pos Pred Value : 0.6860          
#          Neg Pred Value : 0.6578          
#              Prevalence : 0.4379          
#          Detection Rate : 0.1910          
#    Detection Prevalence : 0.2784          
#       Balanced Accuracy : 0.6403          

#CM
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
prune_sex.probs = predict(prune_sex, newdata=cm_subset)

prune_sex.pred = rep(0,831)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 315 128
#          1 197 191
                                          
#                Accuracy : 0.6089          
#                  95% CI : (0.5748, 0.6422)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.679311        
                                          
#                   Kappa : 0.2056          
#  Mcnemar's Test P-Value : 0.000162        
                                          
#             Sensitivity : 0.6152          
#             Specificity : 0.5987          
#          Pos Pred Value : 0.7111          
#          Neg Pred Value : 0.4923          
#              Prevalence : 0.6161          
#          Detection Rate : 0.3791          
#    Detection Prevalence : 0.5331          
#       Balanced Accuracy : 0.6070  

#AF
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
prune_sex.probs = predict(prune_sex, newdata=af_subset)

prune_sex.pred = rep(0,259)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 91 28
#          1 75 65
                                          
#                Accuracy : 0.6023          
#                  95% CI : (0.5399, 0.6624)
#     No Information Rate : 0.6409          
#     P-Value [Acc > NIR] : 0.9124          
                                          
#                   Kappa : 0.2224          
#  Mcnemar's Test P-Value : 5.829e-06       
                                          
#             Sensitivity : 0.5482          
#             Specificity : 0.6989          
#          Pos Pred Value : 0.7647          
#          Neg Pred Value : 0.4643          
#              Prevalence : 0.6409          
#          Detection Rate : 0.3514          
#    Detection Prevalence : 0.4595          
#       Balanced Accuracy : 0.6236         

#CF
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
prune_sex.probs = predict(prune_sex, newdata=cf_subset)

prune_sex.pred = rep(0,245)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 105  44
#          1  55  41
                                          
#                Accuracy : 0.5959          
#                  95% CI : (0.5316, 0.6579)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.9732          
                                          
#                   Kappa : 0.1345          
#  Mcnemar's Test P-Value : 0.3149          
                                          
#             Sensitivity : 0.6562          
#             Specificity : 0.4824          
#          Pos Pred Value : 0.7047          
#          Neg Pred Value : 0.4271          
#              Prevalence : 0.6531          
#          Detection Rate : 0.4286          
#    Detection Prevalence : 0.6082          
#       Balanced Accuracy : 0.5693          
                                
#Random Forest
#AM
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=am_subset)

rf_sex.pred = rep(0,1304)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 294 159
#          1 277 574
                                          
#                Accuracy : 0.6656          
#                  95% CI : (0.6393, 0.6912)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 1.465e-14       
                                          
#                   Kappa : 0.3049          
#  Mcnemar's Test P-Value : 2.103e-08       
                                          
#             Sensitivity : 0.5149          
#             Specificity : 0.7831          
#          Pos Pred Value : 0.6490          
#          Neg Pred Value : 0.6745          
#              Prevalence : 0.4379          
#          Detection Rate : 0.2255          
#    Detection Prevalence : 0.3474          
#       Balanced Accuracy : 0.6490  

#CM
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=cm_subset)

rf_sex.pred = rep(0,831)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 355 141
#          1 157 178
                                          
#                Accuracy : 0.6414          
#                  95% CI : (0.6077, 0.6741)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.07138         
                                          
#                   Kappa : 0.249           
#  Mcnemar's Test P-Value : 0.38489         
                                          
#             Sensitivity : 0.6934          
#             Specificity : 0.5580          
#          Pos Pred Value : 0.7157          
#          Neg Pred Value : 0.5313          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4272          
#    Detection Prevalence : 0.5969          
#       Balanced Accuracy : 0.6257 

#AF
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=af_subset)

rf_sex.pred = rep(0,259)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 132  43
#          1  34  50
                                         
#                Accuracy : 0.7027         
#                  95% CI : (0.643, 0.7577)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.02123        
                                         
#                   Kappa : 0.3401         
#  Mcnemar's Test P-Value : 0.36193        
                                         
#             Sensitivity : 0.7952         
#             Specificity : 0.5376         
#          Pos Pred Value : 0.7543         
#          Neg Pred Value : 0.5952         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5097         
#    Detection Prevalence : 0.6757         
#       Balanced Accuracy : 0.6664          

#CF
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=cf_subset)

rf_sex.pred = rep(0,245)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 141  50
#          1  19  35
                                          
#                Accuracy : 0.7184          
#                  95% CI : (0.6576, 0.7738)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.0175673       
                                          
#                   Kappa : 0.3204          
#  Mcnemar's Test P-Value : 0.0003043       
                                          
             Sensitivity : 0.8812          
#             Specificity : 0.4118          
#          Pos Pred Value : 0.7382          
#          Neg Pred Value : 0.6481          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5755          
#    Detection Prevalence : 0.7796          
#       Balanced Accuracy : 0.6465

#Both
#AM
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
tree_both.probs = predict(tree_both, newdata=am_subset)

tree_both.pred = rep(0,1304)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 249 114
#          1 322 619
                                          
#                Accuracy : 0.6656          
#                  95% CI : (0.6393, 0.6912)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 1.465e-14       
                                          
#                   Kappa : 0.2923          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.4361          
             Specificity : 0.8445          
#          Pos Pred Value : 0.6860          
#          Neg Pred Value : 0.6578          
#              Prevalence : 0.4379          
#          Detection Rate : 0.1910          
#    Detection Prevalence : 0.2784          
#       Balanced Accuracy : 0.6403  

#CM
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
tree_both.probs = predict(tree_both, newdata=cm_subset)

tree_both.pred = rep(0,831)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 315 128
#          1 197 191
                                          
#                Accuracy : 0.6089          
#                  95% CI : (0.5748, 0.6422)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.679311        
                                          
#                   Kappa : 0.2056          
#  Mcnemar's Test P-Value : 0.000162        
                                          
#             Sensitivity : 0.6152          
#             Specificity : 0.5987          
#          Pos Pred Value : 0.7111          
#          Neg Pred Value : 0.4923          
#              Prevalence : 0.6161          
#          Detection Rate : 0.3791          
#    Detection Prevalence : 0.5331          
#       Balanced Accuracy : 0.6070

#AF
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
tree_both.probs = predict(tree_both, newdata=af_subset)

tree_both.pred = rep(0,259)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 91 28
#          1 75 65
                                          
#                Accuracy : 0.6023          
#                  95% CI : (0.5399, 0.6624)
#     No Information Rate : 0.6409          
#     P-Value [Acc > NIR] : 0.9124          
                                          
#                   Kappa : 0.2224          
#  Mcnemar's Test P-Value : 5.829e-06       
                                          
#             Sensitivity : 0.5482          
#             Specificity : 0.6989          
#          Pos Pred Value : 0.7647          
#          Neg Pred Value : 0.4643          
#              Prevalence : 0.6409          
#          Detection Rate : 0.3514          
#    Detection Prevalence : 0.4595          
#       Balanced Accuracy : 0.6236 

#CM
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
tree_both.probs = predict(tree_both, newdata=cf_subset)

tree_both.pred = rep(0,245)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 105  44
#          1  55  41
                                          
#                Accuracy : 0.5959          
#                  95% CI : (0.5316, 0.6579)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.9732          
                                          
#                   Kappa : 0.1345          
#  Mcnemar's Test P-Value : 0.3149          
                                          
#             Sensitivity : 0.6562          
#             Specificity : 0.4824          
#          Pos Pred Value : 0.7047          
#          Neg Pred Value : 0.4271          
#              Prevalence : 0.6531          
#          Detection Rate : 0.4286          
#    Detection Prevalence : 0.6082          
#       Balanced Accuracy : 0.5693

#Pruning
#AM
set.seed(1)
cv_both.tree = cv.tree(tree_both)
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
prune_both.probs = predict(prune_both, newdata=am_subset)

prune_both.pred = rep(0,1304)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 249 114
#          1 322 619
                                          
#                Accuracy : 0.6656          
#                  95% CI : (0.6393, 0.6912)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : 1.465e-14       
                                          
#                   Kappa : 0.2923          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.4361          
             Specificity : 0.8445          
#          Pos Pred Value : 0.6860          
#          Neg Pred Value : 0.6578          
#              Prevalence : 0.4379          
#          Detection Rate : 0.1910          
#    Detection Prevalence : 0.2784          
#       Balanced Accuracy : 0.6403          

#CM
set.seed(1)
cv_both.tree = cv.tree(tree_both)
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
prune_both.probs = predict(prune_both, newdata=cm_subset)

prune_both.pred = rep(0,831)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 315 128
#          1 197 191
                                          
#                Accuracy : 0.6089          
#                  95% CI : (0.5748, 0.6422)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.679311        
                                          
#                   Kappa : 0.2056          
#  Mcnemar's Test P-Value : 0.000162        
                                          
#             Sensitivity : 0.6152          
#             Specificity : 0.5987          
#          Pos Pred Value : 0.7111          
#          Neg Pred Value : 0.4923          
#              Prevalence : 0.6161          
#          Detection Rate : 0.3791          
#    Detection Prevalence : 0.5331          
#       Balanced Accuracy : 0.6070 

#AF
set.seed(1)
cv_both.tree = cv.tree(tree_both)
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
prune_both.probs = predict(prune_both, newdata=af_subset)

prune_both.pred = rep(0,259)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction  0  1
#          0 91 28
#          1 75 65
                                          
#                Accuracy : 0.6023          
#                  95% CI : (0.5399, 0.6624)
#     No Information Rate : 0.6409          
#     P-Value [Acc > NIR] : 0.9124          
                                          
#                   Kappa : 0.2224          
#  Mcnemar's Test P-Value : 5.829e-06       
                                          
#             Sensitivity : 0.5482          
#             Specificity : 0.6989          
#          Pos Pred Value : 0.7647          
#          Neg Pred Value : 0.4643          
#              Prevalence : 0.6409          
#          Detection Rate : 0.3514          
#    Detection Prevalence : 0.4595          
#       Balanced Accuracy : 0.6236           

#CF
set.seed(1)
cv_both.tree = cv.tree(tree_both)
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
prune_both.probs = predict(prune_both, newdata=cf_subset)

prune_both.pred = rep(0,245)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 105  44
#          1  55  41
                                          
#                Accuracy : 0.5959          
#                  95% CI : (0.5316, 0.6579)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.9732          
                                          
#                   Kappa : 0.1345          
#  Mcnemar's Test P-Value : 0.3149          
                                          
#             Sensitivity : 0.6562          
#             Specificity : 0.4824          
#          Pos Pred Value : 0.7047          
#          Neg Pred Value : 0.4271          
#              Prevalence : 0.6531          
#          Detection Rate : 0.4286          
#    Detection Prevalence : 0.6082          
#       Balanced Accuracy : 0.5693  

#Random Forest
#AM
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=am_subset)

rf_both.pred = rep(0,1304)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 305 165
#          1 266 568
                                         
#                Accuracy : 0.6695         
#                  95% CI : (0.6432, 0.695)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 1.523e-15      
                                         
#                   Kappa : 0.3152         
#  Mcnemar's Test P-Value : 1.459e-06      
                                         
#             Sensitivity : 0.5342         
#             Specificity : 0.7749         
#          Pos Pred Value : 0.6489         
#          Neg Pred Value : 0.6811         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2339         
#    Detection Prevalence : 0.3604         
#       Balanced Accuracy : 0.6545  

#CM
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=cm_subset)

rf_both.pred = rep(0,831)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 376 155
#          1 136 164
                                          
#                Accuracy : 0.6498          
#                  95% CI : (0.6163, 0.6823)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.02441         
                                          
#                   Kappa : 0.2513          
#  Mcnemar's Test P-Value : 0.29134         
                                          
#             Sensitivity : 0.7344          
#             Specificity : 0.5141          
#          Pos Pred Value : 0.7081          
#          Neg Pred Value : 0.5467          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4525          
#    Detection Prevalence : 0.6390          
#       Balanced Accuracy : 0.6242          
   
#AF
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=af_subset)

rf_both.pred = rep(0,259)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 133  45
#          1  33  48
                                         
#                Accuracy : 0.6988         
#                  95% CI : (0.639, 0.7541)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.0290         
                                         
#                   Kappa : 0.3266         
#  Mcnemar's Test P-Value : 0.2129         
                                         
             Sensitivity : 0.8012         
#             Specificity : 0.5161         
#          Pos Pred Value : 0.7472         
#          Neg Pred Value : 0.5926         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5135         
#    Detection Prevalence : 0.6873         
#       Balanced Accuracy : 0.6587 

#CF
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=cf_subset)

rf_both.pred = rep(0,245)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 140  48
#          1  20  37
                                          
#                Accuracy : 0.7224          
#                  95% CI : (0.6619, 0.7776)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.012356        
                                          
#                   Kappa : 0.3363          
#  Mcnemar's Test P-Value : 0.001059        
                                          
             Sensitivity : 0.8750          
#             Specificity : 0.4353          
#          Pos Pred Value : 0.7447          
#          Neg Pred Value : 0.6491          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5714          
#    Detection Prevalence : 0.7673          
#       Balanced Accuracy : 0.6551 

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
#AM
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, am_subset, type="response")

gam.pred = rep(0,1304)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 345 196
#          1 226 537
                                          
#                Accuracy : 0.6764          
#                  95% CI : (0.6502, 0.7017)
#     No Information Rate : 0.5621          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3388          
#  Mcnemar's Test P-Value : 0.158           
                                          
#             Sensitivity : 0.6042          
#             Specificity : 0.7326          
#          Pos Pred Value : 0.6377          
#          Neg Pred Value : 0.7038          
#              Prevalence : 0.4379          
#          Detection Rate : 0.2646          
#    Detection Prevalence : 0.4149          
#       Balanced Accuracy : 0.6684    

#CM
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, cm_subset, type="response")

gam.pred = rep(0,831)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 409 175
#          1 103 144
                                          
#                Accuracy : 0.6655          
#                  95% CI : (0.6322, 0.6975)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.001797        
                                          
#                   Kappa : 0.2614          
#  Mcnemar's Test P-Value : 2.06e-05        
                                          
#             Sensitivity : 0.7988          
#             Specificity : 0.4514          
#          Pos Pred Value : 0.7003          
#          Neg Pred Value : 0.5830          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4922          
#    Detection Prevalence : 0.7028          
#       Balanced Accuracy : 0.6251  

#AF
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, af_subset, type="response")

gam.pred = rep(0,259)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 122  37
#          1  44  56
                                         
#                Accuracy : 0.6873         
#                  95% CI : (0.627, 0.7432)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.06709        
                                         
#                   Kappa : 0.3316         
#  Mcnemar's Test P-Value : 0.50499        
                                         
#             Sensitivity : 0.7349         
#             Specificity : 0.6022         
#          Pos Pred Value : 0.7673         
#          Neg Pred Value : 0.5600         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4710         
#    Detection Prevalence : 0.6139         
#       Balanced Accuracy : 0.6685     

#CF
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, cf_subset, type="response")

gam.pred = rep(0,245)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 134  53
#          1  26  32
                                          
#                Accuracy : 0.6776          
#                  95% CI : (0.6151, 0.7357)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.231179        
                                          
#                   Kappa : 0.2312          
#  Mcnemar's Test P-Value : 0.003442        
                                          
             Sensitivity : 0.8375          
#             Specificity : 0.3765          
#          Pos Pred Value : 0.7166          
#          Neg Pred Value : 0.5517          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5469          
#    Detection Prevalence : 0.7633          
#       Balanced Accuracy : 0.6070          
                                  
#Race
#AM
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, am_subset, type="response")

gam_race.pred = rep(0,1304)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 345 201
#          1 226 532
                                         
#                Accuracy : 0.6725         
#                  95% CI : (0.6463, 0.698)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 2.341e-16      
                                         
#                   Kappa : 0.3316         
#  Mcnemar's Test P-Value : 0.2455         
                                         
#             Sensitivity : 0.6042         
#             Specificity : 0.7258         
#          Pos Pred Value : 0.6319         
#          Neg Pred Value : 0.7018         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2646         
#    Detection Prevalence : 0.4187         
#       Balanced Accuracy : 0.6650 

#CM
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, cm_subset, type="response")

gam_race.pred = rep(0,831)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 402 173
#          1 110 146
                                          
#                Accuracy : 0.6594          
#                  95% CI : (0.6261, 0.6917)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.0054034       
                                          
#                   Kappa : 0.2522          
#  Mcnemar's Test P-Value : 0.0002282       
                                          
#             Sensitivity : 0.7852          
#             Specificity : 0.4577          
#          Pos Pred Value : 0.6991          
#          Neg Pred Value : 0.5703          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4838          
#    Detection Prevalence : 0.6919          
#       Balanced Accuracy : 0.6214  

#AF
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, af_subset, type="response")

gam_race.pred = rep(0,259)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 122  37
#          1  44  56
                                         
#                Accuracy : 0.6873         
#                  95% CI : (0.627, 0.7432)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.06709        
                                         
#                   Kappa : 0.3316         
#  Mcnemar's Test P-Value : 0.50499        
                                         
#             Sensitivity : 0.7349         
#             Specificity : 0.6022         
#          Pos Pred Value : 0.7673         
#          Neg Pred Value : 0.5600         
#              Prevalence : 0.6409         
#          Detection Rate : 0.4710         
#    Detection Prevalence : 0.6139         
#       Balanced Accuracy : 0.6685 

#CF
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, cf_subset, type="response")

gam_race.pred = rep(0,245)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 133  52
#          1  27  33
                                          
#                Accuracy : 0.6776          
#                  95% CI : (0.6151, 0.7357)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.23118         
                                          
#                   Kappa : 0.2357          
#  Mcnemar's Test P-Value : 0.00693         
                                          
             Sensitivity : 0.8313          
#             Specificity : 0.3882          
#          Pos Pred Value : 0.7189          
#          Neg Pred Value : 0.5500          
#              Prevalence : 0.6531          
#          Detection Rate : 0.5429          
#    Detection Prevalence : 0.7551          
#       Balanced Accuracy : 0.6097  

#Gender
#AM
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, am_subset, type="response")

gam_sex.pred = rep(0,1304)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 296 152
#          1 275 581
                                         
#                Accuracy : 0.6725         
#                  95% CI : (0.6463, 0.698)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : 2.341e-16      
                                         
#                   Kappa : 0.3186         
#  Mcnemar's Test P-Value : 3.548e-09      
                                         
#             Sensitivity : 0.5184         
#             Specificity : 0.7926         
#          Pos Pred Value : 0.6607         
#          Neg Pred Value : 0.6787         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2270         
#    Detection Prevalence : 0.3436         
#       Balanced Accuracy : 0.6555          

#CM
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, cm_subset, type="response")

gam_sex.pred = rep(0,831)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 352 143
#          1 160 176
                                          
#                Accuracy : 0.6354          
#                  95% CI : (0.6016, 0.6682)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.1343          
                                          
#                   Kappa : 0.2368          
#  Mcnemar's Test P-Value : 0.3580          
                                          
#             Sensitivity : 0.6875          
#             Specificity : 0.5517          
#          Pos Pred Value : 0.7111          
#          Neg Pred Value : 0.5238          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4236          
#    Detection Prevalence : 0.5957          
#       Balanced Accuracy : 0.6196          

#AF
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, af_subset, type="response")

gam_sex.pred = rep(0,259)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 145  56
#          1  21  37
                                         
#                Accuracy : 0.7027         
#                  95% CI : (0.643, 0.7577)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.0212330      
                                         
#                   Kappa : 0.2958         
#  Mcnemar's Test P-Value : 0.0001068      
                                         
             Sensitivity : 0.8735         
#             Specificity : 0.3978         
#          Pos Pred Value : 0.7214         
#          Neg Pred Value : 0.6379         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5598         
#    Detection Prevalence : 0.7761         
#       Balanced Accuracy : 0.6357           

#CF
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, cf_subset, type="response")

gam_sex.pred = rep(0,245)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 150  67
#          1  10  18
                                          
#                Accuracy : 0.6857          
#                  95% CI : (0.6235, 0.7433)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.157           
                                          
#                   Kappa : 0.1771          
#  Mcnemar's Test P-Value : 1.75e-10        
                                          
             Sensitivity : 0.9375          
             Specificity : 0.2118          
#          Pos Pred Value : 0.6912          
#          Neg Pred Value : 0.6429          
#              Prevalence : 0.6531          
#          Detection Rate : 0.6122          
#    Detection Prevalence : 0.8857          
#       Balanced Accuracy : 0.5746 

#Both
#AM
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, am_subset, type="response")

gam_both.pred = rep(0,1304)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=am_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 342 186
#          1 229 547
                                         
#                Accuracy : 0.6817         
#                  95% CI : (0.6557, 0.707)
#     No Information Rate : 0.5621         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3481         
#  Mcnemar's Test P-Value : 0.03924        
                                         
#             Sensitivity : 0.5989         
#             Specificity : 0.7462         
#          Pos Pred Value : 0.6477         
#          Neg Pred Value : 0.7049         
#              Prevalence : 0.4379         
#          Detection Rate : 0.2623         
#    Detection Prevalence : 0.4049         
#       Balanced Accuracy : 0.6726         

#CM
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, cm_subset, type="response")

gam_both.pred = rep(0,831)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=cm_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 385 166
#          1 127 153
                                          
#                Accuracy : 0.6474          
#                  95% CI : (0.6138, 0.6799)
#     No Information Rate : 0.6161          
#     P-Value [Acc > NIR] : 0.03394         
                                          
#                   Kappa : 0.237           
#  Mcnemar's Test P-Value : 0.02642         
                                          
#             Sensitivity : 0.7520          
#             Specificity : 0.4796          
#          Pos Pred Value : 0.6987          
#          Neg Pred Value : 0.5464          
#              Prevalence : 0.6161          
#          Detection Rate : 0.4633          
#    Detection Prevalence : 0.6631          
#       Balanced Accuracy : 0.6158  

#AF
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, af_subset, type="response")

gam_both.pred = rep(0,259)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=af_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 149  68
#          1  17  25
                                         
#                Accuracy : 0.6718         
#                  95% CI : (0.611, 0.7287)
#     No Information Rate : 0.6409         
#     P-Value [Acc > NIR] : 0.1657         
                                         
#                   Kappa : 0.1892         
#  Mcnemar's Test P-Value : 5.852e-08      
                                         
             Sensitivity : 0.8976         
             Specificity : 0.2688         
#          Pos Pred Value : 0.6866         
#          Neg Pred Value : 0.5952         
#              Prevalence : 0.6409         
#          Detection Rate : 0.5753         
#    Detection Prevalence : 0.8378         
#       Balanced Accuracy : 0.5832         

#CF
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, cf_subset, type="response")

gam_both.pred = rep(0,245)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=cf_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 152  68
#          1   8  17
                                          
#                Accuracy : 0.6898          
#                  95% CI : (0.6278, 0.7471)
#     No Information Rate : 0.6531          
#     P-Value [Acc > NIR] : 0.1265          
                                          
#                   Kappa : 0.1797          
#  Mcnemar's Test P-Value : 1.308e-11       
                                          
             Sensitivity : 0.9500          
             Specificity : 0.2000          
#          Pos Pred Value : 0.6909          
#          Neg Pred Value : 0.6800          
#              Prevalence : 0.6531          
#          Detection Rate : 0.6204          
#    Detection Prevalence : 0.8980          
#       Balanced Accuracy : 0.5750  