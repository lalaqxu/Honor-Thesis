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
v_subset = test[which(test$violent_charge=="Violent") ,]
nrow(v_subset)
#726
nv_subset = test[which(test$violent_charge=="NonViolent") ,]
nrow(nv_subset)
#1913

#MULTIPLE LINEAR REGRESSION
#Neither
#Violent
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, v_subset, type="response")
lm.pred = rep(0,726)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=v_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 382 178
#          1  56 110
                                          
#                Accuracy : 0.6777          
#                  95% CI : (0.6423, 0.7116)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 1.995e-05       
                                          
#                   Kappa : 0.274           
#  Mcnemar's Test P-Value : 2.574e-15       
                                          
             Sensitivity : 0.8721          
#             Specificity : 0.3819          
#          Pos Pred Value : 0.6821          
#          Neg Pred Value : 0.6627          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5262          
#    Detection Prevalence : 0.7713          
#       Balanced Accuracy : 0.6270    

#NonViolent
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, nv_subset, type="response")
lm.pred = rep(0,1913)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 628 283
#          1 343 659
                                          
#                Accuracy : 0.6728          
#                  95% CI : (0.6512, 0.6938)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.346           
#  Mcnemar's Test P-Value : 0.01837         
                                          
#             Sensitivity : 0.6468          
#             Specificity : 0.6996          
#          Pos Pred Value : 0.6894          
#          Neg Pred Value : 0.6577          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3283          
#    Detection Prevalence : 0.4762          
#       Balanced Accuracy : 0.6732          
                                  

#Race
#Violent
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, v_subset, type="response")
lm_race.pred = rep(0,726)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 381 178
#          1  57 110
                                          
#                Accuracy : 0.6763          
#                  95% CI : (0.6409, 0.7103)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 2.788e-05       
                                          
#                   Kappa : 0.2713          
#  Mcnemar's Test P-Value : 4.959e-15       
                                          
             Sensitivity : 0.8699          
#             Specificity : 0.3819          
#          Pos Pred Value : 0.6816          
#          Neg Pred Value : 0.6587          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5248          
#    Detection Prevalence : 0.7700          
#       Balanced Accuracy : 0.6259 

#NonViolent
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, nv_subset, type="response")
lm_race.pred = rep(0,1913)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 621 285
#          1 350 657
                                          
#                Accuracy : 0.6681          
#                  95% CI : (0.6465, 0.6892)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.3367          
#  Mcnemar's Test P-Value : 0.01109         
                                          
#             Sensitivity : 0.6395          
#             Specificity : 0.6975          
#          Pos Pred Value : 0.6854          
#          Neg Pred Value : 0.6524          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3246          
#    Detection Prevalence : 0.4736          
#       Balanced Accuracy : 0.6685

#Gender**
#Violent
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, v_subset, type="response")
lm_sex.pred = rep(0,726)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 367 151
#          1  71 137
                                          
#                Accuracy : 0.6942          
#                  95% CI : (0.6593, 0.7276)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 2.232e-07       
                                          
#                   Kappa : 0.3293          
#  Mcnemar's Test P-Value : 1.145e-07       
                                          
             Sensitivity : 0.8379          
#             Specificity : 0.4757          
#          Pos Pred Value : 0.7085          
#          Neg Pred Value : 0.6587          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5055          
#    Detection Prevalence : 0.7135          
#       Balanced Accuracy : 0.6568 

#NonViolent
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, nv_subset, type="response")
lm_sex.pred = rep(0,1913)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 576 267
#          1 395 675
                                          
#                Accuracy : 0.6539          
#                  95% CI : (0.6321, 0.6753)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3091          
#  Mcnemar's Test P-Value : 7.974e-07       
                                          
             Sensitivity : 0.5932          
             Specificity : 0.7166          
#          Pos Pred Value : 0.6833          
#          Neg Pred Value : 0.6308          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3011          
#    Detection Prevalence : 0.4407          
#       Balanced Accuracy : 0.6549  

#Both
#Violent
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, v_subset, type="response")
lm_both.pred = rep(0,726)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 386 172
#          1  52 116
                                          
#                Accuracy : 0.6915          
#                  95% CI : (0.6564, 0.7249)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 5.021e-07       
                                          
#                   Kappa : 0.3059          
#  Mcnemar's Test P-Value : 1.850e-15       
                                          
             Sensitivity : 0.8813          
#             Specificity : 0.4028          
#          Pos Pred Value : 0.6918          
#          Neg Pred Value : 0.6905          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5317          
#    Detection Prevalence : 0.7686          
#       Balanced Accuracy : 0.6420    

#NonViolent
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, nv_subset, type="response")
lm_both.pred = rep(0,1913)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 642 316
#          1 329 626
                                         
#                Accuracy : 0.6628         
#                  95% CI : (0.6412, 0.684)
#     No Information Rate : 0.5076         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3257         
#  Mcnemar's Test P-Value : 0.6366         
                                         
#             Sensitivity : 0.6612         
#             Specificity : 0.6645         
#          Pos Pred Value : 0.6701         
#          Neg Pred Value : 0.6555         
#              Prevalence : 0.5076         
#          Detection Rate : 0.3356         
#    Detection Prevalence : 0.5008         
#       Balanced Accuracy : 0.6629

#Logistic
library(ISLR)
#Neither**
#Violent
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, v_subset, type="response")
glm.pred = rep(0,726)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 374 168
#          1  64 120
                                          
#                Accuracy : 0.6804          
#                  95% CI : (0.6452, 0.7143)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 1.003e-05       
                                          
#                   Kappa : 0.2884          
#  Mcnemar's Test P-Value : 1.358e-11       
                                          
             Sensitivity : 0.8539          
#             Specificity : 0.4167          
#          Pos Pred Value : 0.6900          
#          Neg Pred Value : 0.6522          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5152          
#    Detection Prevalence : 0.7466          
#       Balanced Accuracy : 0.6353   

#NonViolent
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, nv_subset, type="response")
glm.pred = rep(0,1913)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 643 290
#          1 328 652
                                          
#                Accuracy : 0.6769          
#                  95% CI : (0.6555, 0.6979)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3541          
#  Mcnemar's Test P-Value : 0.1367          
                                          
#             Sensitivity : 0.6622          
#             Specificity : 0.6921          
#          Pos Pred Value : 0.6892          
#          Neg Pred Value : 0.6653          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3361          
#    Detection Prevalence : 0.4877          
#       Balanced Accuracy : 0.6772                 
                                 
#Race
#Violent
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, v_subset, type="response")
glm_race.pred = rep(0,726)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 369 159
#          1  69 129
                                          
#                Accuracy : 0.686           
#                  95% CI : (0.6508, 0.7196)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 2.358e-06       
                                          
#                   Kappa : 0.3068          
#  Mcnemar's Test P-Value : 3.766e-09       
                                          
             Sensitivity : 0.8425          
#             Specificity : 0.4479          
#          Pos Pred Value : 0.6989          
#          Neg Pred Value : 0.6515          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5083          
#    Detection Prevalence : 0.7273          
#       Balanced Accuracy : 0.6452                                      

#NonViolent
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, nv_subset, type="response")
glm_race.pred = rep(0,1913)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 602 264
#          1 369 678
                                          
#                Accuracy : 0.6691          
#                  95% CI : (0.6475, 0.6902)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3392          
#  Mcnemar's Test P-Value : 3.571e-05       
                                          
             Sensitivity : 0.6200          
             Specificity : 0.7197          
#          Pos Pred Value : 0.6952          
#          Neg Pred Value : 0.6476          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3147          
#    Detection Prevalence : 0.4527          
#       Balanced Accuracy : 0.6699

#Gender**
#Violent
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, v_subset, type="response")
glm_sex.pred = rep(0,726)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 382 168
#          1  56 120
                                          
#                Accuracy : 0.6915          
#                  95% CI : (0.6564, 0.7249)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 5.021e-07       
                                          
#                   Kappa : 0.3094          
#  Mcnemar's Test P-Value : 1.203e-13       
                                          
             Sensitivity : 0.8721          
#             Specificity : 0.4167          
#          Pos Pred Value : 0.6945          
#          Neg Pred Value : 0.6818          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5262          
#    Detection Prevalence : 0.7576          
#       Balanced Accuracy : 0.6444 

#NonViolent
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, nv_subset, type="response")
glm_sex.pred = rep(0,1913)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 648 307
#          1 323 635
                                          
#                Accuracy : 0.6707          
#                  95% CI : (0.6491, 0.6917)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3414          
#  Mcnemar's Test P-Value : 0.5501          
                                          
#             Sensitivity : 0.6674          
#             Specificity : 0.6741          
#          Pos Pred Value : 0.6785          
#          Neg Pred Value : 0.6628          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3387          
#    Detection Prevalence : 0.4992          
#       Balanced Accuracy : 0.6707 

#Both
#Violent
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, v_subset, type="response")
glm_both.pred = rep(0,726)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 380 167
#          1  58 121
                                         
#                Accuracy : 0.6901         
#                  95% CI : (0.655, 0.7236)
#     No Information Rate : 0.6033         
#     P-Value [Acc > NIR] : 7.460e-07      
                                         
#                   Kappa : 0.3077         
#  Mcnemar's Test P-Value : 6.021e-13      
                                         
             Sensitivity : 0.8676         
#             Specificity : 0.4201         
#          Pos Pred Value : 0.6947         
#          Neg Pred Value : 0.6760         
#              Prevalence : 0.6033         
#          Detection Rate : 0.5234         
#    Detection Prevalence : 0.7534         
#       Balanced Accuracy : 0.6439

#NonViolent
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, nv_subset, type="response")
glm_both.pred = rep(0,1913)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 724 347
#          1 359 705
                                          
#                Accuracy : 0.6693          
#                  95% CI : (0.6489, 0.6893)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3386          
#  Mcnemar's Test P-Value : 0.6789          
                                          
#             Sensitivity : 0.6685          
#             Specificity : 0.6702          
#          Pos Pred Value : 0.6760          
#          Neg Pred Value : 0.6626          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3391          
#    Detection Prevalence : 0.5016          
#       Balanced Accuracy : 0.6693 

#LDA
library(MASS)
#Neither
#Violent
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, v_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,726)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 390 190
#          1  48  98
                                          
#                Accuracy : 0.6722          
#                  95% CI : (0.6367, 0.7063)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 7.339e-05       
                                          
#                   Kappa : 0.252           
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8904          
#             Specificity : 0.3403          
#          Pos Pred Value : 0.6724          
#          Neg Pred Value : 0.6712          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5372          
#    Detection Prevalence : 0.7989          
#       Balanced Accuracy : 0.6153 

#NonViolent
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, nv_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,1913)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 665 319
#          1 306 623
                                          
#                Accuracy : 0.6733          
#                  95% CI : (0.6518, 0.6943)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3463          
#  Mcnemar's Test P-Value : 0.6312          
                                          
#             Sensitivity : 0.6849          
#             Specificity : 0.6614          
#          Pos Pred Value : 0.6758          
#          Neg Pred Value : 0.6706          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3476          
#    Detection Prevalence : 0.5144          
#       Balanced Accuracy : 0.6731 
      
#Race
#Violent
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, v_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,726)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 390 189
#          1  48  99
                                          
#                Accuracy : 0.6736          
#                  95% CI : (0.6381, 0.7076)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 5.347e-05       
                                          
#                   Kappa : 0.2556          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8904          
#             Specificity : 0.3438          
#          Pos Pred Value : 0.6736          
#          Neg Pred Value : 0.6735          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5372          
#    Detection Prevalence : 0.7975          
#       Balanced Accuracy : 0.6171

#NonViolent
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, nv_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,1913)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 664 321
#          1 307 621
                                          
#                Accuracy : 0.6717          
#                  95% CI : (0.6502, 0.6927)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3431          
#  Mcnemar's Test P-Value : 0.6039          
                                          
#             Sensitivity : 0.6838          
#             Specificity : 0.6592          
#          Pos Pred Value : 0.6741          
#          Neg Pred Value : 0.6692          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3471          
#    Detection Prevalence : 0.5149          
#       Balanced Accuracy : 0.6715 

#Gender**
#Violent
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, v_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,726)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 386 174
#          1  52 114
                                          
#                Accuracy : 0.6887          
#                  95% CI : (0.6536, 0.7222)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 1.102e-06       
                                          
#                   Kappa : 0.2988          
#  Mcnemar's Test P-Value : 8.361e-16       
                                          
             Sensitivity : 0.8813          
#             Specificity : 0.3958          
#          Pos Pred Value : 0.6893          
#          Neg Pred Value : 0.6867          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5317          
#    Detection Prevalence : 0.7713          
#       Balanced Accuracy : 0.6386  

#NonViolent
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, nv_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,1913)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 635 314
#          1 336 628
                                          
#                Accuracy : 0.6602          
#                  95% CI : (0.6385, 0.6814)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3205          
#  Mcnemar's Test P-Value : 0.4101          
                                          
#             Sensitivity : 0.6540          
#             Specificity : 0.6667          
#          Pos Pred Value : 0.6691          
#          Neg Pred Value : 0.6515          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3319          
#    Detection Prevalence : 0.4961          
#       Balanced Accuracy : 0.6603 

#Both
#Violent
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_both)

lda_both.probs = predict(lda_both, v_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,726)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 385 171
#          1  53 117
                                          
#                Accuracy : 0.6915          
#                  95% CI : (0.6564, 0.7249)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 5.021e-07       
                                          
#                   Kappa : 0.3068          
#  Mcnemar's Test P-Value : 5.393e-15       
                                          
             Sensitivity : 0.8790          
#             Specificity : 0.4062          
#          Pos Pred Value : 0.6924          
#          Neg Pred Value : 0.6882          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5303          
#    Detection Prevalence : 0.7658          
#       Balanced Accuracy : 0.6426          
                                           

#NonViolent
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_both)

lda_both.probs = predict(lda_both, nv_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,1913)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 642 316
#          1 329 626
                                         
#                Accuracy : 0.6628         
#                  95% CI : (0.6412, 0.684)
#     No Information Rate : 0.5076         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3257         
#  Mcnemar's Test P-Value : 0.6366         
                                         
#             Sensitivity : 0.6612         
#             Specificity : 0.6645         
#          Pos Pred Value : 0.6701         
#          Neg Pred Value : 0.6555         
#              Prevalence : 0.5076         
#          Detection Rate : 0.3356         
#    Detection Prevalence : 0.5008         
#       Balanced Accuracy : 0.6629 

#QDA
#Neither
#Violent
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, v_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,726)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 408 228
#          1  30  60
                                          
#                Accuracy : 0.6446          
#                  95% CI : (0.6086, 0.6795)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.01223         
                                          
#                   Kappa : 0.1585          
#  Mcnemar's Test P-Value : < 2e-16         
                                          
             Sensitivity : 0.9315          
             Specificity : 0.2083          
#          Pos Pred Value : 0.6415          
#          Neg Pred Value : 0.6667          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5620          
#    Detection Prevalence : 0.8760          
#       Balanced Accuracy : 0.5699          
                                
#NonViolent
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, nv_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,1913)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 853 606
#          1 118 336
                                          
#                Accuracy : 0.6215          
#                  95% CI : (0.5994, 0.6433)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.237           
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8785          
#             Specificity : 0.3567          
#          Pos Pred Value : 0.5846          
#          Neg Pred Value : 0.7401          
#              Prevalence : 0.5076          
#          Detection Rate : 0.4459          
#    Detection Prevalence : 0.7627          
#       Balanced Accuracy : 0.6176 

#Race
#Violent
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, v_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,726)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=v_subset$two_year_recid)
#Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 408 228
#          1  30  60
                                          
#                Accuracy : 0.6446          
#                  95% CI : (0.6086, 0.6795)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.01223         
                                          
#                   Kappa : 0.1585          
#  Mcnemar's Test P-Value : < 2e-16         
                                          
             Sensitivity : 0.9315          
             Specificity : 0.2083          
#          Pos Pred Value : 0.6415          
#          Neg Pred Value : 0.6667          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5620          
#    Detection Prevalence : 0.8760          
#       Balanced Accuracy : 0.5699          
                                 
#NonViolent
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, nv_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,1913)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 852 605
#          1 119 337
                                          
#                Accuracy : 0.6215          
#                  95% CI : (0.5994, 0.6433)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.237           
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8774          
#             Specificity : 0.3577          
#          Pos Pred Value : 0.5848          
#          Neg Pred Value : 0.7390          
#              Prevalence : 0.5076          
#          Detection Rate : 0.4454          
#    Detection Prevalence : 0.7616          
#       Balanced Accuracy : 0.6176  

#Gender
#Violent
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, v_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,726)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 409 225
#          1  29  63
                                          
#                Accuracy : 0.6501          
#                  95% CI : (0.6142, 0.6848)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.005269        
                                          
#                   Kappa : 0.1727          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.9338          
             Specificity : 0.2188          
#          Pos Pred Value : 0.6451          
#          Neg Pred Value : 0.6848          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5634          
#    Detection Prevalence : 0.8733          
#       Balanced Accuracy : 0.5763

#NonViolent
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, nv_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,1913)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 851 597
#          1 120 345
                                          
#                Accuracy : 0.6252          
#                  95% CI : (0.6031, 0.6469)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2445          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8764          
#             Specificity : 0.3662          
#          Pos Pred Value : 0.5877          
#          Neg Pred Value : 0.7419          
#              Prevalence : 0.5076          
#          Detection Rate : 0.4449          
#    Detection Prevalence : 0.7569          
#       Balanced Accuracy : 0.6213          
                                 
#Both
#Violent
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, v_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,726)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 409 222
#          1  29  66
                                          
#                Accuracy : 0.6543          
#                  95% CI : (0.6184, 0.6889)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.002643        
                                          
#                   Kappa : 0.1841          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.9338          
             Specificity : 0.2292          
#          Pos Pred Value : 0.6482          
#          Neg Pred Value : 0.6947          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5634          
#    Detection Prevalence : 0.8691          
#       Balanced Accuracy : 0.5815  

#NonViolent
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, nv_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,1913)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 851 595
#          1 120 347
                                         
#                Accuracy : 0.6262         
#                  95% CI : (0.6041, 0.648)
#     No Information Rate : 0.5076         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2466         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
             Sensitivity : 0.8764         
#             Specificity : 0.3684         
#          Pos Pred Value : 0.5885         
#          Neg Pred Value : 0.7430         
#              Prevalence : 0.5076         
#          Detection Rate : 0.4449         
#    Detection Prevalence : 0.7559         
#       Balanced Accuracy : 0.6224  

#KNN
library(class)
attach(df_race)
#Neither
#Violent
train.y = two_year_recid[n.train]
test.y = v_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(v_subset$age, v_subset$juv_fel_misd, v_subset$priors_count, v_subset$crime_factor, v_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(v_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 355 152
#          1  83 136
                                          
#                Accuracy : 0.6763          
#                  95% CI : (0.6409, 0.7103)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 2.788e-05       
                                          
#                   Kappa : 0.2948          
#  Mcnemar's Test P-Value : 9.172e-06       
                                          
             Sensitivity : 0.8105          
#             Specificity : 0.4722          
#          Pos Pred Value : 0.7002          
#          Neg Pred Value : 0.6210          
#              Prevalence : 0.6033          
#          Detection Rate : 0.4890          
#    Detection Prevalence : 0.6983          
#       Balanced Accuracy : 0.6414 

#NonViolent
train.y = two_year_recid[n.train]
test.y = nv_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(nv_subset$age, nv_subset$juv_fel_misd, nv_subset$priors_count, nv_subset$crime_factor, nv_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(nv_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 718 401
#          1 253 541
                                          
#                Accuracy : 0.6581          
#                  95% CI : (0.6364, 0.6794)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3145          
#  Mcnemar's Test P-Value : 9.022e-09       
                                          
#             Sensitivity : 0.7394          
#             Specificity : 0.5743          
#          Pos Pred Value : 0.6416          
#          Neg Pred Value : 0.6814          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3753          
#    Detection Prevalence : 0.5849          
#       Balanced Accuracy : 0.6569  

#Race
#Violent
train.y = two_year_recid[n.train]
test.y = v_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(v_subset$age, v_subset$juv_fel_misd, v_subset$priors_count, v_subset$crime_factor, v_subset$violent_charge, v_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(v_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 359 155
#          1  79 133
                                          
#                Accuracy : 0.6777          
#                  95% CI : (0.6423, 0.7116)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 1.995e-05       
                                          
#                   Kappa : 0.2948          
#  Mcnemar's Test P-Value : 9.443e-07       
                                          
             Sensitivity : 0.8196          
#             Specificity : 0.4618          
#          Pos Pred Value : 0.6984          
#          Neg Pred Value : 0.6274          
#              Prevalence : 0.6033          
#          Detection Rate : 0.4945          
#    Detection Prevalence : 0.7080          
#       Balanced Accuracy : 0.6407 

#NonViolent
train.y = two_year_recid[n.train]
test.y = nv_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(nv_subset$age, nv_subset$juv_fel_misd, nv_subset$priors_count, nv_subset$crime_factor, nv_subset$violent_charge, nv_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(nv_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 726 386
#          1 245 556
                                          
#                Accuracy : 0.6702          
#                  95% CI : (0.6486, 0.6912)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3387          
#  Mcnemar's Test P-Value : 2.499e-08       
                                          
#             Sensitivity : 0.7477          
#             Specificity : 0.5902          
#          Pos Pred Value : 0.6529          
#          Neg Pred Value : 0.6941          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3795          
#    Detection Prevalence : 0.5813          
#       Balanced Accuracy : 0.6690   

#Gender
#Violent
train.y = two_year_recid[n.train]
test.y = v_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(v_subset$age, v_subset$juv_fel_misd, v_subset$priors_count, v_subset$crime_factor, v_subset$violent_charge, v_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(v_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 362 155
#          1  76 133
                                          
#                Accuracy : 0.6818          
#                  95% CI : (0.6466, 0.7156)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 7.048e-06       
                                          
#                   Kappa : 0.3025          
#  Mcnemar's Test P-Value : 2.866e-07       
                                          
             Sensitivity : 0.8265          
#             Specificity : 0.4618          
#          Pos Pred Value : 0.7002          
#          Neg Pred Value : 0.6364          
#              Prevalence : 0.6033          
#          Detection Rate : 0.4986          
#    Detection Prevalence : 0.7121          
#       Balanced Accuracy : 0.6441 

#NonViolent
train.y = two_year_recid[n.train]
test.y = nv_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(nv_subset$age, nv_subset$juv_fel_misd, nv_subset$priors_count, nv_subset$crime_factor, nv_subset$violent_charge, nv_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(nv_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 718 382
#          1 253 560
                                          
#                Accuracy : 0.6681          
#                  95% CI : (0.6465, 0.6892)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3346          
#  Mcnemar's Test P-Value : 3.784e-07       
                                          
#             Sensitivity : 0.7394          
#             Specificity : 0.5945          
#          Pos Pred Value : 0.6527          
#          Neg Pred Value : 0.6888          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3753          
#    Detection Prevalence : 0.5750          
#       Balanced Accuracy : 0.6670          
                                 

#Both**
#Violent
train.y = two_year_recid[n.train]
test.y = v_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(v_subset$age, v_subset$juv_fel_misd, v_subset$priors_count, v_subset$crime_factor, v_subset$violent_charge, v_subset$race_factor, v_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(v_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 361 152
#          1  77 136
                                          
#                Accuracy : 0.6846          
#                  95% CI : (0.6494, 0.7183)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 3.417e-06       
                                          
#                   Kappa : 0.3103          
#  Mcnemar's Test P-Value : 1.008e-06       
                                          
             Sensitivity : 0.8242          
#             Specificity : 0.4722          
#          Pos Pred Value : 0.7037          
#          Neg Pred Value : 0.6385          
#              Prevalence : 0.6033          
#          Detection Rate : 0.4972          
#    Detection Prevalence : 0.7066          
#       Balanced Accuracy : 0.6482

#NonViolent
train.y = two_year_recid[n.train]
test.y = nv_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(nv_subset$age, nv_subset$juv_fel_misd, nv_subset$priors_count, nv_subset$crime_factor, nv_subset$violent_charge, nv_subset$race_factor, nv_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(nv_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 719 375
#          1 252 567
                                          
#                Accuracy : 0.6722          
#                  95% CI : (0.6507, 0.6933)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3431          
#  Mcnemar's Test P-Value : 1.104e-06       
                                          
#             Sensitivity : 0.7405          
#             Specificity : 0.6019          
#          Pos Pred Value : 0.6572          
#          Neg Pred Value : 0.6923          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3758          
#    Detection Prevalence : 0.5719          
#       Balanced Accuracy : 0.6712   

#Tree
library(tree)
set.seed(1)
n <- nrow(df_race)
n.train = sample(1:n, n*.5)
n.test = (-n.train)
train = df_race[n.train,]
test = df_race[n.test,]

#Neither
#Violent<---------------------
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
tree.probs = predict(tree, newdata=v_subset)

tree.pred = rep(0,726)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 274  89
#          1 164 199
                                          
#                Accuracy : 0.6515          
#                  95% CI : (0.6156, 0.6862)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.00421         
                                          
#                   Kappa : 0.303           
#  Mcnemar's Test P-Value : 3.282e-06       
                                          
#             Sensitivity : 0.6256          
#             Specificity : 0.6910          
#          Pos Pred Value : 0.7548          
#          Neg Pred Value : 0.5482          
#              Prevalence : 0.6033          
#          Detection Rate : 0.3774          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6583

#NonViolent
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
tree.probs = predict(tree, newdata=nv_subset)

tree.pred = rep(0,1913)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 486 225
#          1 485 717
                                          
#                Accuracy : 0.6289          
#                  95% CI : (0.6068, 0.6506)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2606          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5005          
#             Specificity : 0.7611          
#          Pos Pred Value : 0.6835          
#          Neg Pred Value : 0.5965          
#              Prevalence : 0.5076          
#          Detection Rate : 0.2541          
#    Detection Prevalence : 0.3717          
#       Balanced Accuracy : 0.6308

#Pruning
#Violent
set.seed(1)
cv.tree = cv.tree(tree)
tree.min = cv.tree$size[which.min(cv.tree$dev)]
prune = prune.tree(tree, best=5)
summary(prune)
prune.probs = predict(prune, newdata=v_subset)

prune.pred = rep(0,nrow(v_subset))
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 274  89
#          1 164 199
                                          
#                Accuracy : 0.6515          
#                  95% CI : (0.6156, 0.6862)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.00421         
                                          
#                   Kappa : 0.303           
#  Mcnemar's Test P-Value : 3.282e-06       
                                          
#             Sensitivity : 0.6256          
#             Specificity : 0.6910          
#          Pos Pred Value : 0.7548          
#          Neg Pred Value : 0.5482          
#              Prevalence : 0.6033          
#          Detection Rate : 0.3774          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6583          

#NonViolent                                    
set.seed(1)
cv.tree = cv.tree(tree)
tree.min = cv.tree$size[which.min(cv.tree$dev)]
prune = prune.tree(tree, best=5)
summary(prune)
prune.probs = predict(prune, newdata=nv_subset)

prune.pred = rep(0, 1913)
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 486 225
#          1 485 717
                                          
#                Accuracy : 0.6289          
#                  95% CI : (0.6068, 0.6506)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2606          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5005          
#             Specificity : 0.7611          
#          Pos Pred Value : 0.6835          
#          Neg Pred Value : 0.5965          
#              Prevalence : 0.5076          
#          Detection Rate : 0.2541          
#    Detection Prevalence : 0.3717          
#       Balanced Accuracy : 0.6308      

#Random Forest
#Violent
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=v_subset)

rf.pred = rep(0,726)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 355 157
#          1  83 131
                                          
#                Accuracy : 0.6694          
#                  95% CI : (0.6339, 0.7036)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.0001358       
                                          
#                   Kappa : 0.2776          
#  Mcnemar's Test P-Value : 2.451e-06       
                                          
             Sensitivity : 0.8105          
#             Specificity : 0.4549          
#          Pos Pred Value : 0.6934          
#          Neg Pred Value : 0.6121          
#              Prevalence : 0.6033          
#          Detection Rate : 0.4890          
#    Detection Prevalence : 0.7052          
#       Balanced Accuracy : 0.6327    

#NonViolent
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=nv_subset)

rf.pred = rep(0,1913)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 615 280
#          1 356 662
                                          
#                Accuracy : 0.6675          
#                  95% CI : (0.6459, 0.6886)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.3357          
#  Mcnemar's Test P-Value : 0.00294         
                                          
#             Sensitivity : 0.6334          
#             Specificity : 0.7028          
#          Pos Pred Value : 0.6872          
#          Neg Pred Value : 0.6503          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3215          
#    Detection Prevalence : 0.4679          
#       Balanced Accuracy : 0.6681          
                                 

#Race
#Violent
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
tree_race.probs = predict(tree_race, newdata=v_subset)

tree_race.pred = rep(0,726)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 274  89
#          1 164 199
                                          
#                Accuracy : 0.6515          
#                  95% CI : (0.6156, 0.6862)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.00421         
                                          
#                   Kappa : 0.303           
#  Mcnemar's Test P-Value : 3.282e-06       
                                          
#             Sensitivity : 0.6256          
#             Specificity : 0.6910          
#          Pos Pred Value : 0.7548          
#          Neg Pred Value : 0.5482          
#              Prevalence : 0.6033          
#          Detection Rate : 0.3774          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6583          
                                 

#NonViolent
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
tree_race.probs = predict(tree_race, newdata=nv_subset)

tree_race.pred = rep(0,1913)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 486 225
#          1 485 717
                                          
#                Accuracy : 0.6289          
#                  95% CI : (0.6068, 0.6506)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2606          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5005          
#             Specificity : 0.7611          
#          Pos Pred Value : 0.6835          
#          Neg Pred Value : 0.5965          
#              Prevalence : 0.5076          
#          Detection Rate : 0.2541          
#    Detection Prevalence : 0.3717          
#       Balanced Accuracy : 0.6308  

#Pruning
#Violent
set.seed(1)
cv_race.tree = cv.tree(tree_race)
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
prune_race.probs = predict(prune_race, newdata=v_subset)

prune_race.pred = rep(0,726)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 274  89
#          1 164 199
                                          
#                Accuracy : 0.6515          
#                  95% CI : (0.6156, 0.6862)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.00421         
                                          
#                   Kappa : 0.303           
#  Mcnemar's Test P-Value : 3.282e-06       
                                          
#             Sensitivity : 0.6256          
#             Specificity : 0.6910          
#          Pos Pred Value : 0.7548          
#          Neg Pred Value : 0.5482          
#              Prevalence : 0.6033          
#          Detection Rate : 0.3774          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6583 

#NonViolent
set.seed(1)
cv_race.tree = cv.tree(tree_race)
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
prune_race.probs = predict(prune_race, newdata=nv_subset)

prune_race.pred = rep(0,1913)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 486 225
#          1 485 717
                                          
#                Accuracy : 0.6289          
#                  95% CI : (0.6068, 0.6506)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2606          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5005          
#             Specificity : 0.7611          
#          Pos Pred Value : 0.6835          
#          Neg Pred Value : 0.5965          
#              Prevalence : 0.5076          
#          Detection Rate : 0.2541          
#    Detection Prevalence : 0.3717          
#       Balanced Accuracy : 0.6308 

#Random Forest
#Violent
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=v_subset)

rf_race.pred = rep(0,726)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 330 129
#          1 108 159
                                          
#                Accuracy : 0.6736          
#                  95% CI : (0.6381, 0.7076)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 5.347e-05       
                                          
#                   Kappa : 0.3094          
#  Mcnemar's Test P-Value : 0.1939          
                                          
#             Sensitivity : 0.7534          
#             Specificity : 0.5521          
#          Pos Pred Value : 0.7190          
#          Neg Pred Value : 0.5955          
#              Prevalence : 0.6033          
#          Detection Rate : 0.4545          
#    Detection Prevalence : 0.6322          
#       Balanced Accuracy : 0.6528        
                                 
#NonViolent
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=nv_subset)

rf_race.pred = rep(0,1913)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 609 289
#          1 362 653
                                         
#                Accuracy : 0.6597         
#                  95% CI : (0.638, 0.6809)
#     No Information Rate : 0.5076         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.32           
#  Mcnemar's Test P-Value : 0.004774       
                                         
#             Sensitivity : 0.6272         
#             Specificity : 0.6932         
#          Pos Pred Value : 0.6782         
#          Neg Pred Value : 0.6433         
#              Prevalence : 0.5076         
#          Detection Rate : 0.3183         
#    Detection Prevalence : 0.4694         
#       Balanced Accuracy : 0.6602   

#Gender
#Violent
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
tree_sex.probs = predict(tree_sex, newdata=v_subset)

tree_sex.pred = rep(0,726)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 274  89
#          1 164 199
                                          
#                Accuracy : 0.6515          
#                  95% CI : (0.6156, 0.6862)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.00421         
                                          
#                   Kappa : 0.303           
#  Mcnemar's Test P-Value : 3.282e-06       
                                          
#             Sensitivity : 0.6256          
#             Specificity : 0.6910          
#          Pos Pred Value : 0.7548          
#          Neg Pred Value : 0.5482          
#              Prevalence : 0.6033          
#          Detection Rate : 0.3774          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6583 

#NonViolent
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
tree_sex.probs = predict(tree_sex, newdata=nv_subset)

tree_sex.pred = rep(0,1913)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 486 225
#          1 485 717
                                          
#                Accuracy : 0.6289          
#                  95% CI : (0.6068, 0.6506)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2606          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5005          
#             Specificity : 0.7611          
#          Pos Pred Value : 0.6835          
#          Neg Pred Value : 0.5965          
#              Prevalence : 0.5076          
#          Detection Rate : 0.2541          
#    Detection Prevalence : 0.3717          
#       Balanced Accuracy : 0.6308   

#Pruning
#Violent
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
prune_sex.probs = predict(prune_sex, newdata=v_subset)

prune_sex.pred = rep(0,726)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 274  89
#          1 164 199
                                          
#                Accuracy : 0.6515          
#                  95% CI : (0.6156, 0.6862)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.00421         
                                          
#                   Kappa : 0.303           
#  Mcnemar's Test P-Value : 3.282e-06       
                                          
#             Sensitivity : 0.6256          
#             Specificity : 0.6910          
#          Pos Pred Value : 0.7548          
#          Neg Pred Value : 0.5482          
#              Prevalence : 0.6033          
#          Detection Rate : 0.3774          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6583          

#NonViolent
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
prune_sex.probs = predict(prune_sex, newdata=nv_subset)

prune_sex.pred = rep(0,1913)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 486 225
#          1 485 717
                                          
#                Accuracy : 0.6289          
#                  95% CI : (0.6068, 0.6506)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2606          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5005          
#             Specificity : 0.7611          
#          Pos Pred Value : 0.6835          
#          Neg Pred Value : 0.5965          
#              Prevalence : 0.5076          
#          Detection Rate : 0.2541          
#    Detection Prevalence : 0.3717          
#       Balanced Accuracy : 0.6308 

#Random Forest
#Violent
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=v_subset)

rf_sex.pred = rep(0,726)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 334 127
#          1 104 161
                                          
#                Accuracy : 0.6818          
#                  95% CI : (0.6466, 0.7156)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 7.048e-06       
                                          
#                   Kappa : 0.326           
#  Mcnemar's Test P-Value : 0.1478          
                                          
#             Sensitivity : 0.7626          
#             Specificity : 0.5590          
#          Pos Pred Value : 0.7245          
#          Neg Pred Value : 0.6075          
#              Prevalence : 0.6033          
#          Detection Rate : 0.4601          
#    Detection Prevalence : 0.6350          
#       Balanced Accuracy : 0.6608

#NonViolent
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=nv_subset)

rf_sex.pred = rep(0,1913)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 588 266
#          1 383 676
                                        
#                Accuracy : 0.6607        
#                  95% CI : (0.639, 0.682)
#     No Information Rate : 0.5076        
#     P-Value [Acc > NIR] : < 2.2e-16     
                                        
#                   Kappa : 0.3226        
#  Mcnemar's Test P-Value : 5.279e-06     
                                        
#             Sensitivity : 0.6056        
#             Specificity : 0.7176        
#          Pos Pred Value : 0.6885        
#          Neg Pred Value : 0.6383        
#              Prevalence : 0.5076        
#          Detection Rate : 0.3074        
#    Detection Prevalence : 0.4464        
#       Balanced Accuracy : 0.6616   

#Both
#Violent
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
tree_both.probs = predict(tree_both, newdata=v_subset)

tree_both.pred = rep(0,726)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 274  89
#          1 164 199
                                          
#                Accuracy : 0.6515          
#                  95% CI : (0.6156, 0.6862)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.00421         
                                          
#                   Kappa : 0.303           
#  Mcnemar's Test P-Value : 3.282e-06       
                                          
#             Sensitivity : 0.6256          
#             Specificity : 0.6910          
#          Pos Pred Value : 0.7548          
#          Neg Pred Value : 0.5482          
#              Prevalence : 0.6033          
#          Detection Rate : 0.3774          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6583          
                                  

#NonViolent
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
tree_both.probs = predict(tree_both, newdata=nv_subset)

tree_both.pred = rep(0,1913)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 486 225
#          1 485 717
                                          
#                Accuracy : 0.6289          
#                  95% CI : (0.6068, 0.6506)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2606          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5005          
#             Specificity : 0.7611          
#          Pos Pred Value : 0.6835          
#          Neg Pred Value : 0.5965          
#              Prevalence : 0.5076          
#          Detection Rate : 0.2541          
#    Detection Prevalence : 0.3717          
#       Balanced Accuracy : 0.6308 

#Pruning
#Violent
set.seed(1)
cv_both.tree = cv.tree(tree_both)
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
prune_both.probs = predict(prune_both, newdata=v_subset)

prune_both.pred = rep(0,726)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 274  89
#          1 164 199
                                          
#                Accuracy : 0.6515          
#                  95% CI : (0.6156, 0.6862)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 0.00421         
                                          
#                   Kappa : 0.303           
#  Mcnemar's Test P-Value : 3.282e-06       
                                          
#             Sensitivity : 0.6256          
#             Specificity : 0.6910          
#          Pos Pred Value : 0.7548          
#          Neg Pred Value : 0.5482          
#              Prevalence : 0.6033          
#          Detection Rate : 0.3774          
#    Detection Prevalence : 0.5000          
#       Balanced Accuracy : 0.6583  

#NonViolent
set.seed(1)
cv_both.tree = cv.tree(tree_both)
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
prune_both.probs = predict(prune_both, newdata=nv_subset)

prune_both.pred = rep(0,1913)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 486 225
#          1 485 717
                                          
#                Accuracy : 0.6289          
#                  95% CI : (0.6068, 0.6506)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2606          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5005          
#             Specificity : 0.7611          
#          Pos Pred Value : 0.6835          
#          Neg Pred Value : 0.5965          
#              Prevalence : 0.5076          
#          Detection Rate : 0.2541          
#    Detection Prevalence : 0.3717          
#       Balanced Accuracy : 0.6308 

#Random Forest
#Violent
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=v_subset)

rf_both.pred = rep(0,726)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 337 127
#          1 101 161
                                          
#                Accuracy : 0.686           
#                  95% CI : (0.6508, 0.7196)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 2.358e-06       
                                          
#                   Kappa : 0.3336          
#  Mcnemar's Test P-Value : 0.09779         
                                          
#             Sensitivity : 0.7694          
#             Specificity : 0.5590          
#          Pos Pred Value : 0.7263          
#          Neg Pred Value : 0.6145          
#              Prevalence : 0.6033          
#          Detection Rate : 0.4642          
#    Detection Prevalence : 0.6391          
#       Balanced Accuracy : 0.6642 

#NonViolent
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=nv_subset)

rf_both.pred = rep(0,1913)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 617 286
#          1 354 656
                                          
#                Accuracy : 0.6654          
#                  95% CI : (0.6438, 0.6866)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3315          
#  Mcnemar's Test P-Value : 0.008087        
                                          
#             Sensitivity : 0.6354          
#             Specificity : 0.6964          
#          Pos Pred Value : 0.6833          
#          Neg Pred Value : 0.6495          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3225          
#    Detection Prevalence : 0.4720          
#       Balanced Accuracy : 0.6659

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
#Violent
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, v_subset, type="response")

gam.pred = rep(0,726)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 382 178
#          1  56 110
                                          
#                Accuracy : 0.6777          
#                  95% CI : (0.6423, 0.7116)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 1.995e-05       
                                          
#                   Kappa : 0.274           
#  Mcnemar's Test P-Value : 2.574e-15       
                                          
             Sensitivity : 0.8721          
#             Specificity : 0.3819          
#          Pos Pred Value : 0.6821          
#          Neg Pred Value : 0.6627          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5262          
#    Detection Prevalence : 0.7713          
#       Balanced Accuracy : 0.6270  

#NonViolent
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, nv_subset, type="response")

gam.pred = rep(0,1913)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 628 283
#          1 343 659
                                          
#                Accuracy : 0.6728          
#                  95% CI : (0.6512, 0.6938)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.346           
#  Mcnemar's Test P-Value : 0.01837         
                                          
#             Sensitivity : 0.6468          
#             Specificity : 0.6996          
#          Pos Pred Value : 0.6894          
#          Neg Pred Value : 0.6577          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3283          
#    Detection Prevalence : 0.4762          
#       Balanced Accuracy : 0.6732  

#Race
#Violent
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, v_subset, type="response")

gam_race.pred = rep(0,726)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 381 178
#          1  57 110
                                          
#                Accuracy : 0.6763          
#                  95% CI : (0.6409, 0.7103)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 2.788e-05       
                                          
#                   Kappa : 0.2713          
#  Mcnemar's Test P-Value : 4.959e-15       
                                          
             Sensitivity : 0.8699          
#             Specificity : 0.3819          
#          Pos Pred Value : 0.6816          
#          Neg Pred Value : 0.6587          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5248          
#    Detection Prevalence : 0.7700          
#       Balanced Accuracy : 0.6259          
                                 
#NonViolent
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, nv_subset, type="response")

gam_race.pred = rep(0,1913)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 621 285
#          1 350 657
                                          
#                Accuracy : 0.6681          
#                  95% CI : (0.6465, 0.6892)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.3367          
#  Mcnemar's Test P-Value : 0.01109         
                                          
#             Sensitivity : 0.6395          
#             Specificity : 0.6975          
#          Pos Pred Value : 0.6854          
#          Neg Pred Value : 0.6524          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3246          
#    Detection Prevalence : 0.4736          
#       Balanced Accuracy : 0.6685

#Gender
#Violent
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, v_subset, type="response")

gam_sex.pred = rep(0,726)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 367 151
#          1  71 137
                                          
#                Accuracy : 0.6942          
#                  95% CI : (0.6593, 0.7276)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 2.232e-07       
                                          
#                   Kappa : 0.3293          
#  Mcnemar's Test P-Value : 1.145e-07       
                                          
             Sensitivity : 0.8379          
#             Specificity : 0.4757          
#          Pos Pred Value : 0.7085          
#          Neg Pred Value : 0.6587          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5055          
#    Detection Prevalence : 0.7135          
#       Balanced Accuracy : 0.6568         

#NonViolent
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, nv_subset, type="response")

gam_sex.pred = rep(0,1913)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 576 267
#          1 395 675
                                          
#                Accuracy : 0.6539          
#                  95% CI : (0.6321, 0.6753)
#     No Information Rate : 0.5076          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3091          
#  Mcnemar's Test P-Value : 7.974e-07       
                                          
#             Sensitivity : 0.5932          
#             Specificity : 0.7166          
#          Pos Pred Value : 0.6833          
#          Neg Pred Value : 0.6308          
#              Prevalence : 0.5076          
#          Detection Rate : 0.3011          
#    Detection Prevalence : 0.4407          
#       Balanced Accuracy : 0.6549          
                                 
#Both
#Violent
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, v_subset, type="response")

gam_both.pred = rep(0,726)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=v_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 386 172
#          1  52 116
                                          
#                Accuracy : 0.6915          
#                  95% CI : (0.6564, 0.7249)
#     No Information Rate : 0.6033          
#     P-Value [Acc > NIR] : 5.021e-07       
                                          
#                   Kappa : 0.3059          
#  Mcnemar's Test P-Value : 1.850e-15       
                                          
             Sensitivity : 0.8813          
#             Specificity : 0.4028          
#          Pos Pred Value : 0.6918          
#          Neg Pred Value : 0.6905          
#              Prevalence : 0.6033          
#          Detection Rate : 0.5317          
#    Detection Prevalence : 0.7686          
#       Balanced Accuracy : 0.6420 

#NonViolent
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, nv_subset, type="response")

gam_both.pred = rep(0,1913)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=nv_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 642 316
#          1 329 626
                                         
#                Accuracy : 0.6628         
#                  95% CI : (0.6412, 0.684)
#     No Information Rate : 0.5076         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3257         
#  Mcnemar's Test P-Value : 0.6366         
                                         
#             Sensitivity : 0.6612         
#             Specificity : 0.6645         
#          Pos Pred Value : 0.6701         
#          Neg Pred Value : 0.6555         
#              Prevalence : 0.5076         
#          Detection Rate : 0.3356         
#    Detection Prevalence : 0.5008         
#       Balanced Accuracy : 0.6629

