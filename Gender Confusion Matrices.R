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
f_subset = test[which(test$gender_factor=="Female") ,]
nrow(f_subset)
#504
m_subset = test[which(test$gender_factor=="Male") ,]
nrow(m_subset)
#2135

#MULTIPLE LINEAR REGRESSION
#Neither
#Female
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, f_subset, type="response")
lm.pred = rep(0,504)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=f_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 256  90
#          1  70  88
                                         
#                Accuracy : 0.6825         
#                  95% CI : (0.6399, 0.723)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.05059        
                                         
#                   Kappa : 0.287          
#  Mcnemar's Test P-Value : 0.13308        
                                         
#             Sensitivity : 0.7853         
#             Specificity : 0.4944         
#          Pos Pred Value : 0.7399         
#          Neg Pred Value : 0.5570         
#              Prevalence : 0.6468         
#          Detection Rate : 0.5079         
#    Detection Prevalence : 0.6865         
#       Balanced Accuracy : 0.6398    

#Male
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, m_subset, type="response")
lm.pred = rep(0,2135)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 754 371
#          1 329 681
                                         
#                Accuracy : 0.6721         
#                  95% CI : (0.6518, 0.692)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3437         
#  Mcnemar's Test P-Value : 0.1212         
                                         
#             Sensitivity : 0.6962         
#             Specificity : 0.6473         
#          Pos Pred Value : 0.6702         
#          Neg Pred Value : 0.6743         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3532         
#    Detection Prevalence : 0.5269         
#       Balanced Accuracy : 0.6718 

#Race
#Female
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, f_subset, type="response")
lm_race.pred = rep(0,504)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 255  89
#          1  71  89
                                         
#                Accuracy : 0.6825         
#                  95% CI : (0.6399, 0.723)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.05059        
                                         
#                   Kappa : 0.2888         
#  Mcnemar's Test P-Value : 0.17896        
                                         
#             Sensitivity : 0.7822         
#             Specificity : 0.5000         
#          Pos Pred Value : 0.7413         
#          Neg Pred Value : 0.5563         
#              Prevalence : 0.6468         
#          Detection Rate : 0.5060         
#    Detection Prevalence : 0.6825         
#       Balanced Accuracy : 0.6411

#Male
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, m_subset, type="response")
lm_race.pred = rep(0,2135)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 747 374
#          1 336 678
                                         
#                Accuracy : 0.6674         
#                  95% CI : (0.647, 0.6874)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3344         
#  Mcnemar's Test P-Value : 0.165          
                                         
#             Sensitivity : 0.6898         
#             Specificity : 0.6445         
#          Pos Pred Value : 0.6664         
#          Neg Pred Value : 0.6686         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3499         
#    Detection Prevalence : 0.5251         
#       Balanced Accuracy : 0.6671 

#Gender**
#Female
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, f_subset, type="response")
lm_sex.pred = rep(0,504)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 295 123
#          1  31  55
                                          
#                Accuracy : 0.6944          
#                  95% CI : (0.6522, 0.7344)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.01357         
                                          
#                   Kappa : 0.2423          
#  Mcnemar's Test P-Value : 2.251e-13       
                                          
             Sensitivity : 0.9049          
             Specificity : 0.3090          
#          Pos Pred Value : 0.7057          
#          Neg Pred Value : 0.6395          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5853          
#    Detection Prevalence : 0.8294          
#       Balanced Accuracy : 0.6069 

#Male
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, m_subset, type="response")
lm_sex.pred = rep(0,2135)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 648 295
#          1 435 757
                                          
#                Accuracy : 0.6581          
#                  95% CI : (0.6375, 0.6782)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3173          
#  Mcnemar's Test P-Value : 2.681e-07       
                                          
#             Sensitivity : 0.5983          
#             Specificity : 0.7196          
#          Pos Pred Value : 0.6872          
#          Neg Pred Value : 0.6351          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3035          
#    Detection Prevalence : 0.4417          
#       Balanced Accuracy : 0.6590  

#Both
#Female
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, f_subset, type="response")
lm_both.pred = rep(0,504)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 301 136
#          1  25  42
                                          
#                Accuracy : 0.6806          
#                  95% CI : (0.6379, 0.7211)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.06124         
                                          
#                   Kappa : 0.1855          
#  Mcnemar's Test P-Value : < 2e-16         
                                          
             Sensitivity : 0.9233          
             Specificity : 0.2360          
#          Pos Pred Value : 0.6888          
#          Neg Pred Value : 0.6269          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5972          
#    Detection Prevalence : 0.8671          
#       Balanced Accuracy : 0.5796    

#Male
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, m_subset, type="response")
lm_both.pred = rep(0,2135)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 727 352
#          1 356 700
                                         
#                Accuracy : 0.6684         
#                  95% CI : (0.648, 0.6883)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3367         
#  Mcnemar's Test P-Value : 0.9102         
                                         
#             Sensitivity : 0.6713         
#             Specificity : 0.6654         
#          Pos Pred Value : 0.6738         
#          Neg Pred Value : 0.6629         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3405         
#    Detection Prevalence : 0.5054         
#       Balanced Accuracy : 0.6683 

#Logistic
library(ISLR)
#Neither**
#Female
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, f_subset, type="response")
glm.pred = rep(0,504)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 258  87
#          1  68  91
                                          
#                Accuracy : 0.6925          
#                  95% CI : (0.6501, 0.7325)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.01725         
                                          
#                   Kappa : 0.3102          
#  Mcnemar's Test P-Value : 0.14823         
                                          
#             Sensitivity : 0.7914          
#             Specificity : 0.5112          
#          Pos Pred Value : 0.7478          
#          Neg Pred Value : 0.5723          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5119          
#    Detection Prevalence : 0.6845          
#       Balanced Accuracy : 0.6513  

#Male
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, m_subset, type="response")
glm.pred = rep(0,2135)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 759 371
#          1 324 681
                                          
#                Accuracy : 0.6745          
#                  95% CI : (0.6541, 0.6943)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.3484          
#  Mcnemar's Test P-Value : 0.08101         
                                          
#             Sensitivity : 0.7008          
#             Specificity : 0.6473          
#          Pos Pred Value : 0.6717          
#          Neg Pred Value : 0.6776          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3555          
#    Detection Prevalence : 0.5293          
#       Balanced Accuracy : 0.6741        
                                 
#Race
#Female
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, f_subset, type="response")
glm_race.pred = rep(0,504)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 252  82
#          1  74  96
                                          
#                Accuracy : 0.6905          
#                  95% CI : (0.6481, 0.7306)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.02175         
                                          
#                   Kappa : 0.3156          
#  Mcnemar's Test P-Value : 0.57517         
                                          
#             Sensitivity : 0.7730          
#             Specificity : 0.5393          
#          Pos Pred Value : 0.7545          
#          Neg Pred Value : 0.5647          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5000          
#    Detection Prevalence : 0.6627          
#       Balanced Accuracy : 0.6562                                      

#Male
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, m_subset, type="response")
glm_race.pred = rep(0,2135)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 719 341
#          1 364 711
                                          
#                Accuracy : 0.6698          
#                  95% CI : (0.6494, 0.6897)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3396          
#  Mcnemar's Test P-Value : 0.4073          
                                          
#             Sensitivity : 0.6639          
#             Specificity : 0.6759          
#          Pos Pred Value : 0.6783          
#          Neg Pred Value : 0.6614          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3368          
#    Detection Prevalence : 0.4965          
#       Balanced Accuracy : 0.6699 

#Gender**
#Female
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, f_subset, type="response")
glm_sex.pred = rep(0,504)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=f_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 300 128
#          1  26  50
                                          
#                Accuracy : 0.6944          
#                  95% CI : (0.6522, 0.7344)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.01357         
                                          
#                   Kappa : 0.2312          
#  Mcnemar's Test P-Value : 3.992e-16       
                                          
             Sensitivity : 0.9202          
             Specificity : 0.2809          
#          Pos Pred Value : 0.7009          
#          Neg Pred Value : 0.6579          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5952          
#    Detection Prevalence : 0.8492          
#       Balanced Accuracy : 0.6006  

#Male
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, m_subset, type="response")
glm_sex.pred = rep(0,2135)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 730 347
#          1 353 705
                                         
#                Accuracy : 0.6721         
#                  95% CI : (0.6518, 0.692)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3442         
#  Mcnemar's Test P-Value : 0.8501         
                                         
#             Sensitivity : 0.6741         
#             Specificity : 0.6702         
#          Pos Pred Value : 0.6778         
#          Neg Pred Value : 0.6664         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3419         
#    Detection Prevalence : 0.5044         
#       Balanced Accuracy : 0.6721  

#Both
#Female
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, f_subset, type="response")
glm_both.pred = rep(0,504)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 300 130
#          1  26  48
                                          
#                Accuracy : 0.6905          
#                  95% CI : (0.6481, 0.7306)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.02175         
                                          
#                   Kappa : 0.2189          
#  Mcnemar's Test P-Value : < 2e-16         
                                          
             Sensitivity : 0.9202          
             Specificity : 0.2697          
#          Pos Pred Value : 0.6977          
#          Neg Pred Value : 0.6486          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5952          
#    Detection Prevalence : 0.8532          
#       Balanced Accuracy : 0.5950 

#Male
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, m_subset, type="response")
glm_both.pred = rep(0,2135)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=m_subset$two_year_recid)
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
#Female
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, f_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,504)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 267  98
#          1  59  80
                                         
#                Accuracy : 0.6885         
#                  95% CI : (0.646, 0.7287)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.027188       
                                         
#                   Kappa : 0.2825         
#  Mcnemar's Test P-Value : 0.002424       
                                         
             Sensitivity : 0.8190         
#             Specificity : 0.4494         
#          Pos Pred Value : 0.7315         
#          Neg Pred Value : 0.5755         
#              Prevalence : 0.6468         
#          Detection Rate : 0.5298         
#    Detection Prevalence : 0.7242         
#       Balanced Accuracy : 0.6342

#Male
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, m_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,2135)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 788 411
#          1 295 641
                                          
#                Accuracy : 0.6693          
#                  95% CI : (0.6489, 0.6893)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3375          
#  Mcnemar's Test P-Value : 1.504e-05       
                                          
#             Sensitivity : 0.7276          
#             Specificity : 0.6093          
#          Pos Pred Value : 0.6572          
#          Neg Pred Value : 0.6848          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3691          
#    Detection Prevalence : 0.5616          
#       Balanced Accuracy : 0.6685 
      
#Race
#Female
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, f_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,504)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 267  97
#          1  59  81
                                          
#                Accuracy : 0.6905          
#                  95% CI : (0.6481, 0.7306)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.021748        
                                          
#                   Kappa : 0.288           
#  Mcnemar's Test P-Value : 0.003053        
                                          
             Sensitivity : 0.8190          
#             Specificity : 0.4551          
#          Pos Pred Value : 0.7335          
#          Neg Pred Value : 0.5786          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5298          
#    Detection Prevalence : 0.7222          
#       Balanced Accuracy : 0.6370  

#Male
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, m_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,2135)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 787 413
#          1 296 639
                                          
#                Accuracy : 0.6679          
#                  95% CI : (0.6475, 0.6879)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3346          
#  Mcnemar's Test P-Value : 1.322e-05       
                                          
#             Sensitivity : 0.7267          
#             Specificity : 0.6074          
#          Pos Pred Value : 0.6558          
#          Neg Pred Value : 0.6834          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3686          
#    Detection Prevalence : 0.5621          
#       Balanced Accuracy : 0.6670 

#Gender**
#Female
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, f_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,504)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 301 137
#          1  25  41
                                          
#                Accuracy : 0.6786          
#                  95% CI : (0.6358, 0.7192)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.07354         
                                          
#                   Kappa : 0.1793          
#  Mcnemar's Test P-Value : < 2e-16         
                                          
             Sensitivity : 0.9233          
             Specificity : 0.2303          
#          Pos Pred Value : 0.6872          
#          Neg Pred Value : 0.6212          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5972          
#    Detection Prevalence : 0.8690          
#       Balanced Accuracy : 0.5768  

#Male
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, m_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,2135)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 720 351
#          1 363 701
                                          
#                Accuracy : 0.6656          
#                  95% CI : (0.6451, 0.6856)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3311          
#  Mcnemar's Test P-Value : 0.6806          
                                          
#             Sensitivity : 0.6648          
#             Specificity : 0.6663          
#          Pos Pred Value : 0.6723          
#          Neg Pred Value : 0.6588          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3372          
#    Detection Prevalence : 0.5016          
#       Balanced Accuracy : 0.6656  

#Both
#Female
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, f_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,504)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=f_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 301 136
#          1  25  42
                                          
#                Accuracy : 0.6806          
#                  95% CI : (0.6379, 0.7211)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.06124         
                                          
#                   Kappa : 0.1855          
#  Mcnemar's Test P-Value : < 2e-16         
                                          
             Sensitivity : 0.9233          
             Specificity : 0.2360          
#          Pos Pred Value : 0.6888          
#          Neg Pred Value : 0.6269          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5972          
#    Detection Prevalence : 0.8671          
#       Balanced Accuracy : 0.5796          

#Male
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, m_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,2135)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 726 351
#          1 357 701
                                         
#                Accuracy : 0.6684         
#                  95% CI : (0.648, 0.6883)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3367         
#  Mcnemar's Test P-Value : 0.8509         
                                         
#             Sensitivity : 0.6704         
#             Specificity : 0.6663         
#          Pos Pred Value : 0.6741         
#          Neg Pred Value : 0.6626         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3400         
#    Detection Prevalence : 0.5044         
#       Balanced Accuracy : 0.6684 

#QDA
#Neither
#Female
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, f_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,504)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 309 145
#          1  17  33
                                          
#                Accuracy : 0.6786          
#                  95% CI : (0.6358, 0.7192)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.07354         
                                          
#                   Kappa : 0.1592          
#  Mcnemar's Test P-Value : < 2e-16         
                                          
             Sensitivity : 0.9479          
             Specificity : 0.1854          
#          Pos Pred Value : 0.6806          
#          Neg Pred Value : 0.6600          
#              Prevalence : 0.6468          
#          Detection Rate : 0.6131          
#    Detection Prevalence : 0.9008          
#       Balanced Accuracy : 0.5666         
                                
#Male
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, m_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,2135)
qda.pred[as.numeric(qda.class)==2] = 1
# confusionMatrix(qda.class, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 952 689
#          1 131 363
                                          
#                Accuracy : 0.6159          
#                  95% CI : (0.5949, 0.6366)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2258          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8790          
#             Specificity : 0.3451          
#          Pos Pred Value : 0.5801          
#          Neg Pred Value : 0.7348          
#              Prevalence : 0.5073          
#          Detection Rate : 0.4459          
#    Detection Prevalence : 0.7686          
#       Balanced Accuracy : 0.6120

#Race
#Female
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, f_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,504)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 309 143
#          1  17  35
                                         
#                Accuracy : 0.6825         
#                  95% CI : (0.6399, 0.723)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.05059        
                                         
#                   Kappa : 0.1721         
#  Mcnemar's Test P-Value : < 2e-16        
                                         
             Sensitivity : 0.9479         
             Specificity : 0.1966         
#          Pos Pred Value : 0.6836         
#          Neg Pred Value : 0.6731         
#              Prevalence : 0.6468         
#          Detection Rate : 0.6131         
#    Detection Prevalence : 0.8968         
#       Balanced Accuracy : 0.5722 

#Male
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, m_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,2135)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 951 690
#          1 132 362
                                         
#                Accuracy : 0.615          
#                  95% CI : (0.594, 0.6357)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2239         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
             Sensitivity : 0.8781         
#             Specificity : 0.3441         
#          Pos Pred Value : 0.5795         
#          Neg Pred Value : 0.7328         
#              Prevalence : 0.5073         
#          Detection Rate : 0.4454         
#    Detection Prevalence : 0.7686         
#       Balanced Accuracy : 0.6111  

#Gender
#Female
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, f_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,504)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=f_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 315 157
#          1  11  21
                                          
#                Accuracy : 0.6667          
#                  95% CI : (0.6236, 0.7077)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.1883          
                                          
#                   Kappa : 0.1035          
#  Mcnemar's Test P-Value : <2e-16          
                                          
             Sensitivity : 0.9663          
             Specificity : 0.1180          
#          Pos Pred Value : 0.6674          
#          Neg Pred Value : 0.6562          
#              Prevalence : 0.6468          
#          Detection Rate : 0.6250          
#    Detection Prevalence : 0.9365          
#       Balanced Accuracy : 0.5421 

#Male
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, m_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,2135)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 945 665
#          1 138 387
                                          
#                Accuracy : 0.6239          
#                  95% CI : (0.6029, 0.6445)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2422          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8726          
#             Specificity : 0.3679          
#          Pos Pred Value : 0.5870          
#          Neg Pred Value : 0.7371          
#              Prevalence : 0.5073          
#          Detection Rate : 0.4426          
#    Detection Prevalence : 0.7541          
#       Balanced Accuracy : 0.6202

#Both
#Female
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, f_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,504)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 315 155
#          1  11  23
                                          
#                Accuracy : 0.6706          
#                  95% CI : (0.6277, 0.7116)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.1417          
                                          
#                   Kappa : 0.1169          
#  Mcnemar's Test P-Value : <2e-16          
                                          
             Sensitivity : 0.9663          
             Specificity : 0.1292          
#          Pos Pred Value : 0.6702          
#          Neg Pred Value : 0.6765          
#              Prevalence : 0.6468          
#          Detection Rate : 0.6250          
#    Detection Prevalence : 0.9325          
#       Balanced Accuracy : 0.5477  

#Male
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, m_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,2135)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 945 662
#          1 138 390
                                          
#                Accuracy : 0.6253          
#                  95% CI : (0.6044, 0.6459)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.245           
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8726          
#             Specificity : 0.3707          
#          Pos Pred Value : 0.5881          
#          Neg Pred Value : 0.7386          
#              Prevalence : 0.5073          
#          Detection Rate : 0.4426          
#    Detection Prevalence : 0.7527          
#       Balanced Accuracy : 0.6216 

#KNN
library(class)
attach(df_race)
#Neither
#Female
train.y = two_year_recid[n.train]
test.y = f_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(f_subset$age, f_subset$juv_fel_misd, f_subset$priors_count, f_subset$crime_factor, f_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(f_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 261 101
#          1  65  77
                                          
#                Accuracy : 0.6706          
#                  95% CI : (0.6277, 0.7116)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.141748        
                                          
#                   Kappa : 0.2444          
#  Mcnemar's Test P-Value : 0.006597        
                                          
             Sensitivity : 0.8006          
#             Specificity : 0.4326          
#          Pos Pred Value : 0.7210          
#          Neg Pred Value : 0.5423          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5179          
#    Detection Prevalence : 0.7183          
#       Balanced Accuracy : 0.6166

#Male
train.y = two_year_recid[n.train]
test.y = m_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(m_subset$age, m_subset$juv_fel_misd, m_subset$priors_count, m_subset$crime_factor, m_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(m_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 812 453
#          1 271 599
                                         
#                Accuracy : 0.6609         
#                  95% CI : (0.6404, 0.681)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.32           
#  Mcnemar's Test P-Value : 1.734e-11      
                                         
#             Sensitivity : 0.7498         
#             Specificity : 0.5694         
#          Pos Pred Value : 0.6419         
#          Neg Pred Value : 0.6885         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3803         
#    Detection Prevalence : 0.5925         
#       Balanced Accuracy : 0.6596  

#Race
#Female
train.y = two_year_recid[n.train]
test.y = f_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(f_subset$age, f_subset$juv_fel_misd, f_subset$priors_count, f_subset$crime_factor, f_subset$violent_charge, f_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(f_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 264  96
#          1  62  82
                                         
#                Accuracy : 0.6865         
#                  95% CI : (0.644, 0.7268)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.033712       
                                         
#                   Kappa : 0.2827         
#  Mcnemar's Test P-Value : 0.008656       
                                         
             Sensitivity : 0.8098         
#             Specificity : 0.4607         
#          Pos Pred Value : 0.7333         
#          Neg Pred Value : 0.5694         
#              Prevalence : 0.6468         
#          Detection Rate : 0.5238         
#    Detection Prevalence : 0.7143         
#       Balanced Accuracy : 0.6352

#Male
train.y = two_year_recid[n.train]
test.y = m_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(m_subset$age, m_subset$juv_fel_misd, m_subset$priors_count, m_subset$crime_factor, m_subset$violent_charge, m_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(m_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 819 446
#          1 264 606
                                         
#                Accuracy : 0.6674         
#                  95% CI : (0.647, 0.6874)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3331         
#  Mcnemar's Test P-Value : 1.1e-11        
                                         
#             Sensitivity : 0.7562         
#             Specificity : 0.5760         
#          Pos Pred Value : 0.6474         
#          Neg Pred Value : 0.6966         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3836         
#    Detection Prevalence : 0.5925         
#       Balanced Accuracy : 0.6661 

#Gender
#Female
train.y = two_year_recid[n.train]
test.y = f_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(f_subset$age, f_subset$juv_fel_misd, f_subset$priors_count, f_subset$crime_factor, f_subset$violent_charge, f_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(f_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 269 101
#          1  57  77
                                         
#                Accuracy : 0.6865         
#                  95% CI : (0.644, 0.7268)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.0337120      
                                         
#                   Kappa : 0.2731         
#  Mcnemar's Test P-Value : 0.0006242      
                                         
             Sensitivity : 0.8252         
#             Specificity : 0.4326         
#          Pos Pred Value : 0.7270         
#          Neg Pred Value : 0.5746         
#              Prevalence : 0.6468         
#          Detection Rate : 0.5337         
#    Detection Prevalence : 0.7341         
#       Balanced Accuracy : 0.6289

#Male
train.y = two_year_recid[n.train]
test.y = m_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(m_subset$age, m_subset$juv_fel_misd, m_subset$priors_count, m_subset$crime_factor, m_subset$violent_charge, m_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(m_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 812 434
#          1 271 618
                                          
#                Accuracy : 0.6698          
#                  95% CI : (0.6494, 0.6897)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.338           
#  Mcnemar's Test P-Value : 1.052e-09       
                                          
#             Sensitivity : 0.7498          
#             Specificity : 0.5875          
#          Pos Pred Value : 0.6517          
#          Neg Pred Value : 0.6952          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3803          
#    Detection Prevalence : 0.5836          
#       Balanced Accuracy : 0.6686 

#Both**
#Female
train.y = two_year_recid[n.train]
test.y = f_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(f_subset$age, f_subset$juv_fel_misd, f_subset$priors_count, f_subset$crime_factor, f_subset$violent_charge, f_subset$race_factor, f_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(f_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 270 100
#          1  56  78
                                          
#                Accuracy : 0.6905          
#                  95% CI : (0.6481, 0.7306)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.0217480       
                                          
#                   Kappa : 0.2823          
#  Mcnemar's Test P-Value : 0.0005758       
                                          
             Sensitivity : 0.8282          
#             Specificity : 0.4382          
#          Pos Pred Value : 0.7297          
#          Neg Pred Value : 0.5821          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5357          
#    Detection Prevalence : 0.7341          
#       Balanced Accuracy : 0.6332 

#Male
train.y = two_year_recid[n.train]
test.y = m_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(m_subset$age, m_subset$juv_fel_misd, m_subset$priors_count, m_subset$crime_factor, m_subset$violent_charge, m_subset$race_factor, m_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(m_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 811 426
#          1 272 626
                                         
#                Accuracy : 0.6731         
#                  95% CI : (0.6527, 0.693)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.3446         
#  Mcnemar's Test P-Value : 6.991e-09      
                                         
#             Sensitivity : 0.7488         
#             Specificity : 0.5951         
#          Pos Pred Value : 0.6556         
#          Neg Pred Value : 0.6971         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3799         
#    Detection Prevalence : 0.5794         
#       Balanced Accuracy : 0.6720 

#Tree
library(tree)
set.seed(1)
n <- nrow(df_race)
n.train = sample(1:n, n*.5)
n.test = (-n.train)
train = df_race[n.train,]
test = df_race[n.test,]

#Neither
#Female
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
plot(tree)
text(tree, pretty=0)
tree.probs = predict(tree, newdata=f_subset)

tree.pred = rep(0,504)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 196  72
#          1 130 106
                                         
#                Accuracy : 0.5992         
#                  95% CI : (0.555, 0.6423)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.9883         
                                         
#                   Kappa : 0.1832         
#  Mcnemar's Test P-Value : 6.059e-05      
                                         
#             Sensitivity : 0.6012         
#             Specificity : 0.5955         
#          Pos Pred Value : 0.7313         
#          Neg Pred Value : 0.4492         
#              Prevalence : 0.6468         
#          Detection Rate : 0.3889         
#    Detection Prevalence : 0.5317         
#       Balanced Accuracy : 0.5984 

#Male
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
plot(tree)
text(tree, pretty=0)
tree.probs = predict(tree, newdata=m_subset)

tree.pred = rep(0,2135)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 564 242
#          1 519 810
                                          
#                Accuracy : 0.6436          
#                  95% CI : (0.6228, 0.6639)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2896          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5208          
#             Specificity : 0.7700          
#          Pos Pred Value : 0.6998          
#          Neg Pred Value : 0.6095          
#              Prevalence : 0.5073          
#          Detection Rate : 0.2642          
#    Detection Prevalence : 0.3775          
#       Balanced Accuracy : 0.6454

#Pruning
#Female
set.seed(1)
cv.tree = cv.tree(tree)
plot(cv.tree$k,cv.tree$dev,type="b")
plot(cv.tree$size, cv.tree$dev, type="b")
tree.min = cv.tree$size[which.min(cv.tree$dev)]
points(tree.min, min(cv.tree$dev), col="red", cex=2, pch=20)
prune = prune.tree(tree, best=5)
summary(prune)
plot(prune)
text(prune,pretty=0)
prune.probs = predict(prune, newdata=f_subset)

prune.pred = rep(0,nrow(f_subset))
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 196  72
#          1 130 106
                                         
#                Accuracy : 0.5992         
#                  95% CI : (0.555, 0.6423)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.9883         
                                         
#                   Kappa : 0.1832         
#  Mcnemar's Test P-Value : 6.059e-05      
                                         
#             Sensitivity : 0.6012         
#             Specificity : 0.5955         
#          Pos Pred Value : 0.7313         
#          Neg Pred Value : 0.4492         
#              Prevalence : 0.6468         
#          Detection Rate : 0.3889         
#    Detection Prevalence : 0.5317         
#       Balanced Accuracy : 0.5984         

#Male                                    
set.seed(1)
cv.tree = cv.tree(tree)
plot(cv.tree$k,cv.tree$dev,type="b")
plot(cv.tree$size, cv.tree$dev, type="b")
tree.min = cv.tree$size[which.min(cv.tree$dev)]
points(tree.min, min(cv.tree$dev), col="red", cex=2, pch=20)
prune = prune.tree(tree, best=5)
summary(prune)
plot(prune)
text(prune,pretty=0)
prune.probs = predict(prune, newdata=m_subset)

prune.pred = rep(0, 2135)
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 564 242
#          1 519 810
                                          
#                Accuracy : 0.6436          
#                  95% CI : (0.6228, 0.6639)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2896          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5208          
#             Specificity : 0.7700          
#          Pos Pred Value : 0.6998          
#          Neg Pred Value : 0.6095          
#              Prevalence : 0.5073          
#          Detection Rate : 0.2642          
#    Detection Prevalence : 0.3775          
#       Balanced Accuracy : 0.6454 

#Random Forest
#Female
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=f_subset)

rf.pred = rep(0,504)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 251  83
#          1  75  95
                                         
#                Accuracy : 0.6865         
#                  95% CI : (0.644, 0.7268)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.03371        
                                         
#                   Kappa : 0.3068         
#  Mcnemar's Test P-Value : 0.57760        
                                         
#             Sensitivity : 0.7699         
#             Specificity : 0.5337         
#          Pos Pred Value : 0.7515         
#          Neg Pred Value : 0.5588         
#              Prevalence : 0.6468         
#          Detection Rate : 0.4980         
#    Detection Prevalence : 0.6627         
#       Balanced Accuracy : 0.6518    

#Male
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=m_subset)

rf.pred = rep(0,2135)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 719 354
#          1 364 698
                                          
#                Accuracy : 0.6637          
#                  95% CI : (0.6432, 0.6837)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3274          
#  Mcnemar's Test P-Value : 0.737           
                                          
#             Sensitivity : 0.6639          
#             Specificity : 0.6635          
#          Pos Pred Value : 0.6701          
#          Neg Pred Value : 0.6573          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3368          
#    Detection Prevalence : 0.5026          
#       Balanced Accuracy : 0.6637  

#Race
#Female
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
plot(tree_race)
text(tree_race, pretty=0)
tree_race.probs = predict(tree_race, newdata=f_subset)

tree_race.pred = rep(0,504)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 196  72
#          1 130 106
                                         
#                Accuracy : 0.5992         
#                  95% CI : (0.555, 0.6423)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.9883         
                                         
#                   Kappa : 0.1832         
#  Mcnemar's Test P-Value : 6.059e-05      
                                         
#             Sensitivity : 0.6012         
#             Specificity : 0.5955         
#          Pos Pred Value : 0.7313         
#          Neg Pred Value : 0.4492         
#              Prevalence : 0.6468         
#          Detection Rate : 0.3889         
#    Detection Prevalence : 0.5317         
#       Balanced Accuracy : 0.5984        

#Male
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
plot(tree_race)
text(tree_race, pretty=0)
tree_race.probs = predict(tree_race, newdata=m_subset)

tree_race.pred = rep(0,2135)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 564 242
#          1 519 810
                                          
#                Accuracy : 0.6436          
#                  95% CI : (0.6228, 0.6639)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2896          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5208          
#             Specificity : 0.7700          
#          Pos Pred Value : 0.6998          
#          Neg Pred Value : 0.6095          
#              Prevalence : 0.5073          
#          Detection Rate : 0.2642          
#    Detection Prevalence : 0.3775          
#       Balanced Accuracy : 0.6454 

#Pruning
#Female
set.seed(1)
cv_race.tree = cv.tree(tree_race)
plot(cv_race.tree$k,cv_race.tree$dev,type="b")
plot(cv_race.tree$size, cv_race.tree$dev, type="b")
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
points(tree_race.min, min(cv_race.tree$dev), col="red", cex=2, pch=20)
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
plot(prune_race)
text(prune_race,pretty=0)
prune_race.probs = predict(prune_race, newdata=f_subset)

prune_race.pred = rep(0,504)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=f_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 196  72
#          1 130 106
                                         
#                Accuracy : 0.5992         
#                  95% CI : (0.555, 0.6423)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.9883         
                                         
#                   Kappa : 0.1832         
#  Mcnemar's Test P-Value : 6.059e-05      
                                         
#             Sensitivity : 0.6012         
#             Specificity : 0.5955         
#          Pos Pred Value : 0.7313         
#          Neg Pred Value : 0.4492         
#              Prevalence : 0.6468         
#          Detection Rate : 0.3889         
#    Detection Prevalence : 0.5317         
#       Balanced Accuracy : 0.5984 

#Male
set.seed(1)
cv_race.tree = cv.tree(tree_race)
plot(cv_race.tree$k,cv_race.tree$dev,type="b")
plot(cv_race.tree$size, cv_race.tree$dev, type="b")
tree_race.min = cv_race.tree$size[which.min(cv_race.tree$dev)]
points(tree_race.min, min(cv_race.tree$dev), col="red", cex=2, pch=20)
prune_race = prune.tree(tree_race, best=5)
summary(prune_race)
plot(prune_race)
text(prune_race,pretty=0)
prune_race.probs = predict(prune_race, newdata=m_subset)

prune_race.pred = rep(0,2135)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 564 242
#          1 519 810
                                          
#                Accuracy : 0.6436          
#                  95% CI : (0.6228, 0.6639)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2896          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5208          
#             Specificity : 0.7700          
#          Pos Pred Value : 0.6998          
#          Neg Pred Value : 0.6095          
#              Prevalence : 0.5073          
#          Detection Rate : 0.2642          
#    Detection Prevalence : 0.3775          
#       Balanced Accuracy : 0.6454 

#Random Forest
#Female
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=f_subset)

rf_race.pred = rep(0,504)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 243  83
#          1  83  95
                                          
#                Accuracy : 0.6706          
#                  95% CI : (0.6277, 0.7116)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.1417          
                                          
#                   Kappa : 0.2791          
#  Mcnemar's Test P-Value : 1.0000          
                                          
#             Sensitivity : 0.7454          
#             Specificity : 0.5337          
#          Pos Pred Value : 0.7454          
#          Neg Pred Value : 0.5337          
#              Prevalence : 0.6468          
#          Detection Rate : 0.4821          
#    Detection Prevalence : 0.6468          
#       Balanced Accuracy : 0.6396          
                                 
#Male
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=m_subset)

rf_race.pred = rep(0,2135)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 696 335
#          1 387 717
                                          
#                Accuracy : 0.6618          
#                  95% CI : (0.6413, 0.6819)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.324           
#  Mcnemar's Test P-Value : 0.05769         
                                          
#             Sensitivity : 0.6427          
#             Specificity : 0.6816          
#          Pos Pred Value : 0.6751          
#          Neg Pred Value : 0.6495          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3260          
#    Detection Prevalence : 0.4829          
#       Balanced Accuracy : 0.6621  

#Gender
#Female
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
plot(tree_sex)
text(tree_sex, pretty=0)
tree_sex.probs = predict(tree_sex, newdata=f_subset)

tree_sex.pred = rep(0,504)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 196  72
#          1 130 106
                                         
#                Accuracy : 0.5992         
#                  95% CI : (0.555, 0.6423)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.9883         
                                         
#                   Kappa : 0.1832         
#  Mcnemar's Test P-Value : 6.059e-05      
                                         
#             Sensitivity : 0.6012         
#             Specificity : 0.5955         
#          Pos Pred Value : 0.7313         
#          Neg Pred Value : 0.4492         
#              Prevalence : 0.6468         
#          Detection Rate : 0.3889         
#    Detection Prevalence : 0.5317         
#       Balanced Accuracy : 0.5984  

#Male
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
plot(tree_sex)
text(tree_sex, pretty=0)
tree_sex.probs = predict(tree_sex, newdata=m_subset)

tree_sex.pred = rep(0,2135)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 564 242
#          1 519 810
                                          
#                Accuracy : 0.6436          
#                  95% CI : (0.6228, 0.6639)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2896          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5208          
#             Specificity : 0.7700          
#          Pos Pred Value : 0.6998          
#          Neg Pred Value : 0.6095          
#              Prevalence : 0.5073          
#          Detection Rate : 0.2642          
#    Detection Prevalence : 0.3775          
#       Balanced Accuracy : 0.6454  

#Pruning
#Female
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
plot(cv_sex.tree$k,cv_sex.tree$dev,type="b")
plot(cv_sex.tree$size, cv_sex.tree$dev, type="b")
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
points(tree_sex.min, min(cv_sex.tree$dev), col="red", cex=2, pch=20)
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
plot(prune_sex)
text(prune_sex,pretty=0)
prune_sex.probs = predict(prune_sex, newdata=f_subset)

prune_sex.pred = rep(0,504)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 196  72
#          1 130 106
                                         
#                Accuracy : 0.5992         
#                  95% CI : (0.555, 0.6423)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.9883         
                                         
#                   Kappa : 0.1832         
#  Mcnemar's Test P-Value : 6.059e-05      
                                         
#             Sensitivity : 0.6012         
#             Specificity : 0.5955         
#          Pos Pred Value : 0.7313         
#          Neg Pred Value : 0.4492         
#              Prevalence : 0.6468         
#          Detection Rate : 0.3889         
#    Detection Prevalence : 0.5317         
#       Balanced Accuracy : 0.5984

#Male
set.seed(1)
cv_sex.tree = cv.tree(tree_sex)
plot(cv_sex.tree$k,cv_sex.tree$dev,type="b")
plot(cv_sex.tree$size, cv_sex.tree$dev, type="b")
tree_sex.min = cv_sex.tree$size[which.min(cv_sex.tree$dev)]
points(tree_sex.min, min(cv_sex.tree$dev), col="red", cex=2, pch=20)
prune_sex = prune.tree(tree_sex, best=5)
summary(prune_sex)
plot(prune_sex)
text(prune_sex,pretty=0)
prune_sex.probs = predict(prune_sex, newdata=m_subset)

prune_sex.pred = rep(0,2135)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 564 242
#          1 519 810
                                          
#                Accuracy : 0.6436          
#                  95% CI : (0.6228, 0.6639)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2896          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5208          
#             Specificity : 0.7700          
#          Pos Pred Value : 0.6998          
#          Neg Pred Value : 0.6095          
#              Prevalence : 0.5073          
#          Detection Rate : 0.2642          
#    Detection Prevalence : 0.3775          
#       Balanced Accuracy : 0.6454  

#Random Forest
#Female
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=f_subset)

rf_sex.pred = rep(0,504)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 273  93
#          1  53  85
                                          
#                Accuracy : 0.7103          
#                  95% CI : (0.6686, 0.7496)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.001461        
                                          
#                   Kappa : 0.3319          
#  Mcnemar's Test P-Value : 0.001248        
                                          
             Sensitivity : 0.8374          
#             Specificity : 0.4775          
#          Pos Pred Value : 0.7459          
#          Neg Pred Value : 0.6159          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5417          
#    Detection Prevalence : 0.7262          
#       Balanced Accuracy : 0.6575  

#Male
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=m_subset)

rf_sex.pred = rep(0,2135)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 649 300
#          1 434 752
                                          
#                Accuracy : 0.6562          
#                  95% CI : (0.6356, 0.6764)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3135          
#  Mcnemar's Test P-Value : 9.149e-07       
                                          
#             Sensitivity : 0.5993          
#             Specificity : 0.7148          
#          Pos Pred Value : 0.6839          
#          Neg Pred Value : 0.6341          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3040          
#    Detection Prevalence : 0.4445          
#       Balanced Accuracy : 0.6570

#Both
#Female
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
plot(tree_both)
text(tree_both, pretty=0)
tree_both.probs = predict(tree_both, newdata=f_subset)

tree_both.pred = rep(0,504)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 196  72
#          1 130 106
                                         
#                Accuracy : 0.5992         
#                  95% CI : (0.555, 0.6423)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.9883         
                                         
#                   Kappa : 0.1832         
#  Mcnemar's Test P-Value : 6.059e-05      
                                         
#             Sensitivity : 0.6012         
#             Specificity : 0.5955         
#          Pos Pred Value : 0.7313         
#          Neg Pred Value : 0.4492         
#              Prevalence : 0.6468         
#          Detection Rate : 0.3889         
#    Detection Prevalence : 0.5317         
#       Balanced Accuracy : 0.5984  

#Male
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
plot(tree_both)
text(tree_both, pretty=0)
tree_both.probs = predict(tree_both, newdata=m_subset)

tree_both.pred = rep(0,2135)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 564 242
#          1 519 810
                                          
#                Accuracy : 0.6436          
#                  95% CI : (0.6228, 0.6639)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2896          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5208          
#             Specificity : 0.7700          
#          Pos Pred Value : 0.6998          
#          Neg Pred Value : 0.6095          
#              Prevalence : 0.5073          
#          Detection Rate : 0.2642          
#    Detection Prevalence : 0.3775          
#       Balanced Accuracy : 0.6454

#Pruning
#Female
set.seed(1)
cv_both.tree = cv.tree(tree_both)
plot(cv_both.tree$k,cv_both.tree$dev,type="b")
plot(cv_both.tree$size, cv_both.tree$dev, type="b")
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
points(tree_both.min, min(cv_both.tree$dev), col="red", cex=2, pch=20)
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
plot(prune_both)
text(prune_both,pretty=0)
prune_both.probs = predict(prune_both, newdata=f_subset)

prune_both.pred = rep(0,504)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 196  72
#          1 130 106
                                         
#                Accuracy : 0.5992         
#                  95% CI : (0.555, 0.6423)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.9883         
                                         
#                   Kappa : 0.1832         
#  Mcnemar's Test P-Value : 6.059e-05      
                                         
#             Sensitivity : 0.6012         
#             Specificity : 0.5955         
#          Pos Pred Value : 0.7313         
#          Neg Pred Value : 0.4492         
#              Prevalence : 0.6468         
#          Detection Rate : 0.3889         
#    Detection Prevalence : 0.5317         
#       Balanced Accuracy : 0.5984

#Male
set.seed(1)
cv_both.tree = cv.tree(tree_both)
plot(cv_both.tree$k,cv_both.tree$dev,type="b")
plot(cv_both.tree$size, cv_both.tree$dev, type="b")
tree_both.min = cv_both.tree$size[which.min(cv_both.tree$dev)]
points(tree_both.min, min(cv_both.tree$dev), col="red", cex=2, pch=20)
prune_both = prune.tree(tree_both, best=5)
summary(prune_both)
plot(prune_both)
text(prune_both,pretty=0)
prune_both.probs = predict(prune_both, newdata=m_subset)

prune_both.pred = rep(0,2135)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 564 242
#          1 519 810
                                          
#                Accuracy : 0.6436          
#                  95% CI : (0.6228, 0.6639)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2896          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5208          
#             Specificity : 0.7700          
#          Pos Pred Value : 0.6998          
#          Neg Pred Value : 0.6095          
#              Prevalence : 0.5073          
#          Detection Rate : 0.2642          
#    Detection Prevalence : 0.3775          
#       Balanced Accuracy : 0.6454

#Random Forest
#Female
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=f_subset)

rf_both.pred = rep(0,504)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 273  93
#          1  53  85
                                          
#                Accuracy : 0.7103          
#                  95% CI : (0.6686, 0.7496)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.001461        
                                          
#                   Kappa : 0.3319          
#  Mcnemar's Test P-Value : 0.001248        
                                          
             Sensitivity : 0.8374          
#             Specificity : 0.4775          
#          Pos Pred Value : 0.7459          
#          Neg Pred Value : 0.6159          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5417          
#    Detection Prevalence : 0.7262          
#       Balanced Accuracy : 0.6575  

#Male
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=m_subset)

rf_both.pred = rep(0,2135)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=m_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 681 320
#          1 402 732
                                          
#                Accuracy : 0.6618          
#                  95% CI : (0.6413, 0.6819)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3243          
#  Mcnemar's Test P-Value : 0.002574        
                                          
#             Sensitivity : 0.6288          
#             Specificity : 0.6958          
#          Pos Pred Value : 0.6803          
#          Neg Pred Value : 0.6455          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3190          
#    Detection Prevalence : 0.4689          
#       Balanced Accuracy : 0.6623 

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
#Female
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, f_subset, type="response")

gam.pred = rep(0,504)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 256  90
#          1  70  88
                                         
#                Accuracy : 0.6825         
#                  95% CI : (0.6399, 0.723)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.05059        
                                         
#                   Kappa : 0.287          
#  Mcnemar's Test P-Value : 0.13308        
                                         
#             Sensitivity : 0.7853         
#             Specificity : 0.4944         
#          Pos Pred Value : 0.7399         
#          Neg Pred Value : 0.5570         
#              Prevalence : 0.6468         
#          Detection Rate : 0.5079         
#    Detection Prevalence : 0.6865         
#       Balanced Accuracy : 0.6398 

#Male
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, m_subset, type="response")

gam.pred = rep(0,2135)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 754 371
#          1 329 681
                                         
#                Accuracy : 0.6721         
#                  95% CI : (0.6518, 0.692)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3437         
#  Mcnemar's Test P-Value : 0.1212         
                                         
#             Sensitivity : 0.6962         
#             Specificity : 0.6473         
#          Pos Pred Value : 0.6702         
#          Neg Pred Value : 0.6743         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3532         
#    Detection Prevalence : 0.5269         
#       Balanced Accuracy : 0.6718

#Race
#Female
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, f_subset, type="response")

gam_race.pred = rep(0,504)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 255  89
#          1  71  89
                                         
#                Accuracy : 0.6825         
#                  95% CI : (0.6399, 0.723)
#     No Information Rate : 0.6468         
#     P-Value [Acc > NIR] : 0.05059        
                                         
#                   Kappa : 0.2888         
#  Mcnemar's Test P-Value : 0.17896        
                                         
#             Sensitivity : 0.7822         
#             Specificity : 0.5000         
#          Pos Pred Value : 0.7413         
#          Neg Pred Value : 0.5563         
#              Prevalence : 0.6468         
#          Detection Rate : 0.5060         
#    Detection Prevalence : 0.6825         
#       Balanced Accuracy : 0.6411

#Male
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, m_subset, type="response")

gam_race.pred = rep(0,2135)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 747 374
#          1 336 678
                                         
#                Accuracy : 0.6674         
#                  95% CI : (0.647, 0.6874)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3344         
#  Mcnemar's Test P-Value : 0.165          
                                         
#             Sensitivity : 0.6898         
#             Specificity : 0.6445         
#          Pos Pred Value : 0.6664         
#          Neg Pred Value : 0.6686         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3499         
#    Detection Prevalence : 0.5251         
#       Balanced Accuracy : 0.6671  

#Gender
#Female
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, f_subset, type="response")

gam_sex.pred = rep(0,504)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 295 123
#          1  31  55
                                          
#                Accuracy : 0.6944          
#                  95% CI : (0.6522, 0.7344)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.01357         
                                          
#                   Kappa : 0.2423          
#  Mcnemar's Test P-Value : 2.251e-13       
                                          
             Sensitivity : 0.9049          
#             Specificity : 0.3090          
#          Pos Pred Value : 0.7057          
#          Neg Pred Value : 0.6395          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5853          
#    Detection Prevalence : 0.8294          
#       Balanced Accuracy : 0.6069          

#Male
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, m_subset, type="response")

gam_sex.pred = rep(0,2135)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 648 295
#          1 435 757
                                          
#                Accuracy : 0.6581          
#                  95% CI : (0.6375, 0.6782)
#     No Information Rate : 0.5073          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3173          
#  Mcnemar's Test P-Value : 2.681e-07       
                                          
#             Sensitivity : 0.5983          
#             Specificity : 0.7196          
#          Pos Pred Value : 0.6872          
#          Neg Pred Value : 0.6351          
#              Prevalence : 0.5073          
#          Detection Rate : 0.3035          
#    Detection Prevalence : 0.4417          
#       Balanced Accuracy : 0.6590

#Both
#Female
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, f_subset, type="response")

gam_both.pred = rep(0,504)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=f_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 301 136
#          1  25  42
                                          
#                Accuracy : 0.6806          
#                  95% CI : (0.6379, 0.7211)
#     No Information Rate : 0.6468          
#     P-Value [Acc > NIR] : 0.06124         
                                          
#                   Kappa : 0.1855          
#  Mcnemar's Test P-Value : < 2e-16         
                                          
             Sensitivity : 0.9233          
#             Specificity : 0.2360          
#          Pos Pred Value : 0.6888          
#          Neg Pred Value : 0.6269          
#              Prevalence : 0.6468          
#          Detection Rate : 0.5972          
#    Detection Prevalence : 0.8671          
#       Balanced Accuracy : 0.5796 

#Male
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, m_subset, type="response")

gam_both.pred = rep(0,2135)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=m_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 727 352
#          1 356 700
                                         
#                Accuracy : 0.6684         
#                  95% CI : (0.648, 0.6883)
#     No Information Rate : 0.5073         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3367         
#  Mcnemar's Test P-Value : 0.9102         
                                         
#             Sensitivity : 0.6713         
#             Specificity : 0.6654         
#          Pos Pred Value : 0.6738         
#          Neg Pred Value : 0.6629         
#              Prevalence : 0.5073         
#          Detection Rate : 0.3405         
#    Detection Prevalence : 0.5054         
#       Balanced Accuracy : 0.6683
