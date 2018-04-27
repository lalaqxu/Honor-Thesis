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
      mutate(violent_charge = factor(Violence)) %>%
      within(violent_charge <- relevel(violent_charge, ref = 2)) %>%
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
aa_subset = test[which(test$race_factor=="African-American") ,]
nrow(aa_subset)
#1563
ca_subset = test[which(test$race_factor=="Caucasian") ,]
nrow(ca_subset)
#1076

#MULTIPLE LINEAR REGRESSION
#Neither
#African American
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, aa_subset, type="response")
lm.pred = rep(0,1563)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=aa_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 467 233
#          1 270 593
                                          
#                Accuracy : 0.6782          
#                  95% CI : (0.6544, 0.7013)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3525          
#  Mcnemar's Test P-Value : 0.1085          
                                          
#             Sensitivity : 0.6336          
#             Specificity : 0.7179          
#          Pos Pred Value : 0.6671          
#          Neg Pred Value : 0.6871          
#              Prevalence : 0.4715          
#          Detection Rate : 0.2988          
#    Detection Prevalence : 0.4479          
#       Balanced Accuracy : 0.6758 

#Caucasian
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, ca_subset, type="response")
lm.pred = rep(0,1076)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 543 228
#          1 129 176
                                          
#                Accuracy : 0.6682          
#                  95% CI : (0.6392, 0.6963)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.001592        
                                          
#                   Kappa : 0.2562          
#  Mcnemar's Test P-Value : 2.14e-07        
                                          
#             Sensitivity : 0.8080*          
#             Specificity : 0.4356          
#          Pos Pred Value : 0.7043          
#          Neg Pred Value : 0.5770          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5046          
#    Detection Prevalence : 0.7165          
#       Balanced Accuracy : 0.6218

#Race
#AA
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, aa_subset, type="response")
lm_race.pred = rep(0,1563)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 467 238
#          1 270 588
                                          
#                Accuracy : 0.675           
#                  95% CI : (0.6511, 0.6982)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3463          
#  Mcnemar's Test P-Value : 0.169           
                                          
#             Sensitivity : 0.6336          
#             Specificity : 0.7119          
#          Pos Pred Value : 0.6624          
#          Neg Pred Value : 0.6853          
#              Prevalence : 0.4715          
#          Detection Rate : 0.2988          
#    Detection Prevalence : 0.4511          
#       Balanced Accuracy : 0.6728 

#CA
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, ca_subset, type="response")
lm_race.pred = rep(0,1076)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 535 225
#          1 137 179
                                          
#                Accuracy : 0.6636          
#                  95% CI : (0.6345, 0.6918)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.004277        
                                          
#                   Kappa : 0.2501          
#  Mcnemar's Test P-Value : 4.817e-06       
                                          
#             Sensitivity : 0.7961*          
#             Specificity : 0.4431          
#          Pos Pred Value : 0.7039          
#          Neg Pred Value : 0.5665          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4972          
#    Detection Prevalence : 0.7063          
#       Balanced Accuracy : 0.6196 

#Gender**
#AA
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, aa_subset, type="response")
lm_sex.pred = rep(0,1563)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 441 208
#          1 296 618
                                          
#                Accuracy : 0.6775          
#                  95% CI : (0.6537, 0.7007)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3488          
#  Mcnemar's Test P-Value : 0.0001065       
                                          
#             Sensitivity : 0.5984          
#             Specificity : 0.7482          
#          Pos Pred Value : 0.6795          
#          Neg Pred Value : 0.6761          
#              Prevalence : 0.4715          
#          Detection Rate : 0.2821          
#    Detection Prevalence : 0.4152          
#       Balanced Accuracy : 0.6733 

#CA
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, ca_subset, type="response")
lm_sex.pred = rep(0,1076)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 502 210
#          1 170 194
                                          
#                Accuracy : 0.6468          
#                  95% CI : (0.6174, 0.6754)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.06907         
                                          
#                   Kappa : 0.2318          
#  Mcnemar's Test P-Value : 0.04543         
                                          
#             Sensitivity : 0.7470          
#             Specificity : 0.4802          
#          Pos Pred Value : 0.7051          
#          Neg Pred Value : 0.5330          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4665          
#    Detection Prevalence : 0.6617          
#       Balanced Accuracy : 0.6136 

#Both
#AA
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, aa_subset, type="response")
lm_both.pred = rep(0,1563)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 491 254
#          1 246 572
                                          
#                Accuracy : 0.6801          
#                  95% CI : (0.6563, 0.7032)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3585          
#  Mcnemar's Test P-Value : 0.7542          
                                          
#             Sensitivity : 0.6662          
#             Specificity : 0.6925          
#          Pos Pred Value : 0.6591          
#          Neg Pred Value : 0.6993          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3141          
#    Detection Prevalence : 0.4766          
#       Balanced Accuracy : 0.6794   

#CA
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, ca_subset, type="response")
lm_both.pred = rep(0,1076)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 537 234
#          1 135 170
                                          
#                Accuracy : 0.6571          
#                  95% CI : (0.6278, 0.6854)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.01454         
                                          
#                   Kappa : 0.2312          
#  Mcnemar's Test P-Value : 3.367e-07       
                                          
#             Sensitivity : 0.7991*          
#             Specificity : 0.4208          
#          Pos Pred Value : 0.6965          
#          Neg Pred Value : 0.5574          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4991          
#    Detection Prevalence : 0.7165          
#       Balanced Accuracy : 0.6099

#Logistic
library(ISLR)
#Neither**
#AA
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, aa_subset, type="response")
glm.pred = rep(0,1563)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 470 232
#          1 267 594
                                         
#                Accuracy : 0.6807         
#                  95% CI : (0.657, 0.7038)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3578         
#  Mcnemar's Test P-Value : 0.128          
                                         
#             Sensitivity : 0.6377         
#             Specificity : 0.7191         
#          Pos Pred Value : 0.6695         
#          Neg Pred Value : 0.6899         
#              Prevalence : 0.4715         
#          Detection Rate : 0.3007         
#    Detection Prevalence : 0.4491         
#       Balanced Accuracy : 0.6784 

#CA
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, ca_subset, type="response")
glm.pred = rep(0,1076)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 547 226
#          1 125 178
                                          
#                Accuracy : 0.6738          
#                  95% CI : (0.6449, 0.7018)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.0004274       
                                          
#                   Kappa : 0.2679          
#  Mcnemar's Test P-Value : 9.418e-08       
                                          
#             Sensitivity : 0.8140*          
#             Specificity : 0.4406          
#          Pos Pred Value : 0.7076          
#          Neg Pred Value : 0.5875          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5084          
#    Detection Prevalence : 0.7184          
#       Balanced Accuracy : 0.6273          
                                 
#Race
#AA
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, aa_subset, type="response")
glm_race.pred = rep(0,1563)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 451 216
#          1 286 610
                                         
#                Accuracy : 0.6788         
#                  95% CI : (0.655, 0.7019)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.3522         
#  Mcnemar's Test P-Value : 0.002073       
                                         
#             Sensitivity : 0.6119         
#             Specificity : 0.7385         
#          Pos Pred Value : 0.6762         
#          Neg Pred Value : 0.6808         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2885         
#    Detection Prevalence : 0.4267         
#       Balanced Accuracy : 0.6752                                       

#CA
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, ca_subset, type="response")
glm_race.pred = rep(0,1076)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 520 207
#          1 152 197
                                          
#                Accuracy : 0.6664          
#                  95% CI : (0.6373, 0.6945)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.002391        
                                          
#                   Kappa : 0.2687          
#  Mcnemar's Test P-Value : 0.004372        
                                          
#             Sensitivity : 0.7738*          
#             Specificity : 0.4876          
#          Pos Pred Value : 0.7153          
#          Neg Pred Value : 0.5645          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4833          
#    Detection Prevalence : 0.6757          
#       Balanced Accuracy : 0.6307

#Gender**
#AA
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, aa_subset, type="response")
glm_sex.pred = rep(0,1563)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=aa_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 483 242
#          1 254 584
                                          
#                Accuracy : 0.6827          
#                  95% CI : (0.6589, 0.7057)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3627          
#  Mcnemar's Test P-Value : 0.6214          
                                          
#             Sensitivity : 0.6554          
#             Specificity : 0.7070          
#          Pos Pred Value : 0.6662          
#          Neg Pred Value : 0.6969          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3090          
#    Detection Prevalence : 0.4639          
#       Balanced Accuracy : 0.6812  

#CA
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, ca_subset, type="response")
glm_sex.pred = rep(0,1076)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 547 233
#          1 125 171
                                          
#                Accuracy : 0.6673          
#                  95% CI : (0.6382, 0.6954)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.001955        
                                          
#                   Kappa : 0.2506          
#  Mcnemar's Test P-Value : 1.557e-08       
                                          
#             Sensitivity : 0.8140*          
#             Specificity : 0.4233          
#          Pos Pred Value : 0.7013          
#          Neg Pred Value : 0.5777          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5084          
#    Detection Prevalence : 0.7249          
#       Balanced Accuracy : 0.6186

#Both
#AA
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, aa_subset, type="response")
glm_both.pred = rep(0,1563)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 484 249
#          1 253 577
                                         
#                Accuracy : 0.6788         
#                  95% CI : (0.655, 0.7019)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3554         
#  Mcnemar's Test P-Value : 0.8935         
                                         
#             Sensitivity : 0.6567         
#             Specificity : 0.6985         
#          Pos Pred Value : 0.6603         
#          Neg Pred Value : 0.6952         
#              Prevalence : 0.4715         
#          Detection Rate : 0.3097         
#    Detection Prevalence : 0.4690         
#       Balanced Accuracy : 0.6776   

#CA
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, ca_subset, type="response")
glm_both.pred = rep(0,1076)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 540 228
#          1 132 176
                                          
#                Accuracy : 0.6654          
#                  95% CI : (0.6363, 0.6936)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.002914        
                                          
#                   Kappa : 0.2511          
#  Mcnemar's Test P-Value : 5.53e-07        
                                          
#             Sensitivity : 0.8036*          
#             Specificity : 0.4356          
#          Pos Pred Value : 0.7031          
#          Neg Pred Value : 0.5714          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5019          
#    Detection Prevalence : 0.7138          
#       Balanced Accuracy : 0.6196

#LDA
library(MASS)
#Neither
#AA
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, aa_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,1563)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 495 265
#          1 242 561
                                          
#                Accuracy : 0.6756          
#                  95% CI : (0.6518, 0.6988)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3502          
#  Mcnemar's Test P-Value : 0.3285          
                                          
#             Sensitivity : 0.6716          
#             Specificity : 0.6792          
#          Pos Pred Value : 0.6513          
#          Neg Pred Value : 0.6986          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3167          
#    Detection Prevalence : 0.4862          
#       Balanced Accuracy : 0.6754 

#CA
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, ca_subset, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,1076)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 560 244
#          1 112 160
                                          
#                Accuracy : 0.6691          
#                  95% CI : (0.6401, 0.6972)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.001291        
                                          
#                   Kappa : 0.2454          
#  Mcnemar's Test P-Value : 3.839e-12       
                                          
#             Sensitivity : 0.8333*          
#             Specificity : 0.3960          
#          Pos Pred Value : 0.6965          
#          Neg Pred Value : 0.5882          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5204          
#    Detection Prevalence : 0.7472          
#       Balanced Accuracy : 0.6147
      
#Race
#AA
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, aa_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,1563)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 497 269
#          1 240 557
                                          
#                Accuracy : 0.6743          
#                  95% CI : (0.6505, 0.6976)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.348           
#  Mcnemar's Test P-Value : 0.2146          
                                          
#             Sensitivity : 0.6744          
#             Specificity : 0.6743          
#          Pos Pred Value : 0.6488          
#          Neg Pred Value : 0.6989          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3180          
#    Detection Prevalence : 0.4901          
#       Balanced Accuracy : 0.6743 

#CA
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, ca_subset, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,1076)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 557 241
#          1 115 163
                                          
#                Accuracy : 0.6691          
#                  95% CI : (0.6401, 0.6972)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.001291        
                                          
#                   Kappa : 0.2477          
#  Mcnemar's Test P-Value : 3.473e-11       
                                          
#             Sensitivity : 0.8289*          
#             Specificity : 0.4035          
#          Pos Pred Value : 0.6980          
#          Neg Pred Value : 0.5863          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5177          
#    Detection Prevalence : 0.7416          
#       Balanced Accuracy : 0.6162 

#Gender**
#AA
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, aa_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,1563)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 480 247
#          1 257 579
                                          
#                Accuracy : 0.6775          
#                  95% CI : (0.6537, 0.7007)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3525          
#  Mcnemar's Test P-Value : 0.6885          
                                          
#             Sensitivity : 0.6513          
#             Specificity : 0.7010          
#          Pos Pred Value : 0.6602          
#          Neg Pred Value : 0.6926          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3071          
#    Detection Prevalence : 0.4651          
#       Balanced Accuracy : 0.6761

#CA
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, ca_subset, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,1076)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 541 241
#          1 131 163
                                         
#                Accuracy : 0.6543         
#                  95% CI : (0.625, 0.6827)
#     No Information Rate : 0.6245         
#     P-Value [Acc > NIR] : 0.02322        
                                         
#                   Kappa : 0.2205         
#  Mcnemar's Test P-Value : 1.592e-08      
                                         
#             Sensitivity : 0.8051*         
#             Specificity : 0.4035         
#          Pos Pred Value : 0.6918         
#          Neg Pred Value : 0.5544         
#              Prevalence : 0.6245         
#          Detection Rate : 0.5028         
#    Detection Prevalence : 0.7268         
#       Balanced Accuracy : 0.6043

#Both
#AA
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, aa_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,1563)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=aa_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 490 253
#          1 247 573
                                          
#                Accuracy : 0.6801          
#                  95% CI : (0.6563, 0.7032)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3584          
#  Mcnemar's Test P-Value : 0.8231          
                                          
#             Sensitivity : 0.6649          
#             Specificity : 0.6937          
#          Pos Pred Value : 0.6595          
#          Neg Pred Value : 0.6988          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3135          
#    Detection Prevalence : 0.4754          
#       Balanced Accuracy : 0.6793 

#CA
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, ca_subset, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,1076)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 537 234
#          1 135 170
                                          
#                Accuracy : 0.6571          
#                  95% CI : (0.6278, 0.6854)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.01454         
                                          
#                   Kappa : 0.2312          
#  Mcnemar's Test P-Value : 3.367e-07       
                                          
#             Sensitivity : 0.7991*          
#             Specificity : 0.4208          
#          Pos Pred Value : 0.6965          
#          Neg Pred Value : 0.5574          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4991          
#    Detection Prevalence : 0.7165          
#       Balanced Accuracy : 0.6099 

#QDA
#Neither
#AA
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, aa_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,1563)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 623 504
#          1 114 322
                                          
#                Accuracy : 0.6046          
#                  95% CI : (0.5799, 0.6289)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : 8.049e-10       
                                          
#                   Kappa : 0.2286          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.8453*          
#             Specificity : 0.3898          
#          Pos Pred Value : 0.5528          
#          Neg Pred Value : 0.7385          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3986          
#    Detection Prevalence : 0.7210          
#       Balanced Accuracy : 0.6176           
                                
#CA
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, ca_subset, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,1076)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 638 330
#          1  34  74
                                        
#                Accuracy : 0.6617        
#                  95% CI : (0.6326, 0.69)
#     No Information Rate : 0.6245        
#     P-Value [Acc > NIR] : 0.006182      
                                        
#                   Kappa : 0.1553        
#  Mcnemar's Test P-Value : < 2.2e-16     
                                        
#             Sensitivity : 0.9494**        
#             Specificity : 0.1832**        
#          Pos Pred Value : 0.6591        
#          Neg Pred Value : 0.6852        
#              Prevalence : 0.6245        
#          Detection Rate : 0.5929        
#    Detection Prevalence : 0.8996        
#       Balanced Accuracy : 0.5663 

#Race
#AA
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, aa_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,1563)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 624 507
#          1 113 319
                                          
#                Accuracy : 0.6033          
#                  95% CI : (0.5786, 0.6277)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : 1.511e-09       
                                          
#                   Kappa : 0.2264          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.8467*          
#             Specificity : 0.3862          
#          Pos Pred Value : 0.5517          
#          Neg Pred Value : 0.7384          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3992          
#    Detection Prevalence : 0.7236          
#       Balanced Accuracy : 0.6164

#CA
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, ca_subset, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,1076)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 636 326
#          1  36  78
                                          
#                Accuracy : 0.6636          
#                  95% CI : (0.6345, 0.6918)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.004277        
                                          
#                   Kappa : 0.1628          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.9464**          
#             Specificity : 0.1931**          
#          Pos Pred Value : 0.6611          
#          Neg Pred Value : 0.6842          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5911          
#    Detection Prevalence : 0.8941          
#       Balanced Accuracy : 0.5697

#Gender
#AA
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, aa_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,1563)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=aa_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 621 492
#          1 116 334
                                          
#                Accuracy : 0.611           
#                  95% CI : (0.5863, 0.6353)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : 2.954e-11       
                                          
#                   Kappa : 0.2404          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.8426*          
#             Specificity : 0.4044          
#          Pos Pred Value : 0.5580          
#          Neg Pred Value : 0.7422          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3973          
#    Detection Prevalence : 0.7121          
#       Balanced Accuracy : 0.6235  

#CA
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, ca_subset, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,1076)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 639 330
#          1  33  74
                                          
#                Accuracy : 0.6626          
#                  95% CI : (0.6335, 0.6909)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.005152        
                                          
#                   Kappa : 0.1571          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.9509**          
#             Specificity : 0.1832**          
#          Pos Pred Value : 0.6594          
#          Neg Pred Value : 0.6916          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5939          
#    Detection Prevalence : 0.9006          
#       Balanced Accuracy : 0.5670

#Both
#AA
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, aa_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,1563)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 622 489
#          1 115 337
                                          
#                Accuracy : 0.6136          
#                  95% CI : (0.5889, 0.6378)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : 7.316e-12       
                                          
#                   Kappa : 0.2452          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.8440*          
#             Specificity : 0.4080          
#          Pos Pred Value : 0.5599          
#          Neg Pred Value : 0.7456          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3980          
#    Detection Prevalence : 0.7108          
#       Balanced Accuracy : 0.6260  

#CA
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, ca_subset, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,1076)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 638 328
#          1  34  76
                                          
#                Accuracy : 0.6636          
#                  95% CI : (0.6345, 0.6918)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.004277        
                                          
#                   Kappa : 0.1609          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.9494**          
#             Specificity : 0.1881**          
#          Pos Pred Value : 0.6605          
#          Neg Pred Value : 0.6909          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5929          
#    Detection Prevalence : 0.8978          
#       Balanced Accuracy : 0.5688 

#KNN
library(class)
attach(df_race)
#Neither
#AA
train.y = two_year_recid[n.train]
test.y = aa_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(aa_subset$age, aa_subset$juv_fel_misd, aa_subset$priors_count, aa_subset$crime_factor, aa_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(aa_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 502 301
#          1 235 525
                                          
#                Accuracy : 0.6571          
#                  95% CI : (0.6329, 0.6806)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3152          
#  Mcnemar's Test P-Value : 0.004992        
                                          
#             Sensitivity : 0.6811          
#             Specificity : 0.6356          
#          Pos Pred Value : 0.6252          
#          Neg Pred Value : 0.6908          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3212          
#    Detection Prevalence : 0.5138          
#       Balanced Accuracy : 0.6584

#CA
train.y = two_year_recid[n.train]
test.y = ca_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(ca_subset$age, ca_subset$juv_fel_misd, ca_subset$priors_count, ca_subset$crime_factor, ca_subset$violent_charge)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(ca_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 570 253
#          1 102 151
                                          
#                Accuracy : 0.6701          
#                  95% CI : (0.6411, 0.6981)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.001043        
                                          
#                   Kappa : 0.2399          
#  Mcnemar's Test P-Value : 1.704e-15       
                                          
#             Sensitivity : 0.8482*          
#             Specificity : 0.3738          
#          Pos Pred Value : 0.6926          
#          Neg Pred Value : 0.5968          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5297          
#    Detection Prevalence : 0.7649          
#       Balanced Accuracy : 0.6110

#Race
#AA
train.y = two_year_recid[n.train]
test.y = aa_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(aa_subset$age, aa_subset$juv_fel_misd, aa_subset$priors_count, aa_subset$crime_factor, aa_subset$violent_charge, aa_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(aa_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 514 292
#          1 223 534
                                          
#                Accuracy : 0.6705          
#                  95% CI : (0.6466, 0.6938)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3422          
#  Mcnemar's Test P-Value : 0.002732        
                                          
#             Sensitivity : 0.6974          
#             Specificity : 0.6465          
#          Pos Pred Value : 0.6377          
#          Neg Pred Value : 0.7054          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3289          
#    Detection Prevalence : 0.5157          
#       Balanced Accuracy : 0.6720

#CA
train.y = two_year_recid[n.train]
test.y = ca_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test.x = cbind(ca_subset$age, ca_subset$juv_fel_misd, ca_subset$priors_count, ca_subset$crime_factor, ca_subset$violent_charge, ca_subset$race_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(ca_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 572 248
#          1 100 156
                                          
#                Accuracy : 0.6766          
#                  95% CI : (0.6477, 0.7045)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.0002099       
                                          
#                   Kappa : 0.256           
#  Mcnemar's Test P-Value : 3.273e-15       
                                          
#             Sensitivity : 0.8512**          
#             Specificity : 0.3861          
#          Pos Pred Value : 0.6976          
#          Neg Pred Value : 0.6094          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5316          
#    Detection Prevalence : 0.7621          
#       Balanced Accuracy : 0.6187

#Gender
#AA
train.y = two_year_recid[n.train]
test.y = aa_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(aa_subset$age, aa_subset$juv_fel_misd, aa_subset$priors_count, aa_subset$crime_factor, aa_subset$violent_charge, aa_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(aa_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 509 287
#          1 228 539
                                          
#                Accuracy : 0.6705          
#                  95% CI : (0.6466, 0.6938)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.3417          
#  Mcnemar's Test P-Value : 0.01059         
                                          
#             Sensitivity : 0.6906          
#             Specificity : 0.6525          
#          Pos Pred Value : 0.6394          
#          Neg Pred Value : 0.7027          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3257          
#    Detection Prevalence : 0.5093          
#       Balanced Accuracy : 0.6716

#CA
train.y = two_year_recid[n.train]
test.y = ca_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test.x = cbind(ca_subset$age, ca_subset$juv_fel_misd, ca_subset$priors_count, ca_subset$crime_factor, ca_subset$violent_charge, ca_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(ca_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 571 249
#          1 101 155
                                          
#                Accuracy : 0.6747          
#                  95% CI : (0.6458, 0.7027)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.0003386       
                                          
#                   Kappa : 0.2518          
#  Mcnemar's Test P-Value : 3.919e-15       
                                          
#             Sensitivity : 0.8497*          
#             Specificity : 0.3837          
#          Pos Pred Value : 0.6963          
#          Neg Pred Value : 0.6055          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5307          
#    Detection Prevalence : 0.7621          
#       Balanced Accuracy : 0.6167 

#Both**
#AA
train.y = two_year_recid[n.train]
test.y = aa_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(aa_subset$age, aa_subset$juv_fel_misd, aa_subset$priors_count, aa_subset$crime_factor, aa_subset$violent_charge, aa_subset$race_factor, aa_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(aa_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 510 278
#          1 227 548
                                          
#                Accuracy : 0.6769          
#                  95% CI : (0.6531, 0.7001)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.3541          
#  Mcnemar's Test P-Value : 0.02608         
                                          
#             Sensitivity : 0.6920          
#             Specificity : 0.6634          
#          Pos Pred Value : 0.6472          
#          Neg Pred Value : 0.7071          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3263          
#    Detection Prevalence : 0.5042          
#       Balanced Accuracy : 0.6777 

#CA
train.y = two_year_recid[n.train]
test.y = ca_subset$two_year_recid
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test.x = cbind(ca_subset$age, ca_subset$juv_fel_misd, ca_subset$priors_count, ca_subset$crime_factor, ca_subset$violent_charge, ca_subset$race_factor, ca_subset$gender_factor)
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,nrow(ca_subset))
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 573 246
#          1  99 158
                                          
#                Accuracy : 0.6794          
#                  95% CI : (0.6506, 0.7072)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 9.946e-05       
                                          
#                   Kappa : 0.2628          
#  Mcnemar's Test P-Value : 3.830e-15       
                                          
#             Sensitivity : 0.8527**          
#             Specificity : 0.3911          
#          Pos Pred Value : 0.6996          
#          Neg Pred Value : 0.6148          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5325          
#    Detection Prevalence : 0.7612          
#       Balanced Accuracy : 0.6219

#Tree
library(tree)
set.seed(1)
n <- nrow(df_race)
n.train = sample(1:n, n*.5)
n.test = (-n.train)
train = df_race[n.train,]
test = df_race[n.test,]

#Neither
#AA
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
plot(tree)
text(tree, pretty=0)
tree.probs = predict(tree, newdata=aa_subset)

tree.pred = rep(0,1563)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 340 142
#          1 397 684
                                         
#                Accuracy : 0.6552         
#                  95% CI : (0.631, 0.6787)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2949         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
#             Sensitivity : 0.4613         
#             Specificity : 0.8281*         
#          Pos Pred Value : 0.7054         
#          Neg Pred Value : 0.6327         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2175         
#    Detection Prevalence : 0.3084         
#       Balanced Accuracy : 0.6447 

#CA
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
plot(tree)
text(tree, pretty=0)
tree.probs = predict(tree, newdata=ca_subset)

tree.pred = rep(0,1076)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 420 172
#          1 252 232
                                         
#                Accuracy : 0.6059         
#                  95% CI : (0.576, 0.6353)
#     No Information Rate : 0.6245         
#     P-Value [Acc > NIR] : 0.9012892      
                                         
#                   Kappa : 0.1917         
#  Mcnemar's Test P-Value : 0.0001248      
                                         
#             Sensitivity : 0.6250         
#             Specificity : 0.5743         
#          Pos Pred Value : 0.7095         
#          Neg Pred Value : 0.4793         
#              Prevalence : 0.6245         
#          Detection Rate : 0.3903         
#    Detection Prevalence : 0.5502         
#       Balanced Accuracy : 0.5996 

#Pruning
#AA
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
prune.probs = predict(prune, newdata=aa_subset)

prune.pred = rep(0,1563)
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 340 142
#          1 397 684
                                         
#                Accuracy : 0.6552         
#                  95% CI : (0.631, 0.6787)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2949         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
#             Sensitivity : 0.4613         
#             Specificity : 0.8281*         
#          Pos Pred Value : 0.7054         
#          Neg Pred Value : 0.6327         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2175         
#    Detection Prevalence : 0.3084         
#       Balanced Accuracy : 0.6447          

#CA                                    
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
prune.probs = predict(prune, newdata=ca_subset)

prune.pred = rep(0, 1076)
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 420 172
#          1 252 232
                                         
#                Accuracy : 0.6059         
#                  95% CI : (0.576, 0.6353)
#     No Information Rate : 0.6245         
#     P-Value [Acc > NIR] : 0.9012892      
                                         
#                   Kappa : 0.1917         
#  Mcnemar's Test P-Value : 0.0001248      
                                         
#             Sensitivity : 0.6250         
#             Specificity : 0.5743         
#          Pos Pred Value : 0.7095         
#          Neg Pred Value : 0.4793         
#              Prevalence : 0.6245         
#          Detection Rate : 0.3903         
#    Detection Prevalence : 0.5502         
#       Balanced Accuracy : 0.5996 

#Random Forest
#AA
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=aa_subset)

rf.pred = rep(0, 1563)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 450 232
#          1 287 594
                                         
#                Accuracy : 0.6679         
#                  95% CI : (0.644, 0.6913)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.331          
#  Mcnemar's Test P-Value : 0.01777        
                                         
#             Sensitivity : 0.6106         
#             Specificity : 0.7191         
#          Pos Pred Value : 0.6598         
#          Neg Pred Value : 0.6742         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2879         
#    Detection Prevalence : 0.4363         
#       Balanced Accuracy : 0.6649  

#CA
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=ca_subset)

rf.pred = rep(0,1076)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 520 205
#          1 152 199
                                          
#                Accuracy : 0.6682          
#                  95% CI : (0.6392, 0.6963)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.001592        
                                          
#                   Kappa : 0.2735          
#  Mcnemar's Test P-Value : 0.005921        
                                          
#             Sensitivity : 0.7738*          
#             Specificity : 0.4926          
#          Pos Pred Value : 0.7172          
#          Neg Pred Value : 0.5670          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4833          
#    Detection Prevalence : 0.6738          
#       Balanced Accuracy : 0.6332  

#Race
#AA
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
plot(tree_race)
text(tree_race, pretty=0)
tree_race.probs = predict(tree_race, newdata=aa_subset)

tree_race.pred = rep(0,1563)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 340 142
#          1 397 684
                                         
#                Accuracy : 0.6552         
#                  95% CI : (0.631, 0.6787)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2949         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
#             Sensitivity : 0.4613         
#             Specificity : 0.8281*         
#          Pos Pred Value : 0.7054         
#          Neg Pred Value : 0.6327         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2175         
#    Detection Prevalence : 0.3084         
#       Balanced Accuracy : 0.6447         

#CA 
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
plot(tree_race)
text(tree_race, pretty=0)
tree_race.probs = predict(tree_race, newdata=ca_subset)

tree_race.pred = rep(0,1076)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 420 172
#          1 252 232
                                         
#                Accuracy : 0.6059         
#                  95% CI : (0.576, 0.6353)
#     No Information Rate : 0.6245         
#     P-Value [Acc > NIR] : 0.9012892      
                                         
#                   Kappa : 0.1917         
#  Mcnemar's Test P-Value : 0.0001248      
                                         
#             Sensitivity : 0.6250         
#             Specificity : 0.5743         
#          Pos Pred Value : 0.7095         
#          Neg Pred Value : 0.4793         
#              Prevalence : 0.6245         
#          Detection Rate : 0.3903         
#    Detection Prevalence : 0.5502         
#       Balanced Accuracy : 0.5996 

#Pruning
#AA
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
prune_race.probs = predict(prune_race, newdata=aa_subset)

prune_race.pred = rep(0,1563)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=aa_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 340 142
#          1 397 684
                                         
#                Accuracy : 0.6552         
#                  95% CI : (0.631, 0.6787)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2949         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
#             Sensitivity : 0.4613         
#             Specificity : 0.8281*         
#          Pos Pred Value : 0.7054         
#          Neg Pred Value : 0.6327         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2175         
#    Detection Prevalence : 0.3084         
#       Balanced Accuracy : 0.6447 

#CA
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
prune_race.probs = predict(prune_race, newdata=ca_subset)

prune_race.pred = rep(0,1076)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 420 172
#          1 252 232
                                         
#                Accuracy : 0.6059         
#                  95% CI : (0.576, 0.6353)
#     No Information Rate : 0.6245         
#     P-Value [Acc > NIR] : 0.9012892      
                                         
#                   Kappa : 0.1917         
#  Mcnemar's Test P-Value : 0.0001248      
                                         
#             Sensitivity : 0.6250         
#             Specificity : 0.5743         
#          Pos Pred Value : 0.7095         
#          Neg Pred Value : 0.4793         
#              Prevalence : 0.6245         
#          Detection Rate : 0.3903         
#    Detection Prevalence : 0.5502         
#       Balanced Accuracy : 0.5996 

#Random Forest
#AA
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=aa_subset)

rf_race.pred = rep(0,1563)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 431 211
#          1 306 615
                                          
#                Accuracy : 0.6692          
#                  95% CI : (0.6453, 0.6925)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3317          
#  Mcnemar's Test P-Value : 3.563e-05       
                                          
#             Sensitivity : 0.5848          
#             Specificity : 0.7446          
#          Pos Pred Value : 0.6713          
#          Neg Pred Value : 0.6678          
#              Prevalence : 0.4715          
#          Detection Rate : 0.2758          
#    Detection Prevalence : 0.4107          
#       Balanced Accuracy : 0.6647  

#CA
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=ca_subset)

rf_race.pred = rep(0,1076)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 508 207
#          1 164 197
                                          
#                Accuracy : 0.6552          
#                  95% CI : (0.6259, 0.6836)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.01994         
                                          
#                   Kappa : 0.2489          
#  Mcnemar's Test P-Value : 0.02922         
                                          
#             Sensitivity : 0.7560*          
#             Specificity : 0.4876          
#          Pos Pred Value : 0.7105          
#          Neg Pred Value : 0.5457          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4721          
#    Detection Prevalence : 0.6645          
#       Balanced Accuracy : 0.6218 

#Gender
#AA
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
plot(tree_sex)
text(tree_sex, pretty=0)
tree_sex.probs = predict(tree_sex, newdata=aa_subset)

tree_sex.pred = rep(0,1563)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 340 142
#          1 397 684
                                         
#                Accuracy : 0.6552         
#                  95% CI : (0.631, 0.6787)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2949         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
#             Sensitivity : 0.4613         
#             Specificity : 0.8281*         
#          Pos Pred Value : 0.7054         
#          Neg Pred Value : 0.6327         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2175         
#    Detection Prevalence : 0.3084         
#       Balanced Accuracy : 0.6447

#CA
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
plot(tree_sex)
text(tree_sex, pretty=0)
tree_sex.probs = predict(tree_sex, newdata=ca_subset)

tree_sex.pred = rep(0,1076)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 420 172
#          1 252 232
                                         
#                Accuracy : 0.6059         
#                  95% CI : (0.576, 0.6353)
#     No Information Rate : 0.6245         
#     P-Value [Acc > NIR] : 0.9012892      
                                         
#                   Kappa : 0.1917         
#  Mcnemar's Test P-Value : 0.0001248      
                                         
#             Sensitivity : 0.6250         
#             Specificity : 0.5743         
#          Pos Pred Value : 0.7095         
#          Neg Pred Value : 0.4793         
#              Prevalence : 0.6245         
#          Detection Rate : 0.3903         
#    Detection Prevalence : 0.5502         
#       Balanced Accuracy : 0.5996

#Pruning
#AA
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
prune_sex.probs = predict(prune_sex, newdata=aa_subset)

prune_sex.pred = rep(0,1563)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 340 142
#          1 397 684
                                         
#                Accuracy : 0.6552         
#                  95% CI : (0.631, 0.6787)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2949         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
#             Sensitivity : 0.4613         
#             Specificity : 0.8281*         
#          Pos Pred Value : 0.7054         
#          Neg Pred Value : 0.6327         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2175         
#    Detection Prevalence : 0.3084         
#       Balanced Accuracy : 0.6447 

#CA
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
prune_sex.probs = predict(prune_sex, newdata=ca_subset)

prune_sex.pred = rep(0,1076)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 420 172
#          1 252 232
                                         
#                Accuracy : 0.6059         
#                  95% CI : (0.576, 0.6353)
#     No Information Rate : 0.6245         
#     P-Value [Acc > NIR] : 0.9012892      
                                         
#                   Kappa : 0.1917         
#  Mcnemar's Test P-Value : 0.0001248      
                                         
#             Sensitivity : 0.6250         
#             Specificity : 0.5743         
#          Pos Pred Value : 0.7095         
#          Neg Pred Value : 0.4793         
#              Prevalence : 0.6245         
#          Detection Rate : 0.3903         
#    Detection Prevalence : 0.5502         
#       Balanced Accuracy : 0.5996 

#Random Forest
#AA
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=aa_subset)

rf_sex.pred = rep(0,1563)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 426 202
#          1 311 624
                                         
#                Accuracy : 0.6718         
#                  95% CI : (0.6479, 0.695)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.3361         
#  Mcnemar's Test P-Value : 1.858e-06      
                                         
#             Sensitivity : 0.5780         
#             Specificity : 0.7554*         
#          Pos Pred Value : 0.6783         
#          Neg Pred Value : 0.6674         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2726         
#    Detection Prevalence : 0.4018         
#       Balanced Accuracy : 0.6667  

#CA
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=ca_subset)

rf_sex.pred = rep(0,1076)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 496 191
#          1 176 213
                                          
#                Accuracy : 0.6589          
#                  95% CI : (0.6297, 0.6872)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.01044         
                                          
#                   Kappa : 0.2673          
#  Mcnemar's Test P-Value : 0.46490         
                                          
#             Sensitivity : 0.7381          
#             Specificity : 0.5272          
#          Pos Pred Value : 0.7220          
#          Neg Pred Value : 0.5476          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4610          
#    Detection Prevalence : 0.6385          
#       Balanced Accuracy : 0.6327  

#Both
#AA
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
plot(tree_both)
text(tree_both, pretty=0)
tree_both.probs = predict(tree_both, newdata=aa_subset)

tree_both.pred = rep(0,1563)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 340 142
#          1 397 684
                                         
#                Accuracy : 0.6552         
#                  95% CI : (0.631, 0.6787)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2949         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
#             Sensitivity : 0.4613         
#             Specificity : 0.8281*         
#          Pos Pred Value : 0.7054         
#          Neg Pred Value : 0.6327         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2175         
#    Detection Prevalence : 0.3084         
#       Balanced Accuracy : 0.6447 

#CA
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
plot(tree_both)
text(tree_both, pretty=0)
tree_both.probs = predict(tree_both, newdata=ca_subset)

tree_both.pred = rep(0,1076)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 420 172
#          1 252 232
                                         
#                Accuracy : 0.6059         
#                  95% CI : (0.576, 0.6353)
#     No Information Rate : 0.6245         
#     P-Value [Acc > NIR] : 0.9012892      
                                         
#                   Kappa : 0.1917         
#  Mcnemar's Test P-Value : 0.0001248      
                                         
#             Sensitivity : 0.6250         
#             Specificity : 0.5743         
#          Pos Pred Value : 0.7095         
#          Neg Pred Value : 0.4793         
#              Prevalence : 0.6245         
#          Detection Rate : 0.3903         
#    Detection Prevalence : 0.5502         
#       Balanced Accuracy : 0.5996 

#Pruning
#AA
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
prune_both.probs = predict(prune_both, newdata=aa_subset)

prune_both.pred = rep(0,1563)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 340 142
#          1 397 684
                                         
#                Accuracy : 0.6552         
#                  95% CI : (0.631, 0.6787)
#     No Information Rate : 0.5285         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.2949         
#  Mcnemar's Test P-Value : < 2.2e-16      
                                         
#             Sensitivity : 0.4613         
#             Specificity : 0.8281*         
#          Pos Pred Value : 0.7054         
#          Neg Pred Value : 0.6327         
#              Prevalence : 0.4715         
#          Detection Rate : 0.2175         
#    Detection Prevalence : 0.3084         
#       Balanced Accuracy : 0.6447

#CA
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
prune_both.probs = predict(prune_both, newdata=ca_subset)

prune_both.pred = rep(0,1076)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 420 172
#          1 252 232
                                         
#                Accuracy : 0.6059         
#                  95% CI : (0.576, 0.6353)
#     No Information Rate : 0.6245         
#     P-Value [Acc > NIR] : 0.9012892      
                                         
#                   Kappa : 0.1917         
#  Mcnemar's Test P-Value : 0.0001248      
                                         
#             Sensitivity : 0.6250         
#             Specificity : 0.5743         
#          Pos Pred Value : 0.7095         
#          Neg Pred Value : 0.4793         
#              Prevalence : 0.6245         
#          Detection Rate : 0.3903         
#    Detection Prevalence : 0.5502         
#       Balanced Accuracy : 0.5996

#Random Forest
#AA
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=aa_subset)

rf_both.pred = rep(0,1563)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 438 210
#          1 299 616
                                          
#                Accuracy : 0.6743          
#                  95% CI : (0.6505, 0.6976)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3423          
#  Mcnemar's Test P-Value : 9.598e-05       
                                          
#             Sensitivity : 0.5943          
#             Specificity : 0.7458          
#          Pos Pred Value : 0.6759          
#          Neg Pred Value : 0.6732          
#              Prevalence : 0.4715          
#          Detection Rate : 0.2802          
#    Detection Prevalence : 0.4146          
#       Balanced Accuracy : 0.6700 

#CA
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=ca_subset)

rf_both.pred = rep(0,1076)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=ca_subset$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 516 203
#          1 156 201
                                          
#                Accuracy : 0.6664          
#                  95% CI : (0.6373, 0.6945)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.002391        
                                          
#                   Kappa : 0.2717          
#  Mcnemar's Test P-Value : 0.015191        
                                          
#             Sensitivity : 0.7679*          
#             Specificity : 0.4975          
#          Pos Pred Value : 0.7177          
#          Neg Pred Value : 0.5630          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4796          
#    Detection Prevalence : 0.6682          
#       Balanced Accuracy : 0.6327 

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
#AA
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, aa_subset, type="response")

gam.pred = rep(0,1563)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 467 233
#          1 270 593
                                          
#                Accuracy : 0.6782          
#                  95% CI : (0.6544, 0.7013)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3525          
#  Mcnemar's Test P-Value : 0.1085          
                                          
#             Sensitivity : 0.6336          
#             Specificity : 0.7179          
#          Pos Pred Value : 0.6671          
#          Neg Pred Value : 0.6871          
#              Prevalence : 0.4715          
#          Detection Rate : 0.2988          
#    Detection Prevalence : 0.4479          
#       Balanced Accuracy : 0.6758  

#CA
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, ca_subset, type="response")

gam.pred = rep(0,1076)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 543 228
#          1 129 176
                                          
#                Accuracy : 0.6682          
#                  95% CI : (0.6392, 0.6963)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.001592        
                                          
#                   Kappa : 0.2562          
#  Mcnemar's Test P-Value : 2.14e-07        
                                          
#             Sensitivity : 0.8080*          
#             Specificity : 0.4356          
#          Pos Pred Value : 0.7043          
#          Neg Pred Value : 0.5770          
#              Prevalence : 0.6245          
#          Detection Rate : 0.5046          
#    Detection Prevalence : 0.7165          
#       Balanced Accuracy : 0.6218 

#Race
#AA
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, aa_subset, type="response")

gam_race.pred = rep(0,1563)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 467 238
#          1 270 588
                                          
#                Accuracy : 0.675           
#                  95% CI : (0.6511, 0.6982)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3463          
#  Mcnemar's Test P-Value : 0.169           
                                          
#             Sensitivity : 0.6336          
#             Specificity : 0.7119          
#          Pos Pred Value : 0.6624          
#          Neg Pred Value : 0.6853          
#              Prevalence : 0.4715          
#          Detection Rate : 0.2988          
#    Detection Prevalence : 0.4511          
#       Balanced Accuracy : 0.6728 

#CA
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, ca_subset, type="response")

gam_race.pred = rep(0,1076)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 535 225
#          1 137 179
                                          
#                Accuracy : 0.6636          
#                  95% CI : (0.6345, 0.6918)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.004277        
                                          
#                   Kappa : 0.2501          
#  Mcnemar's Test P-Value : 4.817e-06       
                                          
#             Sensitivity : 0.7961*          
#             Specificity : 0.4431          
#          Pos Pred Value : 0.7039          
#          Neg Pred Value : 0.5665          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4972          
#    Detection Prevalence : 0.7063          
#       Balanced Accuracy : 0.6196  

#Gender
#AA
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, aa_subset, type="response")

gam_sex.pred = rep(0,1563)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 441 208
#          1 296 618
                                          
#                Accuracy : 0.6775          
#                  95% CI : (0.6537, 0.7007)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3488          
#  Mcnemar's Test P-Value : 0.0001065       
                                          
#             Sensitivity : 0.5984          
#             Specificity : 0.7482          
#          Pos Pred Value : 0.6795          
#          Neg Pred Value : 0.6761          
#              Prevalence : 0.4715          
#          Detection Rate : 0.2821          
#    Detection Prevalence : 0.4152          
#       Balanced Accuracy : 0.6733          

#CA
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, ca_subset, type="response")

gam_sex.pred = rep(0,1076)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 502 210
#          1 170 194
                                          
#                Accuracy : 0.6468          
#                  95% CI : (0.6174, 0.6754)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.06907         
                                          
#                   Kappa : 0.2318          
#  Mcnemar's Test P-Value : 0.04543         
                                          
#             Sensitivity : 0.7470          
#             Specificity : 0.4802          
#          Pos Pred Value : 0.7051          
#          Neg Pred Value : 0.5330          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4665          
#    Detection Prevalence : 0.6617          
#       Balanced Accuracy : 0.6136 

#Both
#AA
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, aa_subset, type="response")

gam_both.pred = rep(0,1563)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=aa_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 491 254
#          1 246 572
                                          
#                Accuracy : 0.6801          
#                  95% CI : (0.6563, 0.7032)
#     No Information Rate : 0.5285          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3585          
#  Mcnemar's Test P-Value : 0.7542          
                                          
#             Sensitivity : 0.6662          
#             Specificity : 0.6925          
#          Pos Pred Value : 0.6591          
#          Neg Pred Value : 0.6993          
#              Prevalence : 0.4715          
#          Detection Rate : 0.3141          
#    Detection Prevalence : 0.4766          
#       Balanced Accuracy : 0.6794  

#CA
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, ca_subset, type="response")

gam_both.pred = rep(0,1076)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=ca_subset$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 537 234
#          1 135 170
                                          
#                Accuracy : 0.6571          
#                  95% CI : (0.6278, 0.6854)
#     No Information Rate : 0.6245          
#     P-Value [Acc > NIR] : 0.01454         
                                          
#                   Kappa : 0.2312          
#  Mcnemar's Test P-Value : 3.367e-07       
                                          
#             Sensitivity : 0.7991*          
#             Specificity : 0.4208          
#          Pos Pred Value : 0.6965          
#          Neg Pred Value : 0.5574          
#              Prevalence : 0.6245          
#          Detection Rate : 0.4991          
#    Detection Prevalence : 0.7165          
#       Balanced Accuracy : 0.6099 
