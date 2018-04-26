#Honors Thesis 

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

df_race$Violence <- "NonViolent"
  df_race$Violence[df_race$c_charge_desc == "Agg Assault Law Enforc Officer"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Agg Assault W/int Com Fel Dome"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Agg Batt W/Arm S/B/I 25 Min/Ma"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Agg Battery Bod Hrm-Deadly Weap"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Agg Battery Grt/Bod/Harm"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Agg Battery Law Enforc Officer"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Agg Flee/Eluding (Injury/Prop Damage)"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Agg Fleeing and Eluding"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Agg Fleeing/Eluding High Speed"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aggrav Battery w/Deadly Weapon"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aggrav Child Abuse-Agg Battery"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aggrav Child Abuse-Causes Harm"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aggravated Assault"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aggravated Assault W/Dead Weap"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aggravated Assault W/dead Weap"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aggravated Assault w/Firearm"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aggravated Battery"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aggravated Battery / Pregnant"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aggravated Battery On 65/Older"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Aid/Abet Burglary Assault/Batt"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Armed Carjacking"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Armed False Imprisonment"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Armed Kidnapping"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Armed Sex Batt/vict 12 Yrs +"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Arson II (Vehicle)"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Arson in the Second Degree"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Assault"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Assault Law Enforcement Officer"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Assault On Law Enforc Officer"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Att Robbery Sudd Snatch No Weap"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Attempt Burglary (Struct)"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Attempt Felony Murder"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Attempt Murder in the First Degree"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Attempted Robbery  No Weapon"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Attempted Robbery Firearm"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Battery"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Battery Emergency Care Provide"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Battery on a Person Over 65"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Battery on Law Enforc Officer"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Battery Spouse Or Girlfriend"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Battery Upon Detainee"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Burglary Conveyance Assault/Bat"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Burglary Dwelling Armed"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Burglary Dwelling Assault/Batt"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Burglary With Assault/battery"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Carjacking"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Child Abuse"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Cruelty To Animals"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "D.U.I. Serious Bodily Injury"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Discharge Firearm in Public/Res"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "DOC/Engage In Fighting"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "DOC/Fighting/Threatening Words"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Felony Batt(Great Bodily Harm)"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Felony Battery"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Felony Battery (Dom Strang)"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Felony Battery w/Prior Convict"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Home Invasion Robbery"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Kidnapping (Facilitate Felony)"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Kidnapping / Domestic Violence"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Manslaughter with Weapon"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Murder in 2nd Degree"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Murder in the First Degree"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Neglect Child / Bodily Harm"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Robbery"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Robbery / No Weapon"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Robbery / Weapon"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Robbery Sudd Snatch No Weapon"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Robbery Sudd Snatch w/Weapon"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Robbery W/Deadly Weapon"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Robbery W/Firearm"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Robbery-Strong Arm W/mask"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Sex Batt Faml/Cust Vict 12-17Y"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Sex Battery Deft 18+/Vict 11-"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Sexual Battery / Vict 12 Yrs +"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Shoot In Occupied Building"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Shoot/Throw Into Vehicle"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Stalking (Aggravated)"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Strong Armed  Robbery"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Threat Public Servant"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Threaten Throw Destruct Device"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Throw Deadly Missile Into Veh"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Throw In Occupied Dwell"] = "Violent"
  df_race$Violence[df_race$c_charge_desc == "Vehicular Homicide"] = "Violent"

#Variable into Factors
df_race <- mutate(df_race, crime_factor = factor(c_charge_degree)) %>%
      mutate(violent_charge = factor(Violence)) %>%
      within(violent_charge <- relevel(violent_charge, ref = 2)) %>%
      mutate(race_factor = factor(race)) %>%
      within(race_factor <- relevel(race_factor, ref = 2)) %>%
      mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
      within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
      mutate(score_factor = factor(score_text != "Low", labels = c("LowScore","HighScore")))

#Summary Statistics

#Race
summary(df_race$race_factor)
#African American
#3175 
#Caucasian
#2103

summary(as.factor(df_race[df_race$race_factor == "African-American", ]$two_year_recid))
#    0    1 
# 1514 1661
1661/3175
# 0.5231496
1514/3175
#0.4768504

summary(as.factor(df_race[df_race$race_factor == "Caucasian", ]$two_year_recid))
#    0    1 
# 1281  822 
822/2103
# 0.3908702
1281/2103
# 0.6091298

#Gender
summary(df_race$gender_factor)
#Female
#1031
#Male
#4247

summary(as.factor(df_race[df_race$gender_factor == "Female", ]$two_year_recid))
#   0   1 
# 658 373
373/1031
# 0.3617847
658/1031
# 0.6382153

summary(as.factor(df_race[df_race$gender_factor == "Male", ]$two_year_recid))
#    0    1 
# 2137 2110
2110/4247
# 0.4968213
2137/4247
# 0.5031787

#Race and Gender
summary(df_race[df_race$race_factor == "African-American", ]$gender_factor)
summary(df_race[df_race$race_factor == "Caucasian", ]$gender_factor)
#AM
2626
#CM
1621
#AF
549
#CF
482

summary(as.factor(df_race[df_race$race_factor == "African-American" & df_race$gender_factor=="Male", ]$two_year_recid))
#    0    1 
# 1168 1458 
1458/2626
# 0.5552171
1168/2626
# 0.4447829

summary(as.factor(df_race[df_race$race_factor == "Caucasian" & df_race$gender_factor=="Male", ]$two_year_recid))
#   0   1 
# 969 652 
652/1621
# 0.4022209
969/1621
# 0.5977791

summary(as.factor(df_race[df_race$race_factor == "African-American" & df_race$gender_factor=="Female", ]$two_year_recid))
#   0   1 
# 346 203 
203/549
# 0.3697632
346/549
# 0.6302368

summary(as.factor(df_race[df_race$race_factor == "Caucasian" & df_race$gender_factor=="Female", ]$two_year_recid))
#   0   1 
# 312 170 
170/482
# 0.3526971
312/482
# 0.6473029

summary(df_race$age_cat)
#Less than 25
#1156
#25-45
#3026
#Greater than 45
#1096

summary(as.factor(df_race[df_race$age_cat == "Less than 25", ]$two_year_recid))
#   0   1 
# 496 660
660/1156
# 0.5709343
496/1156
# 0.4290657

summary(as.factor(df_race[df_race$age_cat == "25 - 45", ]$two_year_recid))
#    0    1 
# 1565 1461
1461/3026
# 0.4828156
1565/3026
# 0.5171844

summary(as.factor(df_race[df_race$age_cat == "Greater than 45", ]$two_year_recid))
#   0   1 
# 734 362 
362/1096
# 0.330292
734/1096
# 0.669708

summary(df_race$violent_charge)
#Violent
#1439
#NonViolent
#3839

summary(as.factor(df_race[df_race$violent_charge == "Violent", ]$two_year_recid))
#   0   1 
# 866 573 
573/1439
# 0.3981932
866/1439
# 0.6018068

summary(as.factor(df_race[df_race$violent_charge == "NonViolent", ]$two_year_recid))
#    0    1 
# 1929 1910 
1910/3839
# 0.4975254
1929/3839
# 0.5024746

#Scatterplot
pairs(~age+crime_factor+race_factor+gender_factor+priors_count+two_year_recid, data=df_race, main="Scatterplot Matrix")
#Histograms
#Race
par(mfrow=c(2,2))
histogram(~ age | race, data = df_race, col=c("white"))
histogram(~ crime_factor | race, data = df_race, col=c("white"))
histogram(~ juv_fel_misd | race, data = df_race, col=c("white"))
histogram(~ priors_count | race, data = df_race, col=c("white"))
#Age
par(mfrow=c(2,2))
histogram(~ age | sex, data = df_race, col=c("white"))
histogram(~ crime_factor | sex, data = df_race, col=c("white"))
histogram(~ juv_fel_misd | sex, data = df_race, col=c("white"))
histogram(~ priors_count | sex, data = df_race, col=c("white"))

#Regression
reg <- lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data = df_race)
summary(reg)
reg_race <- lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data = df_race)
summary(reg_race)
reg_sex <- lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data = df_race)
summary(reg_sex)
reg_both <- lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data = df_race)
summary(reg_both)
AIC(reg, reg_race, reg_sex, reg_both)

#Predicting Race and Gender
pred_race <- lm(as.numeric(race_factor) ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data = df_race)
summary(pred_race)
# Call:
# lm(formula = as.numeric(race_factor) ~ age + juv_fel_misd + priors_count + 
#     crime_factor + violent_charge, data = df_race)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.1469 -0.5097  0.2509  0.3740  0.9429 

# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               1.9120730  0.0240896  79.373  < 2e-16 ***
# age                      -0.0096926  0.0005626 -17.227  < 2e-16 ***
# juv_fel_misd              0.0087503  0.0099101   0.883  0.37729    
# priors_count              0.0219862  0.0014265  15.412  < 2e-16 ***
# crime_factorM            -0.0657762  0.0148554  -4.428 9.71e-06 ***
# violent_chargeNonViolent -0.0429139  0.0157677  -2.722  0.00652 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.465 on 5272 degrees of freedom
# Multiple R-squared:  0.09903, Adjusted R-squared:  0.09818 
# F-statistic: 115.9 on 5 and 5272 DF,  p-value: < 2.2e-16
pred_sex <- lm(as.numeric(gender_factor) ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data = df_race)
summary(pred_sex)
# Call:
# lm(formula = as.numeric(gender_factor) ~ age + juv_fel_misd + 
#     priors_count + crime_factor + violent_charge, data = df_race)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.30449 -0.21574 -0.18969 -0.09127  1.03029 

# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               1.2375918  0.0203180  60.911  < 2e-16 ***
# age                       0.0006006  0.0004745   1.266 0.205721    
# juv_fel_misd             -0.0205145  0.0083585  -2.454 0.014147 *  
# priors_count             -0.0088231  0.0012032  -7.333 2.59e-13 ***
# crime_factorM             0.0188500  0.0125296   1.504 0.132526    
# violent_chargeNonViolent -0.0490605  0.0132990  -3.689 0.000227 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.3922 on 5272 degrees of freedom
# Multiple R-squared:  0.02264, Adjusted R-squared:  0.02171 
# F-statistic: 24.42 on 5 and 5272 DF,  p-value: < 2.2e-16

reg_race_recid <- lm(two_year_recid ~ race_factor, data = df_race)
summary(reg_race_recid)
reg_sex_recid <- lm(two_year_recid ~ gender_factor, data = df_race)
summary(reg_sex_recid)

#Confusion Matrix
library(caret)
#COMPAS
prediction <-
  ifelse(df_race$score_factor == "HighScore", 1, 0)
confusionMatrix(prediction, reference=df_race$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1872  881
#          1  923 1602
                                         
#                Accuracy : 0.6582         
#                  95% CI : (0.6452, 0.671)
#     No Information Rate : 0.5296         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3147         
#  Mcnemar's Test P-Value : 0.3344         
                                         
#             Sensitivity : 0.6698         
#             Specificity : 0.6452         
#          Pos Pred Value : 0.6800         
#          Neg Pred Value : 0.6345         
#              Prevalence : 0.5296         
#          Detection Rate : 0.3547         
#    Detection Prevalence : 0.5216         
#       Balanced Accuracy : 0.6575  

#Test/Training Samples
set.seed(1)
n <- nrow(df_race)
n.train = sample(1:n, n*.5)
n.test = (-n.train)
train = df_race[n.train,]
test = df_race[n.test,]
library(pROC)

#MULTIPLE LINEAR REGRESSION
#Neither
lm = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lm)
lm.probs = predict(lm, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[lm.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}

which.max(roc)
#49
max(roc)
#0.6710118

lm.pred = rep(0,2639)
lm.pred[lm.probs>.49] = 1
confusionMatrix(lm.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1010  461
#          1  399  769
                                         
#                Accuracy : 0.6741         
#                  95% CI : (0.6559, 0.692)
#     No Information Rate : 0.5339         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3431         
#  Mcnemar's Test P-Value : 0.03752        
                                         
#             Sensitivity : 0.7168         
#             Specificity : 0.6252         
#          Pos Pred Value : 0.6866         
#          Neg Pred Value : 0.6584         
#              Prevalence : 0.5339         
#          Detection Rate : 0.3827         
#    Detection Prevalence : 0.5574         
#       Balanced Accuracy : 0.6710  

#Race
lm_race = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lm_race)
lm_race.probs = predict(lm_race, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[lm_race.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#49
max(roc)
#0.6673599

lm_race.pred = rep(0,2639)
lm_race.pred[lm_race.probs>.49] = 1
confusionMatrix(lm_race.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1002  463
#          1  407  767
                                         
#                Accuracy : 0.6703         
#                  95% CI : (0.652, 0.6883)
#     No Information Rate : 0.5339         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3357         
#  Mcnemar's Test P-Value : 0.06223        
                                         
#             Sensitivity : 0.7111         
#             Specificity : 0.6236         
#          Pos Pred Value : 0.6840         
#          Neg Pred Value : 0.6533         
#              Prevalence : 0.5339         
#          Detection Rate : 0.3797         
#    Detection Prevalence : 0.5551         
#       Balanced Accuracy : 0.6674

#Gender**
lm_sex = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lm_sex)
lm_sex.probs = predict(lm_sex, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[lm_sex.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#48
max(roc)
#0.6647158

lm_sex.pred = rep(0,2639)
lm_sex.pred[lm_sex.probs>.48] = 1
confusionMatrix(lm_sex.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 943 418
#          1 466 812
                                         
#                Accuracy : 0.665          
#                  95% CI : (0.6467, 0.683)
#     No Information Rate : 0.5339         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3286         
#  Mcnemar's Test P-Value : 0.1139         
                                         
#             Sensitivity : 0.6693         
#             Specificity : 0.6602         
#          Pos Pred Value : 0.6929         
#          Neg Pred Value : 0.6354         
#              Prevalence : 0.5339         
#          Detection Rate : 0.3573         
#    Detection Prevalence : 0.5157         
#       Balanced Accuracy : 0.6647

#Both
lm_both = lm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lm_both)
lm_both.probs = predict(lm_both, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[lm_both.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#50
max(roc)
#0.6664237

lm_both.pred = rep(0,2639)
lm_both.pred[lm_both.probs>.5] = 1
confusionMatrix(lm_both.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1028  488
#          1  381  742
                                          
#                Accuracy : 0.6707          
#                  95% CI : (0.6524, 0.6886)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3347          
#  Mcnemar's Test P-Value : 0.0003234       
                                          
#             Sensitivity : 0.7296          
#             Specificity : 0.6033          
#          Pos Pred Value : 0.6781          
#          Neg Pred Value : 0.6607          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3895          
#    Detection Prevalence : 0.5745          
#       Balanced Accuracy : 0.6664 

#AIC
AIC(lm, lm_race, lm_sex, lm_both)
#         df      AIC
# lm       7 3427.580
# lm_race  8 3429.545
# lm_sex   8 3418.010
# lm_both  9 3419.920

#Logistic
library(ISLR)
#Neither**
glm = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, family=binomial)
summary(glm)
glm.probs = predict(glm, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[glm.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#49
max(roc)
#0.6747154

glm.pred = rep(0,2639)
glm.pred[glm.probs>.49] = 1
confusionMatrix(glm.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1017  458
#          1  392  772
                                          
#                Accuracy : 0.6779          
#                  95% CI : (0.6597, 0.6957)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2e-16         
                                          
#                   Kappa : 0.3506          
#  Mcnemar's Test P-Value : 0.02578         
                                          
#             Sensitivity : 0.7218          
#             Specificity : 0.6276          
#          Pos Pred Value : 0.6895          
#          Neg Pred Value : 0.6632          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3854          
#    Detection Prevalence : 0.5589          
#       Balanced Accuracy : 0.6747  

#Race
glm_race = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, family=binomial)
summary(glm_race)
glm_race.probs = predict(glm_race, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[glm_race.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#48
max(roc)
#0.6726194

glm_race.pred = rep(0,2639)
glm_race.pred[glm_race.probs>.48] = 1
confusionMatrix(glm_race.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 971 423
#          1 438 807
                                          
#                Accuracy : 0.6737          
#                  95% CI : (0.6555, 0.6916)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.345           
#  Mcnemar's Test P-Value : 0.6333          
                                          
#             Sensitivity : 0.6891          
#             Specificity : 0.6561          
#          Pos Pred Value : 0.6966          
#          Neg Pred Value : 0.6482          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3679          
#    Detection Prevalence : 0.5282          
#       Balanced Accuracy : 0.6726                                          

#Gender**
glm_sex = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, family=binomial)
summary(glm_sex)
glm_sex.probs = predict(glm_sex, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[glm_sex.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#50
max(roc)
#0.672418

glm_sex.pred = rep(0,2639)
glm_sex.pred[glm_sex.probs>.5] = 1
confusionMatrix(glm_sex.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1030  475
#          1  379  755
                                          
#                Accuracy : 0.6764          
#                  95% CI : (0.6582, 0.6942)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3466          
#  Mcnemar's Test P-Value : 0.001151        
                                          
#             Sensitivity : 0.7310          
#             Specificity : 0.6138          
#          Pos Pred Value : 0.6844          
#          Neg Pred Value : 0.6658          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3903          
#    Detection Prevalence : 0.5703          
#       Balanced Accuracy : 0.6724   

#Both
glm_both = glm(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, family=binomial)
summary(glm_both)
glm_both.probs = predict(glm_both, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[glm_both.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#50
max(roc)
#0.6694758

glm_both.pred = rep(0,2639)
glm_both.pred[glm_both.probs>.5] = 1
confusionMatrix(glm_both.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1024  477
#          1  385  753
                                          
#                Accuracy : 0.6734          
#                  95% CI : (0.6551, 0.6912)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3406          
#  Mcnemar's Test P-Value : 0.001939        
                                          
#             Sensitivity : 0.7268          
#             Specificity : 0.6122          
#          Pos Pred Value : 0.6822          
#          Neg Pred Value : 0.6617          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3880          
#    Detection Prevalence : 0.5688          
#       Balanced Accuracy : 0.6695 

#AIC
AIC(glm, glm_race, glm_sex, glm_both)
#          df      AIC
# glm       6 3239.672
# glm_race  7 3241.568
# glm_sex   7 3231.363
# glm_both  8 3233.193

#LDA
library(MASS)
#Neither
lda = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(lda)

lda.probs = predict(lda, test, type="response")
lda.class = lda.probs$class

lda.pred = rep(0,2639)
lda.pred[as.numeric(lda.class)==2] = 1
confusionMatrix(lda.class, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1055  509
#          1  354  721
                                          
#                Accuracy : 0.673           
#                  95% CI : (0.6547, 0.6909)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3376          
#  Mcnemar's Test P-Value : 1.587e-07       
                                          
#             Sensitivity : 0.7488          
#             Specificity : 0.5862          
#          Pos Pred Value : 0.6746          
#          Neg Pred Value : 0.6707          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3998          
#    Detection Prevalence : 0.5926          
#       Balanced Accuracy : 0.6675

roc_lda = roc(test$two_year_recid, lda.pred)
auc(roc_lda)
#0.6675

#Race
lda_race = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(lda_race)

lda_race.probs = predict(lda_race, test, type="response")
lda_race.class = lda_race.probs$class

lda_race.pred = rep(0,2639)
lda_race.pred[as.numeric(lda_race.class)==2] = 1
confusionMatrix(lda_race.class, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1054  510
#          1  355  720
                                          
#                Accuracy : 0.6722          
#                  95% CI : (0.6539, 0.6901)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3361          
#  Mcnemar's Test P-Value : 1.64e-07        
                                          
#             Sensitivity : 0.7480          
#             Specificity : 0.5854          
#          Pos Pred Value : 0.6739          
#          Neg Pred Value : 0.6698          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3994          
#    Detection Prevalence : 0.5926          
#       Balanced Accuracy : 0.6667 

roc_lda_race = roc(test$two_year_recid, lda_race.pred)
auc(roc_lda_race)
#0.6667

#Gender**
lda_sex = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(lda_sex)

lda_sex.probs = predict(lda_sex, test, type="response")
lda_sex.class = lda_sex.probs$class

lda_sex.pred = rep(0,2639)
lda_sex.pred[as.numeric(lda_sex.class)==2] = 1
confusionMatrix(lda_sex.class, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1021  488
#          1  388  742
                                         
#                Accuracy : 0.6681         
#                  95% CI : (0.6497, 0.686)
#     No Information Rate : 0.5339         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.3296         
#  Mcnemar's Test P-Value : 0.0008231      
                                         
#             Sensitivity : 0.7246         
#             Specificity : 0.6033         
#          Pos Pred Value : 0.6766         
#          Neg Pred Value : 0.6566         
#              Prevalence : 0.5339         
#          Detection Rate : 0.3869         
#    Detection Prevalence : 0.5718         
#       Balanced Accuracy : 0.6639   

roc_lda_sex = roc(test$two_year_recid, lda_sex.pred)
auc(roc_lda_sex)
#0.6697

#Both
lda_both = lda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(lda_race)

lda_both.probs = predict(lda_both, test, type="response")
lda_both.class = lda_both.probs$class

lda_both.pred = rep(0,2639)
lda_both.pred[as.numeric(lda_both.class)==2] = 1
confusionMatrix(lda_both.class, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1027  487
#          1  382  743
                                          
#                Accuracy : 0.6707          
#                  95% CI : (0.6524, 0.6886)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3348          
#  Mcnemar's Test P-Value : 0.0004188       
                                          
#             Sensitivity : 0.7289          
#             Specificity : 0.6041          
#          Pos Pred Value : 0.6783          
#          Neg Pred Value : 0.6604          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3892          
#    Detection Prevalence : 0.5737          
#       Balanced Accuracy : 0.6665 

roc_lda_both = roc(test$two_year_recid, lda_both.pred)
auc(roc_lda_both)
#0.6665

#QDA
#Neither
qda = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(qda)
qda.probs = predict(qda, test, type="response")
qda.class = qda.probs$class

qda.pred = rep(0,2639)
qda.pred[as.numeric(qda.class)==2] = 1
confusionMatrix(qda.class, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1261  834
#          1  148  396
                                          
#                Accuracy : 0.6279          
#                  95% CI : (0.6091, 0.6464)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2249          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8950          
             Specificity : 0.3220          
#          Pos Pred Value : 0.6019          
#          Neg Pred Value : 0.7279          
#              Prevalence : 0.5339          
#          Detection Rate : 0.4778          
#    Detection Prevalence : 0.7939          
#       Balanced Accuracy : 0.6085  

roc_qda = roc(test$two_year_recid, qda.pred)
auc(roc_qda)
#0.6085

#Race
qda_race = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(qda_race)
qda_race.probs = predict(qda_race, test, type="response")
qda_race.class = qda_race.probs$class

qda_race.pred = rep(0,2639)
qda_race.pred[as.numeric(qda_race.class)==2] = 1
confusionMatrix(qda_race.class, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1260  833
#          1  149  397
                                          
#                Accuracy : 0.6279          
#                  95% CI : (0.6091, 0.6464)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.225           
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8943          
             Specificity : 0.3228          
#          Pos Pred Value : 0.6020          
#          Neg Pred Value : 0.7271          
#              Prevalence : 0.5339          
#          Detection Rate : 0.4775          
#    Detection Prevalence : 0.7931          
#       Balanced Accuracy : 0.6085  

roc_qda_race = roc(test$two_year_recid, qda_race.pred)
auc(roc_qda_race)
#0.6085

#Gender
qda_sex = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(qda_sex)
qda_sex.probs = predict(qda_sex, test, type="response")
qda_sex.class = qda_sex.probs$class

qda_sex.pred = rep(0,2639)
qda_sex.pred[as.numeric(qda_sex.class)==2] = 1
confusionMatrix(qda_sex.class, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1260  822
#          1  149  408
                                          
#                Accuracy : 0.6321          
#                  95% CI : (0.6133, 0.6505)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2341          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8943          
             Specificity : 0.3317          
#          Pos Pred Value : 0.6052          
#          Neg Pred Value : 0.7325          
#              Prevalence : 0.5339          
#          Detection Rate : 0.4775          
#    Detection Prevalence : 0.7889          
#       Balanced Accuracy : 0.6130   

roc_qda_sex = roc(test$two_year_recid, qda_sex.pred)
auc(roc_qda_sex)
#0.613

#Both
qda_both = qda(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(qda_race)
qda_both.probs = predict(qda_both, test, type="response")
qda_both.class = qda_both.probs$class

qda_both.pred = rep(0,2639)
qda_both.pred[as.numeric(qda_both.class)==2] = 1
confusionMatrix(qda_both.class, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1260  817
#          1  149  413
                                          
#                Accuracy : 0.634           
#                  95% CI : (0.6152, 0.6524)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2382          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
             Sensitivity : 0.8943          
             Specificity : 0.3358          
#          Pos Pred Value : 0.6066          
#          Neg Pred Value : 0.7349          
#              Prevalence : 0.5339          
#          Detection Rate : 0.4775          
#    Detection Prevalence : 0.7870          
#       Balanced Accuracy : 0.6150    

roc_qda_both = roc(test$two_year_recid, qda_both.pred)
auc(roc_qda_both)
#0.615

#KNN
library(class)
attach(df_race)
train.y = two_year_recid[n.train]
test.y = two_year_recid[n.test]
#Neither
train.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.train,]
test.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[n.test,]
errors=rep(0,20)
mean.errors=rep(0,30)
for (j in 51:80){
	for(i in 1:20){
		set.seed(i)
		train = sample(1:n,n*.5)
		test = (-train)
		df_race.train = df_race[train,]
		df_race.test = df_race[test,]

		library(class)
		train.X = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[train,]
		test.X = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge)[test,]
		train.Y = two_year_recid[train]
		#KNN
		set.seed(i)
		knn.pred = knn(train.X, test.X, train.Y,k=j)
		errors[i]=mean(knn.pred != two_year_recid[test])
	}
	mean.errors[j-50]=mean(errors)
}

which.min(mean.errors)
#28
min(mean.errors)
#0.3243085
set.seed(1)
knn.probs = knn(train.x, test.x, train.y, k=28+50)

knn.pred = rep(0,2639)
knn.pred[as.numeric(knn.probs)==2] = 1
confusionMatrix(knn.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1073  554
#          1  336  676
                                          
#                Accuracy : 0.6628          
#                  95% CI : (0.6444, 0.6808)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3147          
#  Mcnemar's Test P-Value : 3.494e-13       
                                          
#             Sensitivity : 0.7615          
#             Specificity : 0.5496          
#          Pos Pred Value : 0.6595          
#          Neg Pred Value : 0.6680          
#              Prevalence : 0.5339          
#          Detection Rate : 0.4066          
#    Detection Prevalence : 0.6165          
#       Balanced Accuracy : 0.6556 

roc_knn = roc(test.y, knn.pred)
auc(roc_knn)
#0.6556

#Race
train_race.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.train,]
test_race.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[n.test,]

errors=rep(0,20)
mean.errors=rep(0,10)
for (j in 41:50){
	for(i in 1:20){
		set.seed(i)
		train = sample(1:n,n*.5)
		test = (-train)
		df_race.train = df_race[train,]
		df_race.test = df_race[test,]

		library(class)
		train_race.X = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[train,]
		test_race.X = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor)[test,]
		train.Y = two_year_recid[train]
		#KNN
		set.seed(i)
		knn_race.pred = knn(train_race.X, test_race.X, train.Y,k=j)
		errors[i]=mean(knn_race.pred != two_year_recid[test])
	}
	mean.errors[j-40]=mean(errors)
}

which.min(mean.errors)
#6
min(mean.errors)
#0.3239295
set.seed(1)
knn_race.probs = knn(train_race.x, test_race.x, train.y, k=6+40)

knn_race.pred = rep(0,2639)
knn_race.pred[as.numeric(knn_race.probs)==2] = 1
confusionMatrix(knn_race.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1069  525
#          1  340  705
                                          
#                Accuracy : 0.6722          
#                  95% CI : (0.6539, 0.6901)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3351          
#  Mcnemar's Test P-Value : 3.945e-10       
                                          
#             Sensitivity : 0.7587          
#             Specificity : 0.5732          
#          Pos Pred Value : 0.6706          
#          Neg Pred Value : 0.6746          
#              Prevalence : 0.5339          
#          Detection Rate : 0.4051          
#    Detection Prevalence : 0.6040          
#       Balanced Accuracy : 0.6659        

roc_knn_race = roc(test.y, knn_race.pred)
auc(roc_knn_race)
#0.6659

#Gender
train_sex.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.train,]
test_sex.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[n.test,]

errors=rep(0,20)
mean.errors=rep(0,30)
for (j in 51:80){
	for(i in 1:20){
		set.seed(i)
		train = sample(1:n,n*.5)
		test = (-train)
		df_race.train = df_race[train,]
		df_race.test = df_race[test,]

		library(class)
		train_sex.X = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[train,]
		test_sex.X = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, gender_factor)[test,]
		train.Y = two_year_recid[train]
		#KNN
		set.seed(i)
		knn_sex.pred = knn(train_sex.X, test_sex.X, train.Y,k=j)
		errors[i]=mean(knn_sex.pred != two_year_recid[test])
	}
	mean.errors[j-50]=mean(errors)
}

which.min(mean.errors)
#22
min(mean.errors)
#0.321978
set.seed(1)
knn_sex.probs = knn(train_sex.x, test_sex.x, train.y, k=22+50)

knn_sex.pred = rep(0,2639)
knn_sex.pred[as.numeric(knn_sex.probs)==2] = 1
confusionMatrix(knn_sex.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1080  528
#          1  329  702
                                         
#                Accuracy : 0.6753         
#                  95% CI : (0.657, 0.6931)
#     No Information Rate : 0.5339         
#     P-Value [Acc > NIR] : < 2.2e-16      
                                         
#                   Kappa : 0.3407         
#  Mcnemar's Test P-Value : 1.346e-11      
                                         
#             Sensitivity : 0.7665         
#             Specificity : 0.5707         
#          Pos Pred Value : 0.6716         
#          Neg Pred Value : 0.6809         
#              Prevalence : 0.5339         
#          Detection Rate : 0.4092         
#    Detection Prevalence : 0.6093         
#       Balanced Accuracy : 0.6686              

roc_knn_sex = roc(test.y, knn_sex.pred)
auc(roc_knn_sex)
#0.6686

#Both**
train_both.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.train,]
test_both.x = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[n.test,]

errors=rep(0,20)
mean.errors=rep(0,30)
for (j in 51:80){
	for(i in 1:20){
		set.seed(i)
		train = sample(1:n,n*.5)
		test = (-train)
		df_race.train = df_race[train,]
		df_race.test = df_race[test,]

		library(class)
		train_both.X = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[train,]
		test_both.X = cbind(age, juv_fel_misd, priors_count, crime_factor, violent_charge, race_factor, gender_factor)[test,]
		train.Y = two_year_recid[train]
		#KNN
		set.seed(i)
		knn_both.pred = knn(train_both.X, test_both.X, train.Y,k=j)
		errors[i]=mean(knn_both.pred != two_year_recid[test])
	}
	mean.errors[j-50]=mean(errors)
}
which.min(mean.errors)
#27
min(mean.errors)
#0.3212959
set.seed(1)
knn_both.probs = knn(train_both.x, test_both.x, train.y, k=27+50)

knn_both.pred = rep(0,2639)
knn_both.pred[as.numeric(knn_both.probs)==2] = 1
confusionMatrix(knn_both.pred, reference=test.y)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1078  525
#          1  331  705
                                          
#                Accuracy : 0.6756          
#                  95% CI : (0.6574, 0.6935)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3417          
#  Mcnemar's Test P-Value : 4.207e-11       
                                          
#             Sensitivity : 0.7651          
#             Specificity : 0.5732          
#          Pos Pred Value : 0.6725          
#          Neg Pred Value : 0.6805          
#              Prevalence : 0.5339          
#          Detection Rate : 0.4085          
#    Detection Prevalence : 0.6074          
#       Balanced Accuracy : 0.6691          

roc_knn_both = roc(test.y, knn_both.pred)
auc(roc_knn_both)
#0.6691

#Tree
library(tree)
set.seed(1)
n <- nrow(df_race)
n.train = sample(1:n, n*.5)
n.test = (-n.train)
train = df_race[n.train,]
test = df_race[n.test,]

#Neither
tree = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(tree)
plot(tree)
text(tree, pretty=0)
tree.probs = predict(tree, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[tree.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#28
max(roc)
#0.6420525

tree.pred = rep(0,2639)
tree.pred[tree.probs>.28] = 1
confusionMatrix(tree.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 760 314
#          1 649 916
                                          
#                Accuracy : 0.6351          
#                  95% CI : (0.6164, 0.6535)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2793          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5394          
#             Specificity : 0.7447          
#          Pos Pred Value : 0.7076          
#          Neg Pred Value : 0.5853          
#              Prevalence : 0.5339          
#          Detection Rate : 0.2880          
#    Detection Prevalence : 0.4070          
#       Balanced Accuracy : 0.6421  

#Pruning
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
prune.probs = predict(prune, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[prune.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#28
max(roc)
#0.6420525

prune.pred = rep(0,2639)
prune.pred[prune.probs>.28] = 1
confusionMatrix(prune.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 760 314
#          1 649 916
                                          
#                Accuracy : 0.6351          
#                  95% CI : (0.6164, 0.6535)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2793          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5394          
#             Specificity : 0.7447          
#          Pos Pred Value : 0.7076          
#          Neg Pred Value : 0.5853          
#              Prevalence : 0.5339          
#          Detection Rate : 0.2880          
#    Detection Prevalence : 0.4070          
#       Balanced Accuracy : 0.6421 

#Random Forest
library(randomForest)
set.seed(1)
rf = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train, ntree=500, type="class")
summary(rf)
rf.probs = predict(rf, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[rf.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#46
max(roc)
#0.6665735

rf.pred = rep(0,2639)
rf.pred[rf.probs>.46] = 1
confusionMatrix(rf.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 970 437
#          1 439 793
                                         
#                Accuracy : 0.6681         
#                  95% CI : (0.6497, 0.686)
#     No Information Rate : 0.5339         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3331         
#  Mcnemar's Test P-Value : 0.973          
                                         
#             Sensitivity : 0.6884         
#             Specificity : 0.6447         
#          Pos Pred Value : 0.6894         
#          Neg Pred Value : 0.6437         
#              Prevalence : 0.5339         
#          Detection Rate : 0.3676         
#    Detection Prevalence : 0.5332         
#       Balanced Accuracy : 0.6666  

#Race
tree_race = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(tree_race)
plot(tree_race)
text(tree_race, pretty=0)
tree_race.probs = predict(tree_race, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[tree_race.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#28
max(roc)
#0.6420525

tree_race.pred = rep(0,2639)
tree_race.pred[tree_race.probs>.28] = 1
confusionMatrix(tree_race.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 760 314
#          1 649 916
                                          
#                Accuracy : 0.6351          
#                  95% CI : (0.6164, 0.6535)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2793          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5394          
#             Specificity : 0.7447          
#          Pos Pred Value : 0.7076          
#          Neg Pred Value : 0.5853          
#              Prevalence : 0.5339          
#          Detection Rate : 0.2880          
#    Detection Prevalence : 0.4070          
#       Balanced Accuracy : 0.6421  

#Pruning
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
prune_race.probs = predict(prune_race, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[prune_race.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#28
max(roc)
#0.6420525

prune_race.pred = rep(0,2639)
prune_race.pred[prune_race.probs>.28] = 1
confusionMatrix(prune_race.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 760 314
#          1 649 916
                                          
#                Accuracy : 0.6351          
#                  95% CI : (0.6164, 0.6535)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2793          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5394          
#             Specificity : 0.7447          
#          Pos Pred Value : 0.7076          
#          Neg Pred Value : 0.5853          
#              Prevalence : 0.5339          
#          Detection Rate : 0.2880          
#    Detection Prevalence : 0.4070          
#       Balanced Accuracy : 0.6421  

#Random Forest
library(randomForest)
set.seed(1)
rf_race = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train, ntree=500, type="class")
summary(rf_race)
rf_race.probs = predict(rf_race, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[rf_race.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#43
max(roc)
#0.6632963

rf_race.pred = rep(0,2639)
rf_race.pred[rf_race.probs>.43] = 1
confusionMatrix(rf_race.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 939 418
#          1 470 812
                                          
#                Accuracy : 0.6635          
#                  95% CI : (0.6451, 0.6815)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : <2e-16          
                                          
#                   Kappa : 0.3257          
#  Mcnemar's Test P-Value : 0.087           
                                          
#             Sensitivity : 0.6664          
#             Specificity : 0.6602          
#          Pos Pred Value : 0.6920          
#          Neg Pred Value : 0.6334          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3558          
#    Detection Prevalence : 0.5142          
#       Balanced Accuracy : 0.6633  

#Gender
tree_sex = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(tree_sex)
plot(tree_sex)
text(tree_sex, pretty=0)
tree_sex.probs = predict(tree_sex, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[tree_sex.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#28
max(roc)
#0.6420525

tree_sex.pred = rep(0,2639)
tree_sex.pred[tree_sex.probs>.28] = 1
confusionMatrix(tree_sex.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 760 314
#          1 649 916
                                          
#                Accuracy : 0.6351          
#                  95% CI : (0.6164, 0.6535)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2793          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5394          
#             Specificity : 0.7447          
#          Pos Pred Value : 0.7076          
#          Neg Pred Value : 0.5853          
#              Prevalence : 0.5339          
#          Detection Rate : 0.2880          
#    Detection Prevalence : 0.4070          
#       Balanced Accuracy : 0.6421 

#Pruning
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
prune_sex.probs = predict(prune_sex, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[prune_sex.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#28
max(roc)
#0.6420525

prune_sex.pred = rep(0,2639)
prune_sex.pred[prune_sex.probs>.28] = 1
confusionMatrix(prune_sex.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 760 314
#          1 649 916
                                          
#                Accuracy : 0.6351          
#                  95% CI : (0.6164, 0.6535)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2793          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5394          
#             Specificity : 0.7447          
#          Pos Pred Value : 0.7076          
#          Neg Pred Value : 0.5853          
#              Prevalence : 0.5339          
#          Detection Rate : 0.2880          
#    Detection Prevalence : 0.4070          
#       Balanced Accuracy : 0.6421  

#Random Forest
library(randomForest)
set.seed(1)
rf_sex = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train, ntree=500, type="class")
summary(rf_sex)
rf_sex.probs = predict(rf_sex, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[rf_sex.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#43
max(roc)
#0.6674263

rf_sex.pred = rep(0,2639)
rf_sex.pred[rf_sex.probs>.43] = 1
confusionMatrix(rf_sex.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 922 393
#          1 487 837
                                          
#                Accuracy : 0.6665          
#                  95% CI : (0.6482, 0.6845)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3332          
#  Mcnemar's Test P-Value : 0.001718        
                                          
#             Sensitivity : 0.6544          
#             Specificity : 0.6805          
#          Pos Pred Value : 0.7011          
#          Neg Pred Value : 0.6322          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3494          
#    Detection Prevalence : 0.4983          
#       Balanced Accuracy : 0.6674     

#Both
tree_both = tree(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(tree_both)
plot(tree_both)
text(tree_both, pretty=0)
tree_both.probs = predict(tree_both, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[tree_both.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#28
max(roc)
#0.6420525

tree_both.pred = rep(0,2639)
tree_both.pred[tree_both.probs>.28] = 1
confusionMatrix(tree_both.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 760 314
#          1 649 916
                                          
#                Accuracy : 0.6351          
#                  95% CI : (0.6164, 0.6535)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2793          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5394          
#             Specificity : 0.7447          
#          Pos Pred Value : 0.7076          
#          Neg Pred Value : 0.5853          
#              Prevalence : 0.5339          
#          Detection Rate : 0.2880          
#    Detection Prevalence : 0.4070          
#       Balanced Accuracy : 0.6421  

#Pruning
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
prune_both.probs = predict(prune_both, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[prune_both.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#28
max(roc)
#0.6420525

prune_both.pred = rep(0,2639)
prune_both.pred[prune_both.probs>.28] = 1
confusionMatrix(prune_both.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 760 314
#          1 649 916
                                          
#                Accuracy : 0.6351          
#                  95% CI : (0.6164, 0.6535)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.2793          
#  Mcnemar's Test P-Value : < 2.2e-16       
                                          
#             Sensitivity : 0.5394          
#             Specificity : 0.7447          
#          Pos Pred Value : 0.7076          
#          Neg Pred Value : 0.5853          
#              Prevalence : 0.5339          
#          Detection Rate : 0.2880          
#    Detection Prevalence : 0.4070          
#       Balanced Accuracy : 0.6421   

#Random Forest
library(randomForest)
set.seed(1)
rf_both = randomForest(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train, ntree=500, type="class")
summary(rf_both)
rf_both.probs = predict(rf_both, newdata=test)
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[rf_both.probs>(j/100)] = 1
  roc_lm = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_lm)
}
which.max(roc)
#44
max(roc)
#0.6706518

rf_both.pred = rep(0,2639)
rf_both.pred[rf_both.probs>.44] = 1
confusionMatrix(rf_both.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 954 413
#          1 455 817
                                         
#                Accuracy : 0.6711         
#                  95% CI : (0.6528, 0.689)
#     No Information Rate : 0.5339         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3406         
#  Mcnemar's Test P-Value : 0.164          
                                         
#             Sensitivity : 0.6771         
#             Specificity : 0.6642         
#          Pos Pred Value : 0.6979         
#          Neg Pred Value : 0.6423         
#              Prevalence : 0.5339         
#          Detection Rate : 0.3615         
#    Detection Prevalence : 0.5180         
#       Balanced Accuracy : 0.6707 

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

x.train = model.matrix(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
y.train = train$two_year_recid
x.test = model.matrix(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=test)
y.test = test$two_year_recid

#Neither
gam = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge, data=train)
summary(gam)
gam.probs = predict(gam, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[gam.probs>(j/100)] = 1
  roc_gam = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_gam)
}

which.max(roc)
#49
max(roc)
#0.6710118

gam.pred = rep(0,2639)
gam.pred[gam.probs>.49] = 1
confusionMatrix(gam.pred, reference=test$two_year_recid)
# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1010  461
#          1  399  769
                                         
#                Accuracy : 0.6741         
#                  95% CI : (0.6559, 0.692)
#     No Information Rate : 0.5339         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3431         
#  Mcnemar's Test P-Value : 0.03752        
                                         
#             Sensitivity : 0.7168         
#             Specificity : 0.6252         
#          Pos Pred Value : 0.6866         
#          Neg Pred Value : 0.6584         
#              Prevalence : 0.5339         
#          Detection Rate : 0.3827         
#    Detection Prevalence : 0.5574         
#       Balanced Accuracy : 0.6710  

cv.out=cv.gamsel(x.train, y.train, alpha = 1, degree = 5)
bestlam=cv.out$lambda.min
bestlam
lasso.mod=gamsel(x.train, y.train, alpha = 1, lambda = bestlam)
lasso.coef=coef(lasso.mod)[,1]
lasso.coef[lasso.coef!=0]

pred.lasso = predict(lasso.mod, newdata = x.test)

#Race
gam_race = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor, data=train)
summary(gam_race)
gam_race.probs = predict(gam_race, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[gam_race.probs>(j/100)] = 1
  roc_gam = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_gam)
}

which.max(roc)
#49
max(roc)
#0.6673599

gam_race.pred = rep(0,2639)
gam_race.pred[gam_race.probs>.49] = 1
confusionMatrix(gam_race.pred, reference=test$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1002  463
#          1  407  767
                                         
#                Accuracy : 0.6703         
#                  95% CI : (0.652, 0.6883)
#     No Information Rate : 0.5339         
#     P-Value [Acc > NIR] : < 2e-16        
                                         
#                   Kappa : 0.3357         
#  Mcnemar's Test P-Value : 0.06223        
                                         
#             Sensitivity : 0.7111         
#             Specificity : 0.6236         
#          Pos Pred Value : 0.6840         
#          Neg Pred Value : 0.6533         
#              Prevalence : 0.5339         
#          Detection Rate : 0.3797         
#    Detection Prevalence : 0.5551         
#       Balanced Accuracy : 0.6674 

#Gender
gam_sex = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + gender_factor, data=train)
summary(gam_sex)
gam_sex.probs = predict(gam_sex, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[gam_sex.probs>(j/100)] = 1
  roc_gam = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_gam)
}

which.max(roc)
#48
max(roc)
#0.6647158

gam_sex.pred = rep(0,2639)
gam_sex.pred[gam_sex.probs>.48] = 1
confusionMatrix(gam_sex.pred, reference=test$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction   0   1
#          0 943 418
#          1 466 812
                                         
#                Accuracy : 0.665          
#                  95% CI : (0.6467, 0.683)
#     No Information Rate : 0.5339         
#     P-Value [Acc > NIR] : <2e-16         
                                         
#                   Kappa : 0.3286         
#  Mcnemar's Test P-Value : 0.1139         
                                         
#             Sensitivity : 0.6693         
#             Specificity : 0.6602         
#          Pos Pred Value : 0.6929         
#          Neg Pred Value : 0.6354         
#              Prevalence : 0.5339         
#          Detection Rate : 0.3573         
#    Detection Prevalence : 0.5157         
#       Balanced Accuracy : 0.6647 

#Both
gam_both = gam(two_year_recid ~ age + juv_fel_misd + priors_count + crime_factor + violent_charge + race_factor + gender_factor, data=train)
summary(gam_both)
gam_both.probs = predict(gam_both, test, type="response")
roc = rep(0,100)
for (j in 1:100){
  pred = rep(0,2639)
  pred[gam_both.probs>(j/100)] = 1
  roc_gam = roc(test$two_year_recid, pred)
  roc[j]=auc(roc_gam)
}

which.max(roc)
#50
max(roc)
#0.6664237

gam_both.pred = rep(0,2639)
gam_both.pred[gam_both.probs>.5] = 1
confusionMatrix(gam_both.pred, reference=test$two_year_recid)

# Confusion Matrix and Statistics

#           Reference
# Prediction    0    1
#          0 1028  488
#          1  381  742
                                          
#                Accuracy : 0.6707          
#                  95% CI : (0.6524, 0.6886)
#     No Information Rate : 0.5339          
#     P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                   Kappa : 0.3347          
#  Mcnemar's Test P-Value : 0.0003234       
                                          
#             Sensitivity : 0.7296          
#             Specificity : 0.6033          
#          Pos Pred Value : 0.6781          
#          Neg Pred Value : 0.6607          
#              Prevalence : 0.5339          
#          Detection Rate : 0.3895          
#    Detection Prevalence : 0.5745          
#       Balanced Accuracy : 0.6664   

