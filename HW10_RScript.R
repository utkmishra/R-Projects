rm(list=ls())
library(plyr)
library(caTools)

work_dir = "G:/Study/UIUC/Second Sem/FIN 567 Financial Risk Management/HW10"
fico_tile = 10
revol_util_tile = 10
verification_switch = TRUE

setwd(work_dir)
load('cleansed.Rdata')
load('function.Rdata')
risk_free = read.csv('GS3.csv', stringsAsFactor=FALSE)
print('Finished reading data...')

selected_names = c('id','funded_amnt','term','open_acc','pub_rec','total_acc','int_rate','grade','sub_grade','emp_length','home_ownership','annual_inc','verification_status','loan_status','purpose','title','zip_code','addr_state','dti','delinq_2yrs','fico_range_low','fico_range_high','revol_util','issue_d')
regress_df = df[selected_names]
regress_df = regress_df[regress_df$loan_status!='',]
rm(df)

regress_df$term = factor(as.numeric(substr(regress_df$term,2,3)) / 12, level=c(3,5))
regress_df$int_rate = as.numeric(sub(regress_df$int_rate,pattern='%',replacement=''))
regress_df$work_ten_plus_year = as.numeric(regress_df$emp_length == '10+ years')
regress_df$term_dummy = as.numeric(regress_df$term == 5)
regress_df$rent_dummy = as.numeric(regress_df$home_ownership == 'RENT')
regress_df$mortgage_dummy = as.numeric(regress_df$home_ownership == 'MORTGAGE')
regress_df$verification_dummy = as.numeric(regress_df$verification_status %in% c('Verified','Source Verified'))
regress_df$verification_status = relevel(factor(regress_df$verification_status),ref='Not Verified')
regress_df$LHS = as.numeric(regress_df$loan_status %in% c('Charged Off','Default','Does not meet the credit policy. Status:Charged Off'))
regress_df$purpose = relevel(factor(regress_df$purpose),ref='other')
regress_df$dti = regress_df$dti
regress_df$delinquency_dummy = as.numeric(regress_df$delinq_2yr>0)
regress_df$fico_mid_point = (regress_df$fico_range_low + regress_df$fico_range_high)/2.0
regress_df$revol_util = as.numeric(sub(regress_df$revol_util,pattern='%',replacement=''))
regress_df$issue_d = as.Date(paste('01-',regress_df$issue_d,sep=''),format='%d-%b-%Y')
regress_df$issue_year = as.numeric(format(regress_df$issue_d,'%Y'))
regress_df$issue_quarter = factor(paste(regress_df$issue_year,'Q',ceiling(as.numeric(format(regress_df$issue_d,'%m'))/3),sep=''))
regress_df$issue_year = factor(regress_df$issue_year)

fico_cdf = ecdf(regress_df$fico_mid_point)
revol_cdf = ecdf(regress_df$revol_util)
regress_df$fico_tile = ceiling(fico_cdf(regress_df$fico_mid_point)*fico_tile)
regress_df$revol_util_tile = ceiling(revol_cdf(regress_df$revol_util)*revol_util_tile)
rm(fico_cdf, revol_cdf)

risk_free$DATE = as.Date(risk_free$DATE, format='%m/%d/%y')
regress_df = merge(regress_df, risk_free, by.x='issue_d', by.y='DATE')
regress_df$credit_spread = regress_df$int_rate - ifelse(regress_df$term==3,regress_df$GS3,regress_df$GS5)
rm(risk_free)
print('Finished preparing regression variables...')

#set.seed(42)
model <- glm(regress_df$LHS ~ regress_df$fico_mid_point+regress_df$dti+regress_df$revol_util+regress_df$work_ten_plus_year
             +regress_df$rent_dummy+regress_df$mortgage_dummy+regress_df$verification_dummy+regress_df$term_dummy,
             data = regress_df, family = binomial)
summary(model)
#library(broom)
#glance(model)
RHS <- data.frame(regress_df$fico_mid_point,regress_df$dti,regress_df$revol_util,regress_df$work_ten_plus_year
                  ,regress_df$rent_dummy,regress_df$mortgage_dummy,regress_df$verification_dummy,regress_df$term_dummy)

summary(RHS)

#Q4
########
regress_df$fico1 = ifelse((regress_df$fico_mid_point>=300 & regress_df$fico_mid_point<=650),1,0)
regress_df$fico2 = ifelse((regress_df$fico_mid_point>650 & regress_df$fico_mid_point<=670),1,0)
regress_df$fico3 = ifelse((regress_df$fico_mid_point>670 & regress_df$fico_mid_point<=700),1,0)
regress_df$fico4 = ifelse((regress_df$fico_mid_point>700 & regress_df$fico_mid_point<=730),1,0)
regress_df$fico5 = ifelse((regress_df$fico_mid_point>730 & regress_df$fico_mid_point<=760),1,0)
regress_df$fico6 = ifelse((regress_df$fico_mid_point>760 & regress_df$fico_mid_point<=800),1,0)
regress_df$fico7 = ifelse((regress_df$fico_mid_point>800 & regress_df$fico_mid_point<=850),1,0)


model_1 <- glm(regress_df$LHS ~ regress_df$fico2+regress_df$fico3+regress_df$fico4+regress_df$fico5+regress_df$fico6+regress_df$fico7+regress_df$dti
             +regress_df$revol_util+regress_df$work_ten_plus_year
             +regress_df$rent_dummy+regress_df$mortgage_dummy
             +regress_df$verification_dummy+regress_df$term_dummy,
             data = regress_df, family = binomial)
summary(model_1)
##########
coeff <- c(0.0235701,-0.002677,-0.0935658,0.1322189,-0.1911212,0.2513554,0.6820044,
           -0.9248254,-1.1564725,-1.548857,-1.87755,-2.2130679,-2.5280271)
incpt <- -0.9452573
DTI <- 17.66
Revol_Util <- 56
Work_ten_plus_year <- 0
Rent_Dummy <- 0
Mortgage_Dummy <- 0
Verification_Dummy <- 1
Term_Dummy <- 0
fico2 <- c(0,1,0,0,0,0,0)
fico3 <- c(0,0,1,0,0,0,0)
fico4 <- c(0,0,0,1,0,0,0)
fico5 <- c(0,0,0,0,1,0,0)
fico6 <- c(0,0,0,0,0,1,0)
fico7 <- c(0,0,0,0,0,0,1)


pd <- 1/(1+exp(-incpt-(coeff[1]*DTI)-(coeff[2]*Revol_Util)-(coeff[3]*Work_ten_plus_year)-(coeff[4]*Rent_Dummy)-(coeff[5]*Mortgage_Dummy)-(coeff[6]*Verification_Dummy)-(coeff[7]*Term_Dummy)-(coeff[8]*fico2)-(coeff[9]*fico3)-(coeff[10]*fico4)-(coeff[11]*fico5)-(coeff[12]*fico6)-(coeff[13]*fico7)))

         