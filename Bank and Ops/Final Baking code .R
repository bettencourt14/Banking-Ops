# Credit risk modelling
# final project
# developed by: Alberto di Salvo, Beatriz Frazão, Francisco Bettencourt and Luiza Maia
# December 2020
require('tidyverse')
require('skimr')
require('lubridate')
require('ggplot2')
require('reshape2')
require('gmodels')
require('aod')
#install.packages('PRROC')
#install.packages('curpointr')
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('CRAN')
#install.packages('installr')
library(installr)
library(PRROC)
require('cutpointr')
require('caret')
library(rpart)
library(rpart.plot)
#install.packages('lubridate')
require(lubridate)
# 0. load any required packages



# 0.1 any bespoke functions that are needed 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
Sys.setlocale("LC_TIME", "C")


# 1. Read data in and check it was read properly
setwd("C:/Users/franc/OneDrive/Ambiente de Trabalho/R. treino")
credit_risk_data <- readRDS('dataset.rds')
head(credit_risk_data)
str(credit_risk_data)
summary(credit_risk_data)
View(credit_risk_data)
skim(credit_risk_data) 

# 1.1 some variables are being read in as factors when they should be numerical
credit_risk_data$funded_amnt_inv <- as.double(as.character(credit_risk_data$funded_amnt_inv))
credit_risk_data$int_rate <- as.double(as.character(credit_risk_data$int_rate))
credit_risk_data$installment <- as.double(as.character(credit_risk_data$installment))
credit_risk_data$annual_inc <- as.double(as.character(credit_risk_data$annual_inc))
credit_risk_data$dti <- as.double(as.character(credit_risk_data$dti))
credit_risk_data$revol_util <- as.double(as.character(credit_risk_data$revol_util))
credit_risk_data$out_prncp <- as.double(as.character(credit_risk_data$out_prncp)) 
credit_risk_data$total_pymnt <- as.double(as.character(credit_risk_data$total_pymnt))
credit_risk_data$loan_amnt <- as.double(as.character(credit_risk_data$loan_amnt))

summary(credit_risk_data)

# 1.2 some variables are being read in as factors when they should be dates
credit_risk_data$earliest_cr_line <- paste0('01-', paste0(credit_risk_data$earliest_cr_line))

credit_risk_data$earliest_cr_line <- as_date(credit_risk_data$earliest_cr_line, format='%d-%b-%y')

credit_risk_data$issue_d <- paste0('01-', paste0(credit_risk_data$issue_d))
credit_risk_data$issue_d  <- as_date(credit_risk_data$issue_d, format='%d-%b-%y')

summary(credit_risk_data)

# 2. Create target variable and remove data that is not going to be used for analysis and modelling

# the target variable is created based on the loan_status variable. We are going to eliminate data related to loans that are
# on going, i.e., that we don't know for sure if they will default or not

credit_risk_data_model <- credit_risk_data

credit_risk_data_model$default_flag <- ifelse(credit_risk_data_model$loan_status %in% c('Fully Paid', 'Current') , 0, 1)

credit_risk_data_model <- credit_risk_data_model[-which(colnames(credit_risk_data_model) == 'loan_status')]

table(credit_risk_data_model$default_flag)
round(table(credit_risk_data_model$default_flag)/nrow(credit_risk_data_model),2)

# 3. Exploratory analysis of variables

# 3.1. treat missing values 
# 3.1.1. remove variables that have NA's and too many classes to be used in the predictive models
credit_risk_data_model <- credit_risk_data_model[-which(colnames(credit_risk_data_model) == 'emp_title')]

# 3.1.2. impute missing values on numerical variables
credit_risk_data_model$dti[is.na(credit_risk_data_model$dti)] <- mean(credit_risk_data_model$dti, na.rm=T)
credit_risk_data_model$revol_util[is.na(credit_risk_data_model$revol_util)] <- mean(credit_risk_data_model$revol_util, na.rm=T)
credit_risk_data_model$inq_last_6mths[is.na(credit_risk_data_model$inq_last_6mths)] <- mean(credit_risk_data_model$inq_last_6mths, na.rm=T)

# 3.1.3. impute values that don't make sense/are not possible - annual income cannot be 0 so we will replace these by the average
credit_risk_data_model$annual_inc[which(credit_risk_data_model$annual_inc < 1000)] <- 
  mean(credit_risk_data_model$annual_inc[which(credit_risk_data_model$annual_inc > 1000)], na.rm=T)

summary(credit_risk_data_model)

# 3.2. variable distribution & outliers
varnames <- colnames(credit_risk_data_model)

# 3.2.1. Numeric variables
credit_risk_data_model_numeric <- select_if(credit_risk_data_model, is.numeric)
credit_risk_data_model_numeric <- credit_risk_data_model_numeric[-which(colnames(credit_risk_data_model_numeric) %in% c('id', 'default_flag'))]

numeric_varnames <- colnames(credit_risk_data_model_numeric)

# 3.2.1.1 histogram
for(i in 1:length(numeric_varnames)) {
  ggplot(credit_risk_data_model_numeric, aes(get(paste0(numeric_varnames[i])))) + 
    geom_histogram(binwidth = 10, colour="black", fill="thistle3") +
    labs(x= paste0(numeric_varnames[i]),y= "Frequency" , title = paste0("Plot of ", numeric_varnames[i]))
  ggsave(paste0(numeric_varnames[i], '.png'))
}

for(i in 1:length(numeric_varnames)) {
  ggplot(credit_risk_data_model_numeric, aes(get(paste0(numeric_varnames[i])))) + 
    geom_histogram(binwidth = 4, colour="black", fill="thistle3") +
    labs(x= paste0(numeric_varnames[i]),y= "Frequency" , title = paste0("Plot of ", numeric_varnames[i]))
  ggsave(paste0(numeric_varnames[i], '2.png'))
}

credit_risk_data_model_numeric <- select_if(credit_risk_data_model, is.numeric)
credit_risk_data_model_numeric <- credit_risk_data_model_numeric[-which(colnames(credit_risk_data_model_numeric) %in% c('id', 'default_flag'))]

numeric_varnames <- colnames(credit_risk_data_model_numeric)

# 3.2.1.2 boxplot
for(i in 1:length(numeric_varnames)) {
  jpeg(paste0(numeric_varnames[i], '_boxplot.jpeg'))
  boxplot(credit_risk_data_model_numeric[i],
          main = paste0(numeric_varnames[i]))
  dev.off()
}

# we only have outliers "on the top" so we need to find out the minimum value to be considered an outlier for each variable
for(i in 1:length(numeric_varnames)) {
  print(numeric_varnames[i])
  assign(paste0(numeric_varnames[i], '_minout'), min(sort(boxplot.stats(credit_risk_data_model_numeric[,i])[[4]])))
}
  
# understand how many rows of data are considered to be outliers
# this will inform our decision to include or not include a variable in the subsequent analysis
for(i in 1:length(numeric_varnames)) {
  print(numeric_varnames[i])
  print(length(which(credit_risk_data_model_numeric[which(colnames(credit_risk_data_model_numeric) == numeric_varnames[i])] 
               >= get(paste0(numeric_varnames[i], '_minout')))))
  print(round(length(which(credit_risk_data_model_numeric[which(colnames(credit_risk_data_model_numeric) == numeric_varnames[i])] 
               >= get(paste0(numeric_varnames[i], '_minout')))) / nrow(credit_risk_data_model_numeric), 2) * 100)
  assign(paste0(numeric_varnames[i], '_outperc'),
         round(length(which(credit_risk_data_model_numeric[which(colnames(credit_risk_data_model_numeric) == numeric_varnames[i])] 
                     >= get(paste0(numeric_varnames[i], '_minout')))) / nrow(credit_risk_data_model_numeric), 2) * 100)
}

# we are going to eliminate all variables from the analysis that have more than 5% outliers

for(i in 1:length(numeric_varnames)) {
   if(get(paste0(numeric_varnames[i], '_outperc')) > 5 )
   {
     credit_risk_data_model <- credit_risk_data_model[-which(colnames(credit_risk_data_model) == numeric_varnames[i])]
   }
}

# for the remaining variables we are going to eliminate the rows that are outliers
for(i in 1:length(numeric_varnames)) {
  if(get(paste0(numeric_varnames[i], '_outperc')) <= 5 )
  {
    credit_risk_data_model <- credit_risk_data_model[credit_risk_data_model[,which(colnames(credit_risk_data_model) == numeric_varnames[i])] 
                                                     < get(paste0(numeric_varnames[i], '_minout')),]
  }
}

# 3.2.2. Factor variables
credit_risk_data_model_non_numeric <- select_if(credit_risk_data_model, is.factor)
factor_varnames <- colnames(credit_risk_data_model_non_numeric)

# 3.2.2.1 bar plots
for(i in 1:length(factor_varnames)) {
ggplot(credit_risk_data_model_non_numeric, aes(get(paste0(factor_varnames[i]))) ) + 
  geom_bar(aes()) + 
  scale_fill_discrete(name= paste0(factor_varnames[i])) + 
  labs(x= paste0(factor_varnames[i]) ,y= "Frequency" , title = paste0("Plot of ", factor_varnames[i]))
  ggsave(paste0(factor_varnames[i], '.png'))
}

for(i in 1:length(factor_varnames)) {
  ggplot(credit_risk_data_model_non_numeric, aes(get(paste0(factor_varnames[i]))) ) + 
    geom_bar(aes(fill= as.factor(credit_risk_data_model$default_flag))) + 
    scale_fill_discrete(name= paste0(factor_varnames[i])) + 
    labs(x= paste0(factor_varnames[i]) ,y= "Frequency" , title = paste0("Plot of ", factor_varnames[i]))
  ggsave(paste0(factor_varnames[i], '2.png'))
}

 
   CrossTable(credit_risk_data_model$default_flag, credit_risk_data_model$term)
   CrossTable(credit_risk_data_model$default_flag, credit_risk_data_model$grade)
   CrossTable(credit_risk_data_model$default_flag, credit_risk_data_model$emp_length)
   CrossTable(credit_risk_data_model$default_flag, credit_risk_data_model$home_ownership)
   CrossTable(credit_risk_data_model$default_flag, credit_risk_data_model$verification_status)
   CrossTable(credit_risk_data_model$default_flag, credit_risk_data_model$purpose)
   CrossTable(credit_risk_data_model$default_flag, credit_risk_data_model$addr_state)
   



# there is a category in the emp_length variable that is coded as n/a meaning that the is.na function does not work
# so we will eliminate them manually
credit_risk_data_model <- credit_risk_data_model[-which(credit_risk_data_model$emp_length == 'n/a'),]

# 3.3. correlation
# 3.3.1 numeric variables
credit_risk_data_model_numeric <- select_if(credit_risk_data_model, is.numeric)
credit_risk_data_model_numeric <- credit_risk_data_model_numeric[-which(colnames(credit_risk_data_model_numeric) %in% c('id'))]

cormat <- round(cor(credit_risk_data_model_numeric),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

melted_cormat[which(melted_cormat$Var1 != melted_cormat$Var2 & abs(melted_cormat$value) >= 0.7),]

# 3.4. feature engineering
# 3.4.1 installment to income ratio
credit_risk_data_model$inst_to_inc_ratio <- round(credit_risk_data_model$installment/credit_risk_data_model$annual_inc,2) 

# 3.4.2 open account to total account ratio
credit_risk_data_model$open_to_total_ratio <- round(credit_risk_data_model$open_acc/credit_risk_data_model$total_acc,2) 

# 3.4.3 % of loan still left to be paid
credit_risk_data_model$perc_loan_left <- round(credit_risk_data_model$out_prncp/credit_risk_data_model$loan_amnt,2)

# 3.4.4 month of issue date
credit_risk_data_model$month_issue_date <- month(credit_risk_data_model$issue_d)

# as funded amount, loan amount, funded amount invest and installments are very correlated (over 0.94 correlation factor) we are only going to keep one of them
credit_risk_data_model <- credit_risk_data_model[-which(colnames(credit_risk_data_model) %in% c('funded_amnt', 'loan_amnt', 'funded_amnt_inv'))]


# 4. logistic regression model
# 4.1. split our dataset between training (60%), validation (20%) and test (20%)
set.seed(65748) 
training_rows <- sample(nrow(credit_risk_data_model), nrow(credit_risk_data_model) * 0.6)

training_data <- credit_risk_data_model[training_rows,]
temp_data <- credit_risk_data_model[-training_rows,]

validation_rows <- sample(nrow(temp_data), nrow(temp_data) * 0.5)
validation_data <- temp_data[validation_rows,]
test_data <- temp_data[-validation_rows,]



# standardise continuous variables

# 4.2. build our saturated model
var_names <- colnames(credit_risk_data_model)
var_names <- var_names[-which(var_names %in% c('id', 'default_flag', 'issue_d', 'earliest_cr_line'))]

model.formula <- as.formula(paste('default_flag', paste(var_names, collapse=" + "), sep=" ~ "))
model.formula
full_model <- glm( model.formula , family = binomial, data = training_data)
summary(full_model)

full_model_step <- step(full_model, direction = 'both')

summary(full_model_step)

saveRDS(full_model_step, 'full_model_step.rds')

full_model_step <- readRDS('full_model_step.rds')

# 4.3 some model statistics, tests and checks
# Odds-Ratio
exp(cbind(OddsRatio = coef(full_model_step), confint(full_model_step)))

# Log-likelihood
logLik(full_model_step)

# LIKELIHOOD RATIO TEST
# REDUCED LOGIT MODEL;
# H0: the reduced model is true
# If P-Value < 5% we reject the null hypothesis

full_modelRed = glm(default_flag ~ 1 , family=binomial(link=logit), data = training_data)

summary(full_modelRed) #AIC is lower in our model than on the reduced model

logLik(full_modelRed)

anova(full_modelRed, full_model_step, test="Chisq")
# our p-value ~ 0 so there is evidence to reject the null hypothesis of the reduce model being true

# Test for an overall effect of a variable using the wald.test function of the aod library.
# The order in which the coefficients are given in the table of coefficients is the same as the order of
# The terms in the model. This is important because the wald.test function refers to the coefficients by their order in the model
# If P-Value < (1%, 5%, 10%), the overall effect of the variable is statistically significant

wald.test(b = coef(full_model_step), Sigma = vcov(full_model_step), Terms = 2:4)

# 5. validate our model using our validation dataset
# 5.1. use our full model to predict the degault probability using the validation dataset
validation_data$predictions_fullmodel <- predict(full_model_step, newdata = validation_data, type="response")

# 5.2. build an ROC curve
PRROC_obj <- roc.curve(scores.class0 = validation_data$predictions_fullmodel, weights.class0=validation_data$default_flag,
                       curve=TRUE)
plot(PRROC_obj)
dev.off

######## it seems that our model will have a lot of false negatives, due to the shape of the ROC curve

# 5.3. calculate cut-off point using Youden's Index
cp <- cutpointr(validation_data, predictions_fullmodel, default_flag, 
                method = maximize_metric, metric = youden)

# 5.4. apply cut-off point so that we can build our confusion matrix 
validation_data$predictions_fullmodel_flag <- ifelse(validation_data$predictions_fullmodel >= cp$optimal_cutpoint, 1, 0)
validation_data$predictions_fullmodel_flag <- as.factor(validation_data$predictions_fullmodel_flag)
validation_data$default_flag <- as.factor(validation_data$default_flag)

table(validation_data$predictions_fullmodel_flag, validation_data$default_flag, dnn=c("Predictions", "Reference"))
# FPR
round(length(which(validation_data$predictions_fullmodel_flag == 1 & validation_data$default_flag == 0))/
  length(which(validation_data$default_flag == 0)), 3) * 100

# FNR
round(length(which(validation_data$predictions_fullmodel_flag == 0 & validation_data$default_flag == 1))/
  length(which(validation_data$default_flag == 1)) , 3) * 100

# 5.5. probability of default by deciles
quanti <- quantile(validation_data$predictions_fullmodel, probs = seq(0,1, 0.1))

validation_data$predictions_fullmodel_quant <- ifelse(validation_data$predictions_fullmodel <= quanti[2], 'quant1',
                                                      ifelse(validation_data$predictions_fullmodel <= quanti[3], 'quant2',
                                                      ifelse(validation_data$predictions_fullmodel <= quanti[4], 'quant3',
                                                      ifelse(validation_data$predictions_fullmodel <= quanti[5], 'quant4',
                                                      ifelse(validation_data$predictions_fullmodel <= quanti[6], 'quant5',
                                                      ifelse(validation_data$predictions_fullmodel <= quanti[7], 'quant6',
                                                      ifelse(validation_data$predictions_fullmodel <= quanti[8], 'quant7',
                                                      ifelse(validation_data$predictions_fullmodel <= quanti[9], 'quant8',
                                                      ifelse(validation_data$predictions_fullmodel <= quanti[10], 'quant9',
                                                      'quant10'
                                                      )))))))))

table(validation_data$predictions_fullmodel_quant, validation_data$default_flag, dnn=c("Predictions Deciles", "Reference"))

#6. adjust a decision tree 
dt_model <- rpart(model.formula, data=training_data, method='class')
summary(dt_model)

saveRDS(dt_model, 'dt_model.rds')

dt_model <- readRDS('dt_model.rds')

rpart.plot(dt_model)

# 6.1. calculate cut-off based on validation data
validation_data$predictions_dtmodel <-  predict(dt_model, validation_data, type = 'class')

# 6.2. confusion matrix

table(validation_data$predictions_dtmodel, validation_data$default_flag, dnn=c("Predictions", "Reference"))

# FPR
round(length(which(validation_data$predictions_dtmodel == 1 & validation_data$default_flag == 0))/
        length(which(validation_data$default_flag == 0)), 3) * 100

# FNR
round(length(which(validation_data$predictions_dtmodel == 0 & validation_data$default_flag == 1))/
        length(which(validation_data$default_flag == 1)) , 3) * 100

# 7. compare both models using the testing data

# 7.1. test the logistic regression model
test_data$predictions_fullmodel <- predict(full_model_step, newdata = test_data, type="response")

# we are using the same cut off as calculated as per the validation dataset to understand how the model would perform under unknown circumpstances

test_data$predictions_fullmodel_flag <- ifelse(test_data$predictions_fullmodel >= cp$optimal_cutpoint, 1, 0)
test_data$predictions_fullmodel_flag <- as.factor(test_data$predictions_fullmodel_flag)
test_data$default_flag <- as.factor(test_data$default_flag)

table(test_data$predictions_fullmodel_flag, test_data$default_flag, dnn=c("Predictions", "Reference"))

# FPR
round(length(which(test_data$predictions_fullmodel_flag == 1 & test_data$default_flag == 0))/
        length(which(test_data$default_flag == 0)), 3) * 100

# FNR
round(length(which(test_data$predictions_fullmodel_flag == 0 & test_data$default_flag == 1))/
        length(which(test_data$default_flag == 1)) , 3) * 100

# 7.2. test the decision tree

test_data$predictions_dt <- predict(dt_model, newdata = test_data, type="class")

table(test_data$predictions_dt, test_data$default_flag, dnn=c("Predictions", "Reference"))

# FPR
round(length(which(test_data$predictions_dt == 1 & test_data$default_flag == 0))/
        length(which(test_data$default_flag == 0)), 3) * 100

# FNR
round(length(which(test_data$predictions_dt == 0 & test_data$default_flag == 1))/
        length(which(test_data$default_flag == 1)) , 3) * 100

