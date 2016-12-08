rm(list=ls()) 
cat("\014") 

library("caret")
library(ggplot2)
library("lubridate")
library("doBy")
library(Amelia)
library(ROCR)
library(plyr)
library(pscl)
library(DAAG)
library(pROC)
library(DMwR)
library(reshape2)
library(RColorBrewer)
library(scales)

options(scipen=5)

setwd('XXX')
data <- read.table("./AD.csv",sep=",",header=TRUE,as.is=TRUE)

#Keeping a copy of the original data
data_initial <- data 

head(data)
summary(data)
str(data)

#################################################################################
## Part 1 - Data Preparation
#################################################################################
## Cleaning column names
names(data) <- gsub("\\.","", names(data))

## Replacing NAs 
missing_values_regex <- "-?(9{3,})+[0-9\\.]*"
missing_values_regex2 <- "-(9{5,})+[0-9]$"

data[, c("PPAverageCurrentDBT", "PPAverageCurrentCompanyDBTIndustryDBT", "PPWorstCompanyDBTIndustryDBTInTheLast12Months",
         "AveNoOfDBT01000", "DBTmostrecentmonth", "Numberunpaidaccountsinpastyear", "EquityGearing", "Score1SCORE", 
         "NumberofactiveSHARErecords", "NumberofallSHAREaccounts", "NumberofactiveSHAREaccounts", "paymentstatusless23mos", 
         "paymentstatusless212mos", "paymentstatusless224mos", "paymentstatusless13mos", "paymentstatusless112mos", 
         "paymentstatusless124mos", "paymentstatus03mos", "paymentstatus012mos", "paymentstatus024mos", "Mthlyrpymtactvfxdtermaccts",
         "Numpymtstatus3plus48updates", "currentbalancetoAnnualIncome", "Currbalrevcrdtocurrlimits", "balrevcrdtocurrlimits3mo",
         "balrevcrdtocurrlimits6mo", "NumRevAcctutilge75", "UnemploymentAmong25_39", "AvgnumberofCourtJudgments", "CreditCardRepaymentAmount",
         "NumCashAdvances3Month")] <- apply(data[, c("PPAverageCurrentDBT", "PPAverageCurrentCompanyDBTIndustryDBT", "PPWorstCompanyDBTIndustryDBTInTheLast12Months",
                                                     "AveNoOfDBT01000", "DBTmostrecentmonth", "Numberunpaidaccountsinpastyear", "EquityGearing", "Score1SCORE", 
                                                     "NumberofactiveSHARErecords", "NumberofallSHAREaccounts", "NumberofactiveSHAREaccounts", "paymentstatusless23mos", 
                                                     "paymentstatusless212mos", "paymentstatusless224mos", "paymentstatusless13mos", "paymentstatusless112mos", 
                                                     "paymentstatusless124mos", "paymentstatus03mos", "paymentstatus012mos", "paymentstatus024mos", "Mthlyrpymtactvfxdtermaccts",
                                                     "Numpymtstatus3plus48updates", "currentbalancetoAnnualIncome", "Currbalrevcrdtocurrlimits", "balrevcrdtocurrlimits3mo",
                                                     "balrevcrdtocurrlimits6mo", "NumRevAcctutilge75", "UnemploymentAmong25_39", "AvgnumberofCourtJudgments", "CreditCardRepaymentAmount",
                                                     "NumCashAdvances3Month")], 2, function(x) as.numeric(gsub(missing_values_regex, 'NA', x)))

data[, c("Totalvalueofactiveaccounts","Totalvalueaccountsinsector2","Totalbalancesactiveaccounts",
         "Totallimitsrevacctactive","Valueactvfxdtermaccts")] <- apply(data[, c("Totalvalueofactiveaccounts","Totalvalueaccountsinsector2","Totalbalancesactiveaccounts",
                                                                                "Totallimitsrevacctactive","Valueactvfxdtermaccts")], 2, function(x) as.numeric(gsub(missing_values_regex2, 'NA', x)))
## Removing duplicates
duplicates <- data[duplicated(data$user_id) | duplicated(data$user_id, fromLast = TRUE),]
data <- unique(data)

## Transforming Response Variable and user_id as factors
data$GB_12 <- as.factor(data$GB_12) # 12 month defaults, 1 is a default and 0 is no default
data$user_id <- as.factor(data$user_id)

## Getting rid of columns with too many missing values
sum(!complete.cases(data)) 
missmap(data, main = "Missing Values vs Observed", col = c("grey","#99317e"),
        x.cex = 0.4, y.cex = 0.3, legend = FALSE)

missing_values <- as.data.frame(colSums(is.na(data)))
names(missing_values)[1] <- 'total_missing'
top_missing <- subset(missing_values, total_missing > 963) #more than 20% missing
top_missing$variables <- row.names(top_missing)
row.names(top_missing) <- NULL

data_select <- data[,!names(data) %in% top_missing$ variables]

## Dealing with outliers - replacing by 5 & 95 pct

for (i in 3:48) {
  boxplot(data_select[,i], xlab = names(data_select)[i], main = names(data_select)[i], col="lightgreen")
}


replace_outliers <- function(x) {
  rep <- quantile(x, probs=c(.05, .95), na.rm = TRUE)
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- rep[1]
  y[x > (qnt[2] + H)] <- rep[2]
  y
}

data_select[,c(3:48)] <- apply(data_select[,c(3:48)],2,replace_outliers)

for (i in 3:48) {
  boxplot(data_select[,i], xlab = names(data_select)[i], main = names(data_select)[i], col="lightgreen")
}

summary(data_select)

## Removing variables that have uniform distributions 

for (i in 3:48) {
  hist(data_select[,i], xlab = names(data_select)[i], main = names(data_select)[i], col="lightgreen", breaks="FD",
       xlim=c(quantile(data_select[,i], 0.1, na.rm = TRUE)[[1]],quantile(data_select[,i], 0.9, na.rm = TRUE)[[1]])
  )
}

data_select <- data_select[,!names(data_select) %in% c("Numpymtstatus3plus48updates", "NumberOfMeetingOfCreditors", "Numberunpaidaccountsinpastyear", "NumCashAdvances3Month")]

## Check correlations between variables to check confounders and remove them
data_select_cor <- round(cor(data_select[,3:44], use="pairwise.complete.obs"), 2)
data_select_cor
write.csv(data_select_cor, file = "data_select_cor2.csv")

data_final <- data_select[,c("user_id","GB_12","PPAverageCurrentDBT", "PPWorstCompanyDBTIndustryDBTInTheLast12Months","NumberOfPreviousSearcheslast3m","TotalFixedAssetsAsAPercentageOfTotalAssets",
                             "CapitalEmployedAsAPercentageOfTotalAssets","EquityGearing","G3DirectorsConsentwithFullCAISConsumerCommercialCommercialDelphiscore",
                             "submitted_profit_after_tax_1","submitted_profit_after_tax_2","Score1SCORE","NumberofactiveSHARErecords",
                             "Totalvalueofactiveaccounts","Totallimitsrevacctactive","Mthlyrpymtactvfxdtermaccts","currentbalancetoAnnualIncome", "Currbalrevcrdtocurrlimits",
                             "NumRevAcctutilge75","UnemploymentAmong25_39","AvgnumberofCourtJudgments","CreditCardRepaymentAmount")]

data_final_cor <- round(cor(data_final[,3:22], use="pairwise.complete.obs"), 2)
data_final_cor
write.csv(data_final_cor, file = "data_final_cor2.csv")

row.names(data_final_cor) <- strtrim(row.names(data_final_cor), 25) 
colnames(data_final_cor) <- strtrim(colnames(data_final_cor), 25) 

melted_data_final_cor <- melt(data_final_cor)
head(melted_data_final_cor)

jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
paletteSize <- 256
jBuPuPalette <- jBuPuFun(paletteSize)

ggplot(data = melted_data_final_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()  +
  scale_fill_gradient2(low = jBuPuPalette[1],
                       mid = jBuPuPalette[paletteSize/2],
                       high = jBuPuPalette[paletteSize],
                       name = "Coefficients") +
  xlab("") +
  ylab("") +
  ggtitle("Correlation Coefficients") + 
  theme(legend.position="right", 
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.minor = element_line(colour = "grey"),
        text = element_text(size=14),
        legend.text = element_text(size=12),
        axis.text.x = element_text(angle = 60, hjust = 1, size=9),
        axis.text.y = element_text(size=9),
        legend.justification = "center") 

summary(data_final$GB_12)

sum(!complete.cases(data_final))

## Use only complete cases
data_final_complete <- data_final[complete.cases(data_final),]

#################################################################################
## Part 2 - Fitting Models
#################################################################################
summary(data_final_complete$GB_12)

## Create training and test sets
set.seed(5)
data_train_complete <- data_final_complete[sample(nrow(data_final_complete), 2316), ] 
summary(data_train_complete$GB_12)

data_test_complete <- subset(data_final_complete, !(data_final_complete$user_id %in% data_train_complete$user_id))
summary(data_test_complete$GB_12)

## Stepwise regression modelling
model_start_complete <- glm(GB_12 ~ Score1SCORE, family=binomial(link='logit'), data=data_train_complete[,-1])
summary(model_start_complete)

model_all_complete <- glm(GB_12 ~ ., family = binomial(link='logit'), data = data_train_complete[,-1])
summary(model_all_complete)

model_step_complete <- step(model_start_complete, scope=formula(model_all_complete), direction="forward", k=2, na.rm = TRUE)
summary(model_step_complete)

## Analysis of deviance
anova(model_step_complete,test="Chisq")

## McFadden R^2
pR2(model_step_complete)

#################################################################################
## Part 3 - Assessing Models
#################################################################################
data_test_complete$fitted_results <- predict(model_step_complete,newdata=data_test_complete,type='response')

## Assessing Cutoff point
pr <- prediction(predictions = data_test_complete$fitted_results, labels = data_test_complete$GB_12)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

ss <- performance(pr, "sens", "spec")
plot(ss)

ss@alpha.values[[1]][which.max(ss@x.values[[1]]+ss@y.values[[1]])]
max(ss@x.values[[1]]+ss@y.values[[1]])

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

data_test_complete$fitted_results_cat <- ifelse(data_test_complete$fitted_results > 0.02707302,1,0)

## Classification 
misClasificError <- mean(data_test_complete$fitted_results_cat != data_test_complete$GB_12)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix
confusionMatrix(data=as.factor(data_test_complete$fitted_results_cat), data_test_complete$GB_12, positive = '1')
summary(as.factor(data_test_complete$fitted_results_cat))

#################################################################################
## Part 4 - Rebalancing the Data using Synthetic minority over-sampling technique
#################################################################################
rebalanced_data_final_complete <- SMOTE(GB_12 ~ ., data_final_complete, perc.over = 600, perc.under = 200, k = 5)
summary(rebalanced_data_final_complete$GB_12) #2318

set.seed(6)
rebalanced_data_train_complete <- rebalanced_data_final_complete[sample(nrow(rebalanced_data_final_complete), 1854), ] 
summary(rebalanced_data_train_complete$GB_12)

rebalanced_data_test_complete <- subset(rebalanced_data_final_complete, !(row.names(rebalanced_data_final_complete) %in% row.names(rebalanced_data_train_complete)))
summary(rebalanced_data_test_complete$GB_12)

## Stepwise regression modelling
rebalanced_model_start_complete <- glm(GB_12 ~ Score1SCORE, family=binomial(link='logit'), data=rebalanced_data_train_complete[,-1])
summary(rebalanced_model_start_complete)

rebalanced_model_all_complete <- glm(GB_12 ~ ., family = binomial(link='logit'), data = rebalanced_data_train_complete[,-1])
summary(rebalanced_model_all_complete)

rebalanced_model_step_complete <- step(rebalanced_model_start_complete, scope=formula(rebalanced_model_all_complete), direction="forward", k=2, na.rm = TRUE)
summary(rebalanced_model_step_complete)

## Predicting on test data
rebalanced_data_test_complete$fitted_results <- predict(rebalanced_model_step_complete,newdata=rebalanced_data_test_complete,type='response')

## Assessing Cutoff point
rebalanced_pr <- prediction(predictions = rebalanced_data_test_complete$fitted_results, labels = rebalanced_data_test_complete$GB_12)

rebalanced_prf <- performance(rebalanced_pr, measure = "tpr", x.measure = "fpr")
plot(rebalanced_prf)

rebalanced_ss <- performance(rebalanced_pr, "sens", "spec")
plot(rebalanced_ss)

rebalanced_ss@alpha.values[[1]][which.max(rebalanced_ss@x.values[[1]]+rebalanced_ss@y.values[[1]])]
max(rebalanced_ss@x.values[[1]]+rebalanced_ss@y.values[[1]])

rebalanced_auc <- performance(rebalanced_pr, measure = "auc")
rebalanced_auc <- rebalanced_auc@y.values[[1]]
rebalanced_auc

rebalanced_data_test_complete$fitted_results_cat <- ifelse(rebalanced_data_test_complete$fitted_results > 0.3798109,1,0)

## Classification 
rebalanced_misClasificError <- mean(rebalanced_data_test_complete$fitted_results_cat != rebalanced_data_test_complete$GB_12)
print(paste('Accuracy',1-rebalanced_misClasificError))

# Confusion matrix
confusionMatrix(data=as.factor(rebalanced_data_test_complete$fitted_results_cat), rebalanced_data_test_complete$GB_12, positive = '1')
summary(as.factor(rebalanced_data_test_complete$fitted_results_cat))

## Analysis of deviance
anova <- anova(rebalanced_model_step_complete,test="Chisq")
anova 
write.csv(as.matrix(anova), file = "anova.csv", na = "")

## Coefficients
exp_coef <- exp(rebalanced_model_step_complete$coefficients)
exp_coef
write.csv(as.matrix(exp_coef), file = "exp_coef.csv", na = "")

## McFadden R^2
pR2(rebalanced_model_step_complete)

## Plotting most influencial variables
ggplot(data_final_complete, aes(factor(GB_12), Score1SCORE)) + 
  geom_boxplot(fill = "#99317e") + 
  xlab("GB_12") +
  ylab("Score1SCORE") +
  ggtitle("Score1SCORE") + 
  theme(legend.position="right", 
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.minor = element_blank(),
        text = element_text(size=14),
        legend.text = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.justification = "center") 

ggplot(data_final_complete, aes(factor(GB_12), TotalFixedAssetsAsAPercentageOfTotalAssets)) + 
  geom_boxplot(fill = "#99317e") + 
  xlab("GB_12") +
  ylab("Total Fixed Assets As A Percentage Of Total Assets") +
  ggtitle("Total Fixed Assets As A Percentage Of Total Assets") + 
  theme(legend.position="right", 
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.minor = element_blank(),
        text = element_text(size=14),
        legend.text = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=10),
        legend.justification = "center") 

ggplot(data_final_complete, aes(factor(GB_12), submitted_profit_after_tax_1)) + 
  geom_boxplot(fill = "#99317e") + 
  xlab("GB_12") +
  ylab("Submitted Profit After Tax 1") +
  ggtitle("Submitted Profit After Tax 1") + 
  scale_y_continuous(labels = comma) +
  theme(legend.position="right", 
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.minor = element_blank(),
        text = element_text(size=14),
        legend.text = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=10),
        legend.justification = "center") 

#################################################################################
## Part 5 - Model Validation
#################################################################################
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(GB_12 ~ Score1SCORE + TotalFixedAssetsAsAPercentageOfTotalAssets + 
                   NumberOfPreviousSearcheslast3m + submitted_profit_after_tax_1 + 
                   Totalvalueofactiveaccounts + Currbalrevcrdtocurrlimits + 
                   CapitalEmployedAsAPercentageOfTotalAssets + PPAverageCurrentDBT + 
                   G3DirectorsConsentwithFullCAISConsumerCommercialCommercialDelphiscore + 
                   AvgnumberofCourtJudgments + currentbalancetoAnnualIncome + 
                   NumberofactiveSHARErecords, data=rebalanced_data_train_complete, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit, newdata=rebalanced_data_test_complete)

confusionMatrix(data=pred, rebalanced_data_test_complete$GB_12)
