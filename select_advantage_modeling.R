library(foreign)
library(caret)
library(randomForest)
library(ggplot2)
library(dplyr)
## Re-run everything with data including distance.
accounts <- read.spss("accounts_SA_historical_snapshots.sav", to.data.frame = T)
str(accounts)
head(accounts)
summary(accounts)
colnames(accounts)
is.na(accounts)
### return the variables that have missing values
sort(colSums(is.na(accounts)), decreasing = T)
## Remove those columns with missing values.
accounts <- accounts[, colSums(is.na(accounts)) == 0]
sort(colSums(is.na(accounts)), decreasing = T)
## Remove additional variables.
accounts$account <- NULL
accounts$sales_missing <- NULL
accounts$cost_6months_a <- NULL
accounts$sales_6months_a <- NULL
accounts$goals_6a <- NULL
accounts$signup <- NULL
accounts$DUNSSBUS <- NULL
accounts$DUNSPUBL <- NULL
accounts$DISTANCE <- NULL
summary(accounts)
table(accounts$dunssub)
table(accounts$dunsman)
table(accounts$dunsstat)
table(accounts$dunsrent)
prop.table(table(accounts$GP_ptg_improved))

## Convert variables to the right type.
accounts$indseg1 <- as.factor(accounts$indseg1)
accounts$dunsstat <- as.factor(accounts$dunsstat)
accounts$dunssub <- as.factor(accounts$dunssub)
accounts$dunsman <- as.factor(accounts$dunsman)
accounts$dunsrent <- as.factor(accounts$dunsrent)
accounts$INVSOLFLG <- as.factor(accounts$INVSOLFLG)
accounts$mro_decile <- as.factor(accounts$mro_decile)
accounts$GP_ptg_improved <- as.factor(accounts$GP_ptg_improved)
colnames(accounts[, lapply(accounts, is.factor) == T])


## Feature engineering
accounts <- mutate(accounts, discount = (WA_S12X - (SALES12X - FINDS12X))/WA_S12X)
# recode discount
summary(accounts$discount)
accounts <- mutate(accounts, sellertype = CSG %/% 10000)
accounts$CSG <- NULL
accounts$sellertype <- as.factor(accounts$sellertype)
accounts <- mutate(accounts, SOW = SALES12X/mrospend)
accounts <- mutate(accounts, trans_3month = TRANS01 + TRANS02 + TRANS03)

## recode missing values and inf values and negative values.
# for some reason, discount < 0 will include NA values.
accounts[accounts$discount < 0 & is.na(accounts$discount) == F,]$discount <- 0
accounts[is.na(accounts$discount), ]$discount <- mean(accounts$discount, na.rm = T)
accounts[accounts$discount > 1,]$discount <- 1
summary(accounts$discount)
summary(accounts$SOW)
accounts[accounts$SOW < 0,]$SOW <- 0
# accounts[accounts$SOW > 1,]$SOW <- 1
## Reorder variables
accounts <- accounts[, c(1:2, 4:197, 3)]
colnames(accounts)

## set levels
levels(accounts$sellertype) <- c(levels(accounts$sellertype), "20", "73", "74", "75", "76",
                                 "77", "78")

## EDA
ggplot(accounts, aes(x = INVSOLFLG)) + geom_bar(aes(fill = GP_ptg_improved))
# only one account has Keepstock.
ggplot(accounts, aes(x = sales_6months_b)) + geom_histogram(aes(fill = GP_ptg_improved))
ggplot(accounts, aes(x = sales_6months_b, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = cost_6months_b, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = RECENCY, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = discount, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = SOW, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = cvm_score, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = TENURE, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = SALES12X, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = TRANS12X, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = WA_S12X, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = LINES24X, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = CONTACTS, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = discount)) + geom_histogram(aes(fill = GP_ptg_improved))
ggplot(accounts, aes(x = WA_S12X)) + geom_histogram(aes(fill = GP_ptg_improved))
# ggplot(accounts, aes(x = discount, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = indseg1, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = mro_decile, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = INVSOLFLG, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = dunsstat, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = dunssub, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = dunsman, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = dunsrent, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = sellertype, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")

########################################################
# CARET training
########################################################
set.seed(998)
inTraining <- createDataPartition(accounts$GP_ptg_improved, p = 0.8, list = F)
training <- accounts[inTraining,]
testing <- accounts[-inTraining,]
levels(training$GP_ptg_improved) <- c("No", "Yes")
levels(testing$GP_ptg_improved) <- c("No", "Yes")
#### Need to make sure all the categorical predictors have the same levels between 
#### training and testing data, otherwise predict function will throw out errors.
levels(training$indseg1) <- levels(accounts$indseg1)
levels(testing$indseg1) <- levels(accounts$indseg1)
## make sure the levels in training set and prediction set are the same.
levels(training$sellertype) <- c(levels(accounts$sellertype), "20", "73", "74", "75", "76",
                                 "77", "78")
levels(testing$sellertype) <- c(levels(accounts$sellertype), "20", "73", "74", "75", "76",
                                "77", "78")
levels(training$dunsstat) <- levels(accounts$dunsstat)
levels(testing$dunsstat) <- levels(accounts$dunsstat)
levels(training$dunssub) <- levels(accounts$dunssub)
levels(testing$dunssub) <- levels(accounts$dunssub)
levels(training$dunsman) <- levels(accounts$dunsman)
levels(testing$dunsman) <- levels(accounts$dunsman)
levels(training$dunsrent) <- levels(accounts$dunsrent)
levels(testing$dunsrent) <- levels(accounts$dunsrent)

set.seed(80)
RF_1000 <- train(training[, c(1:196)],
                 training[, 197],
                     ntree = 1000,
                     method = "rf", 
                     metric = "ROC",
                     trControl = trainControl(method = "cv", 
                                              number = 5,
                                              summaryFunction = multiClassSummary,
                                              classProbs = TRUE),
                     tuneGrid = expand.grid(mtry = c(14, 16, 18, 20)),
                     do.trace = T)
RF_1000
# RF_1000
# mtry  logLoss    ROC        Accuracy   Kappa      Sensitivity  Specificity  Pos_Pred_Value
# 14    0.5066869  0.8167193  0.7460560  0.3785242  0.8895794    0.4585912    0.7669589     
# 16    0.5014928  0.8206743  0.7502002  0.3913992  0.8886234    0.4729510    0.7715379     
# 18    0.4977579  0.8231296  0.7489243  0.3922182  0.8816916    0.4830035    0.7735460     
# 20    0.4942842  0.8251441  0.7492444  0.3968534  0.8759571    0.4954540    0.7766702
# RF_1000_distance
# mtry  logLoss    ROC        Accuracy   Kappa      Sensitivity  Specificity
# 14    0.5077358  0.8166047  0.7454173  0.3768112  0.8893393    0.4571524  
# 16    0.5031400  0.8195273  0.7473309  0.3845044  0.8862344    0.4691210  
# 18    0.4993234  0.8221122  0.7498812  0.3937105  0.8836040    0.4820465  
# 20    0.4951201  0.8249820  0.7514746  0.4006796  0.8800186    0.4940105  
# RF_500
# mtry  logLoss    ROC        Accuracy   Kappa      Sensitivity  Specificity
# 14    0.5072746  0.8162093  0.7441428  0.3730364  0.8893405    0.4533269  
# 16    0.5010712  0.8207144  0.7530698  0.3988430  0.8900576    0.4786961  
# 18    0.4979687  0.8226681  0.7482872  0.3916230  0.8797797    0.4849185  
# 20    0.4947879  0.8240238  0.7521138  0.4035871  0.8783463    0.4992886 
ggplot(varImp(RF_1000), top = 20)
ggplot(RF_RFE_1000)
plot(varImp(RF_RFE_1000), top = 20)

pred_RF_1000 <- predict(RF_1000, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_RF_1000[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy: 0.7532
# Kappa: 0.404
# Sens: 0.4943
# Spec: 0.8824


set.seed(80)
gbm <- train(training[, c(1:196)],
             training[, 197],
                    method = "gbm",
                    metric = "ROC",
                    trControl = trainControl(method = "cv", 
                                             number = 5,
                                             summaryFunction = multiClassSummary,
                                             classProbs = TRUE),
                    tuneGrid = expand.grid(interaction.depth = c(5, 7, 9),
                                           n.trees = (1:30)*20,
                                           shrinkage = c(0.07, 0.05, 0.1),
                                           n.minobsinnode = 20))
gbm
# n.trees = 220, interaction.depth = 9, shrinkage = 0.05 and n.minobsinnode = 20.
sink("gbm.csv")
print(gbm)
sink()
# ROC: 0.8505653  
# Accuracy: 0.7720396  
# Kappa: 0.4660317
# Sens: 0.8678305    
# Spec: 0.5801648
pred_gbm <- predict(gbm, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy: 0.7679
# Kappa: 0.4597
# Sens: 0.5862
# Spec: 0.8585

set.seed(80)
gbm_1 <- train(training[, c(1:196)],
               training[, 197],
             method = "gbm",
             metric = "ROC",
             trControl = trainControl(method = "cv", 
                                      number = 5,
                                      summaryFunction = multiClassSummary,
                                      classProbs = TRUE),
             tuneGrid = expand.grid(interaction.depth = c(3, 6, 8, 11),
                                    n.trees = (1:30)*20,
                                    shrinkage = c(0.07, 0.05, 0.03),
                                    n.minobsinnode = 20))
gbm_1
# n.trees = 200, interaction.depth = 11, shrinkage = 0.03 and n.minobsinnode = 20. 
sink("gbm_1.csv")
print(gbm_1)
sink()
# ROC: 0.8517147  
# Accuracy: 0.7691711 
# Kappa: 0.45653164
# Sens: 0.8704581    
# Spec: 0.56628229 
pred_gbm_1 <- predict(gbm_1, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_1[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy: 0.7608
# Kappa: 0.4403
# Sens: 0.5651
# Spec: 0.8585
set.seed(80)
gbm_2 <- train(training[, c(1:196)],
               training[, 197],
               method = "gbm",
               metric = "ROC",
               trControl = trainControl(method = "cv", 
                                        number = 5,
                                        summaryFunction = multiClassSummary,
                                        classProbs = TRUE),
               tuneGrid = expand.grid(interaction.depth = c(6, 8, 11, 13),
                                      n.trees = (1:30)*20,
                                      shrinkage = c(0.05, 0.03, 0.01),
                                      n.minobsinnode = 20))
gbm_2
#  n.trees = 600, interaction.depth = 13, shrinkage = 0.01 and n.minobsinnode = 20. 
sink("gbm_2.csv")
print(gbm_2)
sink()
# BEST!!!!
# ROC: 0.8546228  
# Accuracy: 0.7726782  
# Kappa: 0.46700831
# Sens: 0.8692634    
# Spec: 0.57920783
pred_gbm_2 <- predict(gbm_2, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_2[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7653
# Kappa : 0.4543
# Sensitivity : 0.5843          
# Specificity : 0.8556 

set.seed(80)
gbm_3 <- train(training[, c(1:196)],
               training[, 197],
               method = "gbm",
               metric = "ROC",
               trControl = trainControl(method = "cv", 
                                        number = 5,
                                        summaryFunction = multiClassSummary,
                                        classProbs = TRUE),
               tuneGrid = expand.grid(interaction.depth = c(6, 8, 11, 13),
                                      n.trees = (1:30)*20,
                                      shrinkage = c(0.05, 0.03, 0.01),
                                      n.minobsinnode = 10))
gbm_3
sink("gbm_3.csv")
print(gbm_3)
sink()
# n.trees = 580, interaction.depth = 13, shrinkage = 0.01 and n.minobsinnode = 10.  
# ROC: 0.8543750
# Accuracy: 0.7717212
# Kappa: 0.46469850
# Sens: 0.8685465
# Spec: 0.57776669
pred_gbm_3 <- predict(gbm_3, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_3[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7666 
# Kappa : 0.4573
# Sensitivity : 0.5862         
# Specificity : 0.8566