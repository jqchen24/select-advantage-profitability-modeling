library(caret)
library(randomForest)
library(ggplot2)
library(dplyr)
library(gbm)
library(ROCR)
library(foreign)
library(pROC)
library(xgboost)
## Re-run everything with data including distance.
accounts <- read.spss("accounts_SA_historical_snapshots_category.sav", to.data.frame = T)
str(accounts)
head(accounts)
summary(accounts)
colnames(accounts)
is.na(accounts)
ggplot(accounts, aes(x = GP_ptg_improved)) + geom_bar()
prop.table(table(accounts$GP_ptg_improved))
### return the variables that have missing values
sort(colSums(is.na(accounts)), decreasing = T)
## Remove those columns with missing values, except GP_ptg.
accounts <- accounts[is.na(accounts$GP_ptg) == F, ]
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
accounts$GP_goal <- NULL
accounts$GP_6months_a <- NULL
accounts$GP_6months_b <- NULL
accounts$GP_actual <- NULL
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
table(accounts$sagroup)
## Feature engineering
accounts <- mutate(accounts, discount = (WA_S12X - (SALES12X - FINDS12X))/WA_S12X)
# recode discount
summary(accounts$discount)
## Sellertype won't work, because there was no ISA (73-78) before.
# accounts <- mutate(accounts, sellertype = CSG %/% 10000)
accounts$CSG <- NULL
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
colnames(accounts)
# the higher the discount_range, the lower the actual price.
accounts <- mutate(accounts, discount_range = ifelse(discount <= 0.2, 0,
                                                     ifelse(discount <= 0.4, 1,
                                                            ifelse(discount <= 0.6, 2,
                                                                   ifelse(discount <= 0.8, 3, 4)))))
accounts$discount_range <- as.factor(accounts$discount_range)
accounts <- mutate(accounts, SOW_range = ifelse(SOW <= 0.2, 0,
                                                ifelse(SOW <= 0.4, 1,
                                                       ifelse(SOW <= 0.6, 2,
                                                              ifelse(SOW <= 0.8, 3, 4)))))
accounts$SOW_range <- as.factor(accounts$SOW_range)
accounts <- accounts[, c(3, 1:2, 5:200, 4)]
## EDA
ggplot(accounts, aes(x = sales_6months_b, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = cost_6months_b, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = RECENCY, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = discount, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = SOW, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = cvm_score, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = TENURE, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = SALES12X, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = TRANS12X, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = WA_S12X, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = LINES24X, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = CONTACTS, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = EPEDS12X, fill = GP_ptg_improved)) + geom_histogram(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = discount_range, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = SOW_range, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = discount)) + geom_histogram(aes(fill = GP_ptg_improved))
# ggplot(accounts, aes(x = discount, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = indseg1, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = mro_decile, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = INVSOLFLG, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = RECENCY_180, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = INVSOLFLG)) + geom_bar(aes(fill = GP_ptg_improved))
# only one account has Keepstock.
ggplot(accounts, aes(x = dunsstat, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = dunssub, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = dunsman, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = dunsrent, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")
ggplot(accounts, aes(x = sagroup, fill = GP_ptg_improved)) + geom_bar(position = "fill") + ylab("% of accounts")

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
# levels(training$sellertype) <- c(levels(accounts$sellertype), "20", "73", "74", "75", "76",
#                                  "77", "78")
# levels(testing$sellertype) <- c(levels(accounts$sellertype), "20", "73", "74", "75", "76",
#                                 "77", "78")
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
# GP_dollar 
# mtry  logLoss    ROC        Accuracy   Kappa      Sensitivity  Specificity
# 14    0.6000997  0.6848021  0.6893028  0.2286389  0.9086814    0.2921118  
# 16    0.5989557  0.6859710  0.6921720  0.2362331  0.9101675    0.2974840  
# 18    0.5996166  0.6840378  0.6905783  0.2340143  0.9064561    0.2997241  
# 20    0.5982738  0.6870234  0.6897811  0.2340285  0.9029914    0.3037590  

# RF_1000_SA_category
# mtry  logLoss    ROC        Accuracy   Kappa      Sensitivity  Specificity
# 14    0.5051652  0.8188036  0.7468525  0.3807365  0.8898181    0.4605062  
# 16    0.5001026  0.8226297  0.7509964  0.3931812  0.8893405    0.4739057  
# 18    0.4969665  0.8241444  0.7508382  0.3971384  0.8826479    0.4868301  
# 20    0.4926184  0.8276250  0.7533886  0.4054539  0.8812148    0.4973621 
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
ggplot(varImp(RF_1000), top = 20)
ggplot(RF_RFE_1000)
plot(varImp(RF_RFE_1000), top = 20)

pred_RF_1000 <- predict(RF_1000, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_RF_1000[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy: 0.7615
# Kappa: 0.4255
# Sens: 0.5115
# Spec: 0.8862


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
# n.trees = 460, interaction.depth = 5, shrinkage = 0.05 and n.minobsinnode = 20. 
sink("gbm.csv")
print(gbm)
sink()
           
## Added program category
# ROC: 0.8523332  
# Accuracy: 0.7736341  
# Kappa: 0.4685891
# Sens: 0.8711770    
# Spec: 0.5782589

# ROC: 0.8505653  
# Accuracy: 0.7720396  
# Kappa: 0.4660317
# Sens: 0.8678305    
# Spec: 0.5801648
pred_gbm <- predict(gbm, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy: 0.7659
# Kappa: 0.4566
# Sens: 0.5881
# Spec: 0.8547

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
# n.trees = 340, interaction.depth = 11, shrinkage = 0.03 and n.minobsinnode = 20. 
sink("gbm_1.csv")
print(gbm_1)
sink()
# ROC: 0.6972299  
# Accuracy: 0.6956808 
# Kappa: 0.253606552
# Sens: 0.9000205    
# Spec: 0.325713025 
                    
# Program category added.
# ROC: 0.8549271  
# Accuracy: 0.7725194 
# Kappa: 0.46614158
# Sens: 0.8699808    
# Spec: 0.57730084 

# ROC: 0.8517147  
# Accuracy: 0.7691711 
# Kappa: 0.45653164
# Sens: 0.8704581    
# Spec: 0.56628229 
pred_gbm_1 <- predict(gbm_1, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_1[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy: 0.7659
# Kappa: 0.4561
# Sens: 0.5862
# Spec: 0.8556
ROCRpred_1 <- prediction(pred_gbm_1[,2], testing$GP_ptg_improved)
as.numeric(performance(ROCRpred_1, "auc")@y.values)
# ROC: 0.857342


set.seed(80)
gbm_2_range <- train(training[, c(1:198)],
               training[, 199],
               method = "gbm",
               metric = "ROC",
               verbose = F,
               trControl = trainControl(method = "cv", 
                                        number = 5,
                                        summaryFunction = multiClassSummary,
                                        classProbs = TRUE),
               tuneGrid = expand.grid(interaction.depth = c(11, 13, 15, 17),
                                      n.trees = (1:30)*30,
                                      shrinkage = c(0.03, 0.01, 0.005),
                                      n.minobsinnode = 20))
gbm_2_range
# n.trees = 450, interaction.depth = 17, shrinkage = 0.005 and n.minobsinnode = 20. 
# Best
# ROC: 0.6995332  
# Accuracy: 0.6948836  
# Sens: 0.9096721    
# Spec: 0.3059971 
#  n.trees = 600, interaction.depth = 13, shrinkage = 0.01 and n.minobsinnode = 20. 
sink("gbm_2.csv")
print(gbm_2)
sink()
getTrainPerf(gbm_2_range)
plot(varImp(gbm_2), top = 70)
## add recency_180 and decile_1
# ROC: 0.6994166  
# Accuracy: 0.6948843  
# Sens: 0.9079401    
# Spec: 0.3091351 
# gbm_2_transformed with recency_180
# ROC: 0.6993608  
# Accuracy: 0.6964786  
# Sens: 0.9086820    
# Spec: 0.312271145 
## gbm_2
# n.trees = 480, interaction.depth =15, shrinkage = 0.005 and n.minobsinnode = 20. 
# ROC: 0.6993635  
# Accuracy: 0.6964786  
# Sens: 0.9086820    
# Spec: 0.312271145 
pred_gbm_train <- predict(gbm_2, newdata = training[, c(1:196)], type = "prob")
confusionMatrix(pred_gbm_train[, 2] >= 0.25, training$GP_ptg_improved == "Yes", positive = "TRUE")
pred_gbm_2 <- predict(gbm_2, newdata = testing[, c(1:196)], type = "prob")
confusionMatrix(pred_gbm_2[, 2] >= 0.29, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7085          
# Kappa : 0.2855          
# Sensitivity : 0.3441          
# Specificity : 0.9099 
# Accuracy : 0.4974          
# Sensitivity : 0.8728          
# Specificity : 0.2901
ROCRpred_2 <- prediction(pred_gbm_2[,2], testing$GP_ptg_improved)
as.numeric(performance(ROCRpred_2, "auc")@y.values)
# ROC: 0.707951
perf <- performance(ROCRpred_2, "tpr", "fpr")
plot(perf, colorize=T, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(1.2,1.2), 
     avg="threshold", 
     lwd=3)

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
# n.trees = 600, interaction.depth = 13, shrinkage = 0.01
## added program category
# ROC: 0.8557703
# Accuracy: 0.7720394
# Kappa: 0.46334758
# Sens: 0.8726089
# Spec: 0.57059080

# ROC: 0.8543750
# Accuracy: 0.7717212
# Kappa: 0.46469850
# Sens: 0.8685465
# Spec: 0.57776669
pred_gbm_3 <- predict(gbm_3, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_3[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7659 
# Kappa : 0.4534
# Sensitivity : 0.5766         
# Specificity : 0.8604
ROCRpred_3 <- prediction(pred_gbm_3[,2], testing$GP_ptg_improved)
as.numeric(performance(ROCRpred_3, "auc")@y.values)
# ROC: 0.8584097


# similar to gbm_2, increase n.trees.
set.seed(80)
gbm_4 <- train(training[, c(1:196)],
               training[, 197],
               method = "gbm",
               metric = "ROC",
               trControl = trainControl(method = "cv", 
                                        number = 5,
                                        summaryFunction = multiClassSummary,
                                        classProbs = TRUE),
               tuneGrid = expand.grid(interaction.depth = c(6, 8, 11, 13),
                                      n.trees = (1:30)*40,
                                      shrinkage = c(0.01, 0.001),
                                      n.minobsinnode = 20))
gbm_4
sink("gbm_4.csv")
print(gbm_4)
sink()
# 2nd best.
# n.trees = 680, interaction.depth = 13, shrinkage = 0.01 and n.minobsinnode = 20
# ROC: 0.8565844  
# Accuracy: 0.7731557
# Kappa: 0.4681540838
# Sens: 0.8695032
# Spec: 0.5801682099
pred_gbm_4 <- predict(gbm_4, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_4[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.773 
# Kappa : 0.4742
# Sensitivity : 0.6034         
# Specificity : 0.8576
ROCRpred_4 <- prediction(pred_gbm_4[,2], testing$GP_ptg_improved)
as.numeric(performance(ROCRpred_4, "auc")@y.values)
# ROC: 0.8587101

set.seed(80)
gbm_repeated_cv_2 <- train(training[, c(1:196)],
                           training[, 197],
                           method = "gbm",
                           metric = "ROC",
                           trControl = trainControl(method = "repeatedcv", 
                                                    number = 10,
                                                    repeats = 3,
                                                    summaryFunction = multiClassSummary,
                                                    classProbs = TRUE),
                           tuneGrid = expand.grid(interaction.depth = c(6, 8, 11, 13),
                                                  n.trees = (1:30)*20,
                                                  shrinkage = c(0.05, 0.03, 0.01),
                                                  n.minobsinnode = 20))
gbm_repeated_cv_2
# n.trees = 600, interaction.depth = 13, shrinkage = 0.01 and n.minobsinnode = 20.
sink("gbm_repeated_cv_2.csv")
print(gbm_repeated_cv_2)
sink()
# gbm_2
# ROC: 0.8566893  
# Accuracy: 0.7734750  
# Kappa: 0.46734430
# Sens: 0.8726095    
# Spec: 0.57490505

# ROC: 0.8560227  
# Accuracy: 0.7739504  
# Kappa: 0.46987069
# Sens: 0.8704545    
# Spec: 0.58065498
pred_gbm_repeated_2 <- predict(gbm_repeated_cv_2, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_repeated_2[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7691
# Kappa : 0.4595
# Sensitivity : 0.5766          
# Specificity : 0.8652 
ROCRpred_repeated_2 <- prediction(pred_gbm_repeated_2[,2], testing$GP_ptg_improved)
as.numeric(performance(ROCRpred_repeated_2, "auc")@y.values)
# ROC: 0.8582559

# Decrease shrinkage, increase n.trees.
set.seed(80)
gbm_5 <- train(training[, c(1:196)],
               training[, 197],
               method = "gbm",
               metric = "ROC",
               trControl = trainControl(method = "cv", 
                                        number = 5,
                                        summaryFunction = multiClassSummary,
                                        classProbs = TRUE),
               tuneGrid = expand.grid(interaction.depth = c(6, 8, 11, 13),
                                      n.trees = (30:80)*40,
                                      shrinkage = 0.001,
                                      n.minobsinnode = 20))
gbm_5
# n.trees = 3200, interaction.depth = 13, shrinkage = 0.001 and n.minobsinnode = 20. 
sink("gbm_5.csv")
print(gbm_5)
sink()

# ROC: 0.8537308  
# Accuracy: 0.7729963  
# Kappa: 0.4616897
# Sens: 0.8802579
# Spec: 0.5581449
pred_gbm_5 <- predict(gbm_5, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_5[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7659          
# Kappa : 0.4434          
# Sensitivity : 0.5421                    
# Specificity : 0.8776 

# Based on gbm_2, except excluding cost.
set.seed(80)
gbm_6 <- train(training[, c(2:196)],
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
gbm_6
## worst model without cost_6months_b.
# n.trees = 600, interaction.depth = 13, shrinkage = 0.01 and n.minobsinnode = 20
sink("gbm_6.csv")
print(gbm_6)
sink()
# ROC: 0.8526177  
# Accuracy: 0.7720398  
# Kappa: 0.46246783
# Sens: 0.8740441    
# Spec: 0.567722282
pred_gbm_6 <- predict(gbm_6, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_6[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7054 (big drop)
# Kappa : 0.2225 (big drop)
# Sensitivity : 0.26437   (big drop)      
# Specificity : 0.92543 

### sales and cost -- minimum converted to zero.
## Based on gbm_2.
training_new <- training
training_new[training_new$sales_6months_b < 0, ]$sales_6months_b <- 0
training_new[training_new$cost_6months_b < 0, ]$cost_6months_b <- 0
training_new[training_new$SALES12X < 0, ]$SALES12X <- 0
training_new[training_new$SALES24X < 0, ]$SALES24X <- 0
training_new[training_new$WA_S12X < 0, ]$WA_S12X <- 0
training_new[training_new$SALES01 < 0, ]$SALES01 <- 0
set.seed(80)
gbm_7 <- train(training_new[, c(1:196)],
               training_new[, 197],
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
gbm_7
#  n.trees = 600, interaction.depth = 13, shrinkage = 0.01 and n.minobsinnode = 20. 
sink("gbm_7.csv")
print(gbm_7)
sink()
# No improvement over gbm_2.  
# ROC: 0.8565664  
# Accuracy: 0.7733157  
# Kappa: 0.46703791
# Sens: 0.8723706    
# Spec: 0.57490505
testing_new <- testing
testing_new[testing_new$sales_6months_b < 0, ]$sales_6months_b <- 0
testing_new[testing_new$cost_6months_b < 0, ]$cost_6months_b <- 0
testing_new[testing_new$SALES12X < 0, ]$SALES12X <- 0
testing_new[testing_new$SALES24X < 0, ]$SALES24X <- 0
testing_new[testing_new$WA_S12X < 0, ]$WA_S12X <- 0
testing_new[testing_new$SALES01 < 0, ]$SALES01 <- 0
pred_gbm_7 <- predict(gbm_7, newdata = testing_new[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_7[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.771
# Kappa : 0.4663
# Sensitivity : 0.5881          
# Specificity : 0.8623 


# based on gbm_2. Increase n.trees.
set.seed(80)
gbm_8 <- train(training[, c(1:196)],
               training[, 197],
               method = "gbm",
               metric = "ROC",
               trControl = trainControl(method = "cv", 
                                        number = 5,
                                        summaryFunction = multiClassSummary,
                                        classProbs = TRUE),
               tuneGrid = expand.grid(interaction.depth = c(6, 8, 11, 13),
                                      n.trees = (1:30)*40,
                                      shrinkage = c(0.05, 0.03, 0.01),
                                      n.minobsinnode = 20))
gbm_8
sink("gbm_8.csv")
print(gbm_8)
sink()
# n.trees = 640, interaction.depth =13, shrinkage = 0.01 and n.minobsinnode = 20.
     
# ROC: 0.8566912  
# Accuracy: 0.7731566 (drop) 
# Kappa: 0.4668864 (drop)
# Sens: 0.8718924 (drop)   
# Spec: 0.5753824
pred_gbm_8 <- predict(gbm_8, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_gbm_8[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.773 (increase)
# Kappa : 0.47 (increase)
# Sensitivity : 0.5881  (increase)       
# Specificity : 0.8652 (increase)
ROCRpred_8 <- prediction(pred_gbm_8[,2], testing$GP_ptg_improved)
as.numeric(performance(ROCRpred_8, "auc")@y.values)
# ROC: 0.8594646

############################################################################
############################################################################
# COMPARING THE PERFORMANCES OF DIFFERENT MODELS
compare_perf <- resamples(list(gbm_1 = gbm_1, gbm_2 = gbm_2, gbm_3 = gbm_3, gbm_4 = gbm_4, gbm_7 = gbm_7, gbm_8 = gbm_8))
summary(compare_perf)
splom(compare_perf, metric = "ROC")
parallelplot(compare_perf, metric = "ROC")
dotplot(compare_perf)
dotplot(compare_perf, metric = "ROC")
# gbm_8 > gbm_2 > gbm_4
dotplot(compare_perf, metric = "Accuracy")
# gbm_2 > gbm_7 > gbm_8
dotplot(compare_perf, metric = "Sensitivity")
# gbm_2 > gbm_3 > gbm_7
dotplot(compare_perf, metric = "Kappa")
# gbm_4 > gbm_2 > gbm_7
dotplot(compare_perf, metric = "Specificity")
# gbm_4 > gbm_1 > gbm_8
rocDiffs <- diff(compare_perf, metric = "ROC")
summary(rocDiffs)
dotplot(rocDiffs, metric = "ROC")
bwplot(compare_perf, metric = "ROC")

set.seed(80)
NN <- train(data.matrix(training[, c(1:196)]),
            training[, 197],
            method = "nnet",
            metric = "ROC",
            maxit = 500,
            trControl = trainControl(method = "cv", 
                                     number = 5,
                                     summaryFunction = multiClassSummary,
                                     classProbs = TRUE),
            tuneGrid = expand.grid(.size = c(6, 9, 12),
                                   .decay = c(4, 10, 13, 15)))
NN

## Note the use of data.matrix.
set.seed(80)
glmnet <- train(data.matrix(training[, c(1:196)]),
                training[, 197],
                    method = "glmnet",
                    metric = "ROC",
                    trControl = trainControl(method = "cv", 
                                             number = 5,
                                             summaryFunction = multiClassSummary,
                                             classProbs = TRUE),
                    tuneGrid = expand.grid(.alpha = seq(0, 1, 0.05),
                                           .lambda = c(1e-05, 2e-05, 4e-05, 1e-06, 3e-06, 5e-06, 1e-04)))
glmnet
alpha  lambda  ROC        Sens       Spec       ROC SD      Sens SD    
0.00   1e-04   0.6446784  0.8854217  0.2414783  0.01271388  0.008044424


set.seed(80)
SVM_linear <- train(training[, c(1:196)],
                    training[, 197],
                    method = "svmLinear",
                    metric = "Accuracy",
                    trControl = trainControl(method = "cv", 
                                             number = 5,
                                             summaryFunction = defaultSummary,
                                             classProbs = TRUE),
                    tuneGrid = expand.grid(.C = c(0.001, 0.003, 0.005, 0.0005)))
SVM_linear

## xgboost 
## doesn't have built-in feature selection capability
set.seed(80)
xgboost_6 <- train(GP_ptg_improved~ sales_6months_b + cost_6months_b +
                     RECENCY + TENURE + WCAL_S12 + 
                     TRANS24X + SALES12X + SALES24X + WA_S12X +
                     LINES24X + GP24X + indseg1 + 
                     cvm_score + sagroup + SOW + discount,
                   data = training,
                   method = "xgbTree",
                   metric = "ROC",
                   subsample = 0.5, 
                   verbose = 1,
                   trControl = trainControl(method = "cv", 
                                            number = 5,
                                            summaryFunction = multiClassSummary,
                                            classProbs = TRUE),
                   tuneGrid =  expand.grid(nrounds = 1000, #the maximum number of iterations
                                           eta = c(0.01,0.1, 0.001), # shrinkage
                                           max_depth = c(2, 4, 6), # max depth of a tree
                                           gamma = c(1, 3, 5),
                                           colsample_bytree = c(0.5, 0.7),
                                           min_child_weight = 1))
getTrainPerf(xgboost_6)
ggplot(varImp(xgboost_6))

# Keep discount and SOW_range only
# ROC: 0.7022432  
# Accuracy: 0.6966357  
# Kappa: 0.2618542
# Sens: 0.8921012    
# Spec: 0.3427313

# Keep SOW, discount, SOW_range
# ROC: 0.7020488  
# Accuracy: 0.6974334  
# Kappa: 0.2624845
# Sens: 0.8945756    
# Spec: 0.3404892

# keep SOW, discount, SOW_range and discount_range.
# ROC: 0.7022074  
# Accuracy: 0.6953608  
# Kappa: 0.258381
# Sens: 0.8916059    
# Spec: 0.3400417
# replace SOW and discount with range. worse.

# change subsample = 0.7 -- slightly worse

# only remove mro_decile
# BEST
# ROC: 0.7038152  
# Accuracy: 0.696796  
# Kappa: 0.2639209
# Sens: 0.889626    
# Spec: 0.347661

# Remove indseg1 and mro_decile -- slightly worse
# only remove indseg1 worse

# Don't remove indseg1 or mro_decile
# ROC: 0.7035627  
# Accuracy: 0.695999  
# Kappa: 0.2605232
# Sens: 0.8911105    
# Spec: 0.3427283
pred_xgboost_6_train <- predict(xgboost_6, newdata = training, type = "prob")
confusionMatrix(pred_xgboost_6_train[, 2] >= 0.3, training$GP_ptg_improved == "Yes", positive = "TRUE")
# determine optimal cutoff point.
# ROCRpred_xgboost_6_train <- prediction(pred_xgboost_6_train[,2], training$GP_ptg_improved)
# perf <- performance(ROCRpred_xgboost_6_train, "tpr", "fpr")
# cutoffs <- data.frame(cut=perf@alpha.values[[1]], 
#                       fpr=perf@x.values[[1]], 
#                       tpr=perf@y.values[[1]])
# #### select the cutoff point where tpr and 1-fpr is maximum.
# cutoffs <- mutate(cutoffs, optimal_criteria = tpr + 1 - fpr)
# names(cutoffs)
# cutoffs[which.max(cutoffs[,4]),]
training <- mutate(training, score = pred_xgboost_6_train[, 2])
training <- mutate(training, score_group = ifelse(score <= 0.05, 1,
                                                  ifelse(score <= 0.1, 2, 
                                                         ifelse(score <= 0.15, 3,
                                                                ifelse(score <= 0.2, 4,
                                                                       ifelse(score <= 0.25, 5,
                                                                              ifelse(score <= 0.3, 6,
                                                                                     ifelse(score <= 0.35, 7,
                                                                                            ifelse(score <= 0.4, 8,
                                                                                                   ifelse(score <= 0.45, 9, 
                                                                                                          ifelse(score <= 0.5, 10,
                                                                                                                 ifelse(score <= 0.55, 11,
                                                                                                                        ifelse(score <= 0.6, 12,
                                                                                                                               ifelse(score <= 0.65, 13,
                                                                                                                                      ifelse(score <= 0.7, 14,
                                                                                                                                             ifelse(score <= 0.75, 15,
                                                                                                                                                    ifelse(score <= 0.8, 16,
                                                                                                                                                           ifelse(score <= 0.85, 17,
                                                                                                                                                                  ifelse(score <= 0.9, 18,
                                                                                                                                                                         ifelse(score <= 0.95, 19, 20))))))))))))))))))))
table(training$score_group)
tapply(training$GP_ptg, training$score_group, sum)
# GP_ptg switches from negative to positive around 0.35-0.4.  
## according to the results from the hold-out sample below, the cut off point is around 0.3.
## Since we would rather target more customers, we will use 0.3 as the cut off. 
training <- mutate(training, target = ifelse(score >= 0.3, "Yes", "No"))
prop.table(table(training$target))
# 57.5% identified as "good" customers.
pred_xgboost_6 <- predict(xgboost_6, newdata = testing, type = "prob")
testing <- mutate(testing, score = pred_xgboost_6[, 2])
testing <- mutate(testing, score_group = ifelse(score <= 0.05, 1,
                                                  ifelse(score <= 0.1, 2, 
                                                         ifelse(score <= 0.15, 3,
                                                                ifelse(score <= 0.2, 4,
                                                                       ifelse(score <= 0.25, 5,
                                                                              ifelse(score <= 0.3, 6,
                                                                                     ifelse(score <= 0.35, 7,
                                                                                            ifelse(score <= 0.4, 8,
                                                                                                   ifelse(score <= 0.45, 9, 
                                                                                                          ifelse(score <= 0.5, 10,
                                                                                                                 ifelse(score <= 0.55, 11,
                                                                                                                        ifelse(score <= 0.6, 12,
                                                                                                                               ifelse(score <= 0.65, 13,
                                                                                                                                      ifelse(score <= 0.7, 14,
                                                                                                                                             ifelse(score <= 0.75, 15,
                                                                                                                                                    ifelse(score <= 0.8, 16,
                                                                                                                                                           ifelse(score <= 0.85, 17,
                                                                                                                                                                  ifelse(score <= 0.9, 18,
                                                                                                                                                                         ifelse(score <= 0.95, 19, 20))))))))))))))))))))
table(testing$score_group)
tapply(testing$GP_ptg, testing$score_group, sum)
## GP_ptg switches from nagative to positive around 0.3-0.35.
## use 0.3 as the cutoff.
testing <- mutate(testing, target = ifelse(score >= 0.3, "Yes", "No"))
prop.table(table(testing$target))
# 57.5% identified as "good" customers.

confusionMatrix(pred_xgboost_6[, 2] >= 0.3, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7092
# Kappa : 0.2962
# Sensitivity : 0.3710          
# Specificity : 0.8960

ROCRpred_xgboost_6 <- prediction(pred_xgboost_6[,2], testing$GP_ptg_improved)
as.numeric(performance(ROCRpred_xgboost_6, "auc")@y.values)
# ROC: 0.7291822
perf <- performance(ROCRpred_xgboost_6, "tpr", "fpr")
plot(perf, colorize=T, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(1.5,1.5), 
     avg="threshold", 
     lwd=5)


cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
cutoffs
head(subset(cutoffs, fpr < 0.2))
plot(performance(ROCRpred_xgboost_6, measure="lift", x.measure="rpp"), colorize=TRUE)
plot(performance(ROCRpred_xgboost_6, measure="sens", x.measure="spec"), colorize=TRUE)

set.seed(80)
xgboost_5 <- train(GP_ptg_improved~ sales_6months_b + cost_6months_b +
                     RECENCY + TENURE + WCAL_S12 + 
                     TRANS24X + SALES12X + SALES24X + WA_S12X +
                     LINES24X + GP24X + 
                     mro_decile + cvm_score + indseg1 + sagroup + discount +
                     SOW,
                   data = training,
                   method = "xgbTree",
                   metric = "ROC",
                   subsample = 0.5,
                   colssample_bytree = 0.5,
                   trControl = trainControl(method = "cv", 
                                            number = 5,
                                            summaryFunction = multiClassSummary,
                                            classProbs = TRUE),
                   tuneGrid =  expand.grid(nrounds = 1000, #the maximum number of iterations
                                           eta = c(0.01,0.1, 0.001), # shrinkage
                                           max_depth = c(2, 4, 6),
                                           gamma = 1,
                                           colsample_bytree = 1,
                                           min_child_weight = 1))
getTrainPerf(xgboost_5)
ggplot(varImp(xgboost_5))
# ROC: 0.7029158  
# Accuracy: 0.6963179  
# Kappa: 0.2623671
# Sens: 0.8898732    
# Spec: 0.3458653
pred_xgboost_5 <- predict(xgboost_5, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_xgboost_5[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7073
# Kappa : 0.2913
# Sensitivity : 0.3674          
# Specificity : 0.8950

set.seed(80)
xgboost_4 <- train(GP_ptg_improved~ sales_6months_b + cost_6months_b +
                   RECENCY + TENURE + WCAL_S12 + 
                   TRANS24X + SALES12X + SALES24X + WA_S12X +
                    LINES24X + GP24X + 
                   mro_decile + cvm_score + indseg1 + sagroup + discount +
                   SOW,
                 data = training,
                 method = "xgbTree",
                 metric = "ROC",
                 subsample = 0.75,
                 colssample_bytree = 0.75,
                 trControl = trainControl(method = "cv", 
                                          number = 5,
                                          summaryFunction = multiClassSummary,
                                          classProbs = TRUE),
                 tuneGrid =  expand.grid(nrounds = 1000, #the maximum number of iterations
                                         eta = c(0.01,0.1, 0.001), # shrinkage
                                         max_depth = c(2, 6, 10),
                                         gamma = 1,
                                         colsample_bytree = 1,
                                         min_child_weight = 1))
getTrainPerf(xgboost_4)
ggplot(varImp(xgboost_4))
## add some new parameters --subsample, colssample_bytree.
# ROC: 0.7030354  
# Accuracy: 0.6944048  
# Kappa: 0.2585343
# Sens: 0.8871507    
# Spec: 0.3454189
pred_xgboost_4 <- predict(xgboost_4, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_xgboost_4[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.706
# Kappa : 0.2876
# Sensitivity : 0.3638          
# Specificity : 0.8950

# adding recency_180 and decile_1 didn't help at all. All metrics remained the same.
# remove INVSOLFLG and EPEDS12X EPEDN12X IVSLN12X, no change.


# add back GP24X
# ROC: 0.700045  
# Accuracy: 0.6942448  
# Kappa: 0.253657
# Sens: 0.8935852    
# Spec: 0.3333173

# further remove trans12x, GP12x, GP24x.
# ROC: 0.7005076  
# Accuracy: 0.6942443  
# Kappa: 0.2537307
# Sens: 0.8935858    
# Spec: 0.3333203

# further remove RET_S12, mrospend, MAIL12X, EBUN12X
# ROC: 0.6983502  
# Accuracy: 0.6958384  
# Kappa: 0.2566042
# Sens: 0.8963089    
# Spec: 0.3328699

# further remove lines12x, GCOM12X, GCOMN12X, CNTR_S12, trans_3month
# ROC: 0.6978429  
# Accuracy: 0.6947225  
# Kappa: 0.2532748
# Sens: 0.8963079    
# Spec: 0.3297339

# remove contacts, EBUS12X, FINDS12X
# ROC: 0.6973521  
# Accuracy: 0.6928096  
# Kappa: 0.2493051
# Sens: 0.8938333    
# Spec: 0.328841

# ROC: 0.6969849  
# Accuracy: 0.6923313  
# Kappa: 0.2478903
# Sens: 0.893833    
# Spec: 0.3274957
pred_xgboost_4 <- predict(xgboost_4, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_xgboost_4[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7015
# Kappa : 0.279
# Sensitivity : 0.3638          
# Specificity : 0.8881



set.seed(80)
xgboost <- train(GP_ptg_improved~.,
                   data = training,
                   method = "xgbTree",
                   metric = "ROC",
                   trControl = trainControl(method = "cv", 
                                            number = 5,
                                            summaryFunction = multiClassSummary,
                                            classProbs = TRUE),
                   tuneGrid =  expand.grid(nrounds = 1000, #the maximum number of iterations
                                           eta = c(0.01,0.1, 0.001), # shrinkage
                                           max_depth = c(2, 6, 10),
                                           gamma = 1,
                                           colsample_bytree = 1,
                                           min_child_weight = 1))
getTrainPerf(xgboost)
# ROC: 0.6936199  
# Accuracy: 0.6944045  
# Kappa: 0.2498864
# Sens: 0.8997711    
# Spec: 0.322566

pred_xgboost <- predict(xgboost, newdata = testing[,c(1:196)], type = "prob")
confusionMatrix(pred_xgboost[, 2] >= 0.5, testing$GP_ptg_improved == "Yes", positive = "TRUE")
# Accuracy : 0.7066
# Kappa : 0.2837
# Sensitivity : 0.3495          
# Specificity : 0.9040

## Pick gbm_2.
## Build gbm_2 on the full dataset.###############
levels(accounts$GP_ptg_improved) <- c("No", "Yes")
set.seed(80)
gbm_2_full <- train(accounts[, c(1:196)],
                    accounts[, 197],
               method = "gbm",
               metric = "ROC",
               trControl = trainControl(method = "cv", 
                                        number = 10,
                                        summaryFunction = multiClassSummary,
                                        classProbs = TRUE),
               tuneGrid = expand.grid(interaction.depth = c(6, 8, 11, 13),
                                      n.trees = (1:30)*25,
                                      shrinkage = c(0.05, 0.03, 0.01),
                                      n.minobsinnode = 20))

## pick xgboost_6.
## build xgboost on the full dataset.
levels(accounts$GP_ptg_improved) <- c("No", "Yes")
levels(accounts$indseg1) <- c("0", levels(accounts$indseg1))
set.seed(80)
xgboost_6_full <- train(GP_ptg_improved~ sales_6months_b + cost_6months_b +
                          RECENCY + TENURE + WCAL_S12 + 
                          TRANS24X + SALES12X + SALES24X + WA_S12X +
                          LINES24X + GP24X + indseg1 + 
                          cvm_score + sagroup + SOW + discount,
                   data = accounts,
                   method = "xgbTree",
                   metric = "ROC",
                   subsample = 0.5, 
                   verbose = 1,
                   trControl = trainControl(method = "cv", 
                                            number = 10,
                                            summaryFunction = multiClassSummary,
                                            classProbs = TRUE),
                   tuneGrid =  expand.grid(nrounds = 1000, #the maximum number of iterations
                                           eta = 0.01, # shrinkage
                                           max_depth = 2, # max depth of a tree
                                           gamma = 3,
                                           colsample_bytree = 0.7,
                                           min_child_weight = 1))

###########################################################################
## Validate the model using a new list of accounts with signup date between
## November 2015 and April 2016
###########################################################################
new_accounts <- read.spss("validation_accounts_historical_category.sav", to.data.frame = T)
prop.table(table(new_accounts$GP_ptg_improved))
new_accounts$DUNSSBUS <- NULL
new_accounts$DUNSPUBL <- NULL
new_accounts$DISTANCE <- NULL
### return the variables that have missing values
sort(colSums(is.na(new_accounts)), decreasing = T)
## Remove those columns with missing values.
new_accounts <- new_accounts[, colSums(is.na(new_accounts)) == 0]
## Feature engineering
new_accounts <- mutate(new_accounts, discount = (WA_S12X - (SALES12X - FINDS12X))/(WA_S12X + 1))
# recode discount
summary(new_accounts$discount)
new_accounts$CSG <- NULL
new_accounts <- mutate(new_accounts, SOW = SALES12X/mrospend)
new_accounts <- mutate(new_accounts, trans_3month = TRANS01 + TRANS02 + TRANS03)
new_accounts[new_accounts$discount < 0 & is.na(new_accounts$discount) == F,]$discount <- 0
new_accounts[new_accounts$discount > 1,]$discount <- 1
summary(new_accounts$discount)
summary(new_accounts$SOW)
new_accounts[new_accounts$SOW < 0,]$SOW <- 0
new_accounts$signup <- NULL
new_accounts$select_flg <- NULL
new_accounts$GP_6months_b <- NULL
new_accounts$goals_6a <- NULL
new_accounts$goals_6b <- NULL
new_accounts$GP_goal <- NULL
new_accounts$GP_actual <- NULL
new_accounts$EMPHERE <- NULL
new_accounts$EMPTOTAL <- NULL
new_accounts$DUNSDOLS <- NULL
new_accounts$dunspop <- NULL
new_accounts$dunsindu <- NULL
new_accounts$DUNSSQFT <- NULL
new_accounts$GP_ptg_improved <- as.factor(new_accounts$GP_ptg_improved)
new_accounts$indseg1 <- as.factor(new_accounts$indseg1)
new_accounts$dunsstat <- as.factor(new_accounts$dunsstat)
new_accounts$dunssub <- as.factor(new_accounts$dunssub)
new_accounts$dunsman <- as.factor(new_accounts$dunsman)
new_accounts$dunsrent <- as.factor(new_accounts$dunsrent)
new_accounts$INVSOLFLG <- as.factor(new_accounts$INVSOLFLG)
new_accounts$mro_decile <- as.factor(new_accounts$mro_decile)
levels(new_accounts$GP_ptg_improved) <- c("No", "Yes")
names(new_accounts)
new_accounts <- new_accounts[, c(4, 2, 3, 6:199, 5)]
predict_new <- predict(xgboost_6_full, newdata = new_accounts[, c(2:197)], type = "prob")
confusionMatrix(predict_new[, 2] >= 0.3, new_accounts$GP_ptg_improved == "Yes", positive = "TRUE")
new_accounts <- mutate(new_accounts, score = predict_new[, 2])
new_accounts <- mutate(new_accounts, score_group = ifelse(score <= 0.05, 1,
                                                ifelse(score <= 0.1, 2, 
                                                       ifelse(score <= 0.15, 3,
                                                              ifelse(score <= 0.2, 4,
                                                                     ifelse(score <= 0.25, 5,
                                                                            ifelse(score <= 0.3, 6,
                                                                                   ifelse(score <= 0.35, 7,
                                                                                          ifelse(score <= 0.4, 8,
                                                                                                 ifelse(score <= 0.45, 9, 
                                                                                                        ifelse(score <= 0.5, 10,
                                                                                                               ifelse(score <= 0.55, 11,
                                                                                                                      ifelse(score <= 0.6, 12,
                                                                                                                             ifelse(score <= 0.65, 13,
                                                                                                                                    ifelse(score <= 0.7, 14,
                                                                                                                                           ifelse(score <= 0.75, 15,
                                                                                                                                                  ifelse(score <= 0.8, 16,
                                                                                                                                                         ifelse(score <= 0.85, 17,
                                                                                                                                                                ifelse(score <= 0.9, 18,
                                                                                                                                                                       ifelse(score <= 0.95, 19, 20))))))))))))))))))))
table(new_accounts$score_group)
tapply(new_accounts$GP_ptg, new_accounts$score_group, sum)
## GP_ptg switches from negative to positive around 0.5-0.55.
new_accounts <- mutate(new_accounts, target = ifelse(score >= 0.3, "Yes", "No"))
prop.table(table(new_accounts$target))

##################################################
# Score the new dataset -- accounts that are not currently enrolled but eligible.
##################################################
eligible_accounts <- read.spss("new_accounts_profile.sav", to.data.frame = T, use.value.labels = F)
eligible_accounts$DUNSSBUS <- NULL
eligible_accounts$DUNSPUBL <- NULL
eligible_accounts$DISTANCE <- NULL
eligible_accounts$indseg1 <- as.factor(eligible_accounts$indseg1)
eligible_accounts$dunsstat <- as.factor(eligible_accounts$dunsstat)
eligible_accounts$dunssub <- as.factor(eligible_accounts$dunssub)
eligible_accounts$dunsman <- as.factor(eligible_accounts$dunsman)
eligible_accounts$dunsrent <- as.factor(eligible_accounts$dunsrent)
eligible_accounts$INVSOLFLG <- as.factor(eligible_accounts$INVSOLFLG)
eligible_accounts$mro_decile <- as.factor(eligible_accounts$mro_decile)
### return the variables that have missing values
sort(colSums(is.na(eligible_accounts)), decreasing = T)
## Remove those columns with missing values.
eligible_accounts <- eligible_accounts[, colSums(is.na(eligible_accounts)) == 0]
## Feature engineering
eligible_accounts <- mutate(eligible_accounts, discount = (WA_S12X - (SALES12X - FINDS12X))/(WA_S12X + 1))
# recode discount
summary(eligible_accounts$discount)
eligible_accounts <- mutate(eligible_accounts, SOW = SALES12X/mrospend)
eligible_accounts <- mutate(eligible_accounts, trans_3month = TRANS01 + TRANS02 + TRANS03)
eligible_accounts[eligible_accounts$discount < 0 & is.na(eligible_accounts$discount) == F,]$discount <- 0
eligible_accounts[eligible_accounts$discount > 1,]$discount <- 1
summary(eligible_accounts$discount)
summary(eligible_accounts$SOW)
eligible_accounts[eligible_accounts$SOW < 0,]$SOW <- 0
names(eligible_accounts)
eligible_accounts <- eligible_accounts[, c(1, 3, 308:309, 2, 4:307, 310:312)]
eligible_accounts <- mutate(eligible_accounts, sagroup = 'SA Only')
predict_eligible_1 <- predict(xgboost_6_full, newdata = eligible_accounts[, c(3:309, 313, 310:312)], type = "prob")
eligible_accounts <- mutate(eligible_accounts, sagroup = 'SA/SA Plus')
predict_eligible_2 <- predict(xgboost_6_full, newdata = eligible_accounts[, c(2:309, 313, 310:312)], type = "prob")
eligible_accounts <- mutate(eligible_accounts, sagroup = 'SA Plus Only')
predict_eligible_3 <- predict(xgboost_6_full, newdata = eligible_accounts[, c(2:309, 313, 310:312)], type = "prob")
eligible_accounts <- mutate(eligible_accounts, SA_score = predict_eligible_1[, 2])
eligible_accounts <- mutate(eligible_accounts, SA_SAPlus_score = predict_eligible_2[, 2])
eligible_accounts <- mutate(eligible_accounts, SAPlus_score = predict_eligible_3[, 2])
names(eligible_accounts)
eligible_accounts <- mutate(eligible_accounts, target = ifelse(SA_score >= 0.3, "Yes", "No"))
prop.table(table(eligible_accounts$target))
# 64.2% of accounts identified as target. 
eligible_accounts <- mutate(eligible_accounts, target = ifelse(SA_SAPlus_score >= 0.3, "Yes", "No"))
prop.table(table(eligible_accounts$target))
# 97.7% of accounts identified as target. 
eligible_accounts <- mutate(eligible_accounts, target = ifelse(SAPlus_score >= 0.3, "Yes", "No"))
prop.table(table(eligible_accounts$target))
# 96.3% of accounts identified as target. 
summary(eligible_accounts$SA_score)
summary(eligible_accounts$SA_SAPlus_score)
summary(eligible_accounts$SAPlus_score)
## the above numbers suggest that SA_SA Plus customers have the highest score and SA
## customers have the lowest score.
## This is align with what we see with training data below.
ggplot(training, aes(x = score_group, fill = sagroup)) + geom_bar(position = "fill") + ylab("% of accounts")

## by coverage and size.
eligible_accounts <- mutate(eligible_accounts, sellertype = trunc(CSG/10000))
eligible_accounts <- mutate(eligible_accounts, coverage = ifelse(sellertype == 73, "ISA",
                                                                 ifelse(sellertype == 84, "AM",
                                                                        ifelse(sellertype == 89, "ARM", "not covered"))))
table(eligible_accounts$coverage)
eligible_accounts <- mutate(eligible_accounts, target = ifelse(SA_score >= 0.3, "Yes", "No"))
table(eligible_accounts$target, eligible_accounts$coverage)
apply(prop.table(table(eligible_accounts$target, eligible_accounts$coverage),1)*100, 2,  
      function(u) sprintf( "%.1f%%", u))
table(eligible_accounts$target, eligible_accounts$mro_decile)

eligible_accounts <- mutate(eligible_accounts, target = ifelse(SA_SAPlus_score >= 0.3, "Yes", "No"))
table(eligible_accounts$target, eligible_accounts$coverage)
apply(prop.table(table(eligible_accounts$target, eligible_accounts$coverage),1)*100, 2,  
      function(u) sprintf( "%.1f%%", u))
table(eligible_accounts$target, eligible_accounts$mro_decile)

eligible_accounts <- mutate(eligible_accounts, target = ifelse(SAPlus_score >= 0.3, "Yes", "No"))
table(eligible_accounts$target, eligible_accounts$coverage)
apply(prop.table(table(eligible_accounts$target, eligible_accounts$coverage),1)*100, 2,  
      function(u) sprintf( "%.1f%%", u))
table(eligible_accounts$target, eligible_accounts$mro_decile)

