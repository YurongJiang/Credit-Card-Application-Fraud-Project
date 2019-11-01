library(xgboost)
library(MASS)
library(caret)
library(dplyr)
setwd("C:/Users/jyuro/Desktop/Project3/[3] feature selection")

train=read.csv("TrTe21VarWithFraud_Zscaled.csv")
test=read.csv("validation21VarWithFraud_Zscaled.csv")
train$X=NULL
test$X=NULL

ntrain = nrow(train)
set.seed(1)
validate.index = sample(ntrain, 0.3 * ntrain, replace = FALSE)
validate=train[validate.index,]
train=train[-validate.index,]

tr_labels=as.matrix(train[,22])
ts_labels=as.matrix(test[,22])
va_labels=as.matrix(validate[,22])

tr=as.matrix(train[,-22])
ts=as.matrix(test[,-22])
va=as.matrix(validate[,-22])


dtrain <- xgb.DMatrix(data = tr, label= tr_labels)
dtest <- xgb.DMatrix(data = ts, label= ts_labels)
dvali <- xgb.DMatrix(data = va, label= va_labels)


#---------------------------------------------------------------------------------------
# model 1
xgb.model1=xgboost(data=dtrain,
              max.depth=5,
              nround=50,
              early_stopping_rounds=3,
              objective="binary:logistic",
              gamma=1,
              num_parallel_tree = 10, 
              subsample = 0.8,
              colsample_bytree =0.5)

# error rate on train set
pred_tr1=predict(xgb.model1,dtrain)
err_tr1=mean(as.numeric(pred_tr1>0.5)!=tr_labels)
err_tr1 #0.008552502

# error rate on vali set
pred_va1=predict(xgb.model1,dvali)
err_va1=mean(as.numeric(pred_va1>0.5)!=va_labels)
err_va1 #0.00846224

# error rate on test set
pred_te1=predict(xgb.model1,dtest)
err_te1=mean(as.numeric(pred_te1>0.5)!=ts_labels)
err_te1 #0.009003382

##fdr
trainpar = floor(nrow(train)*0.03)
valipar = floor(nrow(validate)*0.03)
testpar = floor(nrow(test)*0.03)

#train
n_tr1 = train %>%
  arrange(desc(pred_tr1)) %>%
  slice(1:trainpar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_tr1/nrow(train[train$FraudLabel == 1,])  #0.5406436

#vali
n_vali1 = validate %>%
  arrange(desc(pred_va1)) %>%
  slice(1:valipar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_vali1/nrow(validate[validate$FraudLabel == 1,]) #0.5523915

#test
n_test1 = test %>%
  arrange(desc(pred_te1)) %>%
  slice(1:testpar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_test1/nrow(test[test$FraudLabel == 1,])  #0.5255658


#---------------------------------------------------------------------------------------
# model 2 (repeat code from last model, just changed parameters combination)
xgb.model2=xgboost(data=dtrain,
                   max.depth=15,
                   nround=50,
                   early_stopping_rounds=3,
                   objective="binary:logistic",
                   gamma=1,
                   num_parallel_tree = 10, 
                   subsample = 0.8,
                   colsample_bytree =0.5)

# error rate on train set
pred_tr2=predict(xgb.model2,dtrain)
err_tr2=mean(as.numeric(pred_tr2>0.5)!=tr_labels)
err_tr2 #0.008465092

# error rate on vali set
pred_va2=predict(xgb.model2,dvali)
err_va2=mean(as.numeric(pred_va2>0.5)!=va_labels)
err_va2 #0.008518228

# error rate on test set
pred_te2=predict(xgb.model2,dtest)
err_te2=mean(as.numeric(pred_te2>0.5)!=ts_labels)
err_te2  #0.009045425


##fdr
#train
n_tr2 = train %>%
  arrange(desc(pred_tr2)) %>%
  slice(1:trainpar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_tr2/nrow(train[train$FraudLabel == 1,])  #0.5407628

#vali
n_vali2 = validate %>%
  arrange(desc(pred_va2)) %>%
  slice(1:valipar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_vali2/nrow(validate[validate$FraudLabel == 1,]) #0.552115

#test
n_test2 = test %>%
  arrange(desc(pred_te2)) %>%
  slice(1:testpar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_test2/nrow(test[test$FraudLabel == 1,]) # 0.5259849


#---------------------------------------------------------------------------------------
# model 3 (repeat code from last model, just changed parameters combination)
xgb.model3=xgboost(data=dtrain,
                   max.depth=10,
                   nround=70,
                   early_stopping_rounds=3,
                   objective="binary:logistic",
                   gamma=1,
                   num_parallel_tree = 10, 
                   subsample = 0.8,
                   colsample_bytree =0.5)


# error rate on train set
pred_tr3=predict(xgb.model3,dtrain)
err_tr3=mean(as.numeric(pred_tr3>0.5)!=tr_labels)
err_tr3 #0.008813019

# error rate on vali set
pred_va3=predict(xgb.model3,dvali)
err_va3=mean(as.numeric(pred_va3>0.5)!=va_labels)
err_va3 #0.008738182

# error rate on test set
pred_te3=predict(xgb.model3,dtest)
err_te3=mean(as.numeric(pred_te3>0.5)!=ts_labels)
err_te3 #0.009201588


##fdr
#train
n_tr3 = train %>%
  arrange(desc(pred_tr3)) %>%
  slice(1:trainpar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_tr3/nrow(train[train$FraudLabel == 1,])  #0.5338498

#vali
n_vali3 = validate %>%
  arrange(desc(pred_va3)) %>%
  slice(1:valipar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_vali3/nrow(validate[validate$FraudLabel == 1,]) #0.5449267

#test
n_test3 = test %>%
  arrange(desc(pred_te3)) %>%
  slice(1:testpar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_test3/nrow(test[test$FraudLabel == 1,]) #52.60%


#---------------------------------------------------------------------------------------
# this one has the best performance
xgb.model4=xgboost(data=dtrain,
                   max.depth=15,
                   nround=50,
                   early_stopping_rounds=3,
                   objective="binary:logistic",
                   gamma=1,
                   num_parallel_tree = 10, 
                   subsample = 0.8,
                   colsample_bytree =0.5)

# error rate on train set
pred_tr4=predict(xgb.model4,dtrain)
err_tr4=mean(as.numeric(pred_tr4>0.5)!=tr_labels)
err_tr4 #0.00844281

# error rate on vali set
pred_va4=predict(xgb.model4,dvali)
err_va4=mean(as.numeric(pred_va4>0.5)!=va_labels)
err_va4 #0.008534225

# error rate on test set
pred_te4=predict(xgb.model4,dtest)
err_te4=mean(as.numeric(pred_te4>0.5)!=ts_labels)
err_te4  #0.009009388


##fdr
#train
n_tr4 = train %>%
  arrange(desc(pred_tr4)) %>%
  slice(1:trainpar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_tr4/nrow(train[train$FraudLabel == 1,])  #0.5410012

#vali
n_vali4 = validate %>%
  arrange(desc(pred_va4)) %>%
  slice(1:valipar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_vali4/nrow(validate[validate$FraudLabel == 1,]) #0.5518385

#test
n_test4 = test %>%
  arrange(desc(pred_te4)) %>%
  slice(1:testpar) %>%
  filter(FraudLabel == 1) %>%
  nrow()
n_test4/nrow(test[test$FraudLabel == 1,]) # 0.5268231


# This model is the best one, so we prepare the data that will be used to plot tables
Fraudlabeltr = train$FraudLabel
XGB_tr=data.frame(Fraudlabeltr,pred_tr4)
colnames(XGB_tr) <- c("Fraud", "Pred")
write.csv(XGB_tr,"XGB_tr.csv")

Fraudlabelva = validate$FraudLabel
XGB_vali=data.frame(Fraudlabelva,pred_va4)
colnames(XGB_vali) <- c("Fraud", "Pred")
write.csv(XGB_vali,"XGB_vali.csv")

Fraudlabelte = test$FraudLabel
XGB_te=data.frame(Fraudlabelte,pred_te4)
colnames(XGB_te) <- c("Fraud", "Pred")
write.csv(XGB_te,"XGB_te.csv")
#---------------------------------------------------------------------------------------