## Load Package
library(rlist)
library(dplyr)
library(gbm)

data = read.csv("data60.csv")
data = data[,-1]


record1=data%>%
  select(record, fraud_label, Days_since_fulladdress, fulladdress30, fulladdress14, 
         fulladdress7, fulladdress3, fulladdress1, Days_since_ssn, 
         ssn30, Days_since_firstname_ssn, Days_since_lastname_ssn, 
         Days_since_fulladdresshomephone, Days_since_nameDOB, ssnnameDOB30, 
         Days_since_ssnnameDOB, fulladdresshomephone30, nameDOB30,
         lastnamessn30, firstnamessn30, fulladdresshomephone14,
         nameDOB14, ssnnameDOB14, fulladdresshomephone7, nameDOB7, 
         ssn7, homephone3, homephone7, zip3_risk)

oot = read.csv("oot.csv")

oot1=oot%>%
  select(record, fraud_label, Days_since_fulladdress, fulladdress30, fulladdress14, 
         fulladdress7, fulladdress3, fulladdress1, Days_since_ssn, 
         ssn30, Days_since_firstname_ssn, Days_since_lastname_ssn, 
         Days_since_fulladdresshomephone, Days_since_nameDOB, ssnnameDOB30, 
         Days_since_ssnnameDOB, fulladdresshomephone30, nameDOB30,
         lastnamessn30, firstnamessn30, fulladdresshomephone14,
         nameDOB14, ssnnameDOB14, fulladdresshomephone7, nameDOB7, 
         ssn7, homephone3, homephone7, zip3_risk)
  
  
train3par = floor(nrow(train)*0.03)
test3par = floor(nrow(test)*0.03)
oot3par = floor(nrow(oot)*0.03)


#for loop
#create a list
#train_fdr=c()
#test_fdr=c()
#oot_fdr=c()

list_train = c()   
list_test = c()
list_oot = c()

#Set Seeds
seed=c(1:10)


for (d in c(4)) {
  
  ### OPTIONS for shrinkage
  # 0.06,0.08,0.1,0.2,0.3,0.4
  
  #to record FDR for each seed 
  #each list should only contain 5 elements
  
  for (s in seed){
  
    set.seed(s)
    smp_size <- floor(0.7 * nrow(record1))
    train_ind <- sample(seq_len(nrow(record1)), size = smp_size)
    test <- record1[-train_ind, ]
    train=record1[train_ind, ]
    
    
    # Train Set
    train1=train
    boost.train_0<-gbm(fraud_label ~.,data=train1,distribution="bernoulli",n.trees=500,
                       interaction.depth=d)
    train1$pred=predict(boost.train_0, newdata = train1, n.trees=500,type = "response")
    
    n_train = train1 %>%
      arrange(desc(pred)) %>%
      slice(1:train3par) %>%
      filter(fraud_label == 1) %>%
      nrow()
    
    # Test set
    test1=test
    test1$pred=predict(boost.train_0, newdata = test1, n.trees=500,type = "response")
    
    n_test = test1 %>%
      arrange(desc(pred)) %>%
      slice(1:test3par) %>%
      filter(fraud_label == 1) %>%
      nrow()
    
    
    ## OOT Set
    oot_m=oot1#for test
    oot_m$pred=predict(boost.train_0, newdata = oot_m, n.trees=500,type = "response")
    
    n_oot = oot_m %>%
      arrange(desc(pred)) %>%
      slice(1:oot3par) %>%
      filter(fraud_label == 1) %>%
      nrow()
    
    list_train = c(list_train, n_train/(nrow(train1[train1$fraud_label == 1,])))
    list_test = c(list_test, n_test/(nrow(test1[test1$fraud_label == 1,])))
    list_oot = c(list_oot, n_oot/(nrow(oot_m[oot_m$fraud_label == 1,])))
    
  }
  
  #train_fdr=c(train_fdr, mean(list_train))
  #test_fdr=c(test_fdr, mean(list_test))
  #oot_fdr=c(oot_fdr, mean(list_oot))
  
}


## Show Results
list_train
list_test
list_oot

?importance()
importance(boost.train_0)
library(caret)
varImp(boost.train_0)

summary(boost.train_0)

##########################################
##export to table 
xx=data.table(test_accuracy)
write.csv(xx,"test_accuracy_5000trees")
xy=data.table(train_accuracy)
write.csv(xy,"train_accuracy_5000trees")
x1=data.table(train_auc)
x2=data.table(test_auc)
write.csv(x1,"train_auc_5000trees")
write.csv(x2,'test_auc_5000trees')


#get importance table and plot
par(las=1)
par(mar=c(3,20,1,3))
summary(boost.train_0)



#################
### Check
set.seed(1)
smp_size <- floor(0.7 * nrow(record1))
train_ind <- sample(seq_len(nrow(record1)), size = smp_size)
test <- record1[-train_ind, ]
train=record1[train_ind, ]


# Train Set
train1=train
boost.train_0<-gbm(fraud_label ~.,data=train1,distribution="bernoulli",n.trees=1000,
                   interaction.depth=3)
train1$pred=predict(boost.train_0, newdata = train1, n.trees=1000,type = "response")


# Test Set

test1 = test
test1$pred=predict(boost.train_0, newdata = test1, n.trees=1000,type = "response")


# OOT Set
oot_m=oot1
oot_m$pred=predict(boost.train_0, newdata = oot_m, n.trees=1000,type = "response")

# Write CSV in R
write.csv(train1, file = "train.csv")
write.csv(test1, file = "test.csv")
write.csv(oot_m, file = "oot.csv")


### Combine to the whole dataset
traintest = rbind(train1, test1)
complete = rbind(traintest, oot_m)

write.csv(complete, file = "complete.csv")


# Results
n_train = train1 %>%
  arrange(desc(pred)) %>%
  slice(1:train3par) %>%
  filter(fraud_label == 1) %>%
  nrow()

n_train = train1 %>%
  arrange(desc(pred)) %>%
  slice(1:train3par)

n_train
n_train/nrow(train1[train1$fraud_label == 1,])

mean(list_oot[1:10])
mean(list_oot[11:20])

sample(1:100, 100)

