
#Loading Required Libraries

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(dplyr)

#Loading Dataset

train_data<-read.csv("train.csv",sep=",")
labels = train_data['stroke']
train_data = train_data[ -grep('stroke', colnames(train_data))]
#train_data<-train_data[,2:ncol(train_data)-1]

#Using One-Hot Encoding
ohe_feats = c('gender', 'ever_married', 'work_type','Residence_type','smoking_status')

dummies <- dummyVars(~ gender +  ever_married + work_type + Residence_type + smoking_status,data = train_data)
df_all_ohe <- as.data.frame(predict(dummies, newdata = train_data))
ntrain_data <- cbind(train_data[,-c(which(colnames(train_data) %in% ohe_feats))],df_all_ohe)
ntrain_data<-ntrain_data %>% select(smoking_status.smokes,heart_disease,avg_glucose_level,hypertension,
                                    age,gender.Other,work_type.Never_worked,work_type.Private,bmi,Residence_type.Rural,
                                    Residence_type.Urban)

ntrain_data<-cbind(ntrain_data,labels)

#Model creation

xgb <- xgboost(data = data.matrix(ntrain_data[,-1]), 
               label = labels$stroke, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               #eval_metric = "merror",
               objective = "binary:logistic",
            
               nthread = 3
          
)



#Loading Test Data

test_data<-read.csv("test.csv",sep=",")



ohe_feats = c('gender', 'ever_married', 'work_type','Residence_type','smoking_status')
dummies2 <- dummyVars(~ gender +  ever_married + work_type + Residence_type + smoking_status,data = test_data)
df_all_ohe <- as.data.frame(predict(dummies2, newdata = test_data))
ntest_data <- cbind(test_data[,-c(which(colnames(test_data) %in% ohe_feats))],df_all_ohe)


#Prediction time:

stroke <- predict(xgb, data.matrix(ntest_data))
ntest_data<-cbind(ntest_data,stroke)
final<-ntest_data %>% select(id,stroke)


final$stroke<- as.numeric(final$stroke >= 0.5)
write.csv(final, file = "Output.csv",row.names=FALSE)

