data<-read.csv("/Users/alisher/Downloads/diabetes.csv")
str(data)
summary(data)
colSums(is.na(data))
cols<-c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
data[cols]<-lapply(data[cols], function(x) ifelse(x == 0, NA, x))
summary(data)
colSums(is.na(data))

#Train and test sets
set.seed(1234)
train_idx<-sample(1:nrow(data), 0.8*nrow(data))
train.data<-data[train_idx,]
test.data<-data[-train_idx,]

#NA changing 
train.data$Glucose[is.na(train.data$Glucose)]<-mean(train.data$Glucose, na.rm=TRUE)
train.data$BMI[is.na(train.data$BMI)]<-mean(train.data$BMI, na.rm=TRUE)
test.data$Glucose[is.na(test.data$Glucose)]<-mean(test.data$Glucose, na.rm=TRUE)
test.data$BMI[is.na(test.data$BMI)]<-mean(test.data$BMI, na.rm=TRUE)
train.data$BloodPressure[is.na(train.data$BloodPressure)]<-median(train.data$BloodPressure, na.rm = TRUE)
test.data$BloodPressure[is.na(test.data$BloodPressure)]<-median(test.data$BloodPressure, na.rm = TRUE)
colSums(is.na(data))

###INSULIN
#LM for insulin 
missing_idx<-which(is.na(train.data$Insulin))
insulin_model<-lm(Insulin~Glucose + BMI + Age + Pregnancies, data=train.data, na.action=na.omit)
summary(insulin_model)
predict_insulin.train<-predict(insulin_model, newdata = train.data[is.na(train.data$Insulin),])
lm.datatrain<-train.data
lm.datatrain$Insulin[missing_idx]<-predict_insulin.train

plot(lm.datatrain$Insulin)

predicted_insulin.test <- predict(insulin_model, newdata = test.data[is.na(test.data$Insulin), ])
lm.datatest<-test.data
lm.datatest$Insulin[is.na(lm.datatest$Insulin)] <- predicted_insulin.test

#PMM by MICE
library(mice)
pmm_model<-mice(train.data[, c("Insulin", "Glucose", "BMI", "Age", "Pregnancies")], method="pmm", m=5, seed=123)
completed_pmm_model<-complete(pmm_model)
summary(completed_pmm_model)
hist(completed_pmm_model$Insulin)
test_pmm_model<-mice(test.data[, c("Insulin", "Glucose", "BMI", "Age", "Pregnancies")], method="pmm", m=5, seed=123)
completed_test_pmm_model<-complete(test_pmm_model)

#KNN 
knn_train<-train.data
knn_test<-test.data
knn_train$Insulin<-kNN(train.data, variable="Insulin", k=5)$Insulin
knn_test$Insulin<-kNN(test.data, variable="Insulin", k=5)$Insulin 

##Insulin average by 3 methods
insulin_train<-cbind(lm.datatrain$Insulin, completed_pmm_model$Insulin, knn_train$Insulin)
colnames(insulin_train)<-c("reg", "knn", "pmm")  
pairs(insulin_train)  
cor(insulin_train)

insulin_test<-cbind(lm.datatest$Insulin, completed_test_pmm_model$Insulin, knn_test$Insulin)
colnames(insulin_test)<-c("reg", "knn", "pmm")  
pairs(insulin_train)  
cor(insulin_test)

train.data$Insulin<-apply(insulin_train,1,mean)
test.data$Insulin<-apply(insulin_test,1,mean)

##
###SkinThickness 
#LM for SkinThickness 
missing_idx<-which(is.na(train.data$SkinThickness))
SkinThickness_model<-lm(SkinThickness~Glucose + BMI + Age + Pregnancies, data=train.data, na.action=na.omit)
summary(SkinThickness_model)
predict_SkinThickness.train<-predict(SkinThickness_model, newdata = train.data[is.na(train.data$SkinThickness),])
lm.datatrain_SkinThickness<-train.data
lm.datatrain_SkinThickness$SkinThickness[missing_idx]<-predict_SkinThickness.train

plot(lm.datatrain_SkinThickness$SkinThickness)

predicted_SkinThickness.test <- predict(SkinThickness_model, newdata = test.data[is.na(test.data$SkinThickness), ])
lm.datatest_SkinThickness<-test.data
lm.datatest_SkinThickness$SkinThickness[is.na(lm.datatest_SkinThickness$SkinThickness)] <-
  predicted_SkinThickness.test

#PMM by MICE
library(mice)
pmm_model_SkinThickness<-mice(train.data[, c("SkinThickness", "Glucose", "BMI", "Age", "Pregnancies")], 
                              method="pmm", m=5, seed=123)
completed_pmm_model_SkinThickness<-complete(pmm_model_SkinThickness)
summary(completed_pmm_model_SkinThickness)
hist(completed_pmm_model_SkinThickness$SkinThickness)
test_pmm_model_SkinThickness<-
  mice(test.data[, c("SkinThickness", "Glucose", "BMI", "Age", "Pregnancies")], method="pmm", m=5, seed=123)
completed_test_pmm_model_SkinThickness<-complete(test_pmm_model_SkinThickness)

#KNN 
knn_train<-train.data
knn_test<-test.data
knn_train$SkinThickness<-kNN(train.data, variable="SkinThickness", k=5)$SkinThickness
knn_test$SkinThickness<-kNN(test.data, variable="SkinThickness", k=5)$SkinThickness

##SkinThickness average by 3 methods
SkinThickness_train<-cbind(lm.datatrain_SkinThickness$SkinThickness,
                           completed_pmm_model_SkinThickness$SkinThickness, knn_train$SkinThickness)
colnames(SkinThickness_train)<-c("reg", "knn", "pmm")  
pairs(SkinThickness_train)  
cor(SkinThickness_train)

SkinThickness_test<-cbind(lm.datatest_SkinThickness$SkinThickness, 
                          completed_test_pmm_model_SkinThickness$SkinThickness, knn_test$SkinThickness)
colnames(SkinThickness_test)<-c("reg", "knn", "pmm")  
pairs(SkinThickness_test)  
cor(SkinThickness_test)

train.data$SkinThickness<-apply(SkinThickness_train,1,mean)
test.data$SkinThickness<-apply(SkinThickness_test,1,mean)


#checks for the outlier 
boxplot(train.data$Insulin)
boxplot(train.data$BMI)
Q1 <- quantile(train.data$Insulin, 0.25)
Q3 <- quantile(train.data$Insulin, 0.75)
IQR_val <- Q3 - Q1
lower <- Q1 - 1.5 * IQR_val
upper <- Q3 + 1.5 * IQR_val
which(train.data$Insulin < lower | train.data$Insulin > upper)

#visualistaion 
plot(density(train.data$Insulin))
plot(density(train.data$SkinThickness))

train.data$Insulin_log<-log(train.data$Insulin)
test.data$Insulin_log<-log(test.data$Insulin)

plot(density(train.data$Insulin_log))


#Three MODELS 
model_base<-glm(Outcome~Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + Age +
                DiabetesPedigreeFunction, data=train.data, family = binomial)
summary(model_base)

model_insulinlog<-glm(Outcome~Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin_log + BMI + Age +
                  DiabetesPedigreeFunction, data=train.data, family = binomial)
summary(model_insulinlog)

model_withoutinsulin<-glm(Outcome~Pregnancies + Glucose + BloodPressure + SkinThickness +  BMI + Age +
                        DiabetesPedigreeFunction, data=train.data, family = binomial)
summary(model_withoutinsulin)

pred1<-predict(model_base, newdata = test.data, type="response")
class1 <- ifelse(pred1 > 0.5, 1, 0)
pred2<-predict(model_insulinlog, newdata = test.data, type="response")
class2 <- ifelse(pred2 > 0.5, 1, 0)
pred3<-predict(model_withoutinsulin, newdata = test.data, type="response")
class3 <- ifelse(pred3 > 0.5, 1, 0)


library(pROC)
auc(roc(test.data$Outcome, pred1))
auc(roc(test.data$Outcome, pred2))
auc(roc(test.data$Outcome, pred3))
plot(roc(test.data$Outcome, pred1))
plot(roc(test.data$Outcome, pred2))
plot(roc(test.data$Outcome, pred3))
