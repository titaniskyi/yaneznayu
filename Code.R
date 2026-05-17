titanic.train<- read.csv("/Users/alisher/Desktop/Titanic dataset files-20260415/train.csv", stringsAsFactors = F)
titanic.test<- read.csv("/Users/alisher/Desktop/Titanic dataset files-20260415/test.csv", stringsAsFactors = F)

str(titanic.train)
str(titanic.test)

summary(titanic.train)
summary(titanic.test)

length(which(titanic.train$Cabin==""))
length(which(titanic.test$Cabin==""))

train.class1.no.cabin <- which(titanic.train$Pclass==1 & titanic.train$Cabin=="")
length(train.class1.no.cabin)

test.class1.no.cabin <- which(titanic.test$Pclass==1 & titanic.test$Cabin=="")
length(test.class1.no.cabin)

titanic.train$Cabin[train.class1.no.cabin] <- NA
titanic.test$Cabin[test.class1.no.cabin] <- NA

length(which(is.na(titanic.train$Cabin)))
length(which(is.na(titanic.test$Cabin)))

apply(X = titanic.train[,c("Name","Sex","Ticket","Embarked")],
      MARGIN = 2,FUN = function(x) length(which(x=="")))
apply(X = titanic.test[,c("Name","Sex","Ticket","Embarked")],
      MARGIN = 2,FUN = function(x) length(which(x=="")))


titanic.train$Embarked[titanic.train$Embarked==""] <- NA

library(VIM)

aggr(titanic.train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(titanic.train), cex.axis=.7, gap=3,
     ylab=c("Histogram of missing data","Pattern"))

marginplot(titanic.train[, c("Age", "Fare")])
marginplot(titanic.train[, c("Age", "Parch")])

install.packages("naniar")
library(naniar)
vis_miss(titanic.train)

mcar_result <- mcar_test(titanic.train)
print(mcar_result)

titanic.train$Age_Missing <- is.na(titanic.train$Age)

titanic.train$Pclass <- factor(titanic.train$Pclass)
titanic.train$Sex <- factor(titanic.train$Sex)
titanic.train$Embarked <- factor(titanic.train$Embarked, exclude = NULL)
titanic.test$Pclass <- factor(titanic.test$Pclass)
titanic.test$Sex <- factor(titanic.test$Sex)
titanic.test$Embarked <- factor(titanic.test$Embarked, exclude = NULL)

missing_age_model <- glm(Age_Missing~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                         family = binomial, data = titanic.train)
summary(missing_age_model)

if (!require("mice")) {
  install.packages("mice", dependencies = TRUE)
}

library(mice)

imputation_model <- mice(titanic.train[, c("Age", "Pclass", "SibSp", "Parch", "Fare", "Sex", "Embarked")],
                         method = 'pmm'
                         ,
                         m = 5, # Number of multiple imputations
                         seed = 123)

completed_train_mice<-complete(imputation_model)
summary(completed_train_mice$Age)

hist(completed_train_mice$Age, main = "Histogram of Imputed Ages", col = "lightblue")

test_imputation_model <- mice(titanic.test, method = 'pmm', m = 5, seed = 123)

completed_test_mice <- complete(test_imputation_model, action = 1)

age_model <- lm(Age~ Pclass + SibSp + Parch + Fare + Sex + Embarked, data = titanic.train, na.action = na.omit)

predicted_ages <- predict(age_model, newdata = titanic.train[is.na(titanic.train$Age), ])

completed_train_reg<-titanic.train
completed_train_reg$Age[is.na(completed_train_reg$Age)] <- predicted_ages

completed_test_reg<-titanic.test
predicted_ages <- predict(age_model, newdata = titanic.test[is.na(titanic.test$Age), ])
completed_test_reg$Age[is.na(completed_test_reg$Age)] <- predicted_ages

plot(completed_train_reg$Age,completed_train_mice$Age,ylab="mice",xlab="regression")

completed_train_knn<-titanic.train
completed_test_knn<-titanic.test
completed_train_knn$Age <- kNN(titanic.train, variable = "Age",k=5)$Age
completed_test_knn$Age <- kNN(titanic.test, variable = "Age",k=5)$Age

age.train<-cbind(completed_train_reg$Age,completed_train_mice$Age,
                 completed_train_knn$Age)
colnames(age.train)<-c("Regr","mice","knn")
pairs(age.train)

cor(age.train)

age.test<-cbind(completed_test_reg$Age,completed_test_mice$Age,
                completed_test_knn$Age)
colnames(age.test)<-c("Regr","mice","knn")
pairs(age.test)

cor(age.test)

titanic.train$Age<-apply(age.train,1,mean)
titanic.test$Age<-apply(age.test,1,mean)

unique(titanic.train$Embarked)

unique(titanic.test$Embarked)

xtabs(~ Embarked, data = titanic.train)

titanic.train$Embarked[is.na(titanic.train$Embarked)] <- 'S'
xtabs(~ Embarked, data = titanic.train)

titanic.train$Embarked <- factor(titanic.train$Embarked)
titanic.test$Embarked <- factor(titanic.test$Embarked)

shapiro.test(titanic.test$Fare)

missing.fare.pclass <- titanic.test$Pclass[is.na(titanic.test$Fare)]

median.fare <- median(x = titanic.test$Fare[titanic.test$Pclass== missing.fare.pclass],na.rm = T)

titanic.test$Fare[is.na(titanic.test$Fare)] <- median.fare

summary(titanic.test$Fare)

plot(density(titanic.train$Age))
plot(density(titanic.train$Fare))

titanic.train$Age_Z <- (titanic.train$Age- mean(titanic.train$Age, na.rm = TRUE)) / sd(titanic.train$Age, na.rm = TRUE)
titanic.train$Fare_Z <- (titanic.train$Fare- mean(titanic.train$Fare, na.rm = TRUE)) / sd(titanic.train$Fare, na.rm = TRUE)

age_outliers_z <- which(abs(titanic.train$Age_Z) > 3)
fare_outliers_z <- which(abs(titanic.train$Fare_Z) > 3)

cat("Age outliers based on Z-score method:\n")
print(titanic.train$Age[age_outliers_z])

cat("Fare outliers based on Z-score method:\n")
print(titanic.train$Fare[fare_outliers_z])

IQR_Age <- IQR(titanic.train$Age, na.rm = TRUE)
IQR_Fare <- IQR(titanic.train$Fare, na.rm = TRUE)

lower_bound_age <- quantile(titanic.train$Age, 0.25, na.rm = TRUE)- 1.5 * IQR_Age
upper_bound_age <- quantile(titanic.train$Age, 0.75, na.rm = TRUE) + 1.5 * IQR_Age
lower_bound_fare <- quantile(titanic.train$Fare, 0.25, na.rm = TRUE)- 1.5 * IQR_Fare
upper_bound_fare <- quantile(titanic.train$Fare, 0.75, na.rm = TRUE) + 1.5 * IQR_Fare

age_outliers_iqr <- which(titanic.train$Age < lower_bound_age | titanic.train$Age > upper_bound_age)
fare_outliers_iqr <- which(titanic.train$Fare < lower_bound_fare | titanic.train$Fare > upper_bound_fare)
cat("Age outliers based on IQR method:\n")
print(titanic.train$Age[age_outliers_iqr])

cat("Fare outliers based on IQR method:\n")
print(titanic.train$Fare[fare_outliers_iqr])

library(ggplot2)
ggplot(titanic.train, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  geom_vline(xintercept = c(lower_bound_age, upper_bound_age), color = "red", linetype = "dashed")

ggplot(titanic.train, aes(x = Fare)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  geom_vline(xintercept = c(lower_bound_fare, upper_bound_fare), color = "red", linetype = "dashed")

ggplot(titanic.train, aes(x = Fare, fill = Pclass)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  facet_wrap(~ Pclass, scales = "free_x") + # Allows different x-axis for each subplot
  theme_bw() +
  labs(title = "Fare Distribution by Passenger Class",
       x = "Fare",
       y = "Frequency",
       fill = "Class") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(titanic.test, aes(x = Fare, fill = Pclass)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  facet_wrap(~ Pclass, scales = "free_x") + # Allows different x-axis for each subplot
  theme_bw() +
  labs(title = "Fare Distribution by Passenger Class",
       x = "Fare",
       y = "Frequency",
       fill = "Class") +
  theme(plot.title = element_text(hjust = 0.5))

index.test=which(titanic.test$Fare>500)
index.train=which(titanic.train$Fare>500)
titanic.test$Fare[index.test]
#finished here 15.04.2026
titanic.train$Fare[index.train]

titanic.train$Sex <- factor(titanic.train$Sex)
summary( titanic.train$Sex )

prop.table(summary( titanic.train$Sex ))

table(titanic.train$Sex,titanic.train$Survived)
prop.table(table(titanic.train$Sex,titanic.train$Survived),1)

library(ggplot2)

titanic.train$Survived <- factor(titanic.train$Survived,levels = c(0,1), labels = c('No'
                                                                                    ,
                                                                                    'Yes'))
titanic.train$Pclass <- factor(titanic.train$Pclass,levels = c(1,2,3),labels = c("1st", "2nd", "3rd"))

gp1 <- ggplot(data = titanic.train,mapping = aes(x = Pclass, fill=Survived)) + geom_bar(position = "dodge") +
  ylab("Number of passengers") + xlab("Passenger class") + theme_bw()
gp1

gp2 <- gp1 + facet_wrap(~Sex)
gp2

gp3 <- ggplot(data = titanic.train, mapping = aes(x = Embarked, fill = Survived)) + geom_bar(position = "dodge") +
  ylab("Number of passengers") + xlab("Port of Embarkation") + theme_bw()
gp3

titanic.train$FamilySize <- titanic.train$SibSp + titanic.train$Parch + 1

titanic.train$IsAlone <- ifelse(titanic.train$FamilySize > 1, 0, 1) 

titanic.train$Title <- gsub('(.*, )|(\\..*)'
                            ,
                            '', titanic.train$Name)

titanic.train$AgeGroup <- cut(titanic.train$Age,
                              breaks = c(0, 12, 18, 35, 60, Inf),
                              labels = c("Child", "Teen", "Young Adult", "Adult", "Senior"), right = FALSE)

titanic.train$Title <- factor(titanic.train$Title)
titanic.train$AgeGroup <- factor(titanic.train$AgeGroup)
titanic.train$IsAlone <- factor(titanic.train$IsAlone)

ggplot(titanic.train, aes(x = FamilySize)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 8) +
  ggtitle("Distribution of Family Sizes on the Titanic") +
  xlab("Family Size") +
  ylab("Number of Passengers")

ggplot(titanic.train, aes(x = Title, fill = Survived)) +
  geom_bar(position = "dodge") +
  ggtitle("Survival by Social Title") +
  xlab("Title") +
  ylab("Count") +
  theme_minimal()




