# loading libraries 
library(e1071)
library(caTools)
library(caret)

# loading data
path <- file.choose()
diabetes <- read.csv(path , header = T)

# Data evaluation and Pre-Processing 
head(diabetes)
summary(diabetes)
str(diabetes) # diabetes$Outcome = 0 "NON DIABETIC"
              # diabetes$Outcome = 1 "DIABETIC"
# change outcome variable from numeric to factor for classification model
diabetes$Outcome <- as.factor(diabetes$Outcome)
table(diabetes$Outcome)
prop.table(table(diabetes$Outcome))
upper_out_preg <- quantile(diabetes$Pregnancies, 0.75) + 1.5*IQR(diabetes$Pregnancies)
diabetes$Pregnancies[diabetes$Pregnancies > upper_out_preg] <- upper_out_preg
upper_out_insulin <- quantile(diabetes$Insulin, 0.75) + 1.5*IQR(diabetes$Insulin)
diabetes$Insulin[diabetes$Insulin > upper_out_insulin] <- upper_out_insulin


# Splitting the data into test and train data 
set.seed(111)
split <- sample.split(diabetes, SplitRatio = 0.8)
train <- subset(diabetes, split == T)
test <- subset(diabetes, split == F)
table(train$Outcome)
prop.table(table(train$Outcome))
# here we observe a class imbalance problem 
# outcome "0" has a proportion of 65% in the dataset and Outcome "1" has a proportion of 35%

# Developing a dataset using UNDER SAMPLING 
library(ROSE)
train_under <- ovun.sample(Outcome~. , data = train, method = 'under', N = 416)$data
summary(train_under)

# Developing the model 
svm_fit <- svm(Outcome~. , data = train_under, scale = T, kernel = 'radial')
summary(svm_fit)

# Building Confusion Matrix using train data
confusionMatrix(predict(svm_fit, train_under),train_under$Outcome)

# Building Confusion Matrix using test data
confusionMatrix(predict(svm_fit, test),test$Outcome)

# Tuning Hyperparameters 
svm_tune <- tune(svm, Outcome~. , data = train_under, kernel="radial", scale = T,
                 ranges = list(cost= seq(5,12, by =1),
                               gamma = c(0.0001, 0.001, 1))
                 )
summary(svm_tune$best.model)

# Building Confusion Matrix using training data
confusionMatrix( predict(svm_tune$best.model, train_under),train_under$Outcome)

# Building Confusion Matrix using testing data
confusionMatrix( predict(svm_tune$best.model, test),test$Outcome)


