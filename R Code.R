library(readxl)
library(plyr)
library(tibble)
library(ISLR)
library(pROC)
library(ggplot2)
library(randomForest)
library(ROCR)
library(rpart)
library(rpart.plot)

# Uploading and allocating the sheets in the excel file to separate datasets.

xl_data <- "C:/Users/shriy/Desktop/Studies/2_Sem/DM_1/HW3/IMB579-XLS-ENG.xlsx"
df_manipulator <- read_excel(path = xl_data, sheet = "Manipulator")
df_nonManipulator <- read_excel(path = xl_data, sheet = "Non-Manipulator")
df_data <- read_excel(path = xl_data, sheet = "Complete Data")
df_sample <- read_excel(path = xl_data, sheet = "Sample for Model Development")
colnames(df_sample)[11] <- "C.MANIPULATOR"
C.MANIPULATOR <- as.factor(df_sample$C.MANIPULATOR)
df_sample <- df_sample[-11] 
df_sample <- data.frame(df_sample, C.MANIPULATOR)

colnames(df_data)[11] <- "C.MANIPULATOR"
C.MANIPULATOR <- as.factor(df_data$C.MANIPULATOR)
df_data <- df_data[-11] 
df_data <- data.frame(df_data, C.MANIPULATOR)

Manipulater <- as.factor(df_data$Manipulater)
df_data <- df_data[-10] 
df_data <- data.frame(df_data, Manipulater)


# 1 Logistic Regression Model
# Data: sample data (220 cases including 39 manipulators)
# DSRI+GMI+AQI+SGI+ACCR -We considered only these 5 variables as we get the least AIC with this combination. Any other combination has an AIC greater than 100.
# AIC with the smallest value is picked as it implies that there is a balance between the number of variables included with the highest log of likelihood.

logitModel <-glm(C.MANIPULATOR ~ DSRI+GMI+AQI+SGI+ACCR+DEPI+SGAI+LEVI, data = df_sample, family = "binomial")
summary(logitModel)
layout(matrix(c(1,2,3,4),2,2))
plot(logitModel)

set.seed(123)
indx <- sample(2,nrow(df_sample),replace=TRUE,prob=c(0.7,0.3))
train <- df_sample[indx==1,] #group 1 with 70% of the data for training data
test <- df_sample[indx==2,]
lm_train <- glm(C.MANIPULATOR~ DSRI+GMI+AQI+SGI+ACCR, data=train, family="binomial")
summary(lm_train)

pred <- predict(lm_train, test, type="response")
pred.1 <- ifelse(pred >= 0.5,1,0)
pred.1 <- factor(pred.1,levels=c(0,1),order='TRUE')
cf <- table(test$C.MANIPULATOR, pred.1, dnn = c("Predictions", "Actual"))
cf

mean(unlist(pred))

# Functions to predict accuracy, precision and recall
my.accuracy <- function(actual, predictions)
{
  y <- as.vector(table(predictions, actual))
  names(y) <- c("TN","FP","FN","TP")
  acur <- (y["TN"]+y["TP"])/sum(y)
  return(as.numeric(acur))
}
my.precision<- function(actual, predictions)
{
  y <- as.vector(table(predictions, actual))
  names(y) <- c("TN","FP","FN","TP")
  prec <- y["TP"]/(y["TP"]+y["FP"])
  return(as.numeric(prec))
}

my.recall <- function(actual, predictions)
{
  y <- as.vector(table(predictions, actual))
  names(y) <- c("TN","FP","FN","TP")
  rec <- y["TP"]/(y["FN"]+y["TP"])
  return(as.numeric(rec))
}


# Applying the function to the the logistic regression model
my.precision(test$C.MANIPULATOR, pred.1)
my.recall(test$C.MANIPULATOR, pred.1)
my.accuracy(test$C.MANIPULATOR, pred.1)
mean(unlist(pred))

# Visualising the result

roc.1 <- roc(test$C.MANIPULATOR, pred)
x <- 1-roc.1$specificities
y <- roc.1$sensitivities
auc.1 <- roc.1$auc

ggplot(data=NULL,mapping=aes(x=x,y=y))+geom_line(colour='red')+geom_abline(intercept=0,slope=1)+annotate('text',x=0.4,y=0.4,label=paste('AUC=',round(auc.1,digit=2)))+labs(x='False positive rate',y='True positive rate')

# This model has a high accuracy rate of 0.85. 
# However, it is possible to have high accuracy without being an informative model therefore it is important to perform further evaluation.  
# Since there were no false negatives when the model was applied to the test data, the recall score is 1. 
# The ROC curve for this model of the False positive rate (also known as the "false alarm" rate) vs. True positive rate (also known as sensitivity or recall). 
# A well performing model is above the  45 degree AUC line and very close to the upper left corner, 
# which is where a "perfect" model's ROC curve would make a 90 degree angle at FPR of 0 and TPR of 1. This is further evidence that this is a well performing model.

#2 CART 

sample_treemodel<-rpart(Target~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI,data=train,method='class') 
print(sample_treemodel)
rpart.plot(sample_treemodel)
sample_traintreepred_class <-predict(sample_treemodel, data=train, type="class")
print(sample_traintreepred_class)
mean(train$Target != sample_traintreepred_class) #calculates error rate on training data

# The classification tree is incorrectly predicting the manipulator status outcome 8.8% of the time when applied to the training data. 
# While this is a good result, its performance on the test data is important.

sample_testtreepred_class <-predict(sample_treemodel, data=test, type="class")
print(sample_testtreepred_class)
mean(test$Target == sample_testtreepred_class) 

#The classification tree is accurately predicting manipulator status outcome 74% of the time when applied to the test data, which is an encouragingly high accuracy rate.


# 3 Logistic Regressipn Model
# Data: data (1200 non-manipulators and 39 manipulators)

set.seed(123)
indx.1 <- sample(2,nrow(df_data), replace=TRUE, prob=c(0.7,0.3))
train.data <- df_data[indx.1 == 1,] 
test.data <- df_data[indx.1 == 2,]
lm.1 <- glm(C.MANIPULATOR~ DSRI+GMI+AQI+SGI+ACCR, data = train.data, family = "binomial")
summary(lm.1)

pred1 <- predict(lm.1, test.data, type="response")
pred.2 <- ifelse(pred1 >= 0.5,1,0)
pred.2 <- factor(pred.2,levels=c(0,1),order='TRUE')

cf.1 <- table(test.data$C.MANIPULATOR, pred.2, dnn = c("Predictions", "Actual"))
cf.1

my.accuracy(test.data$C.MANIPULATOR, pred.2)
my.recall(test.data$C.MANIPULATOR, pred.2)

# ROC curve

#pred.3<-prediction(predictions=as.matrix(pred.2),labels=test.data$C.MANIPULATOR)
roc.curve<-roc(test.data$C.MANIPULATOR,pred1)
x<-1-roc.curve$specificities
y<-roc.curve$sensitivities
library(ggplot2)
auc<-roc.curve$auc
ggplot(data=NULL,mapping=aes(x=x,y=y))+geom_line(colour='red')+geom_abline(intercept=0,slope=1)+annotate('text',x=0.4,y=0.4,label=paste('AUC=',round(auc,digit=2)))+labs(x='False positive rate',y='True Positive Rate',title='ROC curve')

# Re-sampled the complete dataset to create a second logistic regression for comparison with the first results. 
# This logistic regression had a slightly lower AIC score, approximately the same accuracy, and much lower recall. 
# Given these results, we feel comfortable using the results from the first logistic regression on the complete dataset for comparison with the sample dataset.

# 4 random forest
# Sample data

Manipulator <- as.factor(df_sample$Manipulator)
df_sample <- df_sample[-10]
df_sample <- data.frame(df_sample, Manipulator)
rf.sample <- randomForest(Manipulator~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI,data=df_sample, mtry = sqrt(ncol(df_sample) - 1), ntree = 350, proximity = T, importance = T)
rf.sample
importance(rf.sample, type = 2)
rf.sample$proximity
rf.sample$predicted
rf.sample$votes
score.s <- rf.sample$votes[, 2]
pred.s <- prediction(score.s, df_sample$Manipulator)
pred.s1 <- ifelse(score.s >= 0.5,1,0)
pred.s1 <- factor(pred.s1,levels=c(0,1),order='TRUE')
cf.rf <- table(df_sample$Manipulator, pred.s1, dnn = c("Predictions", "Actual"))
cf.rf

my.accuracy(df_sample$Manipulator, pred.s1)
# [1] 0.8909091
my.precision(df_sample$Manipulator, pred.s1)
# [1] 0.5384615
my.recall(df_sample$Manipulator, pred.s1)
# [1] 0.9130435
roc.s<-roc(df_sample$Manipulator, score.s)
x<-1-roc.s$specificities
y<-roc.s$sensitivities
auc.s<-roc.s$auc

ggplot(data=NULL,mapping=aes(x=x,y=y))+geom_line(colour='green')+geom_abline(intercept=0,slope=1)+annotate('text',x=0.4,y=0.4,label=paste('AUC=',round(auc.s,digit=2)))+labs(x='False positive rate',y='True Positive Rate',title='ROC curve')

opt.cut <- function(perf){
  # mapply function applies the function FUN to all perf@x.values, perf@y.values,perf@alpha.values
  cut.ind <- mapply(FUN = function(x,y,p){d=(x-0)^2+(y-1)^2 # We compute the distance of all the points from the corner point [1,0]
  ind<- which(d==min(d)) # We find the index of the point that is closest to the corner
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]])},perf@x.values, perf@y.values,perf@alpha.values)
}
perf.s <- performance(pred.s, "tpr", "fpr")
BestcutOffPoint.s <- opt.cut(perf.s)
BestcutOffPoint.s

# Complete Data
complete_data <- Complete.data
Manipulator <- as.factor(complete_data$Manipulater)
complete_data <- complete_data[-10]
complete_data <- data.frame(complete_data, Manipulator)
rf.complete <- randomForest(Manipulator~DSRI+GMI+AQI+SGI++DEPI+SGAI+ACCR+LEVI,data=complete_data, mtry = sqrt(ncol(complete_data) - 1), ntree = 350, proximity = T, importance = T)
rf.complete
importance(rf.complete, type = 2)
rf.complete$proximity
rf.complete$predicted
rf.complete$votes

score.s <- rf.complete$votes[, 2]
pred.s <- prediction(score.s, complete_data$Manipulator)
pred.s1 <- ifelse(score.s >= 0.5,1,0)
pred.s1 <- factor(pred.s1,levels=c(0,1),order='TRUE')
cf.rf <- table(complete_data$Manipulator, pred.s1, dnn = c("Predictions", "Actual"))
cf.rf
my.accuracy(complete_data$Manipulator, pred.s1)
# [1] 0.9701372
my.precision(complete_data$Manipulator, pred.s1)
# [1] 0.0.230769
my.recall(complete_data$Manipulator, pred.s1)
# [1] 0.6 
library(pROC)
roc.s<-roc(complete_data$Manipulator, score.s)
x<-1-roc.s$specificities
y<-roc.s$sensitivities
auc.s<-roc.s$auc

ggplot(data=NULL,mapping=aes(x=x,y=y))+geom_line(colour='green')+geom_abline(intercept=0,slope=1)+annotate('text',x=0.4,y=0.4,label=paste('AUC=',round(auc.s,digit=2)))+labs(x='False positive rate',y='True Positive Rate',title='ROC curve for Entire Dataset')

perf.s <- performance(pred.s, "tpr","fpr")
BestcutOffPoint.s <- opt.cut(perf.s)
BestcutOffPoint.s

# The random forest model for the complete dataset was much higher at 97% than the sample dataset's 89%. 
# However, the sample data was higher for recall, and when looking for a manipulator it is more important to avoid false negatives therefore recall is a higher priority. 
# The AUC scores were almost identical and quite high for both datasets. 
# The random forest model for the sample data had a higher accuracy for predictions on test data (89%) as compared with the classification tree (74%).

# Recommendations
# The Beneish Model considers eight financial ratios to identify whether a company has manipulated earnings. 
# But when we ran the data provided in the case study through various models (such as logistic regression, random forest) 
# we realised only a few ratios are significant (with respect to the data provided) for prediction which are DSRI,GMI,AQI,SGI and ACCR.
# Our experience demonstrates that reliance on a "one size fits all" approach to applying a model can lead to suboptimal performance. 
# We recommend that the Beneish model be used as a starting point for identifying manipulators 
# with the understanding that the model will require analysis and potential adjustment for each dataset. 
# For use on Indian businesses, we recommend using the M-score generated by the sample data but continually assess their outcomes to monitor the risk of overfitting.
