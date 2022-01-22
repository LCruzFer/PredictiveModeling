rm(list=ls())

library(ggplot2)
library(randomForest)
library(tree)
library(dplyr)
library(glmnet)
library(reshape2)
library(plotROC)
library(reliabilitydiag)
library(murphydiagram)
library(quantreg)
#set working directory to data path 
setwd("/Users/llccf/OneDrive/Dokumente/4. Semester/PredictiveModeling/exam/")
#also set seed so replications of this code generate same results 
set.seed(2021)

############
#Functions
############
#This section contains all functions defined for the problems solved in their respective section.
#Comments within the functions explain what is going on in there and whenever applied referenced
#in the comments.

test_train_sample=function(dat, test_n=1000){
  #split a sample in dat into a test and training sample 
  #test_n defines size of test sample created
  #create slicing 
  slices=sample.int(test_n, n=nrow(dat)) 
  #create subsets
  train=dat[-slices, ]
  test=dat[slices, ]
  #return them in a named list
  dat_list=list('train'=train, 'test'=test)
  return (dat_list)
}

adjust_probs=function(probs_series){ 
  #Some predictions might result in probabilities outside of interval [0, 1].
  #Simply set those predictions < 0 to 0 and > 1 to 1.
  #all < 0 to 0
  probs_series=pmax(0, probs_series)
  #all > 1 to 1
  probs_series=pmin(1, probs_series)
  return(probs_series)
}

get_important_feature=function(rf_mod){
  #this function retrieves most important feature based on node purity increase 
  #in random forest model rf_mod
  #get index of most important variable 
  ind=which(rf$importance==min(rf$importance))
  
}
############
#Problem 1
############
#read in data 
heart=read.csv('heart.csv')

#a) split into test and training data 
#apply function test_train_sample
test_train_data=test_train_sample(heart, test_n=dim(heart)[1]/2)
#retrieve test and training sample from named list that is returned by function 
test_dat=test_train_data$test 
train_dat=test_train_data$train
test_preds=test_dat%>%dplyr::select(y)

#b) predict outcome y with 3 reasonable methods 
#first create df in which various predictions for test data are saved alongside true values 

#1. Logit Model with no interactions 
#fit the model on training data 
logit=glm(y~., data=train_dat, family='binomial')
#to predict proba
test_preds$logit_preds=predict(logit, newdata=test_dat, type='response', s='lambda.1se')

#1.1 Logit model with interactions - Lasso for selection
#create a model matrix from all variables that contains interactions between observables
#delete intercept
#note: raising . to power of 2 adds all dual interactions to model matrix
x_train=model.matrix(y~.^2, data=train_dat)[, -1]
#then get y 
y_train=train_dat$y
#and fit cross-validated Lasso 
lasso_logit=cv.glmnet(x_train, y_train, alpha=1, family='binomial')
#then get predictions 
#for these need model matrix of test data 
x_test=model.matrix(y~.^2, data=test_dat)[, -1]
#use lambda.1se predictions as those are convention (seen on stackechange)
#use type=response as this automatically returns probabilities, not odds
test_preds$ll_preds=as.vector(predict(lasso_logit, newx=x_test, s='lambda.1se', type='response'))

#2. Random Forest
#fit random forest on train data
rf=randomForest(y~., data=train_dat)
#again get predictions 
test_preds$rf_preds=predict(rf, newdata=test_dat)
#adjust the probability predictions that are outside of [0, 1]
test_preds$rf_preds=adjust_probs(test_preds$rf_preds)

#3 Nonparametric Regression 
#can do nonparametric prediction based on one variable 
#use the variable that is split on first most frequently in the RF above 
#OR like here now: variable that has highest importance based on Mean Decrease in Impurity (MDI)
#AUTOMATE THIS!!!
which(rf$importance==max(rf$importance))
#-> AGE
#then use loess function 
np=loess(y~age, data=train_dat)
#then make prediction 
test_preds$np_preds=predict(np, newdata=test_dat)
#adjust the probability predictions that are outside of [0, 1]
test_preds$np_preds=adjust_probs(test_preds$np_preds)

#c) Evaluation 
#Note: For discussion of evaluation results consult PDF solutions
#There are different methods for evaluation of probability predictions for 
#binary outcomes; to calculate those need a long data.frame 
#!GETTING A WARNING HERE RIGHT NOW 
test_preds_long=melt(test_preds, measure.vars=-test_preds$y, variable.name='pred_method', value.name='pred_vals')

#Calculate MSE for different prediction methods
mse=test_preds_long%>%group_by(pred_method)%>%summarize(mse=mean((y-pred_vals)^2, na.rm=TRUE))

#ROC curve - plotting HR against FAR
#use the plotROC library and ggplot to visualize the ROC Curve 
p_ROC=ggplot(data=test_preds_long) +
  geom_roc(aes(d=y, m=pred_vals, color=pred_method), 
           pointsize=0.3, position='identity') +
  xlab('FAR') +
  ylab('HR') +
  labs(color='Prediction Method', title='ROC') +
  theme(legend.position='bottom')

#Area under the Curve (AUC)
auc=calc_auc(p_ROC)

#Brier and Log Score 
scores=test_preds_long%>%group_by(pred_method)%>%summarize(Brier=mean((y-pred_vals)^2, na.rm=TRUE), 
                                                           Log=mean(-y*log(pred_vals)-(1-y)*log(pred_vals), na.rm=TRUE))

#Reliability Diagram 
#Logit 
reliabilitydiag(y=test_preds$y, x=test_preds$logit_preds)
#Lasso Logit 
ll_rel=reliabilitydiag(y=test_preds$y, x=test_preds$ll_preds)
#RF
rf_rel=reliabilitydiag(y=test_preds$y, x=test_preds$rf_preds)
#Nonparametric Regression 
np_rel=reliabilitydiag(y=test_preds$y, x=test_preds$np_preds)

brier_decomps=summary(ll_rel)%>%add_row(summary(rf_rel))%>%add_row(summary(np_rel))

#Murphy Diagram
#Logit vs RF 
murphydiagram(f1=test_preds$ll_preds, 
              f2=test_preds$rf_preds, 
              y=test_preds$y)
#Logit vs Nonparametric Regression
murphydiagram(f1=test_preds$ll_preds, 
              f2=test_preds$np_preds, 
              y=test_preds$y)
#RF vs Nonparametric Regression 
murphydiagram(f1=test_preds$rf_preds, 
              f2=test_preds$np_preds, 
              y=test_preds$y)