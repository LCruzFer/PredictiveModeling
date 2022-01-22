rm(list=ls())

library(dplyr)
library(ggplot2)
library(tree)
library(randomForest)
library(grf)
library(quantreg)
library(reshape2)
library(murphydiagram)
library(lmtest)
library(sandwich)

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

mz_quant_test=function(true, preds, tau=0.1){
  #this function runs a Mincer-Zernowitz test for quantile prediction, 
  #i.e. using quantile regression of true values on prediction to determine whether 
  #auto-calibration is fulfilled
  qreg=quantreg::rq(true~preds, tau=tau)
  #get summary with bootstrapped std errors
  summary=summary(qreg, covariance=TRUE, se='boot')
  #make Wald-test of joint significance, intercept should be 0 and prediction coefficient should be 1
  #solve(matrix) returns inverse of matrix
  wald=t(coef(qreg)-c(0, 1))%*%solve(summary$cov)%*%(coef(qreg)-c(0, 1))
  #then get p-values 
  pval=1-pchisq(wald, 2)
  #and create a plot
  p <- ggplot(data=data.frame(y=true, preds=preds), aes(x=true, y=preds)) +
    geom_abline(slope=1, intercept=0, col="black") +
    geom_point(color='darkgreen', size=0.2) +
    geom_quantile(quantiles=tau, color="red")
  
  return(list(estimate=summary, wald=as.numeric(wald), pvals=as.numeric(pval), plot=p))
}

#quantile loss score
qloss_score <- function(q_pred, true, tau){ (1-tau)*(q_pred-true)*(q_pred>true) + tau*(true-q_pred)*(true>=q_pred) }

############
#Problem 3
############
#read in data 
insurance=read.csv('insurance.csv')
#need to turn all numeric variables into numeric type 
insurance$age=as.numeric(insurance$age)
#split data into test and training set 
test_train_data=test_train_sample(insurance, test_n=dim(insurance)[1]/2)
test_dat=test_train_data$test
train_dat=test_train_data$train
#also get df with true values in which predictions will be saved 
test_preds=test_dat%>%dplyr::select(charges)

####
#10% quantile prediction

#1 Quantile Regression 
#fit quantile regression model to training data
qr_10p=rq(charges~., tau=0.1, data=train_dat)
#then predict using test data and save in predictions df 
test_preds$qr10=predict(qr_10p, newdata=test_dat)

#2 Quantile Random Forest 
#create model matrix for observables for training and test data 
x_train=model.matrix(charges~., data=train_dat)
y_train=train_dat$charges
x_test=model.matrix(charges~., data=test_dat)
#then fit a generalized random forest with quantile loss function - aka quantile random forest 
q_rf=grf::quantile_forest(x_train, y_train, quantiles=0.1)
#predict the outcome using quantile random forest 
test_preds$qrf10=as.vector(predict(q_rf, new_x=x_test)[[1]])

####
#Absolute Evaluation 

#Unconditional coverage 
#quantile regression 
qr_un_cover=mean(test_preds$charges<=test_preds$qr10)
#quantile random forest 
qrf_un_cover=mean(test_preds$charges<=test_preds$qrf10)

#Auto-calibration with quantile MZ test
#quantile regression
mz_qr=mz_quant_test(test_preds$charges, test_preds$qr10)
mz_qr$plot
#quantile random forest 
mz_qrf=mz_quant_test(test_preds$charges, test_preds$qrf10)
mz_qrf$plot
#create dataframe for table in rmd 
mzq_table=as.data.frame(mz_qr$estimate$coefficients)%>%
  add_row(as.data.frame(mz_qrf$estimate$coefficients))%>%
  mutate(Estimate=c('QR Intercept', 'QR Coefficient', 'QRF Intercept', 'QRF Coefficient'))
rownames(mzq_table)=NULL
mzq_table=mzq_table[, c(5, 1, 2, 3, 4)]
mzq_wald_table=data.frame(Method=c('QR', 'QRF'), Wald=c(mz_qr$wald, mz_qrf$wald), 
                          'p Value'=c(mz_qr$pval, mz_qrf$pval))
####
#Relative Evaluation

#Quantile Loss Score
#make a Diebold-Mariano test to evaluate which model performs relatively better 
#for interpretation see pdf solutions file
qr_score=qloss_score(test_preds$qr10, test_preds$charges, tau=0.1)
qrf_score=qloss_score(test_preds$qrf10, test_preds$charges, tau=0.1)
score_diff=qr_score-qrf_score

dm_test_quant=lm(score_diff~1)%>%coefci(vcov.=NeweyWest)

#Prediction Interval 
#let's summarize prediction uncertainty with prediction interval with alpha=0.05
#quantile regression 
qr_low=quantreg::rq(charges~., data=train_dat, tau=0.025)
pi_low=predict(qr_low, newdata=test_dat)
qr_high=quantreg::rq(charges~., data=train_dat, tau=0.975)
pi_up=predict(qr_high, newdata=test_dat)
#how many percent of true values are in PI? 
in_pi=(pi_low<=test_preds$charges)&(test_preds$charges<=pi_up)
#pct in 
pct_in_pi=mean(in_pi)
#94.91%! 

#quantile random forest 
#in quantile rf function can fit model to multiple quantiles at once
qrf_both=grf::quantile_forest(X=x_train, Y=y_train, quantiles=c(0.025, 0.975))
#predict both quantiles at once 
qrf_both_pred=predict(qrf_both, new_x=x_test)[[1]]
#first column is lower, second one is upper quantile 
#check coverage of PI 
in_pi_qrf=(qrf_both_pred[, 1]<=test_preds$charges)&(test_preds$charges<=qrf_both_pred[, 2])
#percentage coverage 
pct_in_pi_qrf=mean(in_pi_qrf)
#only 67.9!

int_score=function(y, r, l, alpha){
  #supply the true values, upper and lower bound of PIs as vectors and calculate 
  #the interval score for a given alpha
  scores=(r-l)+2/alpha*(l-y)*as.numeric(y<l)+2/alpha*(y-r)*as.numeric(y>r)
  #sum all scores together to get interval score 
  is=sum(scores)
  return(is)
}
#calculate IS for both predictions
is_qr=int_score(test_preds$charges, pi_low, pi_up, alpha=0.05)
is_qrf=int_score(test_preds$charges, qrf_both_pred[, 1], qrf_both_pred[, 2], alpha=0.05)
