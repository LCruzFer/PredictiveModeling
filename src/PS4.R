rm(list=ls())
library(AmesHousing)
library(dplyr)
library(glmnet)

##############
#FUNCTIONS
##############
run_cvglmnet=function(x, y, a, lambdas='default'){
  #this function fits a cv.glmnet given x, y and alpha values. If lambdas=default,
  #default of cv.glmnet is kept, otherwise provide a sequence of lambdas to be used. 

  if(lambdas=='default'){ 
    cv_glm=cv.glmnet(x=x, y=y, alpha=a)
  }else{
    cv_glm=cv.glmnet(x=x, y=y, alpha=a, lambda=lambdas)
  }
  #return the cv.glmnet object
  return (cv_glm)
}

comp_glm_ols=function(x, y, a, lambdas='default', how='smallest'){
  #Run OLS and cv.glmnet regressions and return a data.frame comparing their coefficients.
  #x, y=data; a=alpha for cv.glmnet, lambdas=sequence of lambdas to use or 'default'
  #how defines what cv.glmnet coefficients are used for comparison: 
    #'smallest'=use smallest lambda used in cv.glmnet
    #'min': use optimal lambda from cv
    #else: use lambda.1se (see cv.glmnet docs) (default for coef(cv.glmnet))
    
  #first run cv.glmnet 
  glm_res=run_cvglmnet(x=x, y=y, a=a, lambdas=lambdas)
  #and ols
  ols=lm(y~x)
  #save ols coefficients, ignoring constant
  betas_ols=coefficients(ols)[-1]
  #now get coeffcients of glmnet depending on choice of 'how'
  if (how=='smallest'){
    betas_glm=glm_res$glmnet.fit$beta[, ncol(glm_res$glmnet.fit$beta)]
  }else if(how=='min'){
    betas_glm=coef(glm_res, s='lambda.min')
  }else{
    betas_glm=coef(glm_res)
  }
  #then bin them together into one df
  compare_betas=cbind(betas_ols, betas_glm)
  compare_df=data.frame(compare_betas)
  compare_df=setNames(compare_df, c('OLS', 'GLMNET'))
  
  return (compare_df)
}

##############
#Problem 1
##############
#load AmesHousing and create a dataset using make_ames() 
data=make_ames()
#what are dimensions of data 
dim(data)
#have n=2930 and p=81
#let's first have a look at the available variables 
names(data)
#drop variables that are not available for prediction: Sale_Type & Sale_Condition 
dat2=data%>%dplyr::select(-Sale_Type, -Sale_Condition)
#create a test and train set choosing observations at random 
#test set should contain 1000 observations, rest contained in training
set.seed(1)
#get integers between 1 and nrow(dat2) and get 1000 of them
slices=sample.int(1000, n=nrow(dat2))
#keep only 1000 obs in test data
test_data=dat2[slices, ]
#all other go into train data
train_data=dat2[-slices, ]
#check whether dimensions are correct 
dim(test_data)
dim(train_data)
#now create a predictor matrix from train data 
#model.matrix takes regression formula as first and data as second argument 
#Y~. means using all columns except Y as predictors
x=model.matrix(Sale_Price~., data=train_data)
#we want to drop all constant predictors
#first can drop intercept in x, that is first column 
x=x[, -1]
#constant predictors are predictors that have sd=0
#hence find all predictors that have sd=0 and remove their column 
const=which(apply(x, 2, sd)==0)
#drop that are in const 
x=x[, -const]
#check dimensions, how many predictors do we have now? 
dim(x)
#also create the variable to be predicted
y=train_data$Sale_Price

##############
#Problem 2
##############

#we have already created test and train set 
#first fit an OLS using the train set 
ols=lm(y~x, data=train_data)
#print summary
summary(ols)
#... and see that there are way too much variables for any useful analysis
#need shrinkage 

##RIDGE REGRESSION 

#perform a ridge regression using cv.glmnet 
#cv.glmnet automatically conducts CV to find optimal lambda parameter for ridge
#glmnet performs ridge when setting alpha=0
cv_ridge=cv.glmnet(x, y, alpha=0)
#plot MSE along lambdas
plot(cv_ridge)
#cv.glmnet returns object with summary on cross validation fit 
#can fit a glmnet object to the entire data which contains summaries on df, MSE for each lambda 
cv_glmnet=cv_ridge$glmnet.fit
#this object has method beta that calculated beta coefficients for each lamdba value and returns
#them as a n_varsXlambda matrix 
#to access the betas for the lowest lambda access the last column (sorted from highest to lowest lambda)
betas_cvglmnet=cv_glmnet$beta[, 100]
#to compare there to the OLS coefficients of the variables bind them together in a vector
#ignore constant in OLS coefficients (first element)
beta_compare=cbind(betas_cvglmnet, coefficients(ols)[-1])
#the coefficients are quite different as even the smallest lambda is still >> 0
#compare to what happens when we allow for lambda being almost zero 
#in fact use lambda grid between 0 and cv_ridge$lambda.min (optimal lambda per CV)
cv_ridge2=cv.glmnet(x, y, lambda=seq(1e-4, cv_ridge$lambda.min, length.out=20), thresh=1e-14)
#again compare the coefficients 
beta_compare2=cbind(cv_ridge2$glmnet.fit$beta[, 20], coefficients(ols)[-1])
