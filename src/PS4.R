rm(list=ls())
library(AmesHousing)
library(dplyr)
library(glmnet)

##############
#FUNCTIONS
##############
gen_test_train=function(data, test_n=1000){
  #using data generate a test and a training data set where the test set consists of 
  #test_n observations and rest are training data
  slices=sample.int(test_n, n=nrow(data)) 
  train=data[-slices, ]
  test=data[slices, ]
  dat_list=list(train, test)
  return (dat_list)
}

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
  glm_res=run_cvglmnet(x, y, a=a, lambdas=lambdas)
  #and ols
  ols=lm(y~x)
  #now get coeffcients of glmnet depending on choice of 'how'
  if (how=='smallest'){
    #with this method glmnet doesn't return a constant
    #hence drop from ols betas
    betas_ols=coefficients(ols)[-1]
    betas_glm=glm_res$glmnet.fit$beta[, ncol(glm_res$glmnet.fit$beta)]
  }else if(how=='min'){
    #with coef() glmnet returns a constant coefficient as well
    betas_ols=coefficients(ols)
    betas_glm=as.matrix(coef(glm_res, s='lambda.min'))
  }else{
    betas_ols=coefficients(ols)
    betas_glm=as.matrix(coef(glm_res))
  }
  #then bin them together into one df
  compare_betas=cbind(betas_ols, betas_glm)
  compare_df=data.frame(compare_betas)
  compare_df=setNames(compare_df, c('OLS', 'GLMNET'))
  
  return (compare_df)
}

oos_mse=function(pred, raw, norm=10^9){
  #calculate out-of-sample mse using columns pred and raw of data 
  #normalize mse by norm
  mse=mean((pred-raw)^2)/norm
  
  return (mse)
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
##RIDGE REGRESSION 

#we have already created test and train set 

#perform a ridge regression using cv.glmnet 
#cv.glmnet automatically conducts CV to find optimal lambda parameter for ridge
#glmnet performs ridge when setting alpha=0
#call run_cvglmnet and use default cv.glmnet lambdas
cv_ridge=run_cvglmnet(x=x, y=y, a=0, lambdas='default')
#plot MSE along lambdas
plot(cv_ridge)
#cv.glmnet returns object with summary on cross validation fit 
#can fit a glmnet object to the entire data which contains summaries on df, MSE for each lambda 
#this object has method beta that calculated beta coefficients for each lamdba value and returns
#them as a n_varsXlambda matrix 
#to access the betas for the lowest lambda access the last column (sorted from highest to lowest lambda)
#all this is done in function 'comp_glm_ols' to find the coefficients and then they are compared
#to OLS ones
#first compare OLS to coefficients with smallest lambda using default lambdas
beta_compare=comp_glm_ols(x=x, y=y, a=0, lambdas='default', how='smallest')
#the coefficients are quite different as even the smallest lambda is still >> 0
#then compare to coefficients with smallest lambda when lambda starts at zero and goes until
#optimal lambda returned by cv.glmnet -> cv.glmnet$lambda.min can be accesses from cv_ridge
beta_compare2=comp_glm_ols(x=x, y=y, a=0, lambdas=seq(1e-4, cv_ridge$lambda.min, length.out=20), how='smallest')
#these are very similar now as the lambda is almost zero and hence the constraint isn't 
#constriaing the optimization process
#what if we compare the OLS coefficients to the optimal ones? 
beta_compare3=comp_glm_ols(x=x, y=y, a=0, lambdas='default', how='min')

#now predict using test set from ols and from optimal ridge regression and compare the mse
#set up a dataframe in which we have the true values and the values from the two predictions
test_pred_data=test_data%>%dplyr::select(Sale_Price)
#need the ols model for this 
ols_mod=lm(y~x)
#and x from test set 
dim(test_data)
x_test=model.matrix(Sale_Price~., data=test_data)
x_test=x_test[, -1]
#use const from before
x_test=x_test[, -const]
#remember: ols prediction similar to the ones when lmabdas almost zero 
#hence run a cv.glmnet with the lambda=1e-4
cv_ridge2=run_cvglmnet(x=x, y=y, a=0, lambdas=seq(1e-4, 10, length.out=3))
#now make 'ols' prediction
test_pred_data$pred_ols=predict(cv_ridge2, newx=x_test, lambda=1e-4) %>% as.numeric()
#and prediction for opt lambda in cv.glmnet
test_pred_data$pred_ridge=predict(cv_ridge, newx=x_test, lambda=cv_ridge$lambda.min)%>%as.numeric()

#now calculate the respective MSEs
mse_ridge=oos_mse(test_pred_data$pred_ridge, test_pred_data$Sale_Price)
mse_ols=oos_mse(test_pred_data$pred_ols, test_pred_data$Sale_Price)
print(mse_ridge)
print(mse_ols)

##############
#Problem 3
##############
##LASSO REGRESSION 
#everything the same except alpha in glmnet is now =1 instead of 0
#add later