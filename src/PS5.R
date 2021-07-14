rm(list=ls())

library(MASS)
library(dplyr)
library(tree)
library(randomForest)

##############
#FUNCTIONS
##############

gen_test_train=function(dat, test_n=1000){
  #using data generate a test and a training data set where the test set consists of 
  #test_n observations and rest are training data
  slices=sample.int(test_n, n=nrow(dat)) 
  train=dat[-slices, ]
  test=dat[slices, ]
  dat_list=list('train'=train, 'test'=test)
  return (dat_list)
}

mse=function(pred, raw){
  val=mean((pred-raw)^2)
  print(val)
  return (val)
}

bagg_trees=function(form, train_dat, test_dat, n_bag){
  #use bagging to create tree predictions following form
  #use train_dat and resample, test_dat for evaulation 
  #n_bag defines number of bags
  
  #set sample size of training sample generated in each iteration
  n1=nrow(train_dat)
  #save trees in a list 
  trees=vector('list', length=n_bag)
  #save predictions in a matrix where columns signal number of tree and we have as many rows
  #as values to be predicted -> length of test data
  tree_preds=matrix(NA, nrow=nrow(test_dat), ncol=n_bag)
  #then loop 
  for (i in 1:n_bag){
    #generate a new training sample bootstrapped from tain_dat
    slice=sample.int(n1, replace=TRUE) 
    train_new=train_dat[slice, ]
    #generate tree and save in tree list at position i 
    trees[[i]]=tree(form, data=train_new)
    #create predictions and save in matrix 
    tree_preds[, i]=predict(trees[[i]], newdata=test_dat)
    print(i)
  }
  #save trees list and tree prediction matrix in named list 
  return_list=list('trees'=trees, 'predictions'=tree_preds)
  #return this list 
  return(return_list)
}

rf_preds=function(train, test, form, mtry){
  #set up a random forest with train and mtry, calculate predictions for test 
  rf=randomForest(form, data=train, mtry=mtry)
  #create prediction 
  rf_preds=predict(rf, newdata=test)
  
  return(rf_preds)
}

##############
# Problem 1
##############
#the variable we want to predict is medv in the Boston dataset

#a) 
#load the boston dataset from MASS
data(Boston)
#create a test and training set using half of observations in each respectively
test_train=gen_test_train(Boston, test_n=nrow(Boston)/2)
#unpack the list returned
train_dat=test_train$train
test_dat=test_train$test
#check dimensions 
dim(train_dat)
dim(test_dat)
#they are the same, perfect!

#b) 
##PREDICTION 
#fit a random tree on medv using all remaining predictors
#can use the tree function from tree library 
#similar to lm, this is also formula based 
r_tree=tree(medv~., data=train_dat)
#this is a 'tree' class 
summary(r_tree)
plot(r_tree)
text(r_tree, cex=0.5)
#maybe check out how to do this better in ggplot or similar later on 

#c) 
##PRUNING
#use cross validation to prune the tree 
#pruning=cost-complexity pruning, i.e. simply adding penalty to original algorithm that penalizes 
#size of tree with a constant factor k
#can simply use the cv.tree function from tree library
cv_pruning=cv.tree(r_tree)
#this calculatse total deviance for each k (complexity of tree) and corresponding tree sizes
#(size=number of terminal nodes)
#!NOTE: deviance is total sum of suwares, for MSE have to divide by N (!)
plot(cv_pruning)
#this plot shows size (bottom) and deviance (top) of tree on x-axis against deviance of tree
#we see that until 4, each additional terminal node largely increases precision of prediction 
#while further terminal nodes add less and less ->at some prediction doesn't improve anymore
#(around size 8)

##PREDICTIONS
#create a tree with optimal size -> size with mind(deviance) in cv_pruning 
opt_size=cv_pruning$size[which.min(cv_pruning$dev)]
#then call function prune.tree and supply optimal size, this fits a tree with this size
#on the tree object initiated earlier
pruned_tree=prune.tree(r_tree, best=opt_size)
#we now want to compute predictions and find MSE of test sample 
summary(pruned_tree)
#create a df with predicted and true values 
test_pred_dat=test_dat%>%dplyr::select(medv)
#now create predictions from pruned tree 
test_pred_dat$pruned_pred=predict(pruned_tree, newdata=test_dat)
#now calculate MSE
mse_pruned=mse(test_pred_dat$pruned_pred, test_pred_dat$medv)

##GRAPHICAL COMPARISON 
#for plotting two plot at the same time need to adjust par()
par(mfrow=c(1, 2))
plot(pruned_tree)
text(pruned_tree, cex=0.6)
plot(r_tree)
text(r_tree, cex=0.6)
#unpruned and pruned tree are almost identical as the former only has one terminal node
#more (11 instead of 10)
par(mfrow=c(1, 1))

#d) 
##PREDICTIONS 
#already calculated predictions for pruned tree 
#add predictions for unpruned tree 
test_pred_dat$unpruned_pred=predict(r_tree, newdata=test_dat)
#and calculate the MSE 
mse_unpruned=mse(test_pred_dat$unpruned_pred, test_pred_dat$medv)
print(mse_pruned)
#the MSEs don't differ much

#e) 
##BAGGING 
#make use of bagging procedure coding it by hand 
#Bagging=bootsstrap aggregating
#make boostrapped samples from train data in each iteration and fit tree on it 
#save predictions in a matrix as well as corresponding trees 
#use 500 trees
nbag=500
bagged_trees=bagg_trees('medv~.', train_dat, test_dat, n_bag=nbag)
trees=bagged_trees$trees
preds=bagged_trees$predictions

##ILLUSTRATION 
#illustrate the first four trees 
par(mfrow=c(2, 2))
for (i in 1:4){
  plot(trees[[i]])
  text(trees[[i]], pretty=0)
}

##FIRST SPLIT INVESTIGATION
#can access the variables that are split on in the frame attribute of a tree 
#save for each tree the first split
#can use an inline function to access the first variable name in frame attribute, which signals
#first split
first_splits=sapply(1:nbag, function(j) trees[[j]]$frame[1, 1])
class(first_splits)
table(first_splits)
#we can see that roouhgly 80% of the trees choose the same variable to split on first, (lstat) 
#while the remainder chooses another variable, which is the same for all of the remainder (rm)

##PREDICTIONS
#take average of single predictions to get predictions of bagging 
test_pred_dat$bagging_preds=rowMeans(preds)
#also calculate MSE 
mse_bagging=mse(test_pred_dat$bagging_preds, test_pred_dat$medv)

#f) 
#now create predictions using a random forest with different number of variables considered 
#to be drawn at each split (determined by argument mtry)
#use the randomForest function from randomForest library to create a random forest class 
rf=randomForest(medv~., data=train_dat)
rf_preds(train_dat, test_dat, medv~., mtry=10)
#use functio rf_preds to generate predictions for different settings of mtry 
#and save predictions in a matrix 
#set max value of mtry to be considered 
max_m=13
#mtry=13 should yield same results as bagging BUT randomForest() function chooses 
#variables to be considered WITH REPLACEMENT by default 
#create matrix predictions will be saved in 
pred_mat=matrix(NA, nrow=nrow(test_dat), ncol=max_m)
#also calculate mse and save in a list 
mse_mtry=vector('list', length=max_m)
#then make predictions and calculate mse for each m form 1 to 13 
for (i in 1:max_m){
  #make predictions
  pred_mat[, i]=rf_preds(train_dat, test_dat, medv~., mtry=i)
  #calculate mse
  mse_mtry[[i]]=mse(pred_mat[, i], test_dat$medv)
}

#g) 
mse_pruned
mse_unpruned
mse_bagging
mse_mtry

#mse is smallest when using randomForest() and consider many variables at split 
#randomForest() performs substantially better than self coded bagging approach (not sure why
#though) 
#in geneal we can say that some kind of bagging/ensemble predictor in form of a forest will
#clearly outperform even a optimally tuned tree even when rf not tuned

##############
# Problem 2
##############
