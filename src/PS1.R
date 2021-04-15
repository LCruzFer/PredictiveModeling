#need Rlab for the bernoulli distribution
library('Rlab')
library('nycflights13')
library('ggplot2')
wd = getwd()

#! Q1 
#creating basic vectors
#seq ends with last defined number and not one short of it 
a = seq(1, 19)
#a vector of specified length with same element 
b = rep(5, 20)
#now a custom vector 
favs = c('1984', 'Hobbit', 'Dune')
#std normal 
n = 100
normal = rnorm(n, 0, 1)
#matrices 
#containing a sequence spread over rows
mat1 = matrix(seq(1, 25), ncol = 5, byrow = TRUE)
#draws from uniform dist in matrix 
mat2 = matrix(runif(400, 0, 1), ncol = 20)
#now draws from a bernoulli dist - need a library for that, namely Rlab
mat3 = matrix(rbern(40, prob = 0.5), nrow = 20)
#now a dataframe 
df = data.frame(a = seq(1, 20), b = letters[1:20])

#! Q2 
#load the data -> do not assign to a new object, it will load an object with the name flights !!
data(flights)
#get max of column "airtime"
#set na.rm = True to ignore the NA values since ow this max() returns NA
max_air = max(flights$air_time, na.rm = TRUE)
#get mean distance 
mean_dist = mean(flights$distance)
#get a subset of the df 
subflights = flights[1:5000, ]
#now make a scatter plot 
plot(subflights$distance, subflights$air_time)
#use ggplot2 to do it as it looks way better and is a nicer plotting tool 
#first declare the plotfigure, i.e. which dataset to use, you could also declare aes in there already but 
#as I use the dataset for several plots I leave this unspecified and only specify it in the respective element
plot = ggplot(subflights)
plot + geom_point(aes(distance, air_time))
#now make a histogram using geom_histogram 
plot + geom_histogram(aes(air_time))

#Q3 
multiply = function(a, b){
  res = a*b
  return(res)
}

oddoreven = function(num){
  if (num %% 2 == 0){
    print('Number is even!')
  } else {
    print('Number is odd!')
  }
}

