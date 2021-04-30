library('ggplot2')
library('nycflights13')
library('tibble')
library('dplyr')

#Problem 1 
#perform all tasks with dplyr
#a) familiarize with %>% pipe operator from dplyr 
#what does it? it it supplies the data object to a function directly, e.g. 
#with x=rnorm(100), x %>% max is the same as max(x)
#each function in dplyr uses a tibble dataframe as its first input 
#hence, we can chain functions easily such that we don't have to save elements in between 
#but supply them directly to the next step of our manipulation 
#for some examples see here: https://seananderson.ca/2014/09/13/dplyr-intro/ (especially the advanced example)

#b) load flights data and use first 5000 rows 
data(flights)
#slice is the dplyr function for slicing data row-wise (rows are also called cases)
flights=flights%>%slice(1:5000)

#c) select variable sched_dep_time as a vector and as a tibble 
#what is a tibble? It is basically a data.frame(). However, because R is old and some stuff has proven to not be 
#helpful while some new helpful thignshave emerged the tibble is replacing the data.frmae() and has some new options and 
#attributes while also removes some that are annoying in data.frame()
#as a vector: can retrieve it with dplyr function 'pull' (only allows for one column to be extracted)
sched_dep_vec=flights%>%pull('sched_dep_time')
#as a tibble: use dplyr function 'select' (allows to select multiple columns)
#difference tibble vs vector: vector only contains data, while tibble carries more metadata (e.g. think of numpy array vs pandas.Series)
sched_dep_tibble=flights%>%select('sched_dep_time')

#d) select multiple variables as a dataframe (tibble)
sub_df=flights%>%select(c('tailnum', 'sched_dep_time', 'sched_arr_time'))

#e) filter flight rows for specific carrier
#i) filter for carrier being AA
AA_flights=flights%>%filter(carrier=='AA')
#i) carrier being AA or DL
AA_DL_flights=flights%>%filter(carrier=='AA' | carrier=='DL')

#f) select all flights from may 2nd, 2013 - empty when only using the first 5000 observations, so for this task load data anew
data(flights)
flights020513=flights%>%filter(year==2013 & day==2 & month==5)

#but now continue for subset for faster speed 
data(flights)
flights=flights%>%slice(1:5000)

#g) get a sub tibble with three variables and then pass this directly to pipe that uses two of rows 
#of the subtibble to calculate a new column
#avg speed is distance/(air_time/60) (as I presume that air_time is in minutes)
sub_df_2=flights%>%select(c('tailnum', 'distance', 'air_time'))%>%mutate(speed=(distance/(air_time/60)))

#h) arrange this new df by ascending (descending) order of speed and then by tailnum (again ascending, then descending)
asc_speed=sub_df_2%>%arrange(speed)
desc_speed=sub_df_2%>%arrange(desc(speed))

#can also use arrange with multiple columns in which case the ordering of the named columns matters 
#data is ordered according to first variable, in case of tie the second variable is used and so on...

#i) calculate avg delay in departure and arrival by carrier
avg_arr_delay=flights%>%select(c('carrier', 'arr_delay', 'dep_delay'))%>%group_by(carrier)%>%summarize(across(c(dep_delay, arr_delay), ~mean(.x, na.rm=TRUE)))
#also calculate percentage of flights delayed (by arrival or by departure) 
pct_delays=flights%>%select(c('carrier', 'arr_delay', 'dep_delay'))%>%group_by(carrier)%>%summarize(mutate(share=mean(arr_delay>0|dep_delay>0, na.rm=TRUE)))
