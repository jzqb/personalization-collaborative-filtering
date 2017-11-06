# Data Prep

#read data
library(data.table)
library(arules)

ratings <- fread('data/jester-data-1.csv')

#Rename Jokes
names(ratings)[2:101] <- paste0('j_',c(1:100))

#Get rid of count
ratings[,V1 := NULL]

#make unrated null
ratings[ratings ==99] <- NA


# Association Rules

#Create Tansaction dataset
ratings$user_id <- c(1:length(ratings$j_1))

# Make rating Discrete
long_ratings <- melt(ratings, id.vars = "user_id")[!is.na(value)]
long_ratings$value <- discretize(long_ratings$value)
#its important to keep negative reviews different from + ones
long_ratings$nv <- paste0(long_ratings$variable,long_ratings$value)
ratings_discrete <- split(long_ratings$nv, long_ratings$user_id)

# Find rules
rules <- apriori(ratings_discrete, parameter =  list(minlen=2), appearance = NULL, control = NULL)
