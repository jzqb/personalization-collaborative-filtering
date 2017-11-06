## @knitr arules


# Data Prep

#read data
ratings.dt <- fread('../data/jester-data-1.csv')

#Rename Jokes
names(ratings.dt)[2:101] <- paste0('j_',c(1:100))

#Get rid of count
ratings.dt[,V1 := NULL]

#make unrated null
ratings.dt[ratings.dt ==99] <- NA


#Create Tansaction dataset
ratings.dt$user_id <- c(1:length(ratings.dt$j_1))

# Make rating Discrete
long_ratings <- melt(ratings.dt, id.vars = "user_id")[!is.na(value)]
long_ratings$value <- discretize(long_ratings$value)
#its important to keep negative reviews different from + ones
long_ratings$nv <- paste0(long_ratings$variable,long_ratings$value)
ratings_discrete <- split(long_ratings$nv, long_ratings$user_id)

# Find rules
rules <- apriori(ratings_discrete, parameter =  list(minlen=2, maxlen=4), appearance = NULL, control = NULL)

## @knitr pca

#impute missing values
ratings2 <- imputePCA(ratings.dt[,.SD, .SDcols=paste0('j_',c(1:100))])
pca1 <- prcomp(ratings2$completeObs)
pca1

#use 0 for missing value
ratings3 <- ratings.dt
ratings3[is.na(ratings3)] <- 0

pca2 <- prcomp(ratings3[,.SD, .SDcols=paste0('j_',c(1:100))])
pca2
