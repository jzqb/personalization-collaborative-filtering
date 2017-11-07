## @knitr arules

# Data Prep
ratings.dt <- fread('../data/jester-data-1.csv') #read data
names(ratings.dt)[2:101] <- paste0('j_',c(1:100)) #Rename Jokes
ratings.dt[,V1 := NULL] #Get rid of count
ratings.dt[ratings.dt ==99] <- NA #make unrated null

#Create Transaction dataset
ratings.dt$user_id <- c(1:length(ratings.dt$j_1))
long_ratings <- melt(ratings.dt, id.vars = "user_id")[!is.na(value)]
long_ratings$value <- discretize(long_ratings$value) # Make rating Discrete.
levels(long_ratings$value) <- c("dislike", "neutral", "like")
long_ratings$nv <- paste0(long_ratings$variable, "-",long_ratings$value) #its important to keep negative reviews different from + ones
ratings_discrete <- split(long_ratings$nv, long_ratings$user_id)

# Find rules
assoc_rules <- apriori(ratings_discrete, parameter =  list(minlen=1, maxlen=3, support=0.01, conf=0.8, maxtime=8), control = NULL)
assoc_rules <- sort(assoc_rules, by="confidence")

## @knitr arules-top10

inspect(assoc_rules[1:10])

## @knitr arules-predict

# find all rules whose lhs matches 
assoc_rules <- sort(assoc_rules, by="lift")
user_a<-as(ratings_discrete[42], "itemMatrix")
rulesMatch <- is.subset(assoc_rules@lhs, user_a)
applicable <- (assoc_rules[which(rulesMatch ==TRUE)])
plot(applicable[1:20], method="graph", control=list(type="items"))

#Look a users predictions
unlist(unique(as(rhs(applicable), "list")))

#What have they rated
inspect(user_a)
       
## @knitr pca

# Rating Matrix
ratings.mat <- as.matrix(as.data.frame(ratings.dt[,user_id:=NULL]))
ratings.mat <- as(ratings.mat, "realRatingMatrix")

# Default Model
ratings.svd <- Recommender(ratings.mat, method = "SVD")

## @knitr pca-42
reco_user_42 <- predict(ratings.svd, newdata=ratings.mat[42,])
getList(reco_user_42)
