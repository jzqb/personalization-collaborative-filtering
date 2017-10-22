
######################################################
##  Personalization: Collaborative Fitlering Part I ##
######################################################
"Importing the libraries here"
library(reshape2)
library(lsa)
library(Hmisc)
##################### jester data ####################
"Reading the data in here."
ratings <- read.csv('../data/jester-data-1.csv', header = FALSE)
ratings <- ratings[-1]
names(ratings) <- paste('j_',c(1:100), sep = "")
ratings[ratings ==99] <- NA
sparsity <- sum(is.na(ratings))/dim(ratings)[1]/dim(ratings)[2]
print(paste('The sparsity in the data is ', round(sparsity*100,2),"%", sep = ""))
"End of Data Reading"

###################### item based collaborative filtering ###################
"Let's calculate the similairty between item and prepare matrices for each similarity metric!"
item_ratings <- ratings
item_ratings_centered <- as.data.frame(t(apply(t(item_ratings), 2, FUN = function(x){x- mean(x, na.rm = T)})))

## starting with cosine sim ##
"Converting all NA values to 0, cosine sim doesn't matter as we want the inner product of the intersection"
cos_ratings <- item_ratings_centered
cos_ratings[is.na(cos_ratings)] <- 0
cosine_item_sim <- cosine(as.matrix(cos_ratings))

## pearson similarity for items ##
pear_item_sim <- as.matrix(cor(item_ratings, use = 'pairwise.complete.obs'))

## to predict particular rating for a user ##
predict_rating_item_cf <- function(k=20, user_no, joke_no, similarity = c('cosine', 'pearson')){
  user <- ratings[user_no,] 
  unrated <- names(user[,is.na(user)])
  rated <- names(user[,!is.na(user)])
  if(paste('j_',joke_no, sep = "") %nin% unrated){
    return(ratings[user_no, joke_no])
  }
  else{
    if(similarity == 'cosine'){use_sim = cosine_item_sim}
    else{use_sim = pear_item_sim}
    sim_mov <- as.data.frame(use_sim[,joke_no])
    sim_mov$item <- row.names(sim_mov)
    sim_mov <- sim_mov[order(-sim_mov$`use_sim[, joke_no]`),]
    sim_mov <- sim_mov[sim_mov$item != paste('j_',joke_no, sep = ""),]
    sim_mov <- sim_mov[1:k,]
    sim_mov <- sim_mov[sim_mov$item %in% rated,]
    um <- as.data.frame(t(user[,!is.na(user)]))
    um$item <- rownames(um)
    rate_insitu <- merge(sim_mov, um, by = 'item', all.x = T)
    names(rate_insitu) <- c('item', 'sim', 'rating')
    rate_insitu$rating <- as.numeric(as.character(rate_insitu$rating))
    return(sum(rate_insitu$sim*rate_insitu$rating)/sum(rate_insitu$sim))
  }
}
"End of Item Based Collaborative Filtering"

######################## user based collaborative filtering ####################
"Similarity calculation for users"
"Because this is infeasible in r to calculate correlation in a go, we will use a workaround"
"ff package preallocates a memory of a constant [big] size"
factorized <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))
  factors <- div[x %% div == 0L]
  return(factors)
}

bigcor <- function(x, verbose = TRUE, ...)
{
  library(ff, quietly = TRUE)
  NCOL <- ncol(x)
  nblocks = factorized(NCOL)[2]
  ## test if ncol(x) %% nblocks gives remainder 0
  if (NCOL %% nblocks != 0) stop("Choose different 'nblocks' so that ncol(x) %% nblocks = 0!")
  
  ## preallocate square matrix of dimension
  ## ncol(x) in 'ff' single format
  corMAT <- ff(vmode = "single", dim = c(NCOL, NCOL))
  
  ## split column numbers into 'nblocks' groups
  SPLIT <- split(1:NCOL, rep(1:nblocks, each = NCOL/nblocks))
  
  ## create all unique combinations of blocks
  COMBS <- expand.grid(1:length(SPLIT), 1:length(SPLIT))
  COMBS <- t(apply(COMBS, 1, sort))
  COMBS <- unique(COMBS)
  
  ## iterate through each block combination, calculate correlation matrix
  ## between blocks and store them in the preallocated matrix on both
  ## symmetric sides of the diagonal
  for (i in 1:nrow(COMBS)) {
    COMB <- COMBS[i, ]
    G1 <- SPLIT[[COMB[1]]]
    G2 <- SPLIT[[COMB[2]]]
    if (verbose) cat("Block", COMB[1], "with Block", COMB[2], "\n")
    flush.console()
    COR <- cor(MAT[, G1], MAT[, G2], use = 'pairwise.complete.obs',...)
    corMAT[G1, G2] <- COR
    corMAT[G2, G1] <- t(COR)
    COR <- NULL
  }
  
  gc()
  return(corMAT)
}
## pearson correlation matrix ##
MAT <- t(ratings)
pear_user_sim <- bigcor(MAT)

## prediction of rating for a user item combo ##
predict_rating_user_cf <- function(peer_size=200, user_no, joke_no, similarity = c('cosine', 'pearson')){
  if(!is.na(ratings[user_no, joke_no])){
    return(ratings[user_no, joke_no])
  }
  user <- ratings[user_no,]
  user_mean <- mean(t(user), na.rm = T)
  item <- as.data.frame(ratings[,paste('j_',joke_no,sep = "")])
  item$user <- rownames(item)
  names(item) <- c('item_rating', 'user')
  rated <- item[!is.na(item$item_rating),]
  #unrated <- names(user[,is.na(user)])
  #rated <- names(user[,!is.na(user)])
  
  if(similarity == 'cosine'){use_sim = pear_user_sim}
  else{use_sim = pear_user_sim}
  peer_set <- as.data.frame(use_sim[,user_no])
  peer_set$user <- row.names(peer_set)
  peer_set <- peer_set[order(-peer_set$`use_sim[, user_no]`),]
  peer_set <- peer_set[peer_set$user!= user_no,]
  peer_set <- peer_set[1:k,]
  names(peer_set) <- c('sim', 'user')
  peer_set <- merge(peer_set, item, by = 'user', all.x = T)
  peer_set <- peer_set[!is.na(peer_set$item_rating),]
  rating_insitu <- sum(peer_set$sim*peer_set$item_rating)/sum(peer_set$sim)
  return(rating_insitu)
}
"These algorithms return the collaborative filtering for neighbourhood based approaches: item and user"
"Have to add more similarity metrics like Jaccard, Eucliedian etc"