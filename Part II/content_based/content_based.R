library(tidyverse)
#library(feather)
library(data.table)
library(viridis)
library(DT)
library(lubridate)
library(magrittr)
library(readr)
library(dplyr)
library(tidyr)
library(Matrix)
library(text2vec)
options(tibble.print_max = 5, tibble.print_min = 5)
setwd("~/Desktop/drive/dsi/fall-17/personalization/Part II")
rm(list= ls())
train <- as.tibble(fread('data/train.csv'))
songs <- as.tibble(fread('data/songs.csv'))
song_extra_info <- as.tibble(fread('data/song_extra_info.csv'))

ratings <- as.data.table(train[c('msno', 'song_id', 'target')])

filter_song_data <- as.data.frame(table(ratings$song_id))
filter_user_data <- as.data.frame(table(ratings$msno))

names(filter_song_data) <- c('song_id', 'count')
names(filter_user_data) <- c('msno', 'count')
ratings$song_count <- filter_song_data$count[match(ratings$song_id, filter_song_data$song_id)]
ratings$user_count <- filter_user_data$count[match(ratings$msno, filter_user_data$msno)]

sc = 1000
uc = 500
filter_data <- subset(ratings, ratings$song_count>= sc & ratings$user_count >=uc)
nrow(filter_data)/nrow(ratings)
paste('Users:', length(unique(filter_data$msno)))
paste('Songs:', length(unique(filter_data$song_id)))

user_key <- as.data.frame(cbind(unique(filter_data$msno),c(1:length(unique(filter_data$msno)))))
names(user_key) <- c('user_code', 'msno')
song_key <- as.data.frame(cbind(unique(filter_data$song_id), c(1:length(unique(filter_data$song_id)))))
names(song_key) <- c('song_code', 'song_id')
filter_data$msno <- user_key$msno[match(filter_data$msno,user_key$user_code)]
filter_data$song_id <- song_key$song_id[match(filter_data$song_id, song_key$song_code)]


set.seed(666)
train_prop = 0.75
train_test_split <- function(ratings, train_proportion = 0.8){
  sample_size <- floor(train_proportion*nrow(ratings))
  train_ind <- sample(seq_len(nrow(ratings)), size = sample_size)
  train_data <- ratings[train_ind,]
  test_data <- ratings[-train_ind,]
  split_data <- list('train_data' = train_data, 'test_data' = test_data)
  return(split_data)
}
split_data <- train_test_split(filter_data,0.75)
training_data <- split_data$train_data
testing_data <- split_data$test_data

#train <- subset(train, train$msno %in% filter_data$msno & train$song_id %in% filter_data$song_id)

songs$song_id <- song_key$song_id[match(songs$song_id, song_key$song_code)]
songs <- subset(songs, songs$song_id %in% filter_data$song_id)
song_extra_info$song_id <- song_key$song_id[match(song_extra_info$song_id, song_key$song_code)]
song_extra_info <- subset(song_extra_info, song_extra_info$song_id %in% filter_data$song_id)
save.image('read_filter.RData')
rm(list = ls())
load('read_filter.RData')
# write.csv(train, 'filter_data/train.csv')
# write.csv(songs, 'filter_data/songs.csv')
# write.csv(song_extra_info, 'filter_data/songs_extra_info.csv')

genres <- as.data.frame(songs$genre_ids, stringsAsFactors=FALSE)
genres <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres) <- c(1:3)
genres$song_id <- songs$song_id

genre_data <- genres[c(4,1)]
for(i in 2:3){
  tester <- genres[c(4,i)]
  tester <- na.omit(tester)
  names(tester) <- names(genre_data)
  genre_data <- rbind(genre_data, tester)
}
names(genre_data) <- c('song_id', 'genre')
genre_binary <- dcast(genre_data, formula = song_id~genre, fun.aggregate = NULL)
genre_binary[-1][!is.na(genre_binary[-1])] <- 1
genre_binary[is.na(genre_binary)] <- 0
#saveRDS(genre_binary, 'genre_binary.rds')
#genre_binary <- readRDS('genre_binary.rds')


lang_data <- songs[c('song_id', 'language')]
lang_data <- na.omit(lang_data)
lang_binary <- dcast(lang_data, formula = song_id~language, fun.aggregate = NULL)
lang_binary[-1][!is.na(lang_binary[-1])] <- 1
lang_binary[is.na(lang_binary)] <- 0
#saveRDS(lang_binary, 'lang_binary.rds')
#lang_binary <- readRDS('lang_binary.rds')


artists <- as.data.frame(songs$artist_name, stringsAsFactors=FALSE)
artists <- as.data.frame(tstrsplit(artists[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(artists) <- c(1:3)
artists$song_id <- songs$song_id

artists_data <- artists[c(4,1)]
for(i in 2:3){
  tester <- artists[c(4,i)]
  tester <- na.omit(tester)
  names(tester) <- names(artists_data)
  artists_data <- rbind(artists_data, tester)
}
names(artists_data) <- c('song_id', 'artist')
artists_binary <- dcast(artists_data, formula = song_id~artist, fun.aggregate = NULL)
artists_binary[-1][!is.na(artists_binary[-1])] <- 1
artists_binary[is.na(artists_binary)] <- 0
#saveRDS(artists_binary, 'artists_binary.rds')
#artists_binary <- readRDS('artists_binary.rds')


# composer
composer <- as.data.frame(songs$composer, stringsAsFactors=FALSE)
composer <- as.data.frame(tstrsplit(composer[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(composer) <- c(1:11)
composer$song_id <- songs$song_id

composer_data <- composer[c(12,1)]
for(i in 2:11){
  tester <- composer[c(12,i)]
  tester <- na.omit(tester)
  names(tester) <- names(composer_data)
  composer_data <- rbind(composer_data, tester)
}
names(composer_data) <- c('song_id', 'composers')
composer_binary <- dcast(composer_data, formula = song_id~composers, fun.aggregate = NULL)
composer_binary[-1][!is.na(composer_binary[-1])] <- 1
composer_binary[is.na(composer_binary)] <- 0
#saveRDS(composer_binary, 'composer_binary.rds')
#composer_binary <- readRDS('composer_binary.rds')

# lyricist
lyricist <- as.data.frame(songs$lyricist, stringsAsFactors=FALSE)
lyricist <- as.data.frame(tstrsplit(lyricist[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(lyricist) <- c(1:11)
lyricist$song_id <- songs$song_id

lyricist_data <- lyricist[c(12,1)]
for(i in 2:11){
  tester <- lyricist[c(12,i)]
  tester <- na.omit(tester)
  names(tester) <- names(lyricist_data)
  lyricist_data <- rbind(lyricist_data, tester)
}
names(lyricist_data) <- c('song_id', 'lyricists')
lyricist_binary <- dcast(lyricist_data, formula = song_id~lyricists, fun.aggregate = NULL)
lyricist_binary[-1][!is.na(lyricist_binary[-1])] <- 1
lyricist_binary[is.na(lyricist_binary)] <- 0
#saveRDS(lyricist_binary, 'lyricist_binary.rds')
#lyricist_binary <- readRDS('lyricist_binary.rds')


library(Hmisc)
library(naivebayes)
songs$song_length_bucket <- cut2(songs$song_length, cuts = quantile(songs$song_length, probs = c(0,0.25,0.75)))
length_data <- songs[c('song_id', 'song_length_bucket')]
length_data <- na.omit(length_data)
length_binary <- dcast(length_data, formula = song_id~song_length_bucket, fun.aggregate = NULL)
length_binary[-1][!is.na(length_binary[-1])] <- 1
length_binary[is.na(length_binary)] <- 0
names(length_binary) <- c('song_id','low', 'med', 'high')
#saveRDS(length_binary, 'length_binary.rds')
#length_binary <- readRDS('length_binary.rds')


big_flat_item_features <- as.data.frame(cbind(length_binary, 
                                              genre_binary[2:ncol(genre_binary)],
                                              artists_binary[2:ncol(artists_binary)],
                                              composer_binary[2:ncol(composer_binary)],
                                              lyricist_binary[2:ncol(lyricist_binary)],
                                              lang_binary[2:ncol(lang_binary)]))
big_flat_item_features[2:ncol(big_flat_item_features)] <- sapply(big_flat_item_features[2:ncol(big_flat_item_features)], as.numeric)

training_data <- as.tibble(training_data)



user_item_rating <- dcast(training_data, formula = msno~song_id, value.var = 'target')


total_prediction_matrix <- data.frame()
near_neighbors = 2^c(2:5)
for(active_user in user_key$msno){
  items_rated <- training_data$song_id[training_data$msno == active_user]
  train <- subset(big_flat_item_features, big_flat_item_features$song_id %in% items_rated)
  user_target <- subset(training_data[1:3], training_data$msno == active_user)
  user_target$target <- as.factor(user_target$target)
  train$target <- user_target$target[match(train$song_id, user_target$song_id)]
  train <- train[-1]
  train_knn <- train[1:2035]
  #test_knn <- train[1:2035]
  cl <- train$target
  
  items_not_rated <- testing_data$song_id[testing_data$msno == active_user]
  test <- subset(big_flat_item_features, big_flat_item_features$song_id %in% items_not_rated)

  user_test_target <- subset(testing_data, testing_data$msno == active_user)
  test$target <- testing_data$target[match(test$song_id, testing_data$song_id)]
  test_knn = test[2:2036]
  for(nn in near_neighbors){
    preds <- knn(train_knn, test_knn, cl, k = nn, prob=TRUE)
    prediction_matrix <- as.data.frame(cbind(active_user, test$song_id, test$target, attributes(preds)$prob, as.numeric(as.character(preds)), nn))
    names(prediction_matrix) <- c('active_user', 'song_id', 'target', 'prediction_prob', 'prediction', 'nn')
    total_prediction_matrix <- rbind(total_prediction_matrix, prediction_matrix)
    
    # library(randomForest)
    # items_rated <- training_data$song_id[training_data$msno == active_user]
    # train <- subset(big_flat_item_features, big_flat_item_features$song_id %in% items_rated)
    # user_target <- subset(training_data[1:3], training_data$msno == active_user)
    # user_target$target <- as.factor(user_target$target)
    # names(train) <- paste('f_', trimws(names(train)), sep= "")
    # train$target <- user_target$target[match(train$f_song_id, user_target$song_id)]
    # user_rf <- randomForest(target ~ . , data = train,  mtry=10, ntree=40)
    
    
  }
  print(paste(active_user, "complete"))
}
total_prediction_matrix$random_prediction <- round(runif(n = nrow(total_prediction_matrix), min = 0, max = 1))
total_prediction_matrix$score = as.numeric(as.character(total_prediction_matrix$prediction_prob))
total_prediction_matrix$score[total_prediction_matrix$prediction == 0] <- 1-total_prediction_matrix$score[total_prediction_matrix$prediction == 0]

evaluate <- function(scores, target){
  ## Evaluate Predictions
  ## input 
  # scores: vector of scores 
  # (i.e. probability that an observation should have target value 1)
  # target: vector of actual target values
  ## returns 
  # dataframe of evaluation metrics
  # confusion matrix for predicted and actual target values
  # roc object for plotting
  
  pred.target <- ifelse(scores >= 0.5, 1, 0)
  accuracy <- sum(pred.target == target)/length(target)
  error <- 1 - accuracy
  
  conf.mat <- table(pred.target, target, dnn = c("predicted","actual"))
  precision <- conf.mat[2,2]/sum(conf.mat[2,])
  recall <- conf.mat[2,2]/sum(conf.mat[,2])
  F1 <- 2 * (precision*recall/(precision + recall))
  auc <- auc(target, scores)
  eval.metrics <- data.frame(cbind(accuracy, error, precision, recall, F1, auc))
  
  roc <- roc(target, scores, direction="<")
  
  return(list(eval.metrics, conf.mat, roc))
}

content_based_eval <- evaluate(total_prediction_matrix$score, total_prediction_matrix$target)
content_based_eval_plot <- ggroc(content_based_eval[[3]], alpha = 0.5, colour = "blue", linetype = 2, size = 2) +
  theme(plot.title = element_text(hjust = 0.5)) 

## @knitr evaluate_FM_print
content_based_eval[[1]] #print evaluation metrics
content_based_eval[[2]] #print confusion matrix
content_based_eval_plot + ggtitle('ROC Curve for Content Based Recommender') +
  theme(plot.title = element_text(hjust = 0.5))


conf_matrix <- table(total_prediction_matrix$target, total_prediction_matrix$prediction)
conf_matrix
(conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)


conf_matrix <- table(total_prediction_matrix$target, total_prediction_matrix$random_prediction)
conf_matrix
(conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)



train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
knn(train, test, cl, k = 3, prob=TRUE)

sum(names(user_item_rating[2:ncol(user_item_rating)]) == big_flat_item_features$song_id)

user_profiles <- as.matrix(sapply(user_item_rating[2:ncol(user_item_rating)], as.numeric)) %*% as.matrix(sapply(big_flat_item_features[2:ncol(big_flat_item_features)], as.numeric))


library(recommenderlab)
training_sparse <- as(as.matrix(user_item_rating[-1]), 'realRatingMatrix')
ubcf <- Recommender(data = training_sparse, method = "UBCF", parameter = list(method = "Jaccard"))

## @knitr predicted_ratings_user_based_algorithm
predicted_ratings <- predict(ubcf, training_sparse, type = 'ratings')
predicted_ratings <- as(predicted_ratings, 'matrix')
predicted_ratings <- as.data.frame(predicted_ratings)
predicted_ratings$msno <- user_item_rating$msno
predicted_ratings_melt <- melt(predicted_ratings, id.vars = 'msno', variable.name = 'song_id')
results <- merge(testing_data, predicted_ratings_melt, by = c('msno', 'song_id'))


predicted_ratings$user <- paste('u_', as.character(predicted_ratings$user), sep = "")
predicted_ratings$item <- as.character(predicted_ratings$item)
test_data$user <- as.character(test_data$user)
test_data$variable <- as.character(test_data$variable)
test_data <- merge(test_data, predicted_ratings, by.x = c('user', 'variable'), by.y = c('user','item'), all.x = TRUE)
