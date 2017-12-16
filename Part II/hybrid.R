## @knitr hybrid_data_prep
load('FM/FM.RData')
user_key <- readRDS('content_based/user_key.rds')
user_key$user_code <- as.character(user_key$user_code)
user_key$msno <- as.character(user_key$msno)
song_key <- readRDS('content_based/song_key.rds')
song_key$song_code <- as.character(song_key$song_code)
song_key$song_id <- as.character(song_key$song_id)
merged_testing$msno <- as.character(merged_testing$msno)
merged_testing$song_id <- as.character(merged_testing$song_id)
merged_testing$user_key <- user_key$msno[match(merged_testing$msno, user_key$user_code)]
merged_testing$song_key <- song_key$song_id[match(merged_testing$song_id, song_key$song_code)]
merged_testing <- cbind(merged_testing, predFM.mcmc)
fm_predictions <- cbind(merged_testing[c('user_key','song_key','target')],predFM.mcmc)
fm_predictions$fm_prediction_class <- 0
fm_predictions$fm_prediction_class[fm_predictions$predFM.mcmc >=0.5] <- 1
fm_predictions <- fm_predictions[c('user_key', 'song_key', 'target', 'predFM.mcmc', 'fm_prediction_class')]
names(fm_predictions) <- c('msno', 'song_id','target',  'predicted_fm_score', 'predicted_fm_class')

svd_model <- readRDS('SVD/FSVD_info/fsvd_test_results.rds')
svd_model$song_id <- as.character(svd_model$song_id)
svd_model$msno <- as.character(svd_model$msno)
svd_model$user_key <- user_key$msno[match(svd_model$msno, user_key$user_code)]
svd_model$song_key <- song_key$song_id[match(svd_model$song_id, song_key$song_code)]
svd_predictions <- svd_model[,c('user_key', 'song_key', 'FunkSVD_scores_10', 'FunkSVD_results_10')]
names(svd_predictions) <- c('msno', 'song_id', 'predicted_svd_score', 'predicted_svd_class')

flat_table <- inner_join(merged_testing, svd_model, by = c("msno", "song_id"))
flat_table <- flat_table[c("msno", "song_id", "target.x", "predFM.mcmc", "FunkSVD_results_10", "FunkSVD_scores_10")]
names(flat_table) <- c('msno', 'song_id', 'target', 'pred_FM_score','pred_SVD_class','pred_SVD_score')
flat_table$target <- as.factor(flat_table$target)
flat_table$pred_FM_class <- 0
flat_table$pred_FM_class[flat_table$pred_FM_score >= 0.5] <- 1

set.seed(666)
split_data <- train_test_split(flat_table,0.75)
train_hybrid <- split_data$train_data
test_hybrid <- split_data$test_data
train_hybrid_feed <- train_hybrid[c("target","pred_FM_score","pred_SVD_score")]
test_hybrid_feed <- test_hybrid[c("pred_FM_score","pred_SVD_score")]

## @knitr load_flat_hybrid_table
flat_table <- readRDS('flat_table_hybrid.rds')

## @knitr load_flat_hybrid_table_head
head(flat_table)

## @knitr training_neural_net
nn_size <- c(25,50,100)
decay_rate <- c(0,0.01, 0.001)
accuracy <- data.frame()
for(nn in nn_size){
  for(dr in decay_rate){
    print(nn + dr)
    nn_hybrid <- nnet(target ~. , data=train_hybrid_feed, size = nn, decay = dr, maxit = 500)
    train_hybrid_feed$predicted_values <- as.vector(nn_hybrid$fitted.values)
    train_hybrid_feed$predicted_class <- 0
    train_hybrid_feed$predicted_class[train_hybrid_feed$predicted_values >= 0.5] <- 1
    conf_matrix <- table(train_hybrid_feed$target, train_hybrid_feed$predicted_class)
    train_hybrid_accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)
    train_append <- c(nn, dr, train_hybrid_accuracy)
    accuracy <- rbind(accuracy, train_append)
  }
}
names(accuracy) <- c("size", "decay_rate","train_error")

## @knitr hybrid_training_accuracy_read
accuracy <- readRDS('hybrid_accuracy.rds')

## @knitr hybrid_training_accuracy
accuracy$size <- as.numeric(accuracy$size)
accuracy$decay_rate <- as.numeric(accuracy$decay_rate)
accuracy$train_error <- as.numeric(accuracy$train_error)
accuracy_mat <- dcast(accuracy, size~decay_rate, value.var = 'train_error')
accuracy_mat

## @knitr testing_accuracy_hybrid
nn_hybrid <- nnet(target ~. , data=train_hybrid_feed, size = 100, decay = 0.01, maxit = 500)
test_hybrid$pred_hybrid <- predict(ann, test_hybrid_feed)
test_hybrid$hybrid_class <- 0
test_hybrid$hybrid_class[test_hybrid$pred_hybrid >= 0.5] <- 1
#saveRDS(test_hybrid, 'test_hybrid.rds')

## @knitr testing_accuracy_hybrid_read
test_hybrid <- readRDS('test_hybrid.rds')

## @knitr testing_accuracy_hybrid_comparison
conf_matrix <- table(test_hybrid$target, test_hybrid$hybrid_class)
hybrid_accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)

conf_matrix <- table(test_hybrid$target, test_hybrid$pred_SVD_class)
svd_accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)

conf_matrix <- table(test_hybrid$target, test_hybrid$pred_FM_class)
fm_accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)
paste('hybrid:', hybrid_accuracy)
paste("fm:", fm_accuracy)
paste("svd:", svd_accuracy)