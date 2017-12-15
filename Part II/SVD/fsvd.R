# source('packages.R')

## @knitr train_test_matrix
###Get train and test matrix for recommenderlab
train_test_matrix <- function(training_data, testing_data, train_prop){
  
  all_ratings <- data.frame(bind_rows(training_data[,1:3],testing_data[,1:3]))
  names(all_ratings) <- c('msno','song_id','target')
  rating_matrix <- dcast(all_ratings, msno ~ song_id ,value.var = "target" , na.rm=FALSE)
  rownames(rating_matrix) <- unlist(rating_matrix[,1])#row indexed by msno
  names(rating_matrix) <- names(rating_matrix) #column indexed by song_id
  rating_matrix <- rating_matrix[,-1] #column indexed by song_id
  
  #split into train and test
  which_split <- sample(x = c(TRUE, FALSE),size = nrow(rating_matrix),replace = TRUE,prob = c(train_prop, 1-train_prop))
  train_matrix <- as.matrix(rating_matrix[which_split, ],rownames.force = NA)
  test_matrix <- as.matrix(rating_matrix[!which_split, ],rownames.force = NA)
  
  return (list('rating_matrix'= rating_matrix, 'test_matrix'=test_matrix,'train_matrix'=train_matrix))
}
train_prop = 0.8
data <- train_test_matrix(training_data, testing_data, 0.8)
train_matrix <- data$train_matrix
test_matrix <- data$test_matrix

## @knitr fsvd_model
#fsvd
k= 10
gamma = 0.015
lambda = 0.001
fsvd_model <- funkSVD(train_matrix, k = 10, gamma = 0.015, lambda = 0.001,min_improvement = 1e-06, min_epochs = 50, max_epochs = 200, verbose = FALSE)

### reconstruct the rating matrix as R = UV'
predicted_train_prob <- tcrossprod(fsvd_model$U, fsvd_model$V)
predicted_train_binary <- ifelse(predicted_train >=0.5,1,0)
dimnames(predicted_train_binary) <- dimnames(train_matrix)

## @knitr fsvd_prediction_error
#RMSE for predicted training data using Funk SVD with k = 10
train_err_rmse <- RMSE(train_matrix, predicted_train_binary)
RMSE(train_matrix, predicted_train)

#saveRDS(fsvd_model, "FSVD_info/fsvd_model.rds")
#saveRDS(train_err_rmse, "FSVD_info/train_err_rmse.rds")

## @knitr fsvd_predicted_test
### Predict ratings for test data with Funk SVD model
predicted_test_prob <- predict(fsvd_model, test_matrix, verbose = TRUE)
dimnames(predicted_test_prob) <- dimnames(test_matrix)
predicted_test_binary <- ifelse(predicted_test_prob >=0.5,1,0)
dimnames(predicted_test_binary) <- dimnames(test_matrix)

#saveRDS(predicted_test_prob, "FSVD_info/predicted_test_prob.rds")
#saveRDS(predicted_test_binary, "FSVD_info/predicted_test_binary.rds")

## @knitr fsvd_results
### Get full rating matrix
fsvd_full_rating_matrix <- rbind(predicted_train_binary,predicted_test_binary)
fsvd_full_score_matrix <- rbind(predicted_train,predicted_test_prob)
### Test rating predictions
FunkSVD_results = rep(0,nrow(testing_data))
FunkSVD_scores = rep(0,nrow(testing_data))
for ( line in 1:nrow(testing_data)){
  uid = as.character(testing_data[line,]$msno)
  iid = as.character(testing_data[line,]$song_id)
  FunkSVD_results[line] = fsvd_full_rating_matrix[uid,iid]
  FunkSVD_scores[line] = fsvd_full_score_matrix[uid,iid]
}
testing_data$FunkSVD_results <- FunkSVD_results 
testing_data$FunkSVD_scores <- FunkSVD_scores 
fsvd_test_results <- testing_data[,c(1,2,3,6,8)]
head(testing_data[,c(1,2,3,6,8)],10)

## @knitr fsvd_prediction_error
err=0
for (k in 1:nrow(fsvd_test_results)){
  err = err + abs(fsvd_test_results$FunkSVD_results[k] - fsvd_test_results$target[k])
}
err/nrow(fsvd_test_results)

#saveRDS(fsvd_full_rating_matrix, "FSVD_info/full_results.rds")
#saveRDS(fsvd_test_results, "FSVD_info/fsvd_test_results.rds")

