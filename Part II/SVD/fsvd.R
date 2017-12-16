# source('packages.R')

## @knitr train_test_matrix
### Get train and test rating matrix
train_test_matrix <- function(training_data, testing_data, train_prop){
  
  #Replace known values
  testing <- testing_data
  testing[,3]<- rep(NA,nrow(testing))
  all_ratings <- data.frame(bind_rows(training_data[,1:3],testing))
  names(all_ratings) <- c('msno','song_id','target')
  
  #Convert to rating matrix
  rating_matrix <- dcast(all_ratings, msno ~ song_id ,value.var = "target" , na.rm=FALSE)
  rownames(rating_matrix) <- unlist(rating_matrix[,1])#row indexed by msno
  names(rating_matrix) <- names(rating_matrix) #column indexed by song_id
  rating_matrix <- rating_matrix[,-1] #column indexed by song_id
  
  #Split into train and test matrices
  which_split <- sample(x = c(TRUE, FALSE),size = nrow(rating_matrix),replace = TRUE,prob = c(train_prop, 1-train_prop))
  train_matrix <- as.matrix(rating_matrix[which_split, ],rownames.force = NA)
  test_matrix <- as.matrix(rating_matrix[!which_split, ],rownames.force = NA)
  
  return (list('rating_matrix'= rating_matrix, 'test_matrix'=test_matrix,'train_matrix'=train_matrix))
}

## @knitr train_predict
### Predict real score and binary ratings using Funk SVD
predict_svd <-function(fsvd_model, train_matrix, test_matrix) {
  
  #Reconstruct the rating matrix as R = UV',get train error
  predicted_train_prob <- tcrossprod(fsvd_model$U, fsvd_model$V)
  predicted_train_binary <- ifelse(predicted_train_prob >=0.5,1,0)
  dimnames(predicted_train_prob) <- dimnames(train_matrix)
  dimnames(predicted_train_binary) <- dimnames(train_matrix)
  train_rmse <- RMSE(train_matrix, predicted_train_binary)
  
  #Predict ratings for test data with Funk SVD model
  predicted_test_prob <- predict(fsvd_model, test_matrix, verbose = TRUE)
  dimnames(predicted_test_prob) <- dimnames(test_matrix)
  predicted_test_binary <- ifelse(predicted_test_prob >=0.5,1,0)
  dimnames(predicted_test_binary) <- dimnames(test_matrix)
  
  return (list(predicted_train_prob, predicted_train_binary,train_rmse,
               predicted_test_prob,predicted_test_binary))
}

## @knitr ignore-1
set.seed(123)
train_prop = 0.8
data <- train_test_matrix(training_data, testing_data, 0.8)
train_matrix <- data$train_matrix
test_matrix <- data$test_matrix
#data <- readRDS("SVD/data/data")

## @knitr fsvd_models
k= c(5,10,20,30,40)
gamma = 0.015 #learning rates
lambda = 0.001 #regularization

#Train Model with different k values
fsvd_model_5 <- funkSVD(train_matrix, k = 5, gamma = 0.015, lambda = 0.001,min_improvement = 1e-06, min_epochs = 50, max_epochs = 200, verbose = FALSE)
fsvd_model_10 <- funkSVD(train_matrix, k = 10, gamma = 0.015, lambda = 0.001,min_improvement = 1e-06, min_epochs = 50, max_epochs = 200, verbose = FALSE)
fsvd_model_20 <- funkSVD(train_matrix, k = 20, gamma = 0.015, lambda = 0.001,min_improvement = 1e-06, min_epochs = 50, max_epochs = 200, verbose = FALSE)
fsvd_model_30 <- funkSVD(train_matrix, k = 30, gamma = 0.015, lambda = 0.001,min_improvement = 1e-06, min_epochs = 50, max_epochs = 200, verbose = FALSE)
fsvd_model_40 <- funkSVD(train_matrix, k = 40, gamma = 0.015, lambda = 0.001,min_improvement = 1e-06, min_epochs = 50, max_epochs = 200, verbose = FALSE)
fsvd_models <- list(fsvd_model_5,fsvd_model_10,fsvd_model_20,fsvd_model_30,fsvd_model_40)

### @knitr fsvd_models_Predict
fsvd_model_5_result <- predict_svd(fsvd_model_5, train_matrix, test_matrix)
fsvd_model_10_result <- predict_svd(fsvd_model_10, train_matrix, test_matrix)
fsvd_model_20_result <- predict_svd(fsvd_model_20, train_matrix, test_matrix)
fsvd_model_30_result <- predict_svd(fsvd_model_30, train_matrix, test_matrix)
fsvd_model_40_result <- predict_svd(fsvd_model_40, train_matrix, test_matrix)
fsvd_models_results <- list(fsvd_model_5_result,fsvd_model_10_result,fsvd_model_20_result,fsvd_model_30_result,fsvd_model_40_result)

## @knitr ignore-2
#saveRDS(fsvd_models, "SVD/FSVD_info/fsvd_models.rds")
#saveRDS(fsvd_models_results, "SVD/FSVD_info/fsvd_models_results.rds")

## @knitr fsvd_results
rmse_all <- c('k=5'= fsvd_models_results[[1]][[3]],'k=10'= fsvd_models_results[[2]][[3]],'k=20'= fsvd_models_results[[3]][[3]],'k=30'= fsvd_models_results[[4]][[3]],'k=40'= fsvd_models_results[[5]][[3]])
rmse_all

## @knitr ignore-3
### Get full rating matrix
fsvd_full_rating_matrix <- rbind(fsvd_models_results[[2]][[2]],fsvd_models_results[[2]][[5]])
fsvd_full_score_matrix <- rbind(fsvd_models_results[[2]][[1]],fsvd_models_results[[2]][[4]])
#dimnames(fsvd_full_score_matrix) <- dimnames(data$rating_matrix)
#dimnames(fsvd_full_rating_matrix) <- dimnames(data$rating_matrix)

### Test rating predictions
FunkSVD_results_10 = rep(0,nrow(testing_data))
FunkSVD_scores_10 = rep(0,nrow(testing_data))
for ( line in 1:nrow(testing_data)){
  uid = as.character(testing_data[line,]$msno)
  iid = as.character(testing_data[line,]$song_id)
  FunkSVD_results_10[line] = fsvd_full_rating_matrix[uid,iid]
  FunkSVD_scores_10[line] = fsvd_full_score_matrix[uid,iid]
}
testing_data$FunkSVD_results_10 <- FunkSVD_results_10 
testing_data$FunkSVD_scores_10 <- FunkSVD_scores_10 
fsvd_test_results <- testing_data[,c('song_id', 'msno','target','FunkSVD_results_10','FunkSVD_scores_10')]
#saveRDS(fsvd_test_results, "SVD/FSVD_info/fsvd_test_results.rds")
#readRDS("SVD/FSVD_info/fsvd_test_results.rds")

## @knitr fsvd_sample_output
head(fsvd_test_results[,1:5],10)

## @knitr fsvd_prediction_error
err_10 =0
for (k in 1:nrow(fsvd_test_results)){
  err_10 = err_10 + abs(fsvd_test_results$FunkSVD_results_10[k] - fsvd_test_results$target[k])}
1-(err_10/nrow(fsvd_test_results))

## @knitr ignore-4
evaluation_result <- evaluate(fsvd_test_results$FunkSVD_scores_10,testing_data$target)

## @knitr svd_evaluate_result
evaluation_result

## @knitr ignore-5
#saveRDS(evaluation_result,"SVD/FSVD_info/evaluation_result.rds")
