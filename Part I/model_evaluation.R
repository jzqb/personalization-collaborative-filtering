# source('packages_libraries.R')
# source('aux_funct.R')
# source('data_exploration.R')
# source('neighborhood_based.R')

## @knitr data_setup
ratings <- ratings[1:10000,]
set.seed(666)
train_proportion = 0.8
split <- train_test_split(ratings, train_proportion = train_proportion)
train_data <- split$train_data
training_sparse <- as(as.matrix(train_data), 'realRatingMatrix')


## @knitr eval_scheme
n_fold <- 5
items_to_keep <- 15 
rating_threshold <- 5
n_recommendations <- c(1, 3, 5, seq(10, 40, 10))
k_items <- c(5,seq(10, 40, 10)) 
nn_users <- c(5, 10, 25, 100, 250)
k_svd <- c(2,4,6,8,9,10,11,12, 14,16, 18,20) #possible values for k in svd
eval_sets <- evaluationScheme(data = training_sparse, method = "cross-validation", 
                  k = n_fold, given = items_to_keep, goodRating = rating_threshold)

## @knitr compare_cf_models
models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  SVD = list(name = "SVD"),
  random = list(name = "RANDOM", param=NULL)
)
list_results <- evaluate(x = eval_sets, method = models_to_evaluate, type="topNList", n = n_recommendations)
rat_results <- evaluate(x = eval_sets, method = models_to_evaluate, type="ratings")
err <- do.call(rbind.data.frame, avg(rat_results))

## @knitr ignore-1
#saveRDS(list_results, "model info/list_results.rds")
#saveRDS(rat_results, "model info/rat_results.rds")
#saveRDS(err, "model info/err.rds")


## @knitr ibcf_tune_k 
models_to_evaluate_ib <- lapply(k_items, function(k){
  list(name = "IBCF", param = list(method = "pearson", k = k))
})
names(models_to_evaluate_ib) <- paste0("IBCF_k_", k_items)
list_results_ib <- evaluate(x = eval_sets, method = models_to_evaluate_ib, n = n_recommendations)
avg_matrices_ib <- lapply(list_results_ib, avg)
rat_results_ib <- evaluate(x = eval_sets, method = models_to_evaluate_ib, type="ratings")
err_ib <- do.call(rbind.data.frame, avg(rat_results_ib))

## @knitr ubcf_tune_nn
models_to_evaluate_ub <- lapply(nn_users, function(k){
  list(name = "UBCF", param = list(method = "cosine", nn = k))
})
names(models_to_evaluate_ub) <- paste0("UBCF_nn_", nn_users)
list_results_ub <- evaluate(x = eval_sets, method = models_to_evaluate_ub, n = n_recommendations)
rat_results_ub <- evaluate(x = eval_sets, method = models_to_evaluate_ub, type="ratings")
err_ub <- do.call(rbind.data.frame, avg(rat_results_ub))


## @knitr svd_tune_k
models_to_evaluate_svd <- lapply(k_svd, function(k){
  list(name = "SVD", param = list(k=k))
})
names(models_to_evaluate_svd) <- paste0("SVD_k_", k_svd)
list_results_svd <- evaluate(x = eval_sets, method = models_to_evaluate_svd, n = n_recommendations)
rat_results_svd <- evaluate(x = eval_sets, method = models_to_evaluate_svd, type="ratings")
err_svd <- do.call(rbind.data.frame, avg(rat_results_svd))

## @knitr ignore-2
#saveRDS(list_results_svd, "model info/list_results_svd.rds")
#saveRDS(rat_results_svd, "model info/rat_results_svd.rds")
#saveRDS(err_svd, "model info/err_svd.rds")


## @knitr err_vs_size
sample_test <- function(x, n = 15){
  ind <- sample(1:length(x[!is.na(x)]), n)
  x[!is.na(x)][-ind] <- NA
  return(x)
}

sample_prop <- c(.001, .005, seq(.01, .1, by = .01), seq(.15, .5, by = .05))
train_eval <- function(proportion, given = 15, method, param, data = ratings){
  split <- train_test_split(data, train_proportion = proportion)
  train_data <- split$train_data
  training_sparse <- as(as.matrix(train_data), 'realRatingMatrix')
  ibcf_pearson <- Recommender(data = training_sparse, method = method, parameter = param)
  
  test_data <- split$test_data
  test_data_sparse <- as(as.matrix(test_data), 'realRatingMatrix')
  test_data_sample <- apply(test_data, 1, sample_test, n = given)
  test_data_sample  <- as.data.frame(t(test_data_sample))
  test_sample_sparse <- as(as.matrix(test_data_sample), 'realRatingMatrix')
  
  predicted_ratings <- predict(ibcf_pearson, test_sample_sparse, type = 'ratings')
  err_metrics <- calcPredictionAccuracy(x = predicted_ratings, data = test_data_sparse)
  return(err_metrics)
}

#Item-based Pearson
size_err_IB <- lapply(sample_prop, train_eval, method = "IBCF", param = list(k = 30, method = 'pearson'))
size_err_IB <- do.call(rbind.data.frame, size_err_IB)
names(size_err_IB) <- c('RMSE','MSE','MAE')
size_err_IB$train_prop <- sample_prop
size_err_IB$model <- "Pearson Item-Based CF"

#User-based Cosine
size_err_UB <- lapply(sample_prop, train_eval, method = "UBCF", param = list(nn = 100, method = 'cosine'))
size_err_UB <- do.call(rbind.data.frame, size_err_UB)
names(size_err_UB) <- c('RMSE','MSE','MAE')
size_err_UB$train_prop <- sample_prop
size_err_UB$model <- "Cosine User-Based CF"

#Random
size_err_rand <- lapply(sample_prop, train_eval, method = "RANDOM", param = NULL)
size_err_rand <- do.call(rbind.data.frame, size_err_rand)
names(size_err_rand) <- c('RMSE','MSE','MAE')
size_err_rand$train_prop <- sample_prop
size_err_rand$model <- "Random"

#SVD
size_err_svd <- lapply(sample_prop, train_eval, method = "SVD", param = list(k=10))
size_err_svd <- do.call(rbind.data.frame, size_err_svd)
names(size_err_svd) <- c('RMSE','MSE','MAE')
size_err_svd$train_prop <- sample_prop
size_err_svd$model <- "SVD"


size_err_all <- rbind(size_err_IB, size_err_UB, size_err_svd, size_err_rand)

## @knitr ignore-3
#saveRDS(size_err_all, "model info/size_err_all.rds")


## @knitr err_vs_numitems
min_ratings <- min(apply(ratings, 1, function(x){length(which(!is.na(x)==TRUE))}))
max_keep <- 5*floor(min_ratings/5)
keep <- c(3, seq(5,max_keep,5))

#Item-based Pearson
keep_err_IB <- lapply(keep, train_eval, proportion = .2, method = "IBCF", param = list(k = 30, method = 'pearson'))
keep_err_IB <- do.call(rbind.data.frame, keep_err_IB)
names(keep_err_IB) <- c('RMSE','MSE','MAE')
keep_err_IB$ratings_given <- keep
keep_err_IB$model <- "Pearson Item-Based CF"

#User-based Cosine
keep_err_UB <- lapply(keep, train_eval, proportion = .2, method = "UBCF", param = list(nn = 100, method = 'cosine'))
keep_err_UB <- do.call(rbind.data.frame, keep_err_UB)
names(keep_err_UB) <- c('RMSE','MSE','MAE')
keep_err_UB$ratings_given <- keep
keep_err_UB$model <- "Cosine User-Based CF"

#Random
keep_err_rand <- lapply(keep, train_eval, proportion = .2, method = "RANDOM", param = NULL)
keep_err_rand <- do.call(rbind.data.frame, keep_err_rand)
names(keep_err_rand) <- c('RMSE','MSE','MAE')
keep_err_rand$ratings_given <- keep
keep_err_rand$model <- "Random"

#add code for keep_err_SVD
keep_err_svd <- lapply(keep, train_eval, proportion = .2, method = "SVD", param = list(k=10))
keep_err_svd <- do.call(rbind.data.frame, keep_err_svd)
names(keep_err_svd) <- c('RMSE','MSE','MAE')
keep_err_svd$ratings_given <- keep
keep_err_svd$model <- "SVD"


keep_err_all <- rbind(keep_err_IB, keep_err_UB, keep_err_svd, keep_err_rand)

## @knitr ignore-4
#saveRDS(keep_err_all, "model info/keep_err_all.rds")


## @knitr err_vs_numitems_complete
ratings_full <- ratings[complete.cases(ratings), ]
keep2 <- seq(5,95,5)
keep2_err_IB <- lapply(keep2, train_eval, data = ratings_full, proportion = .8, method = "IBCF", param = list(k = 30, method = 'pearson'))
keep2_err_IB <- do.call(rbind.data.frame, keep2_err_IB)
names(keep2_err_IB) <- c('RMSE','MSE','MAE')
keep2_err_IB$model <- "Pearson Item-Based CF"

keep2_err_UB <- lapply(keep2, train_eval, data = ratings_full, proportion = .8, method = "UBCF", param = list(nn = 100, method = 'cosine'))
keep2_err_UB <- do.call(rbind.data.frame, keep2_err_UB)
names(keep2_err_UB) <- c('RMSE','MSE','MAE')
keep2_err_UB$model <- "Cosine User-Based CF"

keep2_err_all <- rbind(keep2_err_IB, keep2_err_UB)
keep2_err_all$ratings_given <- keep2

## @knitr ignore-5
#saveRDS(keep2_err_all, "model info/keep2_err_all.rds")

## @knitr size_vs_time
train_eval_time <- function(proportion, given = 15, method, param, data = ratings){
  split <- train_test_split(data, train_proportion = proportion)
  train_data <- split$train_data
  training_sparse <- as(as.matrix(train_data), 'realRatingMatrix')
  test_data <- split$test_data
  test_data_sparse <- as(as.matrix(test_data), 'realRatingMatrix')
  time_begin_train <- Sys.time()
  ibcf_pearson <- Recommender(data = training_sparse, method = method, parameter = param)
  test_data_sample <- apply(test_data, 1, sample_test, n = given)
  time_train <- as.double(Sys.time() - time_begin_train)
  test_data_sample  <- as.data.frame(t(test_data_sample))
  test_sample_sparse <- as(as.matrix(test_data_sample), 'realRatingMatrix')
  time_begin_predict <- Sys.time()
  predict(ibcf_pearson, test_sample_sparse, type = 'ratings')
  time_predict <- as.double(Sys.time() - time_begin_predict)
  return(list(proportion,time_train, time_predict, time_predict+time_train))
}

#Item-based Pearson
size_time_IB <- lapply(sample_prop, train_eval_time, method = "IBCF", param = list(k = 30, method = 'pearson'))
size_time_IB <- do.call(rbind.data.frame, size_time_IB)
names(size_time_IB) <- c('Sample Proportion','Train time','Predict time','Total Running time')
size_time_IB$model <- "Pearson Item-Based CF"

#User-based Cosine
size_time_UB <- lapply(sample_prop, train_eval_time, method = "UBCF", param = list(nn = 100, method = 'cosine'))
size_time_UB <- do.call(rbind.data.frame, size_time_UB)
names(size_time_UB) <- c('Sample Proportion','Train time','Predict time','Total Running time')
size_time_UB$model <- "Cosine User-Based CF"

#Random
size_time_rand <- lapply(sample_prop, train_eval_time, method = "RANDOM", param = NULL)
size_time_rand <- do.call(rbind.data.frame, size_time_rand)
names(size_time_rand) <- c('Sample Proportion','Train time','Predict time','Total Running time')
size_time_rand$model <- "Random"

#SVD
sample_prop_svd <- c(.003,.005,seq(.01, .1, by = .01), seq(.15, .5, by = .05))
size_time_SVD <- lapply(sample_prop_svd, train_eval_time, method = "SVD", param = list(k = 10, maxiter = 100))
size_time_SVD <- do.call(rbind.data.frame, size_time_SVD)
names(size_time_SVD) <- c('Sample Proportion','Train time','Predict time','Total Running time')
size_time_SVD$model <- "SVD Matrix Factorization"

size_time_all <- rbind(size_time_IB, size_time_UB, size_time_SVD, size_time_rand)
