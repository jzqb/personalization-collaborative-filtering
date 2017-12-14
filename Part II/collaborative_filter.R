training_sparse <- as(as.matrix(user_item_rating[-1]), 'realRatingMatrix')

ibcf <- Recommender(data = training_sparse, method = "IBCF", parameter = list(method = "Jaccard"))
predicted_ratings <- predict(ibcf, training_sparse, type = 'ratings')
predicted_ratings <- as(predicted_ratings, 'matrix')
predicted_ratings <- as.data.frame(predicted_ratings)
predicted_ratings$msno <- user_item_rating$msno
predicted_ratings_melt <- melt(predicted_ratings, id.vars = 'msno', variable.name = 'song_id')
results_ibcf <- merge(testing_data, predicted_ratings_melt, by = c('msno', 'song_id'))
results_ibcf$predicted_class <- 1
results_ibcf$predicted_class[results_ibcf$value < 0.5] <- 0

conf_matrix_cf_ibcf <- table(results_ibcf$target, results_ibcf$predicted_class)
conf_matrix_cf_ibcf
(conf_matrix_cf_ibcf[1,1]+conf_matrix_cf_ibcf[2,2])/sum(conf_matrix_cf_ibcf)


ubcf <- Recommender(data = training_sparse, method = "UBCF", parameter = list(method = "Jaccard"))
predicted_ratings <- predict(ubcf, training_sparse, type = 'ratings')
predicted_ratings <- as(predicted_ratings, 'matrix')
predicted_ratings <- as.data.frame(predicted_ratings)
predicted_ratings$msno <- user_item_rating$msno
predicted_ratings_melt <- melt(predicted_ratings, id.vars = 'msno', variable.name = 'song_id')
results_ubcf <- merge(testing_data, predicted_ratings_melt, by = c('msno', 'song_id'))
results_ubcf$predicted_class <- 1
results_ubcf$predicted_class[results_ubcf$value < 0.5] <- 0
results_ubcf$random_class <- 1

conf_matrix_cf_ubcf <- table(results_ubcf$target, results_ubcf$predicted_class)
conf_matrix_cf_ubcf
ubcf_accuracy <- (conf_matrix_cf_ubcf[1,1]+conf_matrix_cf_ubcf[2,2])/sum(conf_matrix_cf_ubcf)

conf_matrix_rand <- table(results$target, results$random_class)
conf_matrix_rand
(conf_matrix_rand[2,1])/sum(conf_matrix_rand)