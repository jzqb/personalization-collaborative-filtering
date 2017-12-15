## @knitr item_based_collaborative_filter
user_item_rating <- dcast(training_data, formula = msno~song_id, value.var = 'target')
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

## @knitr item_based_collaborative_filter_accuracy
conf_matrix_cf_ibcf <- table(results_ibcf$target, results_ibcf$predicted_class)
ibcf_accuracy <- (conf_matrix_cf_ibcf[1,1]+conf_matrix_cf_ibcf[2,2])/sum(conf_matrix_cf_ibcf)

## @knitr user_based_collaborative_filter
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

## @knitr user_based_collaborative_filter_accuracy
conf_matrix_cf_ubcf <- table(results_ubcf$target, results_ubcf$predicted_class)
ubcf_accuracy <- (conf_matrix_cf_ubcf[1,1]+conf_matrix_cf_ubcf[2,2])/sum(conf_matrix_cf_ubcf)

## @knitr random_accuracy
conf_matrix_rand <- table(results_ubcf$target, results_ubcf$random_class)
rand_accuracy <- (conf_matrix_rand[2,1])/sum(conf_matrix_rand)

cf_accuracy <- as.data.frame(cbind(c('item_based', 'user_based', 'random'), c(ibcf_accuracy, ubcf_accuracy, rand_accuracy)))
names(cf_accuracy) <- c('model', 'accuracy')
cf_accuracy$accuracy_r <- round(as.numeric(as.character(cf_accuracy$accuracy)), 4)
p <- ggplot(data=cf_accuracy, aes(x=model, y=accuracy_r)) +
  geom_bar(stat="identity", fill="steelblue")+ ylim(0,0.8) +
  ggtitle('Collaborative Filtering Models') +
  theme_minimal()
p
