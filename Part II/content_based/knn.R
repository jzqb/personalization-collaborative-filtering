## @knitr content_knn_prediction
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
    
  }
  print(paste(active_user, "complete"))
}

## @knitr content_knn_evaluation
total_prediction_matrix <- read_rds('content_based/total_prediction_matrix_update.rds')
total_prediction_matrix$score <- as.numeric(as.character(total_prediction_matrix$prediction_prob))
total_prediction_matrix$prediction <- as.numeric(as.character(total_prediction_matrix$prediction))
total_prediction_matrix$score[total_prediction_matrix$prediction == 0] <- 1 - total_prediction_matrix$score[total_prediction_matrix$prediction == 0]
conf_matrix <- table(total_prediction_matrix$target, total_prediction_matrix$prediction)
content_based_accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)

## @knitr content_knn_evaluation_nearest_neighbors
accuracy_nn <- total_prediction_matrix[c('nn', 'target', 'prediction')]
accuracy_nn$diff <- abs(as.numeric(as.character(accuracy_nn$target)) - as.numeric(as.character(accuracy_nn$prediction)))
accuracy_nn <- accuracy_nn %>% group_by(nn, diff) %>% summarise(count = n())
accuracy_nn$accuracy <- accuracy_nn$count/mean(accuracy_nn$count)/2
accuracy_nn <- subset(accuracy_nn, diff == 0)



## @knitr content_knn_evaluation_nearest_neighbors_plot
h <- round(ibcf_accuracy,2)
i <- round(ubcf_accuracy,2)
j <- round(rand_accuracy,2)
nn_plot <- ggplot(data=accuracy_nn, aes(x=nn, y=accuracy, group=1)) +
  geom_line(size=1.2)+
  ggtitle('Error rate of knn-content recommender system against number of \n nearest neighbors. [CF for benchmarking]') +
  geom_hline(aes(yintercept=h), colour="#BB0000", linetype="dashed") + 
  geom_text(aes(0, h, label = paste("ibcf: ",h), vjust = 1, hjust = 0), size = 3) + 
  geom_hline(aes(yintercept=i), colour="#E69F00", linetype="dashed") + 
  geom_text(aes(0, i, label = paste("ubcf: ",i), vjust = 1, hjust = 0), size = 3) + 
  geom_hline(aes(yintercept=j), colour="#56B4E9", linetype="dashed") + 
  geom_text(aes(0, j, label = paste("rand: ",j), vjust = 1, hjust = 0), size = 3) + 
  geom_point(size = 1.5) + 
  ylim(0.62, 0.72)

nn_plot