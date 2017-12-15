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
total_prediction_matrix <- read.csv('total_prediction_matrix.csv')
conf_matrix <- table(total_prediction_matrix$target, total_prediction_matrix$prediction)
content_based_accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)

## @knitr content_knn_evaluation_nearest_neighbors
accuracy_nn <- total_prediction_matrix[c('nn', 'target', 'prediction')]
accuracy_nn$diff <- abs(accuracy_nn$target -accuracy_nn$prediction)
accuracy_nn <- accuracy_nn %>% group_by(nn, diff) %>% summarise(count = n())
accuracy_nn$accuracy <- accuracy_nn$count/mean(accuracy_nn$count)/2
accuracy_nn <- subset(accuracy_nn, diff == 0)

ggplot(data=accuracy_nn, aes(x=, y=len, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point()
