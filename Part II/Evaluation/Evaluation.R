## @knitr tune_cv
plot_param_cv <- function(k, m, err.bar.width = .1){
  # Plot Parameter Tuning CV Results
  ## input: 
  # k: vector of values to test for the hyperparamter
  # m: an n x k matrix of accuracy rates (n = # of folds)
  ## returns list of:
  # data frame of parameter values, mean error rate, and standard error
  # ggplot of the error rate vs. value of the hyperparameter
  
  err <- 1 - m
  mean <- apply(err, 2, mean)
  se <- apply(err, 2, function(x) sd(x)/sqrt(length(x)))
  df <- data.frame(cbind(k, mean, se))
  
  p <- ggplot(df, aes(x=k, y=mean)) + 
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                  width = err.bar.width, color='blue' ) +
    geom_line(color='blue') +
    ylab("Error Rate") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(list(df, p))
}

## @knitr evaluate_function
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


## @knitr compare_models
evaluate2 <- function(pred.target, target){
  accuracy <- sum(pred.target == target)/length(target)
  conf.mat <- table(pred.target, target, dnn = c("predicted","actual"))
  precision <- conf.mat[2,2]/sum(conf.mat[2,])
  recall <- conf.mat[2,2]/sum(conf.mat[,2])
  F1 <- 2 * (precision*recall/(precision + recall))
  eval.metrics <- data.frame(cbind(accuracy, precision, recall, F1))
  return(eval.metrics)
}

all_models_eval <- rbind(
  evaluate2(test_hybrid$hybrid_class, test_hybrid$target),
  FM.eval[[1]][c(1,3:5)], 
  evaluate2(total_prediction_matrix[total_prediction_matrix$nn == 128, 'prediction'], 
            total_prediction_matrix[total_prediction_matrix$nn == 128, 'target']), 
  evaluation_result[[1]][c(1,3:5)], #svd
  evaluate2(results_ibcf$predicted_class, results_ibcf$target),
  evaluate2(results_ubcf$predicted_class, results_ubcf$target)
)
model <- c('Hybrid', 'FM', 'Content', 'SVD', 'IBCF', 'UBCF')

all_models_eval  <- cbind(model, all_models_eval)

## @knitr song_acc_plot

FM.test.data <- cbind(merged_testing, predFM.mcmc)

FM.test.data %>% 
  group_by(song_id) %>% 
  summarize(count = n(), accuracy = sum(predFM.mcmc = target)/n()) %>%
  group_by(count) %>% 
  summarize(new_count = n(), avg_acc = mean(accuracy)) %>% 
  rename(no_of_items = new_count, occurence = count) %>% 
  arrange(desc(avg_acc)) %>% 
  ggplot(aes(occurence, avg_acc)) +
  geom_line(color='turquoise') +
  geom_smooth(color='turquoise') +
  labs(x = "Song Occurence",
       y = "Accuracy",
       title= "Song Occurence vs FM Predictive Accuracy") 
