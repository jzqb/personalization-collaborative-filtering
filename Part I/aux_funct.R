## @knitr aux_funct_wide_to_long
wide_to_long <- function(ratings){
  ratings <- cbind(user = paste('u_',row.names(ratings), sep = ""), ratings)
  molten <- melt(ratings, na.rm = T, id.vars = 'user')
  return(molten)
}

## @knitr aux_funct_train_test_split
train_test_split <- function(ratings, train_proportion = 0.8){
  sample_size <- floor(train_proportion*nrow(ratings))
  train_ind <- sample(seq_len(nrow(ratings)), size = sample_size)
  train_data <- ratings[train_ind,]
  test_data <- ratings[-train_ind,]
  split_data <- list('train_data' = train_data, 'test_data' = test_data)
  return(split_data)
}