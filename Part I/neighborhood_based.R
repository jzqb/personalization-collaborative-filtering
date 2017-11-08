## @knitr sourcing_ubcf_data
load('ubcf_cf.RData')

## @knitr item_similarity
#### remove this part in final report compilation
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
ratings_sparse <- as(as.matrix(ratings), 'realRatingMatrix')
similarity_item_pearson <- similarity(ratings_sparse, method = 'pearson', which = 'items')
similarity_item_pearson <- as(similarity_item_pearson, "matrix")
heat_map <- plot_ly(
  x = colnames(similarity_item_pearson), y = rownames(similarity_item_pearson),
  z = similarity_item_pearson, type = "heatmap"
) %>%
  layout(title = 'Items [Jokes] Similarity', xaxis = x <- list(title = "items [jokes]", titlefont = f), yaxis = list(title = "items [jokes]", titlefont = f))
heat_map

## @knitr item_based_collaborative_filter
set.seed(666)
train_proportion = 0.8
molten_data <- wide_to_long(ratings) # remomve this in final report
molten_data_split <- train_test_split(molten_data, train_proportion = train_proportion)
train_data <- molten_data_split$train_data
feed_data <- dcast(train_data, formula = user~variable, fun.aggregate = mean, fill = NA_real_)
feed_data$user <- as.numeric(gsub( 'u_', '', feed_data$user))
feed_data <- feed_data[order(feed_data$user),]
feed_data <- feed_data[-1]
rownames(feed_data) <- c(1:nrow(feed_data))
training_sparse <- as(as.matrix(feed_data), 'realRatingMatrix')
ibcf_pearson <- Recommender(data = training_sparse, method = "IBCF", parameter = list(k = 30, method = 'pearson'))
ibcf_pearson
model_details <- getModel(ibcf_pearson)
model_details$description

## @knitr similarity_matrix_model
similarity_built <- model_details$sim
similarity_built <- as(similarity_built, "matrix")
heat_map <- plot_ly(
  x = colnames(similarity_built), y = rownames(similarity_built),
  z = similarity_built, type = "heatmap"
) %>%
  layout(title = 'Items [Jokes] Similarity: Only k items', xaxis = x <- list(title = "items [jokes]", titlefont = f), yaxis = list(title = "items [jokes]", titlefont = f))
heat_map

## @knitr similar_jokes_top
row_sums <- rowSums(model_details$sim > 0)
table(row_sums)
col_sums <- colSums(model_details$sim > 0)
which_max <- order(col_sums, decreasing = TRUE)[1:6]
rownames(model_details$sim)[which_max]

## @knitr predicted_ratings_item_based_algorithm
test_data <- molten_data_split$test_data
predicted_ratings <- predict(ibcf_pearson, training_sparse, type = 'ratings')
predicted_ratings <- as(predicted_ratings, 'data.frame')
predicted_ratings$user <- paste('u_', as.character(predicted_ratings$user), sep = "")
predicted_ratings$item <- as.character(predicted_ratings$item)
test_data$user <- as.character(test_data$user)
test_data$variable <- as.character(test_data$variable)
test_data <- merge(test_data, predicted_ratings, by.x = c('user', 'variable'), by.y = c('user','item'), all.x = TRUE)
pearson_ibcf_mae <- mean(abs(test_data$value-test_data$rating), na.rm = TRUE)
pearson_ibcf_mae

## @knitr top_n_ibcf
top_n_predicted_ibcf <- predict(object = ibcf_pearson, ratings_sparse, n = 5)
first_user_reco <- top_n_predicted_ibcf@items[[1]]
jokes_user_1 <- top_n_predicted_ibcf@itemLabels[first_user_reco]
recc_matrix <- sapply(top_n_predicted_ibcf@items, function(x){
  colnames(ratings_sparse)[x]
})
recc_data <- do.call(rbind, lapply(recc_matrix, data.frame, stringsAsFactors=FALSE))
names(recc_data) <- 'item_recommended'
number_of_items <- factor(table(recc_data$item_recommended))

reco_plot <- plot_ly(x = names(number_of_items), y = number_of_items, type = 'bar', text = names(number_of_items), marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Frequency Plot of Jokes Recommended to Users", xaxis = list(title = "Jokes"), yaxis = list(title = "Recommended Frequency"))

avg_ratings_plot <- plot_ly(x = names(jokes_avg_rating), y = jokes_avg_rating, type = 'bar', text = joke_text$joke_text, marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Jokes Recommended and Their Average Ratings - IBCF", xaxis = list(title = "Jokes"), yaxis = list(title = "Avg Rating"))

plotly::subplot(reco_plot, avg_ratings_plot, nrows = 2, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)

## @knitr item_based_brute_force
item_similarity_cosine <- function(ratings){
  item_ratings <- ratings
  item_ratings_centered <- as.data.frame(t(apply(t(item_ratings), 2, FUN = function(x){x- mean(x, na.rm = T)})))
  cos_ratings <- item_ratings_centered
  cos_ratings[is.na(cos_ratings)] <- 0
  cosine_item_sim <- cosine(as.matrix(cos_ratings))
  return(cosine_item_sim)
}

item_similarity_pearson <- function(ratings){
  item_ratings <- ratings
  pear_item_sim <- as.matrix(cor(item_ratings, use = 'pairwise.complete.obs'))
  return(pear_item_sim)
}

predict_rating_item_cf <- function(ratings, k=20, user_no, item_no, similarity = c('cosine', 'pearson')){
  user <- ratings[user_no,] 
  unrated <- names(user[,is.na(user)])
  rated <- names(user[,!is.na(user)])
  if(paste('i_',item_no, sep = "") %nin% unrated){
    return(ratings[user_no, item_no])
  }
  else{
    if(similarity == 'cosine'){use_sim = i_s_c}
    else{use_sim = i_s_p}
    sim_mov <- as.data.frame(use_sim[,item_no])
    sim_mov$item <- row.names(sim_mov)
    sim_mov <- sim_mov[order(-sim_mov$`use_sim[, item_no]`),]
    sim_mov <- sim_mov[sim_mov$item != paste('i_',item_no, sep = ""),]
    sim_mov <- sim_mov[1:k,]
    sim_mov <- sim_mov[sim_mov$item %in% rated,]
    um <- as.data.frame(t(user[,!is.na(user)]))
    um$item <- rownames(um)
    rate_insitu <- merge(sim_mov, um, by = 'item', all.x = T)
    names(rate_insitu) <- c('item', 'sim', 'rating')
    rate_insitu$rating <- as.numeric(as.character(rate_insitu$rating))
    return(sum(rate_insitu$sim*rate_insitu$rating)/sum(rate_insitu$sim))
  }
}
i_s_c <- item_similarity_cosine(feed_data)
i_s_p <- item_similarity_pearson(feed_data)

data_avg_rating <- mean(molten_data$value)
baseline_mae <- sum(abs(test_data$value - data_avg_rating))/nrow(test_data)
test_users <- as.numeric(gsub('u_', '', test_data$user))
test_items <- as.numeric(gsub('i_', '', test_data$variable))

test_data$prediction <- 0
for(test_case in 1:10000){
  test_user <- as.numeric(gsub('u_', '',test_data[test_case,1]))
  test_item <- as.numeric(gsub('i_', '',test_data[test_case,2]))
  test_data[test_case, 4] <- predict_rating_item_cf(feed_data, k = 20, test_user, test_item, similarity = 'cosine')
}
insitu <- test_data[1:10000,]
cos_mae_ibcf_bf <- sum(abs(insitu$value - insitu$prediction), na.rm = T)/nrow(insitu)


test_data$prediction <- 0
for(test_case in 1:10000){
  test_user <- as.numeric(gsub('u_', '',test_data[test_case,1]))
  test_item <- as.numeric(gsub('i_', '',test_data[test_case,2]))
  test_data[test_case, 4] <- predict_rating_item_cf(feed_data, k = 20, test_user, test_item, similarity = 'pearson')
}
insitu <- test_data[1:10000,]
pear_mae_ibcf_bf <- sum(abs(insitu$value - insitu$prediction))/nrow(insitu)


## @knitr user_based_parameters
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models$UBCF_realRatingMatrix$parameters

## @knitr user_based_collaborative_filter
ubcf_cosine <- Recommender(data = training_sparse, method = "UBCF")
ubcf_cosine
model_details <- getModel(ubcf_cosine)
model_details$description

## @knitr predicted_ratings_user_based_algorithm
test_data <- molten_data_split$test_data
predicted_ratings <- predict(ubcf_cosine, training_sparse, type = 'ratings')
predicted_ratings <- as(predicted_ratings, 'data.frame')
predicted_ratings$user <- paste('u_', as.character(predicted_ratings$user), sep = "")
predicted_ratings$item <- as.character(predicted_ratings$item)
test_data$user <- as.character(test_data$user)
test_data$variable <- as.character(test_data$variable)
test_data <- merge(test_data, predicted_ratings, by.x = c('user', 'variable'), by.y = c('user','item'), all.x = TRUE)

## @knitr ubcf_mae
cosine_ubcf_mae <- mean(abs(test_data$value-test_data$rating), na.rm = TRUE)
cosine_ubcf_mae

## @knitr top_n_ubcf
top_n_predicted_ubcf <- predict(object = ubcf_cosine, ratings_sparse, n = 5)
first_user_reco <- top_n_predicted_ubcf@items[[1]]
jokes_user_1 <- top_n_predicted_ubcf@itemLabels[first_user_reco]

## @knitr ubcf_coverage_viz
recc_matrix <- sapply(top_n_predicted_ubcf@items, function(x){
  colnames(ratings_sparse)[x]
})
recc_data <- do.call(rbind, lapply(recc_matrix, data.frame, stringsAsFactors=FALSE))
names(recc_data) <- 'item_recommended'
number_of_items <- factor(table(recc_data$item_recommended))
reco_plot <- plot_ly(x = names(number_of_items), y = number_of_items, type = 'bar', text = names(number_of_items), marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Frequency Plot of Jokes Recommended to Users - UBCF", xaxis = list(title = "Jokes"), yaxis = list(title = "Recommended Frequency"))

avg_ratings_plot <- plot_ly(x = names(jokes_avg_rating), y = jokes_avg_rating, type = 'bar', text = joke_text$joke_text, marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Jokes Recommended and Their Average Ratings - UBCF", xaxis = list(title = "Jokes"), yaxis = list(title = "Avg Rating"))

plotly::subplot(reco_plot, avg_ratings_plot, nrows = 2, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)