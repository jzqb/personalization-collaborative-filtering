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
  layout(title = "Jokes Recommended and Their Average Ratings", xaxis = list(title = "Jokes"), yaxis = list(title = "Avg Rating"))

subplot(reco_plot, avg_ratings_plot, nrows = 2, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)