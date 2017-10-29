## @knitr reading_ratings
ratings <- read.csv('../data/jester-data-1.csv', header = FALSE)
ratings <- ratings[-1]
names(ratings) <- paste('i_',c(1:100), sep = "")
ratings[ratings ==99] <- NA
kable(ratings[1:4,1:10], "html", caption = "Sample Ratings of First 4 users and 10 jokes: User x Item") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

## @knitr sparsity
sparsity <- sum(is.na(ratings))/dim(ratings)[1]/dim(ratings)[2]
paste('The sparsity in the dataset is ', round(sparsity*100,2), "%", sep = "")

## @knitr histogram_ratings
molten_data <- wide_to_long(ratings)
fit_density <- density(molten_data$value)

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "ratings",
  titlefont = f
)
y1 <- list(
  title = "frequency",
  titlefont = f
)
y2 <- list(
  title = "density",
  titlefont = f,
  overlaying = "y",
  side = "right"
)


histo_ratings <- plot_ly(x = molten_data$value, type = "histogram", name = "Histogram") %>% 
  add_trace(x = fit_density$x, y = fit_density$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
  layout(title = 'Distribution of Ratings', xaxis = x, yaxis = y1, yaxis2 = y2)
histo_ratings


## @knitr ratings_side_user_average
rat_per_user_more <- sum(molten_data$value>=0)/length(unique(molten_data$user[molten_data$value>=0]))
rat_per_user_less <- sum(molten_data$value<0)/length(unique(molten_data$user[molten_data$value<0]))
avg_rat_per_user_more <- mean(molten_data$value[molten_data$value>=0])
avg_rat_per_user_less <- mean(molten_data$value[molten_data$value<0])
paste('On an average, a user rated ', round(rat_per_user_more,0), ' jokes more than 0 with an average rating of ', round(avg_rat_per_user_more,2), '. While it was ', round(rat_per_user_less,0), ' jokes and ',round(avg_rat_per_user_less,2), ' for less than 0 respectively!', sep = "")

## @knitr parse_html
all_files <- list.files('../data/jokes')
joke_numbers <- vector()
joke_text <- vector()
regexp <- "[[:digit:]]+"
for(filer in all_files){
  joke_serial <- as.numeric(str_extract(filer, regexp))
  doc_html <- htmlTreeParse(paste('../data/jokes/', filer,sep = ""), useInternal = TRUE)
  doc_text <- unlist(xpathApply(doc_html, '//font', xmlValue))
  doc_text <- gsub('\\n', ' ', doc_text)
  doc_text <- paste(doc_text, collapse = " ")
  joke_numbers <- append(joke_numbers, joke_serial)
  joke_text <- append(joke_text, doc_text)
}
joke_text <- data.frame(joke_no = joke_numbers, joke_text = joke_text)
# joke_text is now a data.frame with joke_id and joke_text

## @knitr popular_jokes
ratings_sparse <- as(as.matrix(ratings), 'realRatingMatrix') # helps to reduce memory for sparse matrix storage
jokes_rated <- colCounts(ratings_sparse)
jokes_avg_rating <- round(colMeans(ratings_sparse),3)
table_rated <- data.frame(joke_number = names(jokes_rated), jokes_rated, jokes_avg_rating)
# seeing the top3 and bottom3 rated jokes
top3_rated_jokes <- head(table_rated[order(-table_rated$jokes_rated),],3)
bottom3_rated_jokes <- head(table_rated[order(table_rated$jokes_rated),],3)
# best and worst jokes based on ratings
best3_jokes <- head(table_rated[order(-table_rated$jokes_avg_rating),],3)
worst3_jokes <- head(table_rated[order(table_rated$jokes_avg_rating),],3)


## @knitr average_ratings_jokes
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "ratings",
  titlefont = f
)
y1 <- list(
  title = "frequency",
  titlefont = f
)
y2 <- list(
  title = "density",
  titlefont = f,
  overlaying = "y",
  side = "right"
)
fit_density_avg <- density(jokes_avg_rating)
histo_ratings_avg <- plot_ly(x = jokes_avg_rating, type = "histogram", name = "Histogram") %>% 
  add_trace(x = fit_density_avg$x, y = fit_density_avg$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
  layout(title = 'Distribution of Average Ratings', xaxis = x, yaxis = y1, yaxis2 = y2)
histo_ratings_avg

## @knitr ratings_viz_heat
sample_heat_data <- as.matrix(ratings)
heat_map <- plot_ly(
  x = names(ratings), y = rownames(ratings),
  z = sample_heat_data, type = "heatmap",
  colorscale = "Greys"
) %>%
  layout(title = 'Ratings Matrix Visualization', xaxis = x <- list(title = "items [jokes]", titlefont = f), yaxis = list(title = "users", titlefont = f))
heat_map

## @knitr ratings_viz_heat_segment
min_n_jokes <- quantile(rowCounts(ratings_sparse), 0.70)
min_n_users <- quantile(colCounts(ratings_sparse), 0.75)
sample_heat_data <- ratings_sparse[rowCounts(ratings_sparse) > min_n_jokes, colCounts(ratings_sparse) > min_n_users]
sample_heat_data <- as(sample_heat_data, "matrix")
heat_map <- plot_ly(
  x = colnames(sample_heat_data), y = rownames(sample_heat_data),
  z = sample_heat_data, type = "heatmap"
) %>%
  layout(title = 'Relevant User Jokes: Heatmap of Ratings', xaxis = x <- list(title = "items [jokes]", titlefont = f), yaxis = list(title = "users", titlefont = f))
heat_map
