## @knitr reading_filtering_data
options(tibble.print_max = 5, tibble.print_min = 5)
setwd("~/Desktop/drive/dsi/fall-17/personalization/Part II")
rm(list= ls())
train <- as.tibble(fread('data/train.csv'))
songs <- as.tibble(fread('data/songs.csv'))
song_extra_info <- as.tibble(fread('data/song_extra_info.csv'))

ratings <- as.data.table(train[c('msno', 'song_id', 'target')])

filter_song_data <- as.data.frame(table(ratings$song_id))
filter_user_data <- as.data.frame(table(ratings$msno))

names(filter_song_data) <- c('song_id', 'count')
names(filter_user_data) <- c('msno', 'count')
ratings$song_count <- filter_song_data$count[match(ratings$song_id, filter_song_data$song_id)]
ratings$user_count <- filter_user_data$count[match(ratings$msno, filter_user_data$msno)]

sc = 1000
uc = 500
filter_data <- subset(ratings, ratings$song_count>= sc & ratings$user_count >=uc)
nrow(filter_data)/nrow(ratings)
paste('Users:', length(unique(filter_data$msno)))
paste('Songs:', length(unique(filter_data$song_id)))

user_key <- as.data.frame(cbind(unique(filter_data$msno),c(1:length(unique(filter_data$msno)))))
names(user_key) <- c('user_code', 'msno')
song_key <- as.data.frame(cbind(unique(filter_data$song_id), c(1:length(unique(filter_data$song_id)))))
names(song_key) <- c('song_code', 'song_id')
filter_data$msno <- user_key$msno[match(filter_data$msno,user_key$user_code)]
filter_data$song_id <- song_key$song_id[match(filter_data$song_id, song_key$song_code)]


set.seed(666)
train_prop = 0.75
train_test_split <- function(ratings, train_proportion = 0.8){
  sample_size <- floor(train_proportion*nrow(ratings))
  train_ind <- sample(seq_len(nrow(ratings)), size = sample_size)
  train_data <- ratings[train_ind,]
  test_data <- ratings[-train_ind,]
  split_data <- list('train_data' = train_data, 'test_data' = test_data)
  return(split_data)
}
split_data <- train_test_split(filter_data,0.75)
training_data <- split_data$train_data
testing_data <- split_data$test_data
songs$song_id <- song_key$song_id[match(songs$song_id, song_key$song_code)]
songs <- subset(songs, songs$song_id %in% filter_data$song_id)
song_extra_info$song_id <- song_key$song_id[match(song_extra_info$song_id, song_key$song_code)]
song_extra_info <- subset(song_extra_info, song_extra_info$song_id %in% filter_data$song_id)

training_data <- as.tibble(training_data)
