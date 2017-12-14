## @knitr content_feature_generation
genres <- as.data.frame(songs$genre_ids, stringsAsFactors=FALSE)
genres <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres) <- c(1:3)
genres$song_id <- songs$song_id

genre_data <- genres[c(4,1)]
for(i in 2:3){
  tester <- genres[c(4,i)]
  tester <- na.omit(tester)
  names(tester) <- names(genre_data)
  genre_data <- rbind(genre_data, tester)
}
names(genre_data) <- c('song_id', 'genre')
genre_binary <- dcast(genre_data, formula = song_id~genre, fun.aggregate = NULL)
genre_binary[-1][!is.na(genre_binary[-1])] <- 1
genre_binary[is.na(genre_binary)] <- 0

lang_data <- songs[c('song_id', 'language')]
lang_data <- na.omit(lang_data)
lang_binary <- dcast(lang_data, formula = song_id~language, fun.aggregate = NULL)
lang_binary[-1][!is.na(lang_binary[-1])] <- 1
lang_binary[is.na(lang_binary)] <- 0

artists <- as.data.frame(songs$artist_name, stringsAsFactors=FALSE)
artists <- as.data.frame(tstrsplit(artists[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(artists) <- c(1:3)
artists$song_id <- songs$song_id

artists_data <- artists[c(4,1)]
for(i in 2:3){
  tester <- artists[c(4,i)]
  tester <- na.omit(tester)
  names(tester) <- names(artists_data)
  artists_data <- rbind(artists_data, tester)
}
names(artists_data) <- c('song_id', 'artist')
artists_binary <- dcast(artists_data, formula = song_id~artist, fun.aggregate = NULL)
artists_binary[-1][!is.na(artists_binary[-1])] <- 1
artists_binary[is.na(artists_binary)] <- 0

composer <- as.data.frame(songs$composer, stringsAsFactors=FALSE)
composer <- as.data.frame(tstrsplit(composer[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(composer) <- c(1:11)
composer$song_id <- songs$song_id

composer_data <- composer[c(12,1)]
for(i in 2:11){
  tester <- composer[c(12,i)]
  tester <- na.omit(tester)
  names(tester) <- names(composer_data)
  composer_data <- rbind(composer_data, tester)
}
names(composer_data) <- c('song_id', 'composers')
composer_binary <- dcast(composer_data, formula = song_id~composers, fun.aggregate = NULL)
composer_binary[-1][!is.na(composer_binary[-1])] <- 1
composer_binary[is.na(composer_binary)] <- 0

lyricist <- as.data.frame(songs$lyricist, stringsAsFactors=FALSE)
lyricist <- as.data.frame(tstrsplit(lyricist[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(lyricist) <- c(1:11)
lyricist$song_id <- songs$song_id

lyricist_data <- lyricist[c(12,1)]
for(i in 2:11){
  tester <- lyricist[c(12,i)]
  tester <- na.omit(tester)
  names(tester) <- names(lyricist_data)
  lyricist_data <- rbind(lyricist_data, tester)
}
names(lyricist_data) <- c('song_id', 'lyricists')
lyricist_binary <- dcast(lyricist_data, formula = song_id~lyricists, fun.aggregate = NULL)
lyricist_binary[-1][!is.na(lyricist_binary[-1])] <- 1
lyricist_binary[is.na(lyricist_binary)] <- 0


songs$song_length_bucket <- cut2(songs$song_length, cuts = quantile(songs$song_length, probs = c(0,0.25,0.75)))
length_data <- songs[c('song_id', 'song_length_bucket')]
length_data <- na.omit(length_data)
length_binary <- dcast(length_data, formula = song_id~song_length_bucket, fun.aggregate = NULL)
length_binary[-1][!is.na(length_binary[-1])] <- 1
length_binary[is.na(length_binary)] <- 0
names(length_binary) <- c('song_id','low', 'med', 'high')

big_flat_item_features <- as.data.frame(cbind(length_binary, 
                                              genre_binary[2:ncol(genre_binary)],
                                              artists_binary[2:ncol(artists_binary)],
                                              composer_binary[2:ncol(composer_binary)],
                                              lyricist_binary[2:ncol(lyricist_binary)],
                                              lang_binary[2:ncol(lang_binary)]))
big_flat_item_features[2:ncol(big_flat_item_features)] <- sapply(big_flat_item_features[2:ncol(big_flat_item_features)], as.numeric)