setwd("~/Desktop/drive/dsi/fall-17/personalization/personalization-collaborative-filtering/Part II")
songs_info <- readRDS('content_based/big_flat_item_features.rds')
clusterFilms<-function(songs_info){
  set.seed(123)
  i<-1
  #get rid of movie ids and titles
  songs_info<-songs_info[,c(-1)]
  repeat {
    set.seed(123)
    #build two kmeans models starting with 2 and 3 clusters and repeat until dss<0.2
    i <- i + 1
    movieCluster<-kmeans(songs_info,i)
    movieCluster2<-kmeans(songs_info,i+1)
    #decision criterion
    dss<-((movieCluster$tot.withinss-movieCluster2$tot.withinss)/movieCluster$tot.withinss)
    #exit if dss < 0.2
    if (dss < 0.0001) break
  }
  return(movieCluster)
}


userDF <- training_data[c(1:3)]
names(userDF) <- c('userid', 'itemid', 'rating')
activeUser <- getUserInfo(userDF, 1)
getUserInfo<-function(dat,id){
  #Select all rows from user_DF that have the userid==user_id and keep the columns itemid & rating
  a<-subset(dat, userid==id,select=c(itemid, rating))
  # allocate 0 to the cluster column
  cluster<-0
  activeUser <- data.frame( a[order(a$itemid),] ,cluster)
  return(activeUser)
}


movieCluster <- clusterFilms(songs_info)
setUserFilmCluster<-function(movieCluster, activeUser){
  # set up temporary dataframe to match cluster assignments to movie ids
  df1<- data.frame(cbind(songs_info$song_id, clusterNum = movieCluster$cluster))
  names(df1)<-c("movie_id", "cluster")
  #This matches the cluster number to the activeUser movie id
  activeUser$cluster<-df1[match(activeUser$itemid, df1$movie_id),2]
  return(activeUser)
}



getMeanClusterRating<-function(movieCluster, activeUser){
  #aggregate() function is used along with the cluster memberships to determine variable means for each cluster in the original metric
  like<-aggregate(activeUser$rating, by=list(cluster=activeUser$cluster), mean)
  #A bit different approach here: If the max mean rating is below three it gives out the dummy value zero
  if(max(like$x)<0){
    like<-as.vector(0)
    #Else it gives out the cluster number of the max mean value
  } else{
    like<-as.vector(t(max(subset(like, x>=0.2, select=cluster))))
  }
  return(like)
}



getGoodFilms<-function(like, movieCluster, songs_info){
  # Again a temporary dataframe is created to get a list of all movies and their associated clusters
  df1<- data.frame(cbind(songs_info$song_id, clusterNum = movieCluster$cluster))
  names(df1)<-c("movie_id", "cluster")
  #if like has the value zero it selects randomly 100 movies
  if(like==0){
    recommend<-songs_info[sample.int(n = dim(songs_info)[1], size = 100), 1]
  }
  #else it selects all movies from the winning max mean cluster
  else{
    recommend<-as.vector(t(subset(df1, cluster==like, select=movie_id)))
  }
  return(recommend)
}



getRecommendedFilms<-function(songs_info, userDF, userid){
  # according to plan we call all functions in order of our logic
  movieCluster<-clusterFilms(songs_info)
  activeUser<-getUserInfo(userDF, userid)
  activeUser<-setUserFilmCluster(movieCluster, activeUser)
  like<-getMeanClusterRating(movieCluster, activeUser)
  recommend<-getGoodFilms(like, movieCluster, songs_info)
  # only select not yet watched movies
  recommend<-recommend[-as.numeric(activeUser$itemid)]
  # add movietitle
  movtitle<-songs_info[match(recommend,songs_info$song_id),1]
  recommend<-data.frame(recommend,movtitle)
  return(recommend)
}




suggestFilms<-function(songs_info, userDF, userid, no_films){
  #get suggestions
  suggestions = getRecommendedFilms(songs_info, userDF, userid)
  #select stated number of selections
  suggestions = suggestions[1:no_films,]
  #implementing some German here
  writeLines("You may also like these songs:")
  #print suggestions without column headers or row indices
  write.table(suggestions[2], row.names = FALSE, col.names = FALSE)
}



suggestFilms(songs_info, userDF, 1, 15)

