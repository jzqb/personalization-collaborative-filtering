#Hyperparameters: average score, learning Rate,factor Number,regularization
h_par = c(av,learnRate,factorNum,reg)
#########################
meanScore <- function(train_data){
  mean = sum(train_data$target)/nrow(train_data)
  return(mean)
}

predictScore <- function(average, bu, bi, pu, qi){
  pScore = average + bu + bi + pu %*% qi
  return(pScore)
}

binaryScore <- function(realScore){
  prediction_binary <- ifelse(realScore>=0.5, 1,0)
  dimnames(prediction_binary) <- dimnames(realScore)
  return(prediction_binary)
}
##########Model
#ratings_matrix: user/item, binary ratings
##ratings_matrix <- data.matrix(rating_matrix, rownames.force = NA)
#train_data: sampled from train data,individual ratings(user-song-...-target)
#ratings: the entire data set including train and test
#h_par: average score, learning Rate,factor Number,regularization
SGDsvd <- function(train_data, ratings, h_par){
  
  userNum = length(unique(ratings$msno))
  itemNum = length(unique(ratings$song_id))
  av= h_par[1]
  factorNum = h_par[3] #number of k 
  learnRate = h_par[2]
  regularization = h_par[4]
  
  #initialize
  bi = rep(list(0),itemNum)
  names(bi) <- unique(ratings$song_id)
  bu = rep(list(0),userNum)
  names(bu) <- unique(ratings$msno)
  temp = sqrt(factorNum) 
  pu = matrix(rep(0.1/temp,userNum*factorNum),ncol=factorNum, nrow = userNum,dimnames = list(unique(ratings$msno),c(1:factorNum)))
  qi = matrix(rep(0.1/temp,factorNum*itemNum),nrow = factorNum, ncol=itemNum, dimnames = list(c(1:factorNum),unique(ratings$song_id)))
  
  #train model
  preRmse = 10000.0
  for (line in 1:nrow(train_data)){
    uid = as.character(train_data[line,]$msno)
    iid = as.character(train_data[line,]$song_id)
    score = as.double(train_data[line,]$target)
    
    prediction = predictScore(av, bu[[uid]], bi[[iid]], pu[uid,], qi[,iid])
    eui = score - prediction 
    
    #update hyperparamters
    bu[[uid]] =+ learnRate * (eui - regularization * bu[[uid]])  
    bi[[iid]] =+ learnRate * (eui - regularization * bi[[iid]]) 
    
    ##LEFT RIGHT HERE!
    for (k in 1:factorNum){
      temp = pu[uid,k]
      pu[uid,k] =+ learnRate * (eui * qi[k,iid] - regularization * pu[uid,k]) 
      qi[k,iid] =+ learnRate * (eui * (temp - regularization * qi[k,iid]))
    }
  }
  #fo = file(modelInfo,"wb")
  model_par <- list(bu = bu, bi=bi,pu=pu,qi=qi)

  #saveRDS(model-par, "model_par.rds")
  print("Model Info ready")
  return(model_par)
}

#Validate Rmse
#test_data from the random sample
validate <- function(train_data, av,model_par){
  
  bu = model_par$bu
  bi = model_par$bi
  pu = model_par$pu
  qi = model_par$qi
  err = 0
  
  for (line in 1:nrow(train_data)){
    uid = as.character(train_data[line,]$msno)
    iid = as.character(train_data[line,]$song_id)
    score = as.double(train_data[line,]$target)
    prediction = predictScore(av, bu[[uid]], bi[[iid]], pu[uid,], qi[,iid])
    err =+ abs(binaryScore(prediction)-score)
    #rmse =+ (score - prediction) * (score - prediction)
  }
  return (err)
}

##########Make Predictions with model
#model_par: bu, bi, pu, pi
SGDpredict <- function(h_par,model_par,test_data){
  
  #get paramters
  av = h_par[1]
  bu = model_par[[1]]
  bi = model_par[[2]]
  qi = model_par[[4]]
  pu = model_par[[3]]
  results = rep(0,nrow(test_data))
  #predict
  for (line in 1:nrow(test_data)){
    uid = as.character(test_data[line,]$msno)
    iid = as.character(test_data[line,]$song_id)
    
    results[line] = predictScore(av, bu[[uid]], bi[[iid]], pu[uid,], qi[,iid])
  }
  test_data$prediciton = results
  test_data$prediction_binary <- prediction_binary(results)
  #save prediction
  return(test_data)
}

################# Getting data ready
train_data = data.frame(training_data[,1:3])
test_data = data.frame(testing_data[,1:3])
all_ratings <- data.frame(bind_rows(training_data[,1:3],testing_data[,1:2]))
names(all_ratings) <- c('msno','song_id','target')
#length(rownames(train_data))
#all_ratings = data.frame(ratings[,1:3]) #including train and test

#rating_matrix <- reshape(train_data, direction = "wide", idvar = 'msno',timevar='song_id')
#names(rating_matrix) <- names(rating_matrix) #column indexed by song_id


################
sum(all_ratings$target,na.rm=TRUE)
nrow(all_ratings)
av = mean(all_ratings$target,na.rm=TRUE)
h_par = c(av, 0.001, 10, 0.015 )

#################
model_par = SGDsvd(train_data,all_ratings, h_par)
validate(train_data, av,model_par)
dim(test_data)
my_prediction = SGDpredict(h_par,model_par,test_data)
head(my_prediction)
validate(test_data, av,model_par)



