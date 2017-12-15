#source('../Requirements.R')
#source('../filter_train_test.R')

load('FM.RData')

## @knitr FM_merge_clean
all <- data.table(rbind(training_data, testing_data))

# clean song genre
songs %<>% 
  separate(genre_ids, c("one", "two", "three", "four", "five", "six", "seven", "eight"), 
           extra="merge") %>%
  replace_na(list(one = 0, two = 0, three = 0, four = 0,
                  five = 0, six = 0, seven = 0, eight = 0)) %>% 
  mutate(no_of_genre = (if_else(one == 0, 0, 1) + if_else(two == 0, 0, 1) +
                          if_else(three == 0, 0, 1) + if_else(four == 0, 0, 1) +
                          if_else(five == 0, 0, 1) + if_else(six == 0, 0, 1) +
                          if_else(seven == 0, 0, 1) + if_else(eight == 0, 0, 1))) %>% 
  select(song_id, song_length, language, artist_name, no_of_genre, one)

#add metadata
merged_all <- all %>% 
  left_join(songs, by = "song_id") %>%
  left_join(members, by = "msno") %>%
  left_join(train, by = c("msno", "song_id", "target"))

#drop variables from the merged data
merged_all %<>%
  select(-song_count, -user_count, -registered_via, -registration_init_time, -expiration_date)

#clean data: categorical variables must be coded as factors to train the model
merged_all <- within(merged_all, {
  bd[bd < 14 | bd > 100] <- median(bd) #set age outliers to median
  logbd <- log(bd)
  log_song_length <- log10(song_length)
  one <- factor(one)
  msno <- factor(msno)
  song_id <- factor(song_id)
  artist_name <- factor(artist_name)
  language <- factor(language)
  city <- factor(city)
  gender <- factor(gender)
  source_system_tab <- factor(source_system_tab)
  source_screen_name <- factor(source_screen_name)
  source_type <- factor(source_type)
}) 

merged_training <- merged_all[1:nrow(training_data),]
merged_testing <- merged_all[-c(1:nrow(training_data)),]



## @knitr cv_k
k.vec <- seq(0, 30, by = 5)
cv_k <- cv_libFM(merged_training, target ~ song_id + language + artist_name +
                   one + no_of_genre + msno + city + gender + logbd + log_song_length +  
                   source_system_tab + source_screen_name + source_type,
                 task = "c", dims = k.vec, iter = 100, folds = 3,
                 cv_verbosity = 1, verbosity = 1, method = "mcmc")


## @knitr fm_k_cv_results
#use the fm_k_cv data frame in the FM.RData file which contains the console output of cv_libFM
cv_k_results <- plot_param_cv(k.vec, fm_k_cv, err.bar.width = .3)
as.tibble(cv_k_results[[1]])
cv_k_results[[2]] + xlab('Number of Latent Dimensions (k)') +
  ggtitle('Tuning Hyperparameter for Latent Dimensionality') 

k <- k.vec[which.min(cv_k_results[[1]]$mean)]
k

## @knitr cv_sd
sd.vec <- c(.01, .05, .1, .15, .2)
cv_sd <- cv_libFM(merged_training, target ~ song_id + language + artist_name +
                   one + no_of_genre + msno + city + gender + logbd + log_song_length +  
                   source_system_tab + source_screen_name + source_type,
                 task = "c", dim = k, init_stdevs = sd.vec, iter = 100, folds = 3,
                 cv_verbosity = 1, verbosity = 1, method = "mcmc")

## @knitr fm_sd_cv_results
#use the fm_sd_cv data frame in the FM.RData file which contains the console output of cv_libFM
cv_sd_results <- plot_param_cv(sd.vec, fm_sd_cv, err.bar.width = .003)
cv_sd_results[[1]]
cv_sd_results[[2]] + xlab("Initialized Standard Deviation") + 
  ggtitle('Tuning Initialized Standard Deviation') 


## @knitr fm_model
#create an FM model with chosen  hyperparameters (k = 25 and init_stdev = .1)
predFM.mcmc <- libFM(merged_training, merged_testing, target ~ song_id + language + artist_name +
                        one + no_of_genre + msno + city + gender + logbd + log_song_length +  
                        source_system_tab + source_screen_name + source_type,
                      task = "c", dim = k, init_stdev = .1, iter = 500,
                      verbosity = 1, method = "mcmc")

#predFM.mcmc.accuracy contains the console output of libFM 
#(that is, the training and testing accuracy for each iteration of MCMC)
predFM.mcmc.err <- 1 - predFM.mcmc.accuracy
predFM.mcmc.err <- cbind(Iteration = 1:500, predFM.mcmc.err)
names(predFM.mcmc.err)[2:3] <- c('Train', 'Test')

## @knitr err_convergence
err.conv <- predFM.mcmc.err %>% melt(id = 1) %>%
  ggplot(aes(x = Iteration, y = value, color = variable)) +
  geom_line() + ylab('Error Rate') + 
  ggtitle('Convergence of Training and Testing Error') +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) 
err.conv


## @knitr evaluate_FM
FM.eval <- evaluate(scores = predFM.mcmc, target = testing_data$target)
FM.roc.plot <- ggroc(FM.eval[[3]], alpha = 0.5, colour = "blue", linetype = 2, size = 2) +
  theme(plot.title = element_text(hjust = 0.5)) 

## @knitr evaluate_FM_print
FM.eval[[1]] #print evaluation metrics
FM.eval[[2]] #print confusion matrix
FM.roc.plot + ggtitle('ROC Curve for Factorization Machine Model') +
  theme(plot.title = element_text(hjust = 0.5))