## @knitr load_plot_data
load('pal.RData')
load('all_models.RData')
#overwrite with svd models - doing this way so i dont remove anything important
list_results <- readRDS("model info/list_results.rds")
rat_results <- readRDS("model info/rat_results.rds")
err <- readRDS("model info/err.rds")
list_results_svd <- readRDS("model info/list_results_svd.rds")
rat_results_svd <- readRDS("model info/rat_results_svd.rds")
err_svd <- readRDS("model info/err_svd.rds")
size_err_all <- readRDS( "model info/size_err_all.rds")
keep_err_all <- readRDS( "model info/keep_err_all.rds")
#keep2_err_all <- readRDS( "model info/keep2_err_all.rds")
#dont break things
models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  SVD = list(name = "SVD"),
  random = list(name = "RANDOM", param=NULL)
)
k_svd <- c(2,4,6,8,9,10,11,12, 14,16, 18,20) #possible values for k in svd


library(ggplot2)
library(gridExtra)

## @knitr compare_cf_topN
plot(list_results, annotate = 1, legend = "topleft")
title("ROC Curve")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall")

## @knitr compare_cf_ratings
err$model <- row.names(err)
err_melted <- melt(err, id.vars = "model")
p1 <- ggplot(err_melted, aes(variable, value)) +
  geom_bar(aes(fill=model), position = "dodge", stat="identity") +
  scale_fill_manual(values=pal(length(models_to_evaluate))) +
  ggtitle("Error Metrics for Collaborative Filtering Models") +
  ylab("") + xlab("") + theme(plot.title = element_text(hjust = 0.5))
p1 <- ggplotly(p1)
p1

## @knitr ibcf_tune_k_plots
plot(list_results_ib, annotate = 1, legend = "topleft")
title("ROC Curve Item-Based Collaborative Filtering (Pearson)")
plot(list_results_ib, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall Item-Based Collaborative Filtering (Pearson)")
err_ib$k <- k_items
p2 <- ggplot(err_ib, aes(x = k, y = MAE)) + geom_line(color = "blue") +
  ggtitle("Prediction Error versus Neighborhood Size\nfor Item-Based CF (pearson correlation)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Number of Nearest Neighbors")
p2 <- ggplotly(p2)
## @knitr ubcf_tune_nn_plots
plot(list_results_ub, annotate = 1, legend = "topleft")
title("ROC Curve User-Based Collaborative Filtering (Cosine)")
plot(list_results_ub, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall User-Based Collaborative Filtering (Cosine)")
err_ub$nn <- nn_users
p3 <- ggplot(err_ub, aes(x = nn, y = MAE)) + geom_line(color = "red") +
  ggtitle("Prediction Error versus Neighborhood Size\nfor User-Based CF (cosine similarity)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Number of Nearest Neighbors")
p3 <- ggplotly(p3)
p3

## @knitr svd_tune_k_plots
plot(list_results_svd, annotate = 1, legend = "topleft")
title("ROC Curve SVD")
plot(list_results_svd, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall SVD")
err_svd$k <- k_svd
p7 <- ggplot(err_svd, aes(x = k, y = MAE)) + geom_line(color = "blue") +
  ggtitle("Prediction Error versus Number of Dimensions\nfor SVD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Number of Dimensions")
p7 <- ggplotly(p7)
p7

## @knitr err_vs_size_plots
#Error vs. Sample Size
# ggplot(size_err_IB, aes(x = train_prop, y = MAE)) + geom_line() +
#   ggtitle("Error versus Sample Size (Pearson Item-Based CF)") +
#   theme(plot.title = element_text(hjust = 0.5))
#
# ggplot(size_err_UB, aes(x = train_prop, y = MAE)) + geom_line() +
#   ggtitle("Error versus Sample Size (Cosine User-Based CF)") +
#   theme(plot.title = element_text(hjust = 0.5))

## @knitr err_vs_size_all_plot
p4 <- ggplot(size_err_all, aes(x = train_prop, y = MAE)) + geom_line(aes(color = model)) +
  ggtitle("Error versus Sample Size") +
  xlab("Proportion of Data Trained On") +
  theme(plot.title = element_text(hjust = 0.5))
p4 <- ggplotly(p4)
p4
## @knitr err_vs_numitems_plot
p5 <- ggplot(keep_err_all, aes(x = ratings_given, y = MAE)) + geom_line(aes(color = model)) +
   ggtitle("Error versus Information About User Preference") +
   xlab("Ratings Given for Each Test User") +
   theme(plot.title = element_text(hjust = 0.5))
p5 <- ggplotly(p5)
p5

## @knitr err_vs_numitems_complete_plot
p6 <- ggplot(keep2_err_all, aes(x = ratings_given, y = MAE)) + geom_line(aes(color = model)) +
  ggtitle("Error versus Information About User Preference") +
  xlab("Ratings Given for Each Test User") +
  theme(plot.title = element_text(hjust = 0.5))
p6 <- ggplotly(p6)
p6

## @knitr size_vs_time_all_plot
p8 <- ggplot(size_time_all, aes(x = `Sample Proportion`)) + 
  geom_line(aes( y = `Total Running time`,color = model)) +
  ggtitle("Running Time versus Sample Size") +
  xlab("Proportion of Data Trained On") +
  theme(plot.title = element_text(hjust = 0.5))
p8 <- ggplotly(p8)
p8

## @knitr size_vs_time_IB_plot
p9 <- ggplot(size_time_IB,aes(`Sample Proportion`)) +
  geom_line(aes( y = `Total Running time`, col = "Total Running Time")) +
  geom_line(aes(y=`Train time`, col = "Training Time")) +
  geom_line(aes(y = `Predict time`, col = "Prediction Time")) +
  ggtitle("Item-Based CF Running Time versus Sample Size") +
  xlab("Proportion of Data Trained On") +
  ylab("Time")+
  theme(plot.title = element_text(hjust = 0.5),legend.title=element_blank())
p9 <- ggplotly(p9)
p9

## @knitr size_vs_time_UB_plot
p10 <- ggplot(size_time_UB,aes(`Sample Proportion`)) +
  geom_line(aes( y = `Total Running time`, col = "Total Running Time")) +
  geom_line(aes(y=`Train time`, col = "Training Time")) +
  geom_line(aes(y = `Predict time`, col = "Prediction Time")) +
  ggtitle("User-Based CF Running Time versus Sample Size") +
  xlab("Proportion of Data Trained On") +
  ylab("Time")+
  theme(plot.title = element_text(hjust = 0.5),legend.title=element_blank())
p10 <- ggplotly(p10)
p10

## @knitr size_vs_time_svd_plot
p11 <- ggplot(size_time_SVD,aes(`Sample Proportion`)) +
  geom_line(aes( y = `Total Running time`, col = "Total Running Time")) +
  geom_line(aes(y=`Train time`, col = "Training Time")) +
  geom_line(aes(y = `Predict time`, col = "Prediction Time")) +
  ggtitle("SVD Running Time versus Sample Size") +
  xlab("Proportion of Data Trained On") +
  ylab("Time")+
  theme(plot.title = element_text(hjust = 0.5),legend.title=element_blank())
p11 <- ggplotly(p11)
p11
