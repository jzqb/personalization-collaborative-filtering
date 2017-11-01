## @knitr load_plot_data
load('pal.RData')
load('all_models.RData')
library(ggplot2)
library(gridExtra)

## @knitr compare_cf_topN
plot(list_results, annotate = 1, legend = "topleft")
title("ROC Curve Neighborhood-Based Models")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall Neighborhood-Based Models")

## @knitr compare_cf_ratings
err$model <- row.names(err)
err_melted <- melt(err, id.vars = "model")
ggplot(err_melted, aes(variable, value)) +
  geom_bar(aes(fill=model), position = "dodge", stat="identity") +
  scale_fill_manual(values=pal(length(models_to_evaluate))) +
  ggtitle("Error Metrics for Collaborative Filtering Models") +
  ylab("") + xlab("") + theme(plot.title = element_text(hjust = 0.5))

## @knitr ibcf_tune_k_plots
plot(list_results_ib, annotate = 1, legend = "topleft")
title("ROC Curve Item-Based Collaborative Filtering (Pearson)")
plot(list_results_ib, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall Item-Based Collaborative Filtering (Pearson)")
err_ib$k <- k_items
ggplot(err_ib, aes(x = k, y = MAE)) + geom_line(color = "blue") +
  ggtitle("Prediction Error versus Neighborhood Size\nfor Item-Based CF (pearson correlation)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Number of Nearest Neighbors")

## @knitr ubcf_tune_nn_plots
plot(list_results_ub, annotate = 1, legend = "topleft")
title("ROC Curve User-Based Collaborative Filtering (Cosine)")
plot(list_results_ub, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall User-Based Collaborative Filtering (Cosine)")
err_ub$nn <- nn_users
ggplot(err_ub, aes(x = nn, y = MAE)) + geom_line(color = "red") +
  ggtitle("Prediction Error versus Neighborhood Size\nfor User-Based CF (cosine similarity)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Number of Nearest Neighbors")


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
ggplot(size_err_all, aes(x = train_prop, y = MAE)) + geom_line(aes(color = model)) +
  ggtitle("Error versus Sample Size") +
  xlab("Proportion of Data Trained On") +
  theme(plot.title = element_text(hjust = 0.5))

## @knitr err_vs_numitems_plot
ggplot(keep_err_all, aes(x = ratings_given, y = MAE)) + geom_line(aes(color = model)) +
   ggtitle("Error versus Information About User Preference") +
   xlab("Ratings Given for Each Test User") +
   theme(plot.title = element_text(hjust = 0.5))

## @knitr err_vs_numitems_complete_plot
ggplot(keep2_err_all, aes(x = ratings_given, y = MAE)) + geom_line(aes(color = model)) +
  ggtitle("Error versus Information About User Preference") +
  xlab("Ratings Given for Each Test User") +
  theme(plot.title = element_text(hjust = 0.5))
