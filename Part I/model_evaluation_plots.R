library(ggplot2)
load('pal.RData')

#Compare User-Based, Item-Based, and Random Models
plot(list_results, annotate = 1, legend = "topleft") 
title("ROC Curve Neighborhood-Based Models")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall Neighborhood-Based Models")

err$model <- row.names(err)
err_melted <- melt(err, id.vars = "model")
ggplot(err_melted, aes(variable, value)) + 
  geom_bar(aes(fill=model), position = "dodge", stat="identity") + 
  scale_fill_manual(values=pal(length(models_to_evaluate))) + 
  ggtitle("Error Metrics for Collaborative Filtering Models") +
  ylab("") + xlab("") + theme(plot.title = element_text(hjust = 0.5))

##Pearson IBCF Model: tune k 
plot(list_results_ib, annotate = 1, legend = "topleft") 
title("ROC Curve Item-Based Collaborative Filtering (Pearson)")
plot(list_results_ib, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall Item-Based Collaborative Filtering (Pearson)")
err_ib$k <- k_items
ggplot(err_ib, aes(x = k, y = MAE)) + geom_line(color = "blue") +
  ggtitle("Prediction Error versus Neighborhood Size\nfor Item-Based CF (pearson similarity)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Number of Nearest Neighbors")

#Cosine UBCF: tune nn
plot(list_results_ub, annotate = 1, legend = "topleft") 
title("ROC Curve User-Based Collaborative Filtering (Cosine)")
plot(list_results_ub, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall User-Based Collaborative Filtering (Cosine)")
err_ub$nn <- nn_users
ggplot(err_ub, aes(x = nn, y = MAE)) + geom_line(color = "red") +
  ggtitle("Prediction Error versus Neighborhood Size\nfor User-Based CF (cosine similarity)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Number of Nearest Neighbors")


#Error vs. Sample Size
# ggplot(size_err_IB, aes(x = train_prop, y = MAE)) + geom_line() + 
#   ggtitle("Error versus Sample Size (Pearson Item-Based CF)") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ggplot(size_err_UB, aes(x = train_prop, y = MAE)) + geom_line() +
#   ggtitle("Error versus Sample Size (Cosine User-Based CF)") +
#   theme(plot.title = element_text(hjust = 0.5))

ggplot(size_err_all, aes(x = train_prop, y = MAE)) + geom_line(aes(color = model)) +
  ggtitle("Error versus Sample Size") +
  theme(plot.title = element_text(hjust = 0.5))

#Error vs. Number of items per user given in test set
ggplot(keep_err_all, aes(x = ratings_given, y = MAE)) + geom_line(aes(color = model)) +
   ggtitle("Error versus Number of Items Given Per User in Test Set") +
   theme(plot.title = element_text(hjust = 0.5))

