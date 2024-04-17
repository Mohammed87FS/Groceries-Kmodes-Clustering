packages <- c("klaR", "cluster", "dplyr", "caret", "ggplot2")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


lapply(packages, require, character.only = TRUE)



library(klaR)
library(cluster)

library(dplyr)
library(caret)
library(ggplot2)



my_data <- read.csv('Complete.csv', stringsAsFactors = TRUE)
km_data <- my_data[, c('Marke', 'Produktart', 'Verpackungsart', 'Material')]
km_data[] <- lapply(km_data, factor)


diss <- daisy(km_data)


sil_width <- numeric(32)
for(k in 2:33) {
  km <- kmodes(km_data, modes = k, iter.max = 33, weighted = FALSE)
  ss <- silhouette(km$cluster, diss)
  sil_width[k - 1] <- mean(ss[, "sil_width"])
}






plot(2:33, sil_width, type='b', xlab="Anzahl der Cluster", ylab="Durchschnittliche Silhouettenbreite", main="Silhouettenanalyse für optimales K")
optimal_k <- which.max(sil_width) + 1 
abline(v=optimal_k, col="red")  
print(sil_width)


kmodes_model <- kmodes(km_data, modes = optimal_k, iter.max = 33, weighted = FALSE)
my_data$group_ID <- kmodes_model$cluster


my_data_ordered <- my_data[order(my_data$group_ID), ]
head(my_data_ordered)



write.csv(my_data_ordered, 'Ordered_CompleteEco.csv', row.names = FALSE, fileEncoding="UTF-8")


model <- glm(Ecofriendly ~ Produktart, family = binomial(link = "logit"), data = my_data_ordered)


my_data_ordered$PredictedProbability <- predict(model, type = "response")


predicted_classes <- ifelse(my_data_ordered$PredictedProbability > 0.5, 1, 0)
conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(my_data_ordered$Ecofriendly))


print(conf_matrix)

ggplot(my_data_ordered, aes(x = as.numeric(Ecofriendly), y = PredictedProbability, color = Ecofriendly)) +
  geom_jitter(alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(title = "Predicted Probabilities vs. Actual Class", x = "Actual Class", y = "Predicted Probability")
