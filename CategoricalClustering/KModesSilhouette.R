
library(klaR)         
library(cluster)      
library(factoextra)  
my_data <- read.csv('Complete.csv', stringsAsFactors = TRUE)


km_data <- my_data[, c('Marke', 'Produktart', 'Verpackungsart', 'Material')] 


for(col in colnames(km_data)) {
  km_data[[col]] <- as.factor(km_data[[col]]) 
}


sil_width <- numeric(9) 


for(k in 2:33) {
  km <- kmodes(km_data, modes = k, iter.max = 33, weighted = FALSE) 
  diss <- daisy(km_data)
  ss <- silhouette(km$cluster, diss) 
  sil_width[k - 1] <- mean(ss[, "sil_width"])
}


plot(2:33, sil_width, type='b', xlab="Anzahl der Cluster", ylab="Durchschnittliche Silhouettenbreite", main="Silhouettenanalyse für optimales K")


optimal_k <- 33 


kmodes_model <- kmodes(km_data, modes = optimal_k, iter.max = 33, weighted = FALSE)


my_data$group_ID <- kmodes_model$cluster 


my_data_ordered <- my_data[order(my_data$group_ID), ]


row.names(my_data_ordered) <- NULL 

head(my_data_ordered) 


write.csv(my_data_ordered, 'Ordered_Complete.csv', row.names = FALSE) 

