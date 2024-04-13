# Load necessary libraries
library(cluster)    # For Gower's distance and hierarchical clustering
library(factoextra) # For visualizing the dendrogram

# Step 1: Data Preparation
my_data <- read.csv('Complete.csv', stringsAsFactors = TRUE)
my_data <- my_data[, !colnames(my_data) %in% c('Größe')] # Exclude 'Größe' column

# Step 2: Encode Categorical Variables
# Convert all categorical variables to factors for Gower distance
for(col in colnames(my_data)) {
  my_data[[col]] <- as.factor(my_data[[col]])
}

# Step 3: Distance Calculation
gower_dist <- daisy(my_data, metric = "gower")

# Step 4: Hierarchical Clustering
hc <- hclust(gower_dist, method = "ward.D2")

# Step 5: Plot the Dendrogram to help determine the number of clusters
fviz_dend(hc, rect = TRUE, cex = 0.5)

# Step 6: Cut the dendrogram to form clusters
# You might need to replace the value of k with the number of clusters you decide upon
k <- 15 # for example, you might decide on 5 clusters after viewing the dendrogram
groups <- cutree(hc, k = k)

# Assign cluster labels
my_data$cluster_ID <- groups

# Now, you can inspect the first few rows to see the cluster labels
head(my_data)

