library(ggplot2)
library(dplyr)

# Read data with target variable
data <- read.table("seeds_dataset.txt", sep="\t", header=TRUE, col.names=c("feature1", "feature2", "feature3", "feature4", "feature5", "feature6", "feature7", "target"))

# Print head and check for missing values
print(head(data))
print(colSums(is.na(data)))

# Outlier detection function
odlehle_hodnoty <- function(data, threshold=1.25) {
  Q1 <- apply(data, 2, quantile, probs=0.25)
  Q3 <- apply(data, 2, quantile, probs=0.75)
  IQR <- Q3 - Q1
  return(rowSums(data < (Q1 - threshold * IQR) | data > (Q3 + threshold * IQR)))
}

# Print the number of outliers
print(odlehle_hodnoty(data))

# Boxplot
boxplot(data, main="Boxplot of Features")

library(GGally)
ggplot(data2, aes(col = target)) +
  geom_parallel_coordinates(alpha = 0.4, col = "viridis") +
  theme_minimal()

# Min-max scaling
columns_to_normalize <- c("feature1", "feature2", "feature3", "feature4", "feature5", "feature6", "feature7")
data[columns_to_normalize] <- scale(data[columns_to_normalize])

# Print head after scaling
print(head(data))







# Specify columns for clustering
columns_for_clustering <- c("feature1", "feature2", "feature3", "feature4", "feature5", "feature6", "feature7")

# Specify distance metric and linkage method
distance_metric <- 'euclidean'
linkage_method <- 'ward'

# Perform hierarchical clustering
dendrogram_data <- hclust(dist(data[columns_for_clustering], method=distance_metric), method=linkage_method)

# Plot dendrogram
plot(dendrogram_data, labels=data$target, hang=-1, main='Dendrogram Hierarchického Shlukování', xlab='Vzorky', ylab='Vzdálenost', sub='')

# Rotate labels for better visibility
text(dendrogram_data, labels=data$target, hang=-1, cex=0.8, col='blue', xpd=NA, srt=90)





# Specify the number of clusters (K)
k_clusters <- 3

# Columns for clustering
columns_for_clustering <- c("feature1", "feature2", "feature3", "feature4", "feature5", "feature6", "feature7")

# Initialize and fit the K-means model
kmeans_model <- kmeans(data[columns_for_clustering], centers = k_clusters, iter.max = 100, nstart = 10)

# Add cluster assignment to the data
data$cluster <- kmeans_model$cluster

# Visualization of clustering results
plot_title <- "Výsledek K-means Shlukování"
x_label <- columns_for_clustering[1]
y_label <- columns_for_clustering[2]

plot(data[columns_for_clustering[1:2]], col = data$cluster, pch = 16, main = plot_title, xlab = x_label, ylab = y_label)

# Add centroids to the plot
points(kmeans_model$centers[, c(1, 2)], col = 1:k_clusters, pch = "X", cex = 2)

# Add legend
legend("topright", legend = paste("Cluster", 1:k_clusters), col = 1:k_clusters, pch = 16)

# Show the plot





linkage_method <- 'ward'

# Hierarchical clustering
hierarchical_clusters <- cutree(hclust(dist(data[columns_for_clustering], method='euclidean'), method=linkage_method), k = 3)

# Add hierarchical clustering results to the original data
data$hierarchical_cluster <- hierarchical_clusters

# Initialize K-means centroids from hierarchical clustering
initial_centers <- aggregate(. ~ hierarchical_cluster, data[columns_for_clustering], mean)

# K-means with initial centroids from hierarchical clustering
k_clusters <- 3
kmeans_model <- kmeans(data[columns_for_clustering], centers = initial_centers[, -1], nstart = 1)

# Add K-means clustering results to the original data
data$kmeans_cluster <- kmeans_model$cluster

# Visualization of results
par(mfrow=c(1,2), mar=c(5, 4, 4, 2) + 0.1)

# Hierarchical clustering results
plot(data[columns_for_clustering[1:2]], col = data$hierarchical_cluster, pch = 16, main = "Hierarchické Shlukování", xlab = columns_for_clustering[1], ylab = columns_for_clustering[2])

# K-means results with initialization from hierarchical clustering
plot(data[columns_for_clustering[1:2]], col = data$kmeans_cluster, pch = 16, main = "K-means s Inicializaèními Støedy z Hierarchického Shlukování", xlab = columns_for_clustering[1], ylab = columns_for_clustering[2])

# Show the plots