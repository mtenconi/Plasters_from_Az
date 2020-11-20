# Assess clustering tendency: evaluate whether the dataset contains meaningful clusters or not
#Hopkins' statistic & visual approach
hc_data <- new_data[,3:17]   

# Hopkins' statistic: H value > 0.5 = the dataset is clusterable
gradient.color <- list(low = "red",  high = "yellow")
set.seed(123)
hc_data %>%
  scale() %>%     # Scale variables
  get_clust_tendency(n = 35, gradient = gradient.color) # the dataset is clusterable (H value = 0.75 )

#########################################################################################
# Determining the optimal number of clusters
hc_data <- scale(hc_data)
head(hc_data,2)
dim(hc_data)
rownames(hc_data) <- new_data$sample_id

# Methods:
# Elbow method
fviz_nbclust(hc_data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+ 
  labs(subtitle = "Elbow method")   # Elbow method: 4 clusters solution suggested

# Silhouette method
fviz_nbclust(hc_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")  # Silhouette method: 2 clusters solution suggested

# Gap statistic
set.seed(123)
gap_stat <- clusGap(hc_data, FUN = kmeans, nstart = 30, K.max = 10, B = 250)
fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")   # Gap statistic method: 1 cluster solution suggested

# NbClust()
set.seed(123)
res.nbclust <- hc_data %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 10, 
          method = "kmeans", index ="all") 
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())  # NbClust, method "kmeans": 2 clusters solution suggested

###########################################################################################
# K-means clustering: 
set.seed(123)
km.res <- kmeans(hc_data, 2, nstart = 30)   # k = 2

# visualization
fviz_cluster(km.res, data = hc_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# comparison of kmean clusters with fabrics:
new_data$group[which(km.res$cluster == 1)]
new_data$group[which(km.res$cluster == 2)]

#######################################
# Examine different values of k:
kmean_calc <- function(df, ...){
  kmeans(df, scaled = ..., nstart = 30)
}

km2 <- kmean_calc(hc_data, 2)
km3 <- kmean_calc(hc_data, 3)
km4 <- kmeans(hc_data, 4)
km5 <- kmeans(hc_data, 5)

p2 <- fviz_cluster(km2, data = hc_data, frame.type = "convex") + theme_minimal() + ggtitle("k = 2") 
p3 <- fviz_cluster(km3, data = hc_data, frame.type = "convex") + theme_minimal() + ggtitle("k = 3")
p4 <- fviz_cluster(km4, data = hc_data, frame.type = "convex") + theme_minimal() + ggtitle("k = 4")
p5 <- fviz_cluster(km5, data = hc_data, frame.type = "convex") + theme_minimal() + ggtitle("k = 5")

plot_grid(p2, p3, p4, p5, labels = c("k2", "k3", "k4", "k5"))
plot_grid(p2, labels = c("k2"))
plot_grid(p3, labels = c("k3"))
plot_grid(p4, labels = c("k4"))

# comparison of fabrics with kmean clusters
new_data$group[which(km4$cluster == 1)]
new_data$group[which(km4$cluster == 2)]
new_data$group[which(km4$cluster == 3)]
new_data$group[which(km4$cluster == 4)]

###########################################################################################
# Hierarchical clustering
hc_data <- new_data[,3:17]
rownames(hc_data) <- new_data$sample_id

hc_ward <- hc_data %>%
  scale() %>%                    
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")     

hc_single <- hc_data %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "single")

hc_complete <- hc_data %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "complete")

hc_median <- hc_data %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "median")

hc_average <- hc_data %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "average")

hc_centroid <- hc_data %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "centroid")

# ward method
fviz_dend(hc_ward, k = 4,
          main = "Dendrogram - ward",
          cex = 0.5, # label size
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE  # color labels by groups
)

# sinlge method
fviz_dend(hc_single, k = 4,
          main = "Dendrogram - sinlge",
          cex = 0.5,
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE
)

# complete method
fviz_dend(hc_complete, k = 4,
          main = "Dendrogram - complete",
          cex = 0.5,
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE
)

# median method
fviz_dend(hc_median, k = 4,
          main = "Dendrogram - median",
          cex = 0.5,
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE
)

# average method
fviz_dend(hc_average, k = 4,
          main = "Dendrogram - average",
          cex = 0.5,
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE
)

# centroid method
fviz_dend(hc_centroid, k = 4,
          main = "Dendrogram - centroid",
          cex = 0.5,
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE
)
