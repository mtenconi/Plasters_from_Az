# Principal Component Analysis (PCA):
# log10 transformation
log10_data <-   log10(new_data[,3:17]) 
log10_data <- data.frame(sample_id = new_data$sample_id,
                         group = new_data$group, log10_data)
rownames(log10_data) <- new_data$sample_id

# pca
pca <- prcomp(log10_data[,3:17], center = TRUE, scale = FALSE)

# Analysis of results:
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 57))
round(get_eigenvalue(pca)[1:4,],2)   # first 4 principal components explain 89% of total variation in the dataset

# Most important (or, contributing) variables highlighted
# Biplot:
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "contrib", # Variables color
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                col.ind = "#696969",  # Individuals color
) + theme_minimal() + ggtitle("PCA - Biplot")

# Contribution of variables to PC1, PC2, PC3
var <- get_pca_var(pca)
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)

# PCA graph of individuals with most important (or, contributing) highlighted
fviz_contrib(pca, choice = "ind", axes = 1:2, top = 10)

fviz_pca_ind(pca, habillage=new_data$group, 
             labelsize = 1, pointsize =3, font.family = "Arial",
             geom = "point",
             pointshape = 16) + 
  scale_color_brewer(palette="Paired",name = "Group")

#########################################################################################
# new data set obtained through pca (pc1, pc2, pc3, pc4):
pca_df <- data.frame(pc1 = pca$x[,1], pc2 = pca$x[,2], pc3 = pca$x[,3],
                     group = log10_data$group)

# Scatter plot of pc1 and pc2, grouped by groups
pca12 <- pca_df %>% 
  ggplot(aes(pc1, pc2,color = group, fill = group)) +
  geom_point(aes(color = group, fill = group), size = 3)+
  ggtitle("Principal Component Analysis")

pca13 <- pca_df %>% 
  ggplot(aes(pc1, pc3,color = group, fill = group)) +
  geom_point(aes(color = group, fill = group), size = 3) +
  ggtitle("Principal Component Analysis")

pca23 <- pca_df %>% 
  ggplot(aes(pc2, pc3,color = group, fill = group)) +
  geom_point(aes(color = group, fill = group), size = 3) +
  ggtitle("Principal Component Analysis")

p <- pca12 + theme(panel.background = element_blank(),
          axis.line = element_line(size = 0.2,linetype = 'solid'),
          legend.key = element_rect(colour = NA, fill = NA))

p1 <- pca12 + theme(
  # get rid of panel grids
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change plot and panel background
  plot.background=element_rect(fill = "black"),
  panel.background = element_rect(fill = 'black'),
  # Change legend 
  legend.background = element_rect(fill = "black", color = NA),
  legend.key = element_rect( fill = "black"),
  legend.title = element_text(color = "white"),
  legend.text = element_text(color = "white"),
  title = element_text(colour = "white"),
  axis.text = element_text(color = "white"), axis.line = element_line(color = "white"),
  axis.title = element_text(color = "white")
)

p2 <- pca13 + theme(
  # get rid of panel grids
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change plot and panel background
  plot.background=element_rect(fill = "black"),
  panel.background = element_rect(fill = 'black'),
  # Change legend 
  legend.background = element_rect(fill = "black", color = NA),
  legend.key = element_rect( fill = "black"),
  legend.title = element_text(color = "white"),
  legend.text = element_text(color = "white"),
  title = element_text(colour = "white"),
  axis.text = element_text(color = "white"), axis.line = element_line(color = "white"),
  axis.title = element_text(color = "white")
)

p3 <- pca23 + theme(
  # get rid of panel grids
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change plot and panel background
  plot.background=element_rect(fill = "black"),
  panel.background = element_rect(fill = 'black'),
  # Change legend 
  legend.background = element_rect(fill = "black", color = NA),
  legend.key = element_rect( fill = "black"),
  legend.title = element_text(color = "white"),
  legend.text = element_text(color = "white"),
  title = element_text(colour = "white"),
  axis.text = element_text(color = "white"), axis.line = element_line(color = "white"),
  axis.title = element_text(color = "white")
)

p
p1
p2
p3
