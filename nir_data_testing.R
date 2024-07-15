---
title: "Compiled Chocolate Analysis"
author: "Kelly Lee"
date: "`r Sys.Date()`"
output: html_document
---
```{r}
# Load everything
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(caret)
library(FactoMineR)
library(tidyverse)
library(plotly)
library(tibble)
library(knitr)
library(scutr)
library(mdatools)
library(pls)
mytheme <- theme(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.border = element_blank(),
  axis.line.x = element_line(color = 'black', linewidth = 0.7),
  axis.line.y = element_line(color = 'black', linewidth = 0.7),
  axis.ticks = element_line(color = 'black', linewidth = 0.7),
  axis.title.x = element_text(size = 12, color = "black"),
  axis.title.y = element_text(size = 12, color = "black"),
  axis.text.x = element_text(size = 12, color = "black", hjust = 0.5),
  axis.text.y = element_text(size = 12, color = "black")
)

```

```{r}
file_path <- "/Users/kellychaeyeonlee/Downloads/compiled_chocolatenir.csv"  
data <- read_csv(file_path)
data$shell <- as.factor(data$shell)
features <- data[, -c(1, 2)]
features_scaled <-scale(features)
```
##Preprocessing with Spectra Plot 
#What is a Spectra Plot ? It allows for us to examine the clynic structure (frequnecy) of a time series. A time series represents a progression of some parameter of time. 
#prime technique used in statistics and data analytics for determining cylcic nature of one-time series in frequency domain


Spectra plot just using wavelengths
```{r}
data$shell <- as.factor(data$shell)
wavenumbers <- as.numeric(names(data)[3:ncol(data)])
wavelengths <- 1 / wavenumbers

# Convert data to long format for ggplot
data_long <- data %>%
  pivot_longer(cols = 3:ncol(data), names_to = "wavelengths", values_to = "value") 
data_long$wavelengths <- as.numeric(data_long$wavelengths)

# spectra plot, more smoothed out 
ggplot(data_long, aes(x = wavelengths, y = value, group = sample)) +  # group by sample
   geom_smooth(aes(color = shell), method = "loess", span = 0.1, se = FALSE, size = 0.2, alpha = 0.8) + #smoothing 
   scale_x_continuous(breaks = seq(min(data_long$wavelengths), max(data_long$wavelengths), by = 1000))+ #wavelengths by 200 
  labs(title = "NIR Spectra", x = "wavelenghts (nm)", y = "Absorbance/Reflectance") + #x-plot and y-plot 
  theme_minimal() +
  theme(legend.position = "bottom")

# Average spectra plot
average_data <- data_long %>%
  group_by(wavelengths) %>%
  summarize(sample_means = mean(value))
ggplot(average_data, aes(x = wavelengths, y = sample_means)) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, size = 0.4, alpha = 0.8) + # smoothing
  scale_x_continuous(breaks = seq(min(average_data$wavelengths), max(average_data$wavelengths), by = 1000)) + # wavelengths by 1000
  labs(title = "Average NIR Spectra", x = "Wavelengths (nm)", y = "Absorbance/Reflectance") # x and y labels
```


 
 
 ##PCA
```{r}
pca.model <- FactoMineR::PCA(features_scaled, scale.unit = TRUE, graph = FALSE)
# Extract PCA results
pca_results <- data.frame(pca.model$ind$coord)
pca_results$shell <- data$shell


# Plot 
fviz_eig(pca.model, addlabels = TRUE, hjust = -0.3) +
  mytheme +
  theme_minimal()

fviz_pca_ind(pca.model,
             label = "none", 
             habillage = data$shell,
             addEllipses = TRUE
)

```
```{r}
# 3D plot
fig <- plot_ly(
  data = pca_results,
  x = ~Dim.1,
  y = ~Dim.2,
  z = ~Dim.3,
  color = ~shell,
  marker = list(size = 5)
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = 'PC1'),
      yaxis = list(title = 'PC2'),
      zaxis = list(title = 'PC3')
    ),
    title = '3D PCA Plot'
  )

fig
```
 ##DAPC Test
```{r}
library(adegenet)

# remove the first 2 columns which are non-numerical, other data is already clean 
data_numerical<- data[, -c(1:2)]

# Performing PCA(using a different method) to to choose the number of PCs
pca_dapc <- prcomp(data_numerical, scale. = TRUE)
# Plot the PCA (scree plot) to see how many PCs to retain
plot(pca_dapc, type = "l")


dapc_shell_graph <- dapc(x = data_numerical, grp = data$shell, n.pca = 3, n.da = length(unique(data$shell)) - 1)

fviz_dapc_shell_graph <- fviz_cluster(list(data = dapc_shell_graph$ind.coord, 
                                       cluster = dapc_shell_graph$grp),
                                  ellipse.type = "norm",
                                  palette = "jco",
                                  geom = "point",
                                  stand = FALSE, 
                                  repel = TRUE) + 
  theme_minimal() +
  coord_fixed(ratio = 1) 

fviz_dapc_shell_graph



```
 ##KNN Testing 
 
 do 80 20 
```{r}
library(caTools)
library(class)
cv <- sort(sample(nrow(data), nrow(data)*0.7))
train_set <- data[cv,] #80%
test_set <- data[-cv,] #20%
train_scaled = scale(train_set[,5:952])
test_scaled = scale(test_set[,5:952])

test_pred <- knn(
  train = train_scaled,
  test = test_scaled,
  cl = train_set$shell,
  #start with k = 10 (just a starting point)
  k = 10
)

actual <- test_set$shell
conf_matrix <- table(actual, test_pred)
conf_matrix

accuracy <- sum(diag(conf_matrix))/length(actual)
sprintf("Accuracy: %.2f%%", accuracy*100)
```

```{r}
# Load necessary libraries
library(caTools)
library(class)
library(plotly)
library(FactoMineR)

##pca first (using data from the KNN)
pca <- PCA(train_scaled, scale.unit = TRUE, ncp = 3, graph = FALSE)
train_pca <- as.data.frame(pca$ind$coord)
test_pca <- as.data.frame(predict(pca, newdata = test_scaled)$coord)

#plotly 
fig <- plot_ly(
  x = test_pca[, 1],
  y = test_pca[, 2],
  z = test_pca[, 3],
  type = "scatter3d",
  mode = "markers",
  color = test_pred,
  marker = list(size = 7)
) %>% layout(
  title = "3D Scatter Plot of k-NN Predictions",
  scene = list(
    xaxis = list(title = "PC1"),
    yaxis = list(title = "PC2"),
    zaxis = list(title = "PC3")
  )
)


fig
htmlwidgets::saveWidget(fig, "~/Downloads/knn.html")





```
 
 
##SVM
```{r}

suppressWarnings(suppressMessages(library(e1071)))
suppressWarnings(suppressMessages(library(dplyr)))



model.svm <- svm(x = data[,3:952], y = data$shell, type = "C")
par(mfrow = c(1,1))
plot(cmdscale(dist(data[,3:952])),
     col = as.integer(data$shell),
     pch = c("o","+")[1:150 %in% model.svm$index + 1],
     xlab = "Multidimensional Scaling Dimension 1 ",
     ylab = "Multidimensional Scaling Dimension 2", 
     main = "SVM Plot")
      
legend("topright", legend = levels(data$shell), col =1:length(levels(data$shell)), pch =1, title = "Shell Content")
```
```{r}
library(plotly)
library(e1071)
library(dplyr)

# MDS 
mds_coords <- cmdscale(dist(data[, 3:952]), k = 3) #computes pairwise distances between the samples, dist calculates the eucilidean distance. cmdscale performs the MDS on the distance matrix, k = 3 means that it is 3 dimensions
mds_df <- as.data.frame(mds_coords) 
mds_df$shell <- as.factor(data$shell) 


fig <- plot_ly(
  data = mds_df,
  x = ~V1,
  y = ~V2,
  z = ~V3,
  color = ~shell,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 6)
) %>% layout(
  title = "3D SVM Plot of Shell Content Types",
  scene = list(
    xaxis = list(title = "Multidimensional Scaling Dimension 1"),
    yaxis = list(title = "Multidimensional Scaling Dimension 2"),
    zaxis = list(title = "Multidimensional Scaling Dimension 3")
  ),
  legend = list(title = list(text = "Shell Content"))
)

#graph the plot 
fig

```
Random Forest 
```{r}

library(randomForest)
library(reprtree)
library(gplots)


names(data)[3:ncol(data)] <- make.names(names(data)[3:ncol(data)], unique = TRUE)
data$shell <- as.factor(data$shell)

rounded_data <- data[, 3:ncol(data)]  # Include all feature columns
rounded_data = round(rounded_data, digits = 3)
rounded_data$shell <- data$shell

set.seed(123) 
RFmodel <- randomForest(shell ~ ., data = rounded_data, importance = TRUE, ntree = 500, mtry = 2, do.trace = 100)
print(RFmodel)
#plot the tree
reprtree::plot.getTree(RFmodel, k = 1, depth = 9)  # Plot the 1st tree up to a depth of 5
# Plot the variable importance
varImpPlot(RFmodel, main = "Variable Importance", cex = 0.5)




```





