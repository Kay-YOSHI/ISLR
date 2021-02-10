# An Introduction to Statistical Learning with Applications in R
# Ch.10: Unsupervised Learning

#========================================================================#
# 10.4 Principal Components Analysis
#========================================================================#

# Briefly examine the data
#=============================================
# We use "USArrests" data set

# The rows of the data set contain the 50 states, in alphabetical order
row.names(USArrests)

# The columns of the data set contain the four variables
names(USArrests)

# Mean and variance of each variable
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# Perform PCA 
#=============================================
# pr.out$center: means of the variables that were used for scaling
# pr.out$scale: standard deviations of the variables that were used for scaling
# pr.out$rotation: principal component loading vector
# pr.out$x: principal component score vectors
pr.out = prcomp(USArrests, scale = TRUE)

# Plot the first two principal components (Mirror image of FIGURE 10.1)
biplot(pr.out, scale = 0)

# Replication of FIGURE 10.1 in page 378
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)

# Standard deviation of each component
pr.out$sdev

# Variance explained by each component
pr.var = pr.out$sdev^2

# Proportion of variance explained by each component
pve = pr.var/sum(pr.var)

# Plot the PVE explained by each component as well as the cumulative PVE
# Replication of FIGURE 10.4 in page 383
library(ggplot2)

# Left figure
d1 = data.frame(x = seq(1, 4, length = 4), y = pve)
g1 = ggplot(d1, aes(x = x, y = y)) + 
     geom_line(color = "blue") + 
     geom_point(shape = 1, color = "blue") + 
     labs(x = "Principal Component", y = "Prop. Variance Explained") + 
     scale_x_continuous(breaks = seq(1, 4, by = 0.5), limits = c(1, 4)) + 
     scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1))

# Right figure
d2 = data.frame(x = seq(1, 4, length = 4), y = cumsum(pve))
g2 = ggplot(d2, aes(x = x, y = y)) + 
     geom_line(color = "blue") + 
     geom_point(shape = 1, color = "blue") + 
     labs(x = "Principal Component", y = "Cumulative Prop. Variance Explained") + 
     scale_x_continuous(breaks = seq(1, 4, by = 0.5), limits = c(1, 4)) + 
     scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1))

# Combine two figures
library(patchwork)
g1 + g2 + plot_layout(ncol = 2)

#========================================================================#
# 10.5.1 K-Means Clustering
#========================================================================#

# Generate observations in which there truly are two clusters
set.seed(2)
x = matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4

# Perform K-means clustering with K = 2
km.out = kmeans(x, 2, nstart = 20)

# Cluster assignment
km.out$cluster

# Plot the data with each observation colored according to its cluster assignment
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K = 2", xlab = "", ylab = "", pch = 20, cex = 2)

# Perform K-means clustering with K = 3
set.seed(4)
km.out2 = kmeans(x, 3, nstart = 20)
km.out2
plot(x, col = (km.out2$cluster + 1), main = "K-Means Clustering Results with K = 3", xlab = "", ylab = "", pch = 20, cex = 2)

# Compare "nstart = 1" to "nstart = 20"
set.seed(3)
km.out3 = kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out4 = kmeans(x, 3, nstart = 20)
km.out4$tot.withinss

#========================================================================#
# 10.5.2 Hierarchical Clustering
#========================================================================#

# Perform hierarchical clustering with complete, average, and single linkage
# with Euclidean distance as the dissimilarity measure
hc.complete = hclust(dist(x), method = "complete")
hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")

# Plot the dedrograms
par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = 0.9)

# Determine the cluster labels for each observation associated with a given cut of the dendrogram
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

# Perform hierarchical clustering with complete linkage after scaling the variables
xsc = scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features")

# Perform hierarchical clustering with complete linkage using correlation-based distance measure
x2 = matrix(rnorm(30 * 3), ncol = 3)
dd = as.dist(1 - cor(t(x2)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation-based Distance", xlab = "", sub = "")

#========================================================================#
# 10.6 NCI60 Data Example
#========================================================================#
# NCI60 data consists of 6,830 gene expression measurements on 64 cancer cell lines

# Load "ISLR" library
library(ISLR)

# Cancer types and features
nci.labs = NCI60$labs
nci.data = NCI60$data

# Examine the cancer types for the cell lines
table(nci.labs)

#========================================================================#
# 10.6.1 PCA on the NCI60 Data
#========================================================================#

# Perform PCA after scaling the variables
pr.out = prcomp(nci.data, scale = TRUE)

# Create a function that assigns a distinct color to each element of a numeric vector
Cols = function(vec){
	cols = rainbow(length(unique(vec)))
	return(cols[as.numeric(as.factor(vec))])
}

# Plot the first few principal component score vectors
# Replication of FIGURE 10.15 in page 409
par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1, 3)], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z3")

# Summary of the PVE of the first few principal components
summary(pr.out)

# Plot the VE by the first few principal components
plot(pr.out)

# Replication of FIGURE 10.16 in page 410
#============================================

# Plot the PVE
pve = 100 * (pr.out$sdev^2 / sum(pr.out$sdev^2))
dd1 = data.frame(x = seq(1, 64, length = 64), y = pve)
g11 = ggplot(dd1, aes(x = x, y = y)) + 
     geom_line(color = "blue") + 
     geom_point(shape = 1, color = "blue") + 
     labs(x = "Principal Component", y = "PVE") + 
     scale_x_continuous(breaks = seq(0, 64, by = 10), limits = c(0, 64)) + 
     scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 12))

# Plot the cumulative PVE
dd2 = data.frame(x = seq(1, 64, length = 64), y = cumsum(pve))
g22 = ggplot(dd2, aes(x = x, y = y)) + 
     geom_line(color = "brown3") + 
     geom_point(shape = 1, color = "brown3") + 
     labs(x = "Principal Component", y = "Cumulative PVE") + 
     scale_x_continuous(breaks = seq(0, 64, by = 10), limits = c(0, 64)) + 
     scale_y_continuous(breaks = seq(20, 100, by = 20), limits = c(0, 100))

# Combine two figures
g11 + g22 + plot_layout(ncol = 2)

#========================================================================#
# 10.6.2 Clustering the observations of the NCI60 Data
#========================================================================#

# Standardize the variables to have mean zero and s.d one
sd.data = scale(nci.data)

# Compute the Euclidean distance matrix
data.dist = dist(sd.data)

# Perform hierarchical clustering with complete, single, and average linkage
# Euclidean distance is used as the dissimilarity measure
# Replication of FIGURE 10.17 in page 411
par(mfrow = c(1, 3))
plot(hclust(data.dist, method = "complete"), labels = nci.labs, main = "Complete Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single Linkage", xlab = "", sub = "", ylab = "")

# Cut the dendrogram from hierarchical clustering with complete linkage
# at the height that will yield four clusters
hc.out = hclust(data.dist, method = "complete")
hc.clusters = cutree(hc.out, 4)
table(hc.clusters, nci.labs)

# Plot the cut on the dendrogram that produces four clusters
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

# Compare the above results with K-means clustering with K = 4
set.seed(2)
km.out = kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)

# Perform hierarchical clustering on the first few principal component score vectors
hc.out = hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs, main = "Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)
