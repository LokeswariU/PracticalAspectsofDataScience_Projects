# LOKESWARI UMAKANTHAN (LXU190000)
#LAB ASSIGNMENT

#Load Required Libraries
require(jpeg)
require(RCurl)

#ReadImage URL from Public link
url <- "http://all4desktop.com/data_images/original/4186002-feathers-in-colors.jpg"
readImage <- readJPEG(getURLContent(url, binary=TRUE))

#Dimensions of the original Image
dim(readImage)

#Split the Image into RGB Components
dm <- dim(readImage)
rgbImage <- data.frame(x = rep(1:dm[2],each = dm[1]), y = rep(dm[1]:1,dm[2]), 
                       r.value = as.vector(readImage[,,1]),
                       g.value = as.vector(readImage[,,2]),
                       b.value = as.vector(readImage[,,3]))

#Top 6 Rows with RGB Component values
head(rgbImage)

#Save the plot of the original Image using JPEG function
jpeg("originalImage.jpg",width = 960, height = 960)
plot(y ~ x, data=rgbImage, main="Feathers",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]),
     asp = 1, pch = ".")
dev.off()

#------------------------K-Means---------------------------------#
#Dimensionality Reduction using K-Means

# Image when K = 5
jpeg("K_means_5.jpg",width = 960, height = 960)
kColors <- 5
kMeans_5 <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")],
                 centers = kColors)
clusterColour <- rgb(kMeans_5$centers[kMeans_5$cluster, ])
plot(y ~ x, data=rgbImage, main="Feathers",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="",
     xlab="k-means cluster analysis of 5 colours")
dev.off()

# Image when k=10
jpeg("K_means_10.jpg",width = 960, height = 960)
kColors <- 10
kMeans_10 <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")],
                 centers = kColors)
clusterColour <- rgb(kMeans_10$centers[kMeans_10$cluster, ])
plot(y ~ x, data=rgbImage, main="Feathers",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="",
     xlab="k-means cluster analysis of 10 colours")
dev.off()

#Image when K =50
jpeg("K_means_50.jpg",width = 960, height = 960)
kColors <- 50
kMeans_50 <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")],
                 centers = kColors)
clusterColour <- rgb(kMeans_50$centers[kMeans_50$cluster, ])
plot(y ~ x, data=rgbImage, main="Feathers",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="",
     xlab="k-means cluster analysis of 50 colours")
dev.off()

#Image when K = 100
jpeg("K_means_100.jpg",width = 960, height = 960)
kColors <- 100
kMeans_100 <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")],
                 centers = kColors)
clusterColour <- rgb(kMeans_100$centers[kMeans_100$cluster, ])
plot(y ~ x, data=rgbImage, main="Feathers",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="",
     xlab="k-means cluster analysis of 100 colours")
dev.off()

#Image when K = 200
jpeg("K_means_200.jpg",width = 960, height = 960)
kColors <- 200
kMeans_200 <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")],
                 centers = kColors)
clusterColour <- rgb(kMeans_200$centers[kMeans_200$cluster, ])
plot(y ~ x, data=rgbImage, main="Feathers",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="",
     xlab="k-means cluster analysis of 200 colours")
dev.off()

#Image when K = 500
jpeg("K_means_500.jpg",width = 960, height = 960)
kColors <- 500
kMeans_500 <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")],
                 centers = kColors)
clusterColour <- rgb(kMeans_500$centers[kMeans_500$cluster, ])
plot(y ~ x, data=rgbImage, main="Feathers",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="",
     xlab="k-means cluster analysis of 500 colours")
dev.off()

#Image when K = 900
jpeg("K_means_900.jpg",width = 960, height = 960)
kColors <- 900
kMeans_900 <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")],
                 centers = kColors)
clusterColour <- rgb(kMeans_900$centers[kMeans_900$cluster, ])
plot(y ~ x, data=rgbImage, main="Feathers",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="",
     xlab="k-means cluster analysis of 900 colours")
dev.off()

# Calculating the WCSS (Within Cluster Sum of Squares)
wcss <- c()
wcss[1] <- sum(kMeans_5$withinss)
wcss[2] <- sum(kMeans_10$withinss)
wcss[3] <- sum(kMeans_50$withinss)
wcss[4] <- sum(kMeans_100$withinss)
wcss[5] <- sum(kMeans_200$withinss)
wcss[6] <- sum(kMeans_500$withinss)
wcss[7] <- sum(kMeans_900$withinss)

# Plotting the WCSS against their respective number of clusters
plot(c(5,10,50,100,200,500,900), wcss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



#-------------------------------- PCA ----------------------------------# 
# Dimensionality Reduction using PCA

#Split the each RGB features of the Image
r <- readImage[,,1]
g <- readImage[,,2]
b <- readImage[,,3]

#Get the Principal Components for each RGB features
readImage.r.pca <- prcomp(r, center = FALSE)
readImage.g.pca <- prcomp(g, center = FALSE)
readImage.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(readImage.r.pca, readImage.g.pca, readImage.b.pca)

#Save teh compressed Images for different number of PCA Components
for (i in seq.int(3, round(nrow(readImage) - 10), length.out = 10)) {
        pca.img <- sapply(rgb.pca, function(j) {
                compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
        }, simplify = 'array')
        writeJPEG(pca.img, paste('feathers_compressed_', round(i,0), '_components.jpg', sep = ''))
}

# Getting the Compression ratio of each file or the Image
original <- file.info('originalImage.jpg')$size / 1000
imgs <- dir('~/')

for (i in imgs) {
        full.path <- paste('~/', i, sep='')
        print(paste(i, ' size: ', file.info(full.path)$size / 1000, ' original: ', original, ' % diff: ', round((file.info(full.path)$size / 1000 - original) / original, 2) * 100, '%', sep = ''))
}

