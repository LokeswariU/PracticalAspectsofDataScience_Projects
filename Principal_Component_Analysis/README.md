1. Open the Rstudio IDE or any other R IDE to run the R file.
2. You Can retrieve the Image from the following public URL: http://all4desktop.com/data_images/original/4186002-feathers-in-colors.jpg
3. Pre-install the libraries like RCurl for clustering, jpeg for image retrieval needed for running the R file.
4. Open the R file in th Rstudio and run in the PCA.R file available.
5. The dimentionality of the image has been reduced based on two strategies : K-means clustering and Principal Component Analysis
6. The K-means clustering makes the colors in the image as clusters and form the best image by forming more cluters. The best image is formed when k = 100 and this is proved by the K-means clustering Elbow method estimation. The higher the K-value underfits the model and increases the run time.
7. The second strategy is the principal component analysis, in this the components/features of the image are identified. Initially less number of components are identified as the component increase we get a better resolution of images. With 708 principal components we get an image similar to original image. Increase in components than 708 compresses the image size.
The image similar to the original image is also analysed based on the compression ratio obtained in two strategies.
