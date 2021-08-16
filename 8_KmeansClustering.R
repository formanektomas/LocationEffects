#####################################################################
# We continue with lists of estimated models for each (58) product category -- models[[ii]]
# 
# Export all estimates from models to matrix
estimates = matrix(0,58,11)
for (ii in 1:58) { 
  estimates[ii,] = t(summary(models[[ii]])$coefficients[,1])
}
# Intercept is not of our interest for clustering, so we remove it, also we scale the remaining estimated coefficients such that the mean is zero and the variance is equal to one
estimates = scale(estimates[,-1])

#####################################################################
# We run the kmeans algorithm for values of K > 1 up to maxK parameter (for example 20)
library(clusterSim) #to compute DB statistics
maxK = 20
clusterings = list()
DB_stat = rep(0,maxK)
between = rep(0,maxK)
for (K in seq(2,maxK)){
  clusterings[[K]] = kmeans(estimates, K, iter.max = 500, nstart = 50, trace=FALSE)
  # between clusters variance is computed
  between[K] = clusterings[[K]]$betweenss/clusterings[[K]]$totss
  # also Davies-Bouldin statistics is computed
  DB_stat[K] = index.DB(estimates, clusterings[[K]]$cluster, d=NULL, centrotypes="centroids")$DB 
}

# Show DB statistics and the proportion of between cluster variance to total variance
plot(between, type = "l") #looking for a break
plot(DB_stat, type = "l") #the lower the value the better
# Keep in mind future usage -- is it efficient to work with a large number of clusters? Isn't a smaller number of clusters enough?

#####################################################################
# Choose "optimal" value of K, for example 6; further we will work with 6 clusters
K = 6
# The assigment of product category to cluster is 
cluster = clusterings[[K]]$cluster


