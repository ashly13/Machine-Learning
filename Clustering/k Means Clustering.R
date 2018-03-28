# My implementation of k-Means Clustering

data = data.frame(X1 = c(1, 1.5, 3, 5, 3.5, 4.5, 3.5), 
                  X2 = c(1, 2, 4, 7, 5, 5, 4.5))

# Function to find the Euclidean Distance
euclidean_distance <- function(p1, p2) {
  dist = 0
  
  for ( i in 1:ncol(p1) ){
    dist = dist + ( p1[, i] - p2[, i] )^2
  }
  
  return(sqrt(dist))
  
}

# Number of clusters
k = 3

# Clusters
clusters = list()
length(clusters) = k

for ( i in 1:k ){
  clusters[[i]] = data.frame(X1 = numeric(), X2 = numeric())
}

# Initial Centroids
# Choose the first k points as the initial centroids
centroids = data[1:k, ]

cat(c("\n\nThe Initial Clusters : \n"))
print(clusters)

# Iterations
iter = 1
while ( TRUE ){
  cat(c("\n\n----------Iteration ", iter, "\n\n"))
  
  # For every point, find the distance to each centroid
  for ( i in 1:nrow(data) ){
    distances = list()
    length(distances) = k
    for ( cluster in 1:length(clusters) ){
      distances[[cluster]] = euclidean_distance(data[i, ], centroids[cluster, ])
    }
    
    c = which.min(as.numeric(distances))
    
    # Add the point to that cluster
    clusters[[c]] = rbind(clusters[[c]], data.frame(X1 = data$X1[i], X2 = data$X2[i]))
    
    '
    cat(c("\n\nDistances : \n"))
    print(distances)
    cat(c("\nThe Correct Cluster = ", c))
    '
 
  }
  
  # Calculate new Centroids
  newCentroids = data.frame(X1 = numeric(), X2 = numeric())
  for ( cluster in 1:length(clusters) ){
    newCentroids[cluster, ] = data.frame(X1 = mean(clusters[[cluster]]$X1), X2 = mean(clusters[[cluster]]$X2))
  }
  
  cat(c("\n\nNew Centroids : \n"))
  print(newCentroids)
  
  cat(c("\n\nOld Centroids : \n"))
  print(centroids)
  
  cat(c("\n\nClusters : \n"))
  print(clusters)
  
  # Check whether algorithm has to continue
  if ( all(newCentroids == centroids) ){
    break
  }
  else{
    centroids = newCentroids
    for ( i in 1:k ){
      clusters[[i]] = data.frame(X1 = numeric(), X2 = numeric())
    }
  }  
  
  iter = iter + 1
  cat(c("\nPress ENTER to continue to next iteration."))
  readline()
}
