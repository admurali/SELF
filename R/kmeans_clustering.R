#' K-Means Clustering
#'
#' @author Adithya Murali
#' @param data is the dataset containing the observations.
#' @param x is a reference to a column in the dataset that is could be the independent variable.
#' @param y is a reference to another column in the dataset that could be the dependent variable.
#' @param cluster is the number of clusters to perform k-means operation on.
#' @param rstart is how many random starting cluster assignments to try before choosing the one with the lowest within cluster variation
#' @return Returns a list that contains values corresponding to the cluster number, and other details.
#' @details
#' Subsets given data using column names or number and performs k-means clustering on the subset data with error handling.
#' @examples print(KmeansClustList(iris, 'Sepal.Length', "Sepal.Width", 3))
#' @seealso \code{kmeans}
#' @export
KmeansClust <- function(data, x, y, cluster, rstart = 1) {
  errorStep <- ''
  result <- tryCatch({
    error <- 'subsetting data'
    subsetdata <- data[, c(x, y)]
    error <- 'getting kmeans for provided cluster value and any random starting cluster assignment value'
    kmeans(subsetdata, cluster, nstart = rstart)
  }, error = function(e) {
    print(paste('Error in ', error, sep = ""))
  })
}

#' K-Means Clustering Classification Table
#'
#' @author Adithya Murali
#' @param kclusters is a k-means cluster, which can be generated using the KmeansClust function.
#' @param labels is a vector of values to classify the cluster data with.
#' @return Returns a table that classifies all the provided label values based on cluster number with error handling.
#' @examples print(KmeansClustTable(KmeansClust(iris, 'Sepal.Length', "Sepal.Width", 3), iris$Species))
#' @seealso \code{KmeansClust}
#' @export
KmeansClustTable <- function(kclusters, labels) {
  errorStep <- ''
  result <- tryCatch({
    error <- 'creating cluster table'
    table(kclusters$cluster, labels)
  }, error = function(e) {
    print(paste('Error in ', error, sep = ""))
  })
}
