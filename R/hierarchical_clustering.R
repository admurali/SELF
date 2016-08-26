#' Hierarchical Clustering
#'
#' @author Adithya Murali
#' @param data is the dataset containing the observations.
#' @param x is a reference to a column in the dataset that is could be the independent variable.
#' @param y is a reference to another column in the dataset that could be the dependent variable.
#' @param dmethod is the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#' @return Returns a list that contains values corresponding to the cluster number, and other details
#' Subsets given data using column names or number and performs hierarchical clustering on the subset data with error handling.
#' @examples print(HClust(iris, 'Sepal.Length', "Sepal.Width"))
#' @seealso \code{hclust}
#' @export
HClust <- function(data, x, y, dmethod="euclidean", p) {
  errorStep <- ''
  result <- tryCatch({
    error <- 'subsetting data'
    subsetdata <- data[, c(x, y)]
    error <- 'creating cluster'
    hclust(dist(subsetdata, method=dmethod))
  }, error = function(e) {
    print(paste('Error in ', error, sep = ""))
  })
}

#' Hierarchical Clustering Classification Table
#'
#' @author Adithya Murali
#' @param hclusters is a hierarchical cluster, which can be generated using the HClust function.
#' @param labels is a vector of values to classify the cluster data with.
#' @param nsplit is number of parts to split the hierarchical cluster into.
#' @return Returns a table that classifies all the provided label values based on cluster number with error handling.
#' @examples print(HClustTable(HClust(iris, 'Sepal.Length', "Sepal.Width"), iris$Species, 3))
#' @seealso \code{HClust}
#' @export
HClustTable <- function(hclusters, labels, nsplit=1) {
  errorStep <- ''
  result <- tryCatch({
    error <- 'splitting cluster'
    clusterCut <- cutree(hclusters, nsplit)
    error <- 'creating cluster table'
    table(clusterCut, labels)
  }, error = function(e) {
    print(paste('Error in ', error, sep = ""))
  })
}
