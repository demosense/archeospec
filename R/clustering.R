#' Clustering algorithm
#'
#' Apply the clustering algorithm to group samples into k clusters
#'
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#' @param signatures A spectral object built using the load_files function
#' @param k K value or list of k's to be used as number of clusters
#' @return An extended data frame which includes the k (cluster index) for each signature
#'
#' @seealso \code{\link{load_files}}
#'
clustering_kmeans <- function(signatures, k) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  signatures$clusters <-
    as.character(kmeans(x = signatures$data, k)$cluster) %>%
      data.frame() %>%
      setNames("cluster")

  signatures$clustering <- "kmeans"

  signatures
}

#'
#'
#' @export
#'
clustering_endmembers <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  data <- signatures$data
  endmembers <- signatures$endmembers
  dataEnd <- data[endmembers,]

  clusters <- apply(data, 1, FUN = function (x) .euclideanKmeans(x, dataEnd))

  signatures$clusters <- as.character(clusters)

  signatures$clustering <- "endmembers"

  signatures
}

#' Generate the elbow plot
#'
#' Plot of the sum of the inter-clusters distances (y-axis) for every number of clusters in k (x-axis)
#'
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @param signatures A spectral object built using the load_files function
#' @param k List of values of k to use in the x-axis
#' @return The plot of the sum of the distances intra-clusters (y-axis) for every number of clusters in k (x-axis)
#'
#' @seealso \code{\link{load_files}}
#'
plot_elbow <- function(signatures, k = 1:20, selected=NULL) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if (!selected %in% k) {
    stop("Error. Selected k is not within the range provided for elbow graph")
  }

  kclusts <- data.frame(k=k) %>% group_by(k) %>% do(kclust=kmeans(signatures$data, .$k))
  clusters <- kclusts %>% dplyr::mutate( withinss = kclust$tot.withins)

  plot <- ggplot(clusters, aes(k, withinss)) +
    geom_line()

  if (!is.null(selected))
    plot <- plot +
      geom_point(
        aes(x=c(selected), y=c(clusters[clusters$k==selected,]$withinss)),
        color="red",
        size=3
      )

  plot
}


.euclideanKmeans <- function(X, centroids) {
  distances <- apply(centroids, 1, FUN = function (x) sqrt(sum((x-X)**2)) )
  match( min(distances), distances)
}
