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
clustering <- function(signatures, k) {

  # Apply kmeans for each class
  signatures$clusters <-
    sapply(
      k,
      function (ki) as.character(kmeans(x = signatures$data, ki)$cluster)
    ) %>%
    data.frame() %>%
    setNames(paste0('k', k))

  signatures
}


#' Generate the elbow plot
#'
#' Plot of the sum of the distances intra-clusters (y-axis) for every number of clusters in k (x-axis)
#'
#' @export
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @param signatures A spectral object built using the load_files function
#' @param k List of values of k to use in the x-axis
#' @return The plot of the sum of the distances intra-clusters (y-axis) for every number of clusters in k (x-axis)
#'
#' @seealso \code{\link{load_files}}
#'
elbow_withinss <- function(signatures, k = 1:20) {
  kclusts <- data.frame(k=1:15) %>% group_by(k) %>% do(kclust=kmeans(signatures$data, .$k))
  clusters <- kclusts %>% mutate( withinss = kclust$tot.withins)
  ggplot(clusters, aes(k, withinss)) + geom_line()
}
