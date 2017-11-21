#' Clustering algorithm
#'
#' Apply the clustering algorithm to group samples into k clusters
#'
#' @export
#' @import dplyr
#' @importFrom maggritr %>%
#' @param signatures A dataframe which contains the set of signatures. Each one contains the value for the wavelengths
#' @return An extended data frame which includes the k (cluster index) for each signature
#'
clustering <- function(signatures, k) {

  classes <- data.frame(cluster=as.character(kmeans(signatures$data[signatures$range], k)[[1]]))

  signatures$data <- cbind(signatures$data, classes) %>%
    dplyr::select(file, cluster, signatures$range)

  signatures
}
