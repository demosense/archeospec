#' K-Means clustering
#'
#' Apply the k-means clustering algorithm to group samples into k clusters
#'
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#' @param signatures A spectral object built using the load_signature_files function
#' @param k K value to be used as number of clusters
#' @return An extended spectral object which includes the clustering indices of the signatures
#'
#' @seealso \code{\link{load_signature_files}}
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

#' Clustering fixing centroids to endmembers
#'
#' Assign an endmember to each signature based on its distance, similarly
#' to k-means clustering being fixed the centroids to the endmembers
#'
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#' @param signatures A spectral object built using the load_signature_files function.
#' #' The spectral object needs to be unmixed (using unmixing_fixed or unmixing_vca).
#' @return An extended spectral object which includes the clustering indices of the signatures
#'
#' @seealso \code{\link{unmixing_fixed}}
#' @seealso \code{\link{unmixing_vca}}
#' @seealso \code{\link{load_signature_files}}
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


.euclideanKmeans <- function(X, centroids) {
  distances <- apply(centroids, 1, FUN = function (x) sqrt(sum((x-X)**2)) )
  match( min(distances), distances)
}
