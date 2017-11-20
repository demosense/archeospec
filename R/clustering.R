
#' @export
#' @import dplyr
#' @importFrom maggritr %>%
#'
clustering <- function(signature, k) {

  classes <- data.frame(cluster=as.character(kmeans(signature$data[signature$range], k)[[1]]))

  signature$data <- cbind(signature$data, classes) %>%
    dplyr::select(file, cluster, signature$range)

  signature
}
