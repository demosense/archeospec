#' Clustering algorithm
#'
#' Apply the clustering algorithm to group samples into k clusters
#'
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#' @param signatures A dataframe which contains the set of signatures. Each one contains the value for the wavelengths
#' @return An extended data frame which includes the k (cluster index) for each signature
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


elbow_withinss <- function(signatures, k = 1:20) {
  kclusts <- data.frame(k=1:15) %>% group_by(k) %>% do(kclust=kmeans(signatures$data, .$k))
  clusters <- kclusts %>% mutate( withinss = kclust$tot.withins)
  ggplot(clusters, aes(k, withinss)) + geom_line()
}
