#' Plot signatures
#'
#' Plot all the signatures together using one color per sample.
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#'
#' @param signatures A spectral object built using the load_signature_files function
#' @return A ggplot2 object
#'
#' @seealso \code{\link{load_signature_files}}
#'
plot_signatures <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  data <- cbind(file = rownames(signatures$data), signatures$data)

  dataGathered <- tidyr::gather(data, 'wavelength', 'value', signatures$range)
  dataGathered$wavelength <- as.numeric(dataGathered$wavelength)

  ggplot2::ggplot(dataGathered, aes(x=wavelength, y=value, color=file)) +
    geom_line() +
    theme(legend.position='none')
}


#' Intercorrelation plot
#'
#' Plot of the intracorrelation among the wavelengths of the signatures.
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 scale_fill_distiller
#' @importFrom ggplot2 scale_y_reverse
#' @param signatures A spectral object built using the load_signature_files function
#' @return A ggplot2 object
#'
#' @seealso \code{\link{load_signature_files}}
#'
plot_intracorrelation <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  correlation <- data.frame(cor(signatures$data))
  names(correlation) <- signatures$range
  correlation <- cbind(wl1=rownames(correlation), correlation)
  correlationsGathered <- tidyr::gather(correlation, 'wl2', 'corr', 2:length(correlation))

  correlationsGathered$wl1 <- as.numeric(as.character(correlationsGathered$wl1))
  correlationsGathered$wl2 <- as.numeric(as.character(correlationsGathered$wl2))

  ggplot2::ggplot(correlationsGathered, aes(wl1, wl2)) +
    geom_raster(aes(fill = corr)) +
    scale_fill_distiller(palette = "Spectral")
    # scale_y_reverse()
}

#' Plot clustering
#'
#' Plot the signatures coloured by its cluster asignment.
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 scale_color_brewer
#' @importFrom ggplot2 scale_color_manual
#' @param signatures A spectral object built using the load_signature_files function.
#' The spectral object needs to be clustered (using clustering_kmeans or clustering_endmembers).
#' @return A ggplot2 object
#'
#' @seealso \code{\link{clustering_kmeans}}
#' @seealso \code{\link{clustering_endmembers}}
#' @seealso \code{\link{load_signature_files}}
#'
plot_clusters <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$clusters)) {
    stop("Error. Spectral data is not clustered")
  }

  # Concat signatues and cluster data
  clusterNames <- paste0("k", seq(signatures$k))
  data <- cbind(files = signatures$files, cluster=signatures$clusters, signatures$data)

  # Gather data by wavelength
  data_gathered <- data %>%
    tidyr::gather('wavelength', 'value', signatures$range)
  data_gathered$wavelength <- as.numeric(data_gathered$wavelength)

  ggplot2::ggplot(data_gathered, aes(x=wavelength, y=value, group=files, color=cluster)) +
    geom_line() +
    facet_wrap(~ cluster, ncol=3)
}

#' Mutual information plot
#'
#' Plot of the mutual information among wavelengths for all the signatures.
#'
#' @export
#' @import infotheo
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @param signatures A spectral object built using the load_signature_files function
#' @return A ggplot2 object
#'
#' @seealso \code{\link{load_signature_files}}
#'
plot_mutualinfo <- function(signatures, bins=10) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$clusters)) {
    stop("Error. Spectral data is not clustered")
  }

  computeMI <- function(x, y, bins) {
    discWave <- infotheo::discretize(x, disc = "equalwidth", nbins = bins)
    infotheo::condinformation(discWave, y)
  }

  data <- signatures$data
  range <- signatures$range
  clusters <- signatures$clusters

  mi <- sapply(range, function(x) computeMI(data[[x]], clusters, bins))
  dataMI = data.frame(wavelength=as.numeric(range), mi=mi)

  ggplot(dataMI, aes(x=wavelength, y=mi)) +
    geom_line()
}


#' Plot endmember signatures
#'
#' Plot the endmember signatures.
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_color_brewer
#' @importFrom ggplot2 scale_color_manual
#' @param signatures A spectral object built using the load_signature_files function.
#' The spectral object needs to be unmixed (using unmixing_fixed or unmixing_vca).
#' @return A ggplot2 object
#'
#' @seealso \code{\link{unmixing_fixed}}
#' @seealso \code{\link{unmixing_vca}}
#' @seealso \code{\link{load_signature_files}}
#'
plot_endmembers <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  data <- cbind(file = signatures$files, signatures$data)
  endmembers <- signatures$endmembers
  endNames <- signatures$endNames

  k <- length(endmembers)
  emData <- data[endmembers,]
  emData <- cbind(endmember=endNames, emData)
  emDataGather <- tidyr::gather(emData, 'wavelength', 'value', signatures$range)
  emDataGather$wavelength <- as.numeric(emDataGather$wavelength)

  plot <- ggplot(emDataGather, aes(x=wavelength, y=value, group=file, color=endmember)) +
    geom_line(size=1.2)

  # If signatures has assigned colors for endmembers apply them on the plot
  if(is.null(signatures$colors)) {
    plot <- plot + scale_color_brewer(palette = "Pastel1")
  } else {
    plot <- plot + scale_color_manual(values=signatures$colors)
  }

  plot
}

#' Plot endmembers coloured by clustering
#'
#' Plot the endmembers using the colors of the clusters that they belongs to.
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_color_brewer
#' @importFrom ggplot2 scale_color_manual
#' @param signatures A spectral object built using the load_signature_files function.
#' The spectral object needs to be clustered (using clustering_kmeans or clustering_endmembers)
#' and unmixed (using unmixing_fixed or unmixing_vca).
#' @return A ggplot2 object
#'
#' @seealso \code{\link{clustering_kmeans}}
#' @seealso \code{\link{clustering_endmembers}}
#' @seealso \code{\link{unmixing_fixed}}
#' @seealso \code{\link{unmixing_vca}}
#' @seealso \code{\link{load_signature_files}}
#'
plot_endmember_cluster <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$clusters)) {
    stop("Error. Spectral data is not clustered")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  clusters <- signatures$clusters
  endmembers <- signatures$endmembers
  endNames <- signatures$endNames

  data <- cbind(cluster = clusters, signatures$data)
  k <- length(endmembers)
  emData <- data[endmembers,]
  emData <- cbind(endmember=endNames, emData)
  emDataGather <- tidyr::gather(emData, 'wavelength', 'value', signatures$range)
  emDataGather$wavelength <- as.numeric(emDataGather$wavelength)

  plot <- ggplot2::ggplot(emDataGather, aes(x=wavelength, y=value, group=endmember, color=cluster)) +
    geom_line(size=1.2)

  # # If signatures has assigned colors for endmembers apply them on the plot
  # if(signatures$clustering=="endmembers" && !is.null(signatures$colors)) {
  #   plot <- plot + scale_color_manual(values=signatures$colors)
  # } else {
  #   plot <- plot + scale_color_brewer(palette = "Pastel1")
  # }

  plot
}

#' Plot of endmember density bar by clusters
#'
#' Plot the mean composition of each signature expressed as a combination of endmembers, grouped by clusters.
#'
#' The result is a set of bar plots
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 scale_fill_brewer
#' @importFrom ggplot2 scale_fill_manual
#' @param signatures A spectral object built using the load_signature_files function.
#' The spectral object needs to be clustered (using clustering_kmeans or clustering_endmembers)
#' and unmixed (using unmixing_fixed or unmixing_vca).
#' @return A ggplot2 object
#'
#' @seealso \code{\link{clustering_kmeans}}
#' @seealso \code{\link{clustering_endmembers}}
#' @seealso \code{\link{unmixing_fixed}}
#' @seealso \code{\link{unmixing_vca}}
#' @seealso \code{\link{load_signature_files}}
#'
plot_endmember_density_bar <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$clusters)) {
    stop("Error. Spectral data is not clustered")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  clusters <- signatures$clusters
  endmembers <- signatures$endmembers
  endNames <- signatures$endNames

  weightsRaw <- .compute_weights(signatures)

  plot <- cbind(file = signatures$files, cluster = clusters, weightsRaw) %>%
    tidyr::gather("endmember", "weight", endNames) %>%
    ggplot2::ggplot(aes(x=cluster, y=weight, group=endmember, fill=endmember, color=cluster)) +
      geom_bar(stat = "summary", fun.y = "mean", size=2.5)

  # If signartures has assigned colors for endmembers apply them on the plot
  if(is.null(signatures$colors)) {
    plot <- plot + scale_fill_brewer(palette = "Pastel1")
  } else {
    plot <- plot + scale_fill_manual(values=signatures$colors)
  }

  plot
}

#' Plot of endmember density box by clusters
#'
#' Plot the mean composition of each signature expressed as a combination of endmembers, grouped by clusters.
#'
#' The result is a set of whiskers box plots
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 scale_fill_brewer
#' @importFrom ggplot2 scale_fill_manual
#' @param signatures A spectral object built using the load_signature_files function.
#' The spectral object needs to be clustered (using clustering_kmeans or clustering_endmembers)
#' and unmixed (using unmixing_fixed or unmixing_vca).
#' @return A ggplot2 object
#'
#' @seealso \code{\link{clustering_kmeans}}
#' @seealso \code{\link{clustering_endmembers}}
#' @seealso \code{\link{unmixing_fixed}}
#' @seealso \code{\link{unmixing_vca}}
#' @seealso \code{\link{load_signature_files}}
#'
plot_endmember_density_box <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$clusters)) {
    stop("Error. Spectral data is not clustered")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  clusters <- signatures$clusters
  endmembers <- signatures$endmembers
  endNames <- signatures$endNames

  weightsRaw <- .compute_weights(signatures)

  plot <- cbind(file = signatures$files, cluster = clusters, weightsRaw) %>%
    tidyr::gather("endmember", "weight", endNames) %>%
    ggplot(aes(x=" ", y=weight, group=endmember, fill=endmember)) +
      geom_boxplot() +
      facet_wrap(~cluster, ncol = 3) +
      ylim(c(0,1))

  # If signartures has assigned colors for endmembers apply them on the plot
  if(is.null(signatures$colors)) {
    plot <- plot + scale_fill_brewer(palette = "Pastel1")
  } else {
    plot <- plot + scale_fill_manual(values=signatures$colors)
  }

  plot
}

#' Residuals plot
#'
#' Represents each signature as a weighted combination of endmembers.
#' Plot the residuals the residuals of this adjustment: the sum of the errors between the represented and the real values.
#'
#' The result is a histogram and a density plot of the residuals.
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ylim
#' @param signatures A spectral object built using the load_signature_files function.
#' The spectral object needs to be unmixed (using unmixing_fixed or unmixing_vca).
#' @return A ggplot2 object
#'
#' @seealso \code{\link{unmixing_fixed}}
#' @seealso \code{\link{unmixing_vca}}
#' @seealso \code{\link{load_signature_files}}
#'
plot_residuals <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("Error. Signatures parameter is not a spectral data collection")
  }

  if(is.null(signatures$clusters)) {
    stop("Error. Spectral data is not clustered")
  }

  if(is.null(signatures$endmembers)) {
    stop("Error. Spectral data is not unmixed")
  }

  clusters <- signatures$clusters
  endmembers <- signatures$endmembers
  endNames <- signatures$endNames

  data <- cbind(cluster = clusters, signatures$data)
  k <- length(endmembers)
  emData <- data[endmembers,]
  emData <- cbind(endmember=endNames, emData)

  weightsRaw <- .compute_weights(signatures)
  weights <- cbind(file = row.names(signatures$data), cluster = clusters, weightsRaw)

  residuals <- apply(cbind(signatures$data, weightsRaw), 1, FUN = function (x) {
    original <- x[ 1:length(signatures$range) ]
    weights  <- x[ (length(signatures$range)+1):length(x) ]
    .get_residuals(original, weights, emData[,signatures$range])
  })

  ggplot(data.frame(residuals), aes(x=residuals)) +
    geom_histogram(aes(y =..density..), fill="lightskyblue3", color="lightskyblue3", alpha=0.5, binwidth = 10) +
    geom_density(color="salmon", size=1)
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
#' @param signatures A spectral object built using the load_signature_files function
#' @param k List of values of k to use in the x-axis
#' @param selected The selected value for the report. If provided, it will be printed as a red point in this plot
#' @return A ggplot2 object
#'
#' @seealso \code{\link{load_signature_files}}
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
