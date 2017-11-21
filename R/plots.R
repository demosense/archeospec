#' Plot all the signatures together using one color per sample.
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#'
#' @param signatures A spectral object built using the load_files function
#' @return A ggplot object
#'
#' @seealso \code{\link{load_files}}
#'
plot_signatures <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("signaturess parameter is not a spectral data collection")
  }

  data <- cbind(file = rownames(signatures$data), signatures$data)

  dataGathered <- tidyr::gather(data, 'wavelength', 'value', signatures$range)
  dataGathered$wavelength <- as.numeric(dataGathered$wavelength)

  ggplot2::ggplot(dataGathered, aes(x=wavelength, y=value, color=file)) +
    geom_line() +
    theme(legend.position='none')
}


#' Plot of the intracorrelation among the wavelengths of the signatures.
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 scale_fill_distiller
#' @importFrom ggplot2 scale_y_reverse
#' @param signatures A spectral object built using the load_files function
#' @return A ggplot object
#'
#' @seealso \code{\link{load_files}}
#'
plot_intracorrelation <- function(signatures) {

  correlation <- data.frame(cor(signatures$data))
  names(correlation) <- signatures$range
  correlation <- cbind(wl1=rownames(correlation), correlation)
  correlationsGathered <- tidyr::gather(correlation, 'wl2', 'corr', 2:length(correlation))

  correlationsGathered$wl1 <- as.numeric(as.character(correlationsGathered$wl1))
  correlationsGathered$wl2 <- as.numeric(as.character(correlationsGathered$wl2))

  ggplot2::ggplot(correlationsGathered, aes(wl1, wl2)) +
    geom_raster(aes(fill = corr)) +
    scale_fill_distiller(palette = "Spectral") +
    scale_y_reverse()
}


#' Plot the signatures coloured by its cluster asignment.
#' The function clustering must be run before of this one.
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 facet_wrap
#' @param signatures A spectral object built using the load_files function
#'
#' @seealso \code{\link{clustering}}
#' @seealso \code{\link{load_files}}
#'
plot_cluster <- function(signatures) {

  # Concat signatues and cluster data
  clusterNames <- names(signatures$clusters)
  data <- cbind(files = signatures$files, signatures$clusters, signatures$data)

  # Gather data by wavelength
  data_gathered <- data %>%
    tidyr::gather('wavelength', 'value', signatures$range)
  data_gathered$wavelength <- as.numeric(data_gathered$wavelength)

  # Generate a plot for each cluster
  lapply(
    clusterNames,
    function (k) {
      ggplot2::ggplot(data_gathered, aes(x=wavelength, y=value, group=files)) +
        geom_line(aes_string(color = k)) +
        facet_wrap(as.formula(paste0("~", k)), ncol = 2)
    }
  )
}

#' Plot the endmember signatures. The function set_endmembers must be run before of this one.
#'
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @param signatures A spectral object built using the load_files function
#'
#' @seealso \code{\link{set_endmembers}}
#' @seealso \code{\link{load_files}}
#'
plot_endmembers <- function(signatures) {

  data <- cbind(file = signatures$files, signatures$data)
  endmembers <- signatures$endmembers

  lapply(endmembers, function(ems) {
    k <- length(ems)
    emData <- data[ems,]
    endNames <- paste0("end", 1:k)
    emData <- cbind(endmember=endNames, emData)
    emDataGather <- tidyr::gather(emData, 'wavelength', 'value', signatures$range)
    emDataGather$wavelength <- as.numeric(emDataGather$wavelength)
    ggplot(emDataGather, aes(x=wavelength, y=value, group=file, color=endmember)) +
      geom_line()
  })
}





