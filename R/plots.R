
#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#'
plot_signatures <- function(signatures) {

  if(!is.spectral(signatures)) {
    stop("signaturess parameter is not a spectral data collection")
  }

  dataGathered <- tidyr::gather(signatures$data, 'wavelength', 'value', signatures$range)
  dataGathered$wavelength <- as.numeric(dataGathered$wavelength)

  ggplot2::ggplot(dataGathered, aes(x=wavelength, y=value, color=file)) +
    geom_line() +
    theme(legend.position='none')
}


#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 scale_fill_distiller
#' @importFrom ggplot2 scale_y_reverse
#'
plot_intracorrelation <- function(signatures) {

  correlation <- data.frame(cor(signatures$data[,-1]))
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

#' @export
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom geom_line
#' @importFrom facet_wrap
#'
plot_cluster <- function(signatures) {

  data_gathered <- signatures$data %>%
    tidyr::gather('wavelength', 'value', signatures$range)

  data_gathered$wavelength <- as.numeric(data_gathered$wavelength)

  ggplot2::ggplot(data_gathered, aes(x=wavelength, y=value, group=file, color=cluster)) +
    geom_line() +
    facet_wrap(~cluster, ncol = 2)
}


plot_endmembers <- function(signatures) {

  ems <- signatures$endmembers
  k <- length(ems)

  emData <- signatures$data[ems,]
  endNames <- paste0("end", 1:k)
  emData <- cbind(endmember=endNames, emData)

  # We are asumming column order id, waves 350:2500, ks
  emDataGather <- tidyr::gather(emData, 'wavelength', 'value', signatures$range)
  emDataGather$wavelength <- as.numeric(emDataGather$wavelength)

  ggplot(emDataGather, aes(x=wavelength, y=value, group=file, color=endmember)) +
    geom_line()

}





