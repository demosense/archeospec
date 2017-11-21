#' Create a new report
#'
#' @export
#' @param signatures A dataframe which contains the set of samples. Each sample represents the value for each wavelength
#' @param path The output path where documents are saved. By default, "/tmp/spectral"
#' @param data_path The path for the folder which contains the .asd.txt files (which include the wavelength measurements)
#' @param title The title included at the begining of the reports
#' @param kclusters The number of clusters to group the data
#' @param kendmembers The number of endmembers to find in the data
#' @return It creates a report in html, latex, pdf and word formats
#'
#'
#'
genReport <- function(
  signatures,
  path="/tmp/spectral",
  data_path,

  title,
  kclusters,
  kendmembers
) {
  s <- sprintf("---
title: '%s'
subtitle: '%s'
html_document:
  toc: yes
  toc_depth: '2'
---
", title, date())


  s <- .add_chunk_anonymous(s, "library(spectral)")
  s <- .add_chunk_anonymous(s, sprintf('data <- load_files(path = "%s")', data_path))
  s <- .add_chunk_anonymous(s, sprintf('data'))
  s <- .add_chunk_md(s, "## Dataset visualization")
  s <- .add_chunk_anonymous(s, sprintf('plot_signatures(data)'))
  s <- .add_chunk_md(s, "## Intracorrelation analysis")
  s <- .add_chunk_anonymous(s, sprintf('plot_intracorrelation(data)'))
  s <- .add_chunk_md(s, sprintf("## Clustering k = %d", kclusters))
  s <- .add_chunk_anonymous(s, sprintf('dataClustered <- clustering(data, %d)', kclusters))
  s <- .add_chunk_anonymous(s, sprintf('plot_cluster(dataClustered)'))
  s <- .add_chunk_md(s, sprintf("## Unmixing k = %d", kendmembers))
  s <- .add_chunk_anonymous(s, sprintf('dataUnmixed <- set_endmembers(dataClustered, %d, 1234)', kendmembers))
  s <- .add_chunk_anonymous(s, sprintf('endmember_files(dataUnmixed)'))
  s <- .add_chunk_anonymous(s, sprintf('plot_endmembers(dataUnmixed)'))
  s <- .add_chunk_md(s, sprintf("## Files included"))
  s <- .add_chunk_anonymous(s, sprintf('data$files'))


#   "plot_signatures(data)"
# plot_intracorrelation(data)
#
# dataClustered <- clustering(data, 5)
# plot_cluster(dataClustered)
#
# dataUnmixed <- set_endmembers(dataClustered, 5, 1234)
# endmember_files(dataUnmixed)
# plot_endmembers(dataUnmixed)

  # s <- paste(s, , sep="\n")

  # tmpPath <- tempfile(pattern = "file", tmpdir = tempdir())
  fileConn<-file(paste0(path, "/report.rmd"))
  writeLines(c(s), fileConn)
  close(fileConn)
  rmarkdown::render(paste0(path, "/report.rmd"), output_dir=path, output_format="all")
}

.add_chunk_md <- function(s, text) {
  template <-"\n%s\n\n"
  chunk <- sprintf(template, text)
  paste(s, chunk, sep="\n")
}

.add_chunk_anonymous <- function(s, code) {
  template <-"```{r, echo=FALSE, warning=FALSE}\n%s\n```\n\n"
  chunk <- sprintf(template, code)
  paste(s, chunk, sep="\n")
}
