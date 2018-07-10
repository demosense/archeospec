#' Create a new report
#'
#' @export
#' @import rmarkdown
#' @import knitr
#' @param input_source A spectral object built using the load_files function or the path of the
#' folder which contains the .asd.txt files (which include the wavelength measurements)
#' @param path The output path where documents are saved. By default, "/tmp/spectral"
#' @param data_path The path for
#' @param title The title included at the begining of the reports
#' @param clusters The number of clusters to group the data
#' @param endmembers The number of endmembers to find in the data
#' @return It creates a report in html, latex, pdf and word formats
#'
#' @seealso \code{\link{load_files}}
#'
#'
genReport <- function(
  input_source,
  output,
  # format,
  endmembers,
  clustering,
  title="archeospec",
  clean_head=NULL,
  clean_tail=NULL,
  smooth_points=NULL,
  file_header=NULL,
  wavelength_start=NULL,
  wavelength_end=NULL,
  intracorrelation=T,
  mutualinfo=T
) {
  s <- sprintf("---
title: '%s'
subtitle: '%s'
output:
  html_document:
    toc: yes
    toc_depth: '2'
    keep_md: yes
  pdf_document:
    toc: yes
    toc_depth: '2'
    keep_tex: true
  word_document:
    toc: yes
    toc_depth: '2'
---
", title, date())

  ###
  ### Code
  s <- .add_chunk_anonymous(s,
    "library(archeospec)\n
    library(knitr)"
  )

  # Loading data
  s <- .add_chunk_anonymous(s,
    sprintf('signatures <- load_signature_files(path = "%s", header=%s, wavelength_start=%s, wavelength_end=%s)',
    input_source, file_header, wavelength_start, wavelength_end)
  )

  # cleaning
  if (!is.null(smooth_points))
    sprintf("signatures <- smooth_leaps(signatures, leaps=c(%s))", paste0(smooth_points, collapse=","))

  if (!is.null(clean_head))
    sprintf("signatures <- remove_head(signatures, head=%s)", clean_head)

  if (!is.null(clean_tail))
    sprintf("signatures <- remove_head(signatures, head=%s)", clean_tail)

  # Unmixing VCA
  # s <- .add_chunk_anonymous(s,
  #   sprintf('signatures <- unmixing_vca(signatures, k=%s)', endmembers)
  # )
  #
  # # Clustering Kmeans
  # s <- .add_chunk_anonymous(s,
  #   sprintf('signatures <- clustering_kmeans(signatures, k=%s)', endmembers)
  # )

  ###
  ### Document

  # Signatures plot
  s <- .add_chunk_anonymous(s, "plot_signatures(signatures)")


  # s <- .add_chunk_md(s, "## Detailed results for every measure and endmember")
  # s <- .add_chunk_md(s, "The following table shows the weights for every measure and each endmember, it also shows the corresponding class assigned by the clustering and the corresponding residual value of the adjustment.")
  # s <- .add_chunk_anonymous(s, sprintf('knitr::kable(table_weights(signatures))'))


  #
#   s <- .add_chunk_md(s, "## Signatures visualization")
#   s <- .add_chunk_md(s, "Each line correspond with a particular measure. The colors are random")
#   s <- .add_chunk_anonymous(s, sprintf('plot_signatures(signatures)'))
#
#   s <- .add_chunk_md(s, "## Intracorrelation analysis")
#   s <- .add_chunk_md(s, "Intracorrelation measured for each pairwise combination of the wavelength ranges")
#   s <- .add_chunk_anonymous(s, sprintf('plot_intracorrelation(signatures)'))
#
#   s <- .add_chunk_md(s, "## Analysis of the inter-cluster distances")
#   s <- .add_chunk_md(s, "To select the optimal number of k clusters we can identify the elbow of the inter cluster distance distribution for a wide range of k. Represented by the following plot.")
#   s <- .add_chunk_anonymous(s, sprintf('signatures %s elbow_withinss(k=elbow_range)',"%>%"))
#
#   s <- .add_chunk_md(s, "## Apply Clustering and VCA")
#   s <- .add_chunk_anonymous(s, sprintf('processed <- signatures %s clustering(k = %d) %s set_endmembers(k = %d)', "%>%", clusters, "%>%", endmembers))
#
#   s <- .add_chunk_md(s, sprintf("## Clustering k = %d", clusters))
#   s <- .add_chunk_md(s, "Representation of the different signatures grouped by cluster obtained via kmeans")
#   s <- .add_chunk_anonymous(s, sprintf('plot_cluster(processed)'))
#
#   s <- .add_chunk_md(s, sprintf("## Unmixing k = %d", endmembers))
#   s <- .add_chunk_md(s, "Results of the unmixing algorithm")
#   s <- .add_chunk_md(s, "The following measures have been identified as the most pure components")
#   s <- .add_chunk_anonymous(s, sprintf('endmember_files(processed)'))
#   s <- .add_chunk_anonymous(s, sprintf('plot_endmembers(processed)'))
#
#   s <- .add_chunk_md(s, "The following plot represent the classes assigned to each endmember for each particular endmember")
#   s <- .add_chunk_anonymous(s, sprintf('plot_endmember_cluster(processed)'))
#
#   s <- .add_chunk_md(s, "The following plot shows the distribution of weights for the measures of each cluster by endmember. The weights are obtained by adjusting each measure to the endmembers via nonegative restricted least squares")
#   s <- .add_chunk_anonymous(s, sprintf('plot_endmember_density_bar(processed)'))
#   s <- .add_chunk_anonymous(s, sprintf('plot_endmember_density_box(processed)'))
#
#   # s <- .add_chunk_md(s, "Dstribution of the residual components obtained from the adjustment to the endmembers for the whole dataset.")
#   # s <- .add_chunk_anonymous(s, sprintf('kable(table_residuals(processed))'))
#
#   s <- .add_chunk_md(s, "## Detailed results for every measure and endmember")
#   s <- .add_chunk_md(s, "The following table shows the weights for every measure and each endmember, it also shows the corresponding class assigned by the clustering and the corresponding residual value of the adjustment.")
#   s <- .add_chunk_anonymous(s, sprintf('kable(table_weights(processed), digits=3)'))
#
#   s <- .add_chunk_md(s, "## Summary of files involved in the report")
#   s <- .add_chunk_anonymous(s, sprintf('signatures$files'))

  # tmpPath <- tempfile(pattern = "file", tmpdir = tempdir())
  fileConn<-file(paste0(output, "/report.rmd"))
  writeLines(c(s), fileConn)
  close(fileConn)
  rmarkdown::render(paste0(output, "/report.rmd"), output_dir=output, output_format="all", clean=FALSE)
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
