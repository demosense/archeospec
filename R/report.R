#' Create a new report
#'
#' @export
#' @import rmarkdown
#' @param input_source A spectral object built using the load_files function or the path of the
#' folder which contains the .asd.txt files (which include the wavelength measurements)
#' @param path The output path where documents are saved. By default, "/tmp/spectral"
#' @param data_path The path for
#' @param title The title included at the begining of the reports
#' @param kclusters The number of clusters to group the data
#' @param kendmembers The number of endmembers to find in the data
#' @return It creates a report in html, latex, pdf and word formats
#'
#' @seealso \code{\link{load_files}}
#'
#'
genReport <- function(
  input_source,
  path="/tmp/spectral",

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


  s <- .add_chunk_anonymous(s, "library(archeospec)")

  if(is.spectral(input_source))
    s <- .add_chunk_anonymous(s, 'signatures <- input_source')
  else
    s <- .add_chunk_anonymous(s, sprintf('signatures <- load_files(path = "%s")', input_source))

  s <- .add_chunk_md(s, "## Source of signatures")
  s <- .add_chunk_anonymous(s, sprintf('signatures'))

  s <- .add_chunk_md(s, "## Signatures visualization")
  s <- .add_chunk_anonymous(s, sprintf('plot_signatures(signatures)'))

  s <- .add_chunk_md(s, "## Intracorrelation analysis")
  s <- .add_chunk_anonymous(s, sprintf('plot_intracorrelation(signatures)'))

  s <- .add_chunk_md(s, "## Analysis of the inter-cluster distances")
  s <- .add_chunk_anonymous(s, sprintf('signatures %s elbow_withinss()',"%>%"))

  s <- .add_chunk_md(s, "## Apply Clustering and VCA")
  s <- .add_chunk_anonymous(s, sprintf('signatures %s elbow_withinss()',"%>%"))

  s <- .add_chunk_anonymous(s, sprintf('processed <- signatures %s clustering(k = %d) %s set_endmembers(k = %d)', "%>%", kclusters, "%>%", kendmembers))

  s <- .add_chunk_md(s, sprintf("## Clustering k = %d", kclusters))
  s <- .add_chunk_anonymous(s, sprintf('plot_cluster(processed)'))

  s <- .add_chunk_md(s, sprintf("## Unmixing k = %d", kendmembers))
  s <- .add_chunk_anonymous(s, sprintf('endmember_files(processed)'))
  s <- .add_chunk_anonymous(s, sprintf('plot_endmembers(processed)'))

  s <- .add_chunk_anonymous(s, sprintf('plot_endmember_cluster(processed)'))

  s <- .add_chunk_anonymous(s, sprintf('plot_endmember_density_bar(processed)'))

  s <- .add_chunk_anonymous(s, sprintf('plot_endmember_density_box(processed)'))

  s <- .add_chunk_anonymous(s, sprintf('table_weights(processed)'))

  s <- .add_chunk_anonymous(s, sprintf('table_residuals(processed)'))

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
