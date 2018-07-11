#' Create a new report
#'
#' @export
#' @import rmarkdown
#' @import knitr
#' @importFrom magrittr %>%
#'
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
  format="all",
  endmembers,
  kmeans=T,
  title="archeospec",
  clean_head=NULL,
  clean_tail=NULL,
  clean_leaps=NULL,
  file_header="Wavelength\t%s",
  wavelength_start=350,
  wavelength_end=2500,
  intracorrelation=T,
  mutualinfo=T,
  endmember_names=NULL,
  endmember_colors=NULL
) {

  ###
  ### Code Chunks

  #'
  #'  Libraries
  #'
  #'  Load R libraries
  #'
  libs <- c(
    "library(archeospec)",
    "library(knitr)"
  )

  chunk_libs <- paste0(libs, collapse="\n")


  #'
  #' Loading
  #'
  #' Load spectral data
  #'
  chunk_loading <- sprintf(
    'signatures <- load_signature_files(path = "%s", header="%s", wavelength_start=%s, wavelength_end=%s)',
    input_source,
    file_header,
    wavelength_start,
    wavelength_end
  )

  #'
  #' Smooth Leaps
  #'
  #' Smooth leaps conditioned on argument
  #'
  chunk_cleaning_leaps <-
    if (!is.null(clean_leaps)) {
      leaps <- paste0(clean_leaps, collapse=",")
      sprintf("signatures <- smooth_leaps(signatures, leaps=c(%s))", leaps)
    } else {

      "# Clean Leaps not activated"
    }

  #'
  #' Clean head frequencies
  #'
  #' Conditioned on argument
  #'
  chunk_clean_head <-
    if (!is.null(clean_head)) {
      sprintf("signatures <- remove_head(signatures, head=%s)", clean_head)
    } else {

      "# Clean head not activated"
    }

  #'
  #' Clean tail frequencies
  #'
  #' Conditioned on argument
  #'
  chunk_clean_tail <-
    if (!is.null(clean_tail)) {
      sprintf("signatures <- remove_tail(signatures, tail=%s)", clean_tail)
    } else {

      "# Clean tail not activated"
    }

  #'
  #'  Unmixing
  #'
  #'  Perform unmixing, conditioned on argument 'endmembers'
  #'
  #'  if endmembers is 'vca' call function, otherwise pass on fixed lists
  #'
  chunk_unmixing <-
  if (class(endmembers) == "numeric") {
    # Perform VCA
    sprintf('signatures <- unmixing_vca(signatures, k=%s)', endmembers)

  } else {
    # Create lists of files, names and colors if available
    endmember_list <- paste0('"', endmembers, '"', collapse=",")
    endmember_names_list <- paste0('"', endmember_names, '"', collapse=",")
    endmember_colors_list <- paste0('"', endmember_colors, '"', collapse=",")
    # Set endmembers
    sprintf(
      'signatures <- unmixing_fixed(signatures, files=c(%s), names=c(%s), colors=c(%s))',
      endmember_list,
      endmember_names_list,
      endmember_colors_list
    )
  }

  #'
  #' Clustering
  #'
  #' Conditioned on type of clustering
  #'
  #' Can be kmeans of fixed. The k argument in kmeans is computed from the endmembers arg
  #'

  # k is set as the number of endmembers, that can be a number or a list
  k <- if (class(endmembers) == "numeric") endmembers else length(endmembers)

  chunk_clustering <-
    if (kmeans) {
      # Perform kemans
      sprintf('signatures <- clustering_kmeans(signatures, k=%s)', k)

    } else {
      # Fixed clustering do not requires parameters
      'signatures <- clustering_endmembers(signatures)'
    }


  #'
  #'  Plots
  #'

  chunk_plot_signatures <- "plot_signatures(signatures)"

  chunk_plot_intracorrelation <-
    if (intracorrelation) {
      "plot_intracorrelation(signatures)"
    } else {
      "# Intracorrelation not activated"
    }

  chunk_plot_endmembers <- "plot_endmembers(signatures)"

  chunk_plot_elbow <-
    if (kmeans) {
      sprintf("plot_elbow(signatures, k=2:%s, selected=%s)", k+2, k)
    } else {
      "# Elbow not activated for fixed clustering"
    }


  chunk_plot_clusters <- "plot_clusters(signatures)"

  chunk_plot_mutual_info <-
    if (intracorrelation) {
      "plot_mutualinfo(signatures)"
    } else {
      "# Mutual info not activated"
    }

  chunk_plot_endmbember_clusters <- "plot_endmember_cluster(signatures)"

  chunk_plot_density_bar <- "plot_endmember_density_bar(signatures)"

  chunk_plot_density_box <- "plot_endmember_density_box(signatures)"

  chunk_plot_residuals <- "plot_residuals(signatures)"

  #'
  #' Tables
  #'

  chunk_table_endmembers <- "knitr::kable(table_endmembers(signatures), row.names=F)"
  chunk_table_residuals <- "knitr::kable(table_residuals_summary(signatures))"
  chunk_table_weights <- "knitr::kable(table_weights(signatures), row.names=F)"


  #'
  #' Texts
  #'

s_unmix_tech <- if (class(endmembers) == "numeric") "Manually selected endmembers" else sprintf("VCA algorithm with %s endmembers", k)
s_kmeans_tech <- if (kmeans) sprintf("Kmeans algorithm with k=%s", k) else "Endmembers selected as centroids"
s_cleaning_head <- if(clean_head) sprintf("Noise reduction has removed wavelengths ranges until %s", clean_head) else "No wavelength ranges have been cleaned at the beggining of the signature"
s_cleaning_tail <- if(clean_tail) sprintf("Noise reduction has removed wavelengths ranges from %s", clean_tail) else "No wavelength ranges have been cleaned at the end of the signature"
s_cleaning_smooth <- if(clean_leaps) sprintf("Signatures have been smoothed on ranges %s", paste0(clean_leaps, collapse=",")) else "No smoothing has been performed"

  text_introduction <- sprintf(
"
## Dataset
This report includes the signatures for `r length(signatures$files)` files. A completed listing can be found at the end of the document.

### Parameters
The following parameters have been used:

* Unmixing Technique: %s
* Clustering Technique: %s
* %s
* %s
* %s

\\newpage

## Visualization
The following figure shows the representation of every signature in the dataset. Colors are assigned at random.
", s_unmix_tech, s_kmeans_tech, s_cleaning_head, s_cleaning_tail, s_cleaning_smooth)


  text_intracorrelation <-
    if (intracorrelation) {
"
\\newpage
## Intracorrelation
The following figure measures the intracorrelation for each pair of wavelengths.
"
    } else {
""
    }


  text_unmixing <-
    if (class(endmembers) == "numeric") {
      sprintf(
"
\\newpage
## Unmixing (VCA)
The following table and figure show the %s endmembers selected by the VCA algorithm.
",
      k)
    } else {
      sprintf(
"
\\newpage
## Unmixing (manual)
The following table and figure show the %s endmembers selected manually from the available samples.
",
      k)
    }


  text_clustering <-
    if (kmeans) {
      sprintf(
"
\\newpage
## Clustering (kmeans)
The following figure shows the error of the kmeans algorithm for incresing values of k, measured as the distance withing clusters. The red dot represent the selected k value.
The next figure shows the graphical representation of the dataset classified by the corresponding cluster assigned by kmeans.
",
        k)
    } else {
      sprintf(
"
\\newpage
## Clustering (manual)
The next figure shows the graphical representation of the dataset classified by the corresponding cluster assigned according to the specified centroids (corresponding with the endmembers).
",
        k)
    }

  text_mutualinfo <-
    if (mutualinfo) {
"
\\newpage
### Mutual Information
The following figure shows the mutual information for each wavelength and the assigned cluster as the averaged value for all the samples in the dataset.
"
    } else {
      ""
    }

  text_endmember_clusters <-
"
\\newpage
### Endmember classification
The next figure shows the correspondence between endmembers and clusters, note that kmeans can assign more than one ensemble to the same cluster, leaving some clusters unrepresented.
"

  text_density <-
"
\\newpage
## Endmember weights by cluster
The next two figures show the average mixture for each cluster among all endmembers. In the barplot Each column represents a single cluster and averages the weights of the contained signatures. The different colored segments of the bar represent the weight proportion for each endmember.
The boxplot is analogous, with each box representing the distribution of weight for a particular endmember, separated in an individual plot for each cluster.
"

  text_residuals <-
"
\\newpage
## Residual values
The following table and figure show the distribution of the residual component from the the unmixing process. The distribution includes all endmembers and signatures in the dataset.
"

  text_summary <-
"
\\newpage
## Summary table
The following table contains the summary of the whole experiment, showing the assigned cluster, distribution of weights and residuals for each signature in the dataset.
"

  text_about <-
"
## About archeospec

This report has been generated automatically using the
[archeospec](https://github.com/jacintoArias/archeospec) package version `r packageVersion('archeospec')`.
"


  ###
  ### Document

  preamble <- sprintf(
"---
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
",
    title,
    date()
  )

  document <- preamble %>%
    .add_chunk_anonymous("libraries", chunk_libs) %>%
    .add_chunk_anonymous("load", chunk_loading) %>%
    .add_chunk_anonymous("cleanLeaps", chunk_cleaning_leaps) %>%
    .add_chunk_anonymous("cleanHead", chunk_clean_head) %>%
    .add_chunk_anonymous("cleanTail", chunk_clean_tail) %>%
    .add_chunk_anonymous("unmixing", chunk_unmixing) %>%
    .add_chunk_anonymous("clustering", chunk_clustering) %>%
    .add_chunk_md(text_introduction) %>%
    .add_chunk_anonymous("plotSignatures", chunk_plot_signatures) %>%
    .add_chunk_md(text_intracorrelation) %>%
    .add_chunk_anonymous("plotIntracorrelation", chunk_plot_intracorrelation) %>%
    .add_chunk_md(text_unmixing) %>%
    .add_chunk_anonymous("tableEndmembers", chunk_table_endmembers) %>%
    .add_chunk_anonymous("plotEndmembers", chunk_plot_endmembers) %>%
    .add_chunk_anonymous("plotElbow", chunk_plot_elbow) %>%
    .add_chunk_md(text_clustering) %>%
    .add_chunk_anonymous("plotClusters", chunk_plot_clusters) %>%
    .add_chunk_md(text_mutualinfo) %>%
    .add_chunk_anonymous("plotMutualinfo", chunk_plot_mutual_info) %>%
    .add_chunk_md(text_endmember_clusters) %>%
    .add_chunk_anonymous("plotEndmbemberClusters", chunk_plot_endmbember_clusters) %>%
    .add_chunk_md(text_density) %>%
    .add_chunk_anonymous("plotDensityBar", chunk_plot_density_bar) %>%
    .add_chunk_anonymous("plotDensityBox", chunk_plot_density_box) %>%
    .add_chunk_md(text_residuals) %>%
    .add_chunk_anonymous("tableResiduals", chunk_table_residuals) %>%
    .add_chunk_anonymous("plotResiduals", chunk_plot_residuals) %>%
    .add_chunk_md(text_summary) %>%
    .add_chunk_anonymous("tableWeights", chunk_table_weights) %>%
    .add_chunk_md(text_about)


  dir.create(file.path(output), showWarnings = FALSE)
  fileConn<-file(paste0(output, "/report.rmd"))
  writeLines(c(document), fileConn)
  close(fileConn)
  rmarkdown::render(paste0(output, "/report.rmd"), output_dir=output, output_format=format, clean=FALSE)
}

.add_chunk_md <- function(s, text) {
  template <-"\n%s\n\n"
  chunk <- sprintf(template, text)
  paste(s, chunk, sep="\n")
}

.add_chunk_anonymous <- function(s, name, code) {
  template <-"```{r %s, echo=FALSE, warning=FALSE}\n%s\n```\n\n"
  chunk <- sprintf(template, name, code)
  paste(s, chunk, sep="\n")
}
