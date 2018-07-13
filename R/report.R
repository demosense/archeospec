#' Create a report
#'
#' Create a report composed of elements specified in the parametrization.
#'
#' First load the data from input source. This must be an ABSOLUTE path to a folder which contains .asd files, or a tree of
#' folders which contains in the end .asd files. These files must contain the data as a pair wavelength<tab>value, and the
#' data must starts after the line file_header (by default "Wavelength<tab>%s").
#'
#' Then, a filtering process can be applied to the signatures if any of clean_head, clean_tail or clean_leaps are specified.
#' This consists, respectively, on cutting the lower / higher wavelength values. clean_leaps smooth the leaps produced when
#' using multiple sensors for different range of wavelengths, produced between sensor changes.
#'
#' Finally, the signatures are unmixed. If endmembers is a number, it computes such number of endmembers using the VCA algorithm.
#' If endmembers is a vector of filenames, then those signatures will be selected as endmembers. In this case, we can assign them a
#' representative name and a fixed color (using endmember_names and endmember_colors), so we can easily recognize this signatures
#' in the report. endmember_names is a vector of string names, and endmember_colors is a vector of string colors (check \code{\link{colors}}).
#'
#' Different outputs can optionaly be included in the report:
#' \itemize{
#' \item   kmeans: included (TRUE) or not (FALSE). If endmembers is a number, the centroids are chosen randomly. Otherwise, they will be the selected endmembers.
#' \item   intracorrelation: included (TRUE) or not (FALSE).
#' \item   mutualinfo: included (TRUE) or not (FALSE).
#' }
#'
#' The report will always include:
#' \itemize{
#' \item A brief description of the dataset (list of signatures)
#' \item The list of parameters used to create the report
#' \item The signature plot
#' \item The selected or computed endmembers, in tabular and plot formats
#' \item The elbow plot
#' \item The correspondence between endmembers and clusters plot
#' \item The endmember weights by cluster bar and whisker box plot
#' \item The residual values for each signature plot
#' \item The summary table for the signatures and its weights by endmembers
#' }
#'
#'
#' For more details see the doc in [GitHub](https://github.com/jacintoArias/archeospec/).
#'
#' @export
#' @import rmarkdown
#' @import knitr
#' @importFrom magrittr %>%
#' @param input_source The absolute path to a folder which contains .asd files (or subfolders with .asd files).
#' These files must contain the data as a pair wavelength<tab>value.
#' @param output The path for the output folder. It will contain the report files.
#' @param format The report formats to be generated. One option of "html_document", "pdf_document", "word" or "all" (by default).
#' The "pdf_document" will also include the latex sources.
#' @param title The title for the report, included at the begining of them.
#' @param endmembers It can be a number or a vector of strings. If it is a number, this number of endmembers will be computer from the signatures.
#' If it is a vector of strings, they will be the filenames of the endmembers which will be fixed.
#' @param kmeans Whether to include the clustering plot or not (TRUE / FALSE)
#' @param clean_head Optional. If provided, it filters out the wavelengths smaller than clean_head (a number).
#' @param clean_tail Optional. If provided, it filters out the wavelengths greater than clean_tail (a number)
#' @param clean_leaps Optional. If provided, it must be a vector of wavelength values. It will smooth the leaps produced between sensor changes,
#' when using multiple sensors for different wavelength ranges.
#' @param file_header The format of the hehad in .asd files. After the header, the measurements will be in format wavelength<tab>value.
#' By default, the file header is Wavelength\\\\t\%s (note the double backslash to escape the special character bakslash).
#' @param wavelength_start The smallest wavelength of the measured range.
#' @param wavelength_end The greatest wavelength of the measured range.
#' @param intracorrelation Whether to include the intracorrelation plot or not (TRUE / FALSE)
#' @param mutualinfo Whether to include the mutual information plot or not (TRUE / FALSE)
#' @param endmember_names Optional. If provided, it must be a list of names (strings) to be related to selected endmembers, so we
#' can easily recognise the endmembers in the report. If endmembers is not a list of filenames, this parameter is ignored.
#' @param endmember_colors Optional. If provided, it must be a list of colors (strings) to be related to selected endmembers, so we
#' can easily recognise the endmembers in the report. If endmembers is not a list of filenames, this parameter is ignored.
#' To check the available colors, refer to \code{\link{colors}}.
#' @return It creates a report in html, latex, pdf and / or word formats.
#'
#' @seealso \code{\link{load_signature_files}}
#'
#' @examples
#' \dontrun{
#' input_source <- "/path/to/asd/folder"
#' # generate a report computing three endmembers.
#' genReport(input_source, endmembers=3)
#'
#' # generate a report selecting two endmembers.
#' genReport(input_source, endmembers=c("almagre.asd.txt", "blanco.asd.txt"))
#'
#' # generate a report filtering the signatures and computing three endmembers.
#' genReport(input_source, clean_head=400, clean_tail=2400, clean_leaps=c(1001, 1831), endmembers=3)
#'
#' # generate a report filtering the signatures and selecting two endmembers, giving them a name and a color.
#' endmembers <- c("almagre.asd.txt", "blanco.asd.txt")
#' names <- c("red", "white")
#' colors <- c("red2", "white")
#' genReport(input_source, clean_head=400, clean_tail=2400, clean_leaps=c(1001, 1831),
#'           endmembers=endmembers, endmember_names=names, endmember_colors=colors)
#' }
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
  file_header="Wavelength\\t%s",
  wavelength_start=350,
  wavelength_end=2500,
  intracorrelation=T,
  mutualinfo=T,
  endmember_names=NULL,
  endmember_colors=NULL
) {

  ###
  ### Code Chunks

  #
  #  Libraries
  #
  #  Load R libraries
  #
  libs <- c(
    "library(archeospec)",
    "library(knitr)"
  )

  chunk_libs <- paste0(libs, collapse="\n")


  #
  # Loading
  #
  # Load spectral data
  #
  chunk_loading <- sprintf(
    'signatures <- load_signature_files(path = "%s", header="%s", wavelength_start=%s, wavelength_end=%s)',
    input_source,
    file_header,
    wavelength_start,
    wavelength_end
  )

  #
  # Smooth Leaps
  #
  # Smooth leaps conditioned on argument
  #
  chunk_cleaning_leaps <-
    if (!is.null(clean_leaps)) {
      leaps <- paste0(clean_leaps, collapse=",")
      sprintf("signatures <- smooth_leaps(signatures, leaps=c(%s))", leaps)
    } else {

      "# Clean Leaps not activated"
    }

  #
  # Clean head frequencies
  #
  # Conditioned on argument
  #
  chunk_clean_head <-
    if (!is.null(clean_head)) {
      sprintf("signatures <- remove_head(signatures, head=%s)", clean_head)
    } else {

      "# Clean head not activated"
    }

  #
  # Clean tail frequencies
  #
  # Conditioned on argument
  #
  chunk_clean_tail <-
    if (!is.null(clean_tail)) {
      sprintf("signatures <- remove_tail(signatures, tail=%s)", clean_tail)
    } else {

      "# Clean tail not activated"
    }

  #
  #  Unmixing
  #
  #  Perform unmixing, conditioned on argument 'endmembers'
  #
  #  if endmembers is 'vca' call function, otherwise pass on fixed lists
  #
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

  #
  # Clustering
  #
  # Conditioned on type of clustering
  #
  # Can be kmeans of fixed. The k argument in kmeans is computed from the endmembers arg
  #

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


  #
  #  Plots
  #

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

  #
  # Tables
  #

  chunk_table_endmembers <- "knitr::kable(table_endmembers(signatures), row.names=F)"
  chunk_table_residuals <- "knitr::kable(table_residuals_summary(signatures))"
  chunk_table_weights <- "knitr::kable(table_weights(signatures), row.names=F)"


  #
  # Texts
  #

s_unmix_tech <- if (class(endmembers) == "numeric") "Manually selected endmembers" else sprintf("VCA algorithm with %s endmembers", k)
s_kmeans_tech <- if (kmeans) sprintf("Kmeans algorithm with k=%s", k) else "Endmembers selected as centroids"
s_cleaning_head <- if(!is.null(clean_head)) sprintf("Noise reduction has removed wavelengths ranges until %s", clean_head) else "No wavelength ranges have been cleaned at the beggining of the signature"
s_cleaning_tail <- if(!is.null(clean_tail)) sprintf("Noise reduction has removed wavelengths ranges from %s", clean_tail) else "No wavelength ranges have been cleaned at the end of the signature"
s_cleaning_smooth <- if(!is.null(clean_leaps)) sprintf("Signatures have been smoothed on ranges %s", paste0(clean_leaps, collapse=",")) else "No smoothing has been performed"

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
The following figure shows the error of the kmeans algorithm for incresing values of k, measured as the distance within clusters. The red dot represent the selected k value.
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
    .add_chunk_md(text_clustering) %>%
    .add_chunk_anonymous("plotElbow", chunk_plot_elbow) %>%
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
