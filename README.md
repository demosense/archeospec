# archeospec

Analysis of the reflectance spectra from paintings: classification and endmembers.

---

`archeospec` is a reporting tool for automation and reproducibility of research results. It encapsulates a particular analysis on the data gathered by spectometry sensors and automatically generates the research reports in different formats.


## Install from github via devtools

```R
# install.packages("devtools")
# archeospec depends on this non CRAN external dependency
devtools::install_github("jcozar87/unmixR", subdir = "pkg/unmixR")
# archeospec in currently only available trough github
devtools::install_github("jacintoarias/archeospec")
```

## Purpose

This package has been developed for managing high spectral resolution reflectance from paintings.

One objective is to locate the spectral regions with the maximum information contained in the spectral signature of all the pigments.

On the other hand, when not having previous information about the pigments, like chemical composition, the package allows to statistically discriminate between the optimal (minimum number of classes and maximum information) number of them.

It is also possible the alternative way, consisting of labelling some spectral signatures with previous information to stablish the main classes and pigments.

Finally, the package provides the contribution of the endmembers to each of the spectral signature in the samples.

## API Docs

Please refer to our comprehensive [documentation]() for a complete quickstart.

## Basic usage

We provide a direct and simple API to quickly generate report documents from data sources. It can be configured to reflect any combination of the mentioned study.

The following example generates a report, using VCA to compute 3 endmembers and kmeans to perform unsupervised classification.

```r
genReport(
  input_source="/path/to/asd_folder/",
  output="/tmp/archeospec/",
  format="html_document"
  endmembers=3,
  kmeans=T
  )
```

### Output formats

The following formats are available:

- `html_document` (and md)
- `pdf_document` (and LaTeX)
- `word_document`
- `all` (generating all previous document)

### Additional configuration

The reports may be configured to show/hide some of the plots and customize properties like colors and series names. Please refer to our [documentation]() for a comprehensive list of parameters.

## Advanced Usage

We provide an advanced API for a fine grained control of the different plots. tables and statistics. You can use it to generate your own documents via `rmarkdown` or to extend the results with additional analysis. Please refer to our [documentation]() and [examples]() to explore the package's capabilities.


## Authors

This work was carried out by the Remote Sensing Group in collaboration with the Intelligent Systems and Data Mining Research Group and the Laboratory of Archaeology, Heritage and Emerging Technologies in the University of Castilla-La Mancha. The Cueva Pintada Museum and the Cabildo of Gran Canaria have funded the work.
