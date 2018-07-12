# archeospec
Analysis of the reflectance spectra from paintings: classification and endmembers.

## NOTE

This package is currently a **preview**, it is under desing so we can expect heavy changes on the API. Use it on your own risk and contact the authors if you have any requests.

## Install from github via devtools

```R
# install.packages("devtools")
devtools::install_git("https://gitlab.com/chemometrics/unmixR.git", subdir = "pkg/unmixR")
devtools::install_github("jacintoarias/archeospec")
```

## Purpose

This package has been developed for managing high spectral resolution reflectance from paintings.

One objective is to locate the spectral regions with the maximum information contained in the spectral signature of all the pigments.

On the other hand, when not having previous information about the pigments, like chemical composition, the package allows to statistically
discriminate between the optimal (minimum number of classes and maximum information) number of them.

It is also possible the alternative way, consisting of labelling some spectral signatures with previous information to stablish the main classes and pigments.

Finally, the package provides the contribution of the endmembers to each of the spectral signature in the samples.


## Basic usage

Example generating a report with all the information (default parametrization), using VCA to compute 3 endmembers

```r
genReport(
  input_source="/path/to/asd_folder/",
  output="/tmp/archeospec/",
  endmembers=3
  )
```

## Authors

It was carried out by the Remote Sensing Group in collaboration with the Intelligent Systems and Data Mining Research Group and the Laboratory of Archaeology,
Heritage and Emerging Technologies in the University of Castilla-La Mancha. The Cueva Pintada Museum and the Cabildo of Gran Canaria have funded the work.
