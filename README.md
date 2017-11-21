# archeospec
Analysis of the reflectance spectra from paintings: classification and endmembers.

## Install from github via devtools

```R
# install.packages("devtools")
devtools::install_github("jacintoarias/archeospec")
```

## Purpose

This package has been developed for managing high spectral resolution reflectance from paintings.
One objective is to locate the spectral regions with the maximum information contained in the spectral signature of all the pigments.
On the other hand, when not having previous information about the pigments, like chemical composition, the package allows to statistically
discriminate between the optimal (minimum number of classes and maximum information) number of them. It is also possible the alternative way,
consisting of labelling some spectral signatures with previous information to stablish the main classes and pigments. Finally, the package
provides the contribution of the endmembers to each of the spectral signature in the samples.

## Authors

It was carried out by the Remote Sensing Group in collaboration with the Intelligent Systems and Data Mining Research Group and the Laboratory of Archaeology,
Heritage and Emerging Technologies in the University of Castilla-La Mancha. The Cueva Pintada Museum and the Cabildo of Gran Canaria have funded the work.
