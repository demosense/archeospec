# Changelog

## v1.0.2 (26/07/2018)

### Bugfixes

* Added a global seed to report generation to ensure that each format outputs the same results

## v1.0.1 (25/07/2018)

### Bugfixes

* Some weights retrieved from the unmixing process showed numerical precision errors that could make values above 1 or below 0. These errors do not affect the numerical results but were removing particular data points from bounded boxplot graphs.

### Other changes

* Added information on how to configure tinytex

* Updated documentation in Readme, get started vignette and genReport function




