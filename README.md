
# labelColoR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Scalable visualization of groups within a hierarchical clustering from
binary data matrix.

## Installation

You can install the development version of labelcolor like so:

``` r
install.packages("devtools")
library("devtools")
devtools::install_github("NoeDemange/labelcolor")
```

## Example

This is a basic example which shows you how to run the app:

``` r
library(labelcolor)
#> Registered S3 method overwritten by 'seriation':
#>   method         from 
#>   reorder.hclust gclus
#> Warning: remplacement de l'importation précédente 'ape::degree' par
#> 'circlize::degree' lors du chargement de 'labelcolor'
#> Warning: remplacement de l'importation précédente 'ape::rotate' par
#> 'dendextend::rotate' lors du chargement de 'labelcolor'
#> Warning: remplacement de l'importation précédente 'ape::ladderize' par
#> 'dendextend::ladderize' lors du chargement de 'labelcolor'
#> Warning: remplacement de l'importation précédente 'dendextend::cutree' par
#> 'stats::cutree' lors du chargement de 'labelcolor'
labelcolor::run_app(options=list("launch.browser"=TRUE))
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>

## Credits

This app was developed by [Noe Demange](https://github.com/NoeDemange).
It was developed for and is maintained by [Guillaume
Sapriel](https://orcid.org/0000-0003-0549-9376). It is deployed on the
[MIGALE platform](https://migale.inrae.fr/) by [Cédric
Midoux](https://orcid.org/0000-0002-7964-0929). We are grateful to the
INRAE MIGALE bioinformatics facility (MIGALE, INRAE, 2020. Migale
bioinformatics Facility, doi: 10.15454/1.5572390655343293E12) for
providing help and storage resources.
