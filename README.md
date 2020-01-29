## Interpretive Clustering

[![Build Status](https://travis-ci.org/markheckmann/OpenRepGrid.ic.svg?branch=master)](https://travis-ci.org/markheckmann/OpenRepGrid.ic)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/markheckmann/OpenRepGrid.ic?branch=master&svg=true)](https://ci.appveyor.com/project/markheckmann/OpenRepGrid.ic)
[![Codecov test coverage](https://codecov.io/gh/markheckmann/OpenRepGrid.ic/branch/master/graph/badge.svg)](https://codecov.io/gh/markheckmann/OpenRepGrid.ic?branch=master)

This package accompanies the paper [Interpretive Clustering](#) by [Burr, King, and Heckmann (forthcoming)](#). The authors describe a variant of construct clustering which uses a procedure from graph theory called [maxmimal cliques enumeration](https://en.wikipedia.org/wiki/Clique_problem#Listing_all_maximal_cliques). Given a similarity measure, in our case the number of matching scores between two constructs, a network graph of relatedness between constructs is construed.
A clique is a group of constructs which are all mutually related, given some cut-off criterion for relatedness (e.g. 6 matching scores in a grid with 7 elements). While an offline approach is also described to find the construct cliques, this software automates the process. Below you see the resulting cliques for Sylvia's sample grid. In the paper, the interpretation of the discovered cliques is discussed in detail.
                       
![example](inst/shiny/www/sylvia_cliques.png "Construct cliques for Sylvias's grid")


## Installation

You can install tha latest official release version from CRAN

``` r
install.packages("OpenRepGrid.ic")
```

or the development version from GitHub

``` r
# install.packages("devtools")
devtools::install_github("markheckmann/OpenRepGrid.ic")
```
 
