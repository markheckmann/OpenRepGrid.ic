## Interpretive Clustering

[![](https://www.r-pkg.org/badges/version/OpenRepGrid.ic?color=success)](https://cran.r-project.org/package=OpenRepGrid.ic)
[![Build Status](https://travis-ci.org/markheckmann/OpenRepGrid.ic.svg?branch=master)](https://travis-ci.org/markheckmann/OpenRepGrid.ic)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/markheckmann/OpenRepGrid.ic?branch=master&svg=true)](https://ci.appveyor.com/project/markheckmann/OpenRepGrid.ic)
[![Codecov test coverage](https://codecov.io/gh/markheckmann/OpenRepGrid.ic/branch/master/graph/badge.svg)](https://codecov.io/gh/markheckmann/OpenRepGrid.ic?branch=master)


This package implements the [Interpretive Clustering (IC)](https://doi.org/10.1080/14780887.2020.1794088) 
method as described in [Burr, King, and Heckmann (2020)](https://doi.org/10.1080/14780887.2020.1794088).
An introduction to the software is available on [YouTube](https://youtu.be/f9oINYA22Rc).
Interpretive Clustering is a variant of construct clustering for [repertory grid](https://en.wikipedia.org/wiki/Repertory_grid)
data. While derived from theoretical considerations based on [Personal Construct Theory](https://en.wikipedia.org/wiki/Personal_construct_theory), the procedure itself is mathematically equivalent to a problem from graph theory called [maximal cliques enumeration](https://en.wikipedia.org/wiki/Clique_problem#Listing_all_maximal_cliques). Given a similarity measure, in our case the number of matching scores between two constructs, a network graph of relatedness between constructs is construed. A clique is a group of constructs which are all mutually related, given some cut-off criterion for relatedness (e.g. 6 matching scores in a grid with 7 elements). While an offline approach is also described to find the construct cliques, this software automates the process. Below you see the resulting cliques for Sylvia's sample grid. In the paper, the interpretation of the discovered cliques is discussed in detail.
                       
![example](inst/shiny/www/sylvia_cliques.png "Construct cliques for Sylvias's grid")


## Installation

You can install the latest official release version from CRAN

``` r
install.packages("OpenRepGrid.ic")
```

or the development version from GitHub

``` r
# install.packages("devtools")
devtools::install_github("markheckmann/OpenRepGrid.ic")
```

Then type `ic()` to start the shiny app.


## Datasets

You can find the datasets used in our publication on Zenodo

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3629868.svg)](https://doi.org/10.5281/zenodo.3629868)

