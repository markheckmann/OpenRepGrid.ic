# Interpretive Clustering

<!-- badges: start -->
[![](https://www.r-pkg.org/badges/version/OpenRepGrid.ic?color=success)](https://cran.r-project.org/package=OpenRepGrid.ic)
[![Codecov test coverage](https://codecov.io/gh/markheckmann/OpenRepGrid.ic/branch/master/graph/badge.svg)](https://codecov.io/gh/markheckmann/OpenRepGrid.ic?branch=master)
[![R-CMD-check](https://github.com/markheckmann/OpenRepGrid.ic/workflows/R-CMD-check/badge.svg)](https://github.com/markheckmann/OpenRepGrid.ic/actions)
<!-- badges: end -->


This package implements the [Interpretive Clustering (IC)](https://doi.org/10.1080/14780887.2020.1794088) 
method as described in [Burr, King, and Heckmann (2020)](https://doi.org/10.1080/14780887.2020.1794088).
An introduction to the software is available on [YouTube](https://youtu.be/f9oINYA22Rc).
Interpretive Clustering is a variant of construct clustering for [repertory grid](https://en.wikipedia.org/wiki/Repertory_grid)
data. While derived from theoretical considerations based on [Personal Construct Theory](https://en.wikipedia.org/wiki/Personal_construct_theory), the procedure itself is mathematically equivalent to a problem from graph theory called [maximal cliques enumeration](https://en.wikipedia.org/wiki/Clique_problem#Listing_all_maximal_cliques). 
Given a similarity measure, in our case the number of matching scores between
two constructs, a network graph of relatedness between constructs is construed.
A clique is a group of constructs which are all mutually related, given some
cut-off criterion for relatedness (e.g. 6 matching scores in a grid with 7
elements). While an offline approach is also described to find the construct
cliques, this software automates the process. Below you see the resulting
cliques for Sylvia's sample grid. In the paper, the interpretation of the
discovered cliques is discussed in detail.
                       
![example](inst/shiny/www/sylvia_cliques.png "Construct cliques for Sylvias's grid")


## Statement of need 

Currently, the IC method is not implemented in any other existing repertory grid
software. While IC can also be conducted by hand, this is very time consuming,
error-prone and only feasible for small-sized grids. Hence, a software solution
to support the IC procedure is needed. Without proper software support which
facilitates the process, the IC method is likely to become a methodological
contribution which will rarely be used in research due to above mentioned
reasons.



## Installation

You can install the latest official release version from CRAN

``` r
install.packages("OpenRepGrid.ic")
```

or the development version from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("markheckmann/OpenRepGrid.ic")
```

Then type the following to start the shiny app.

```r
library(OpenRepGrid.ic)
ic()
``` 


## Datasets

You can find the datasets used in our publication on Zenodo.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3629868.svg)](https://doi.org/10.5281/zenodo.3629868)


## Contributing

In order to maximize the package's usefulness for the research community, we
welcome participation in the package's development. Experienced R programmers
are asked to make pull requests to the [`OpenRepGrid.ic` github repository](https://github.com/markheckmann/OpenRepGrid.ic), [report issues](https://github.com/markheckmann/OpenRepGrid.ic/issues), or commit code
snippets. Non-technical oriented researchers are invited to send us feature
requests or suggestions for improvement.

