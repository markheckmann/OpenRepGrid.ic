## Interpretive Clustering

This package accompanies the paper [Interpretive clustering](#) by [Burr, King, Heckmann and Jankowicz (2020)](#). The authors describe a variant of construct clustering which uses a procedure from graph theory called [maxmimal cliques enumeration](https://en.wikipedia.org/wiki/Clique_problem#Listing_all_maximal_cliques). Given a similarity measure, in our case the number of matching scores between two constructs, a network graph of relatedness between constructs is construed.
A clique is a group of constructs in this network which are mutually related to each other, given some cut-off criterion for relatedness (e.g. 6 matching scores). While an offline approach is also described to find the construct cliques, the shiny app automates the process. Below you see the resulting construct cliques for Sylvia's sample grid as discussed in our paper.

![example](inst/shiny/www/ic_logo.png "Construct cliques for Sylvias's grid")

