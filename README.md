# borealis

This package gathers functions developed for commons activities in R 
for use in a biology research lab. 

## Installation

You can install the released version of borealis from [CRAN](https://CRAN.R-project.org) with:

``` r
library(devtools)
install_github("aphanotus/borealis")
```

## Functions

### create.tps

Reformats X and Y coordinate positions from a spreadsheet into the `tps` ("thin-plate spline") file format defined by [Rohlf (2015)](https://doi.org/10.4404/hystrix-26.1-11264).

### qPCR.plot

This function takes raw qPCR data and produces a convenient plot and table, which can be used to assess the data.

## Data

### East1916

A dataset reporting the corolla lengths of *Nicotiana* flowers of different breeds, after hybridization, and after several generations of artificial selection for longer or shorter corolla lengths. These data originally appeared in [East (1916)](http://www.genetics.org/content/1/2/164/). 

This dataset is often used in genetics courses to present the effects of selection on quantitative traits. `East1916` organizes the data in a "tidy" long format. `East1916.wide` presents the data in a dateframe that most closely resembles the original Table 1 from East (1916).

### Bombus.tree

This phylogeny covers 26 bumblebee species (*Bombus*) focusing on those found in northeastern North America. The tree is based on sequence data from five genes, reported by [Cameron et al. (2007)](https://doi.org/10.1111/j.1095-8312.2007.00784.x). The taxa in that study were subset to the species included here. Sequences were realigned using ClustalW and the consensus tree was inferred using RAxML. The `phylo` list also includes data on taxonomy, node support, species names, and convenient short species codes.




