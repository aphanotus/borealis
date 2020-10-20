# borealis

**Tools for Integrative Biology**

[GMM workflow template](https://www.bugsinourbackyard.org/wp-content/uploads/2020/10/gmm-template.html)

[GMM tutorial](https://www.bugsinourbackyard.org/wp-content/uploads/2020/10/gmm-tutorial.html)


## Installation

The package can be installed in an instance of R from Git Hub using the package `devtools`.

``` r
devtools::install_github("aphanotus/borealis")
library(borealis)
```

## Functions

### Morphometrics

One of the goals of this package is to provide a system to record [data provenance](https://en.wikipedia.org/wiki/Data_lineage#Data_provenance) in the objects
produced through a morphometry workflow. Many of the major GMM step employ functions from the R package [geomorph](https://cran.r-project.org/web/packages/geomorph/index.html).

For a [tutorial of a GMM workflow](https://www.bugsinourbackyard.org/wp-content/uploads/2020/10/gmm-tutorial.html) using `borealis`, see the vignettes [online](https://www.bugsinourbackyard.org/wp-content/uploads/2020/10/gmm-tutorial.html) or in Rstudio.

```r
vignette("gmm-tutorial", package="borealis")
vignette("gmm-template", package="borealis")
```

#### Other functions 

##### AIC.adonis

A simple function to calculate generalized [AIC](https://en.wikipedia.org/wiki/Akaike_information_criterion) for models created by [`vegan::adonis`](https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/adonis).

##### centroid.scaled.distances

Calculate the matrix of distances between landmarks in 2-D shape data, optionally scaled by centroid size.

##### pcvar
 
A function that returns the proportion of variance explained by each axis in a principal component analysis (PCA).

##### read.mmm

A function to read in linear multivariate morphometric (MMM) data from a `csv` or `xlsx` file.
The input spread sheet is assumed to be "long," in the sense that one column includes a list measurements made either by hand or in graphics software such as [ImageJ](https://imagej.net).
This organization is typically convenient for rapid data entry.
The function re-formats the multiple measurements into a list containing the data in a traditional "wide" tabular format, with each measurement in a separate column. It also returns several elements
describing the data and providing data provenance.


### Molecular stuff

#### qPCR.plot

This function takes raw qPCR data and produces a convenient plot and table, which can be used to assess the data.

![](https://i.imgur.com/wSiHqsD.png)

## Data

### Jadera

Shape data and associated metadata for soapberry bug specimens (*Jadera haematoloma*) from [Fawcett et al. 2018 *Nature Communications*](https://www.nature.com/articles/s41467-018-04102-1). `Jadera` contains data for bugs collected in the wild and raised under various environmental conditions in the lab. `JaderaRNAi` includes similar data for bugs subjected to juvenile stage RNA interference targeting *FoxO*. Both datasets include 42 landmarks from the dorsal side and linear metadata for the length of ventral appendages. Landmarks include the head, pronotum, mesonotum and one (upper) forewing.

![](https://i.imgur.com/ZCmMXav.png)

### Bombus.forewings

Preliminary shape data from bumblebee forewings. These data were imported from a `tps` file using `read.tps` . (The file was created from raw data using `create.tps`.) These data are very preliminary. They have not been curated and have not undergone Procrustes alignment. The main purpose of these data are for trouble shooting morphometric workflows.

![](https://i.imgur.com/QDTUaFK.jpg)

### Bombus.tree

This phylogeny covers 26 bumblebee species (*Bombus*) focusing on those found in northeastern North America. The tree is based on sequence data from five genes, reported by [Cameron et al. (2007)](https://doi.org/10.1111/j.1095-8312.2007.00784.x). The taxa in that study were subset to the species included here. Sequences were realigned using ClustalW and the consensus tree was inferred using RAxML. The `phylo` list also includes data on taxonomy, node support, species names, and convenient short species codes.

![](https://i.imgur.com/KjWXYHs.jpg)

### mantis

A simulated dataset of with 16 landmarks on the femur and tibia for 100 mantis specimens. Two species with male and female shape differences are included.

![](https://i.imgur.com/Dxk2DyJ.png)

### East1916

A dataset reporting the corolla lengths of *Nicotiana* flowers of different breeds, after hybridization, and after several generations of artificial selection for longer or shorter corolla lengths. These data originally appeared in [East (1916)](http://www.genetics.org/content/1/2/164/). 

This dataset is often used in genetics courses to present the effects of selection on quantitative traits. `East1916` organizes the data in a "tidy" long format. `East1916.wide` presents the data in a data frame that most closely resembles the original Table 1 from East (1916).

![](https://i.imgur.com/M55tSko.jpg)




