# borealis

An R package for common activities in an integrative biology lab.

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

#### AIC.adonis

A simple function to calculate generalized [AIC](https://en.wikipedia.org/wiki/Akaike_information_criterion) for models created by [`vegan::adonis`](https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/adonis).

#### align.joint

Align a subset of landmarks by rotation from one point.
Joints can introduce nuisance variation in landmark-based geometric morphometrics.
This function rotates a subset of landmarks about a pivot point, such that all specimens
will align to a designated reference specimen.
This step should be run before Procrustes alignment, and it is robust to differences
in the relative position, orientation and size of specimens.

#### create.tps

Reformats X and Y coordinate positions from a spreadsheet into the `tps` ("thin-plate spline") file format defined by [Rohlf (2015)](https://doi.org/10.4404/hystrix-26.1-11264).

#### distance

A simple function to find the [Euclidean distance](https://en.wikipedia.org/wiki/Euclidean_distance) between two points in 2D space.

#### gg.scaling.plot

Make pretty plots of the relationship between two variables, tailored for use in examining
[allometric scaling](https://en.wikipedia.org/wiki/Allometry#Allometric_scaling) relationships.

#### ggGMMplot

This function is designed to make pretty plots from geometric morphometric ordinations using the output of `geomorph::gm.prcomp`.

#### id.metadata.to.gdf

Extracts metadata from the specimen IDs in an array of coordinate shape data, producing a `geomorph.data.frame` or regular `data.frame`.

#### landmark.plot

Plot the relative position of landmarks.

#### orient

Consistently orient multiple specimens in an array of coordinate shape values.

#### pcvar
 
A function that returns the proportion of variance explained by each axis in a principal component analysis (PCA).

#### procrustes.analysis

Generalized Prorustes alignment with interactive outlier detection and removal.
GPA is performed by a call to `geomorph::gpagen`. If specified, the function will display the results of
`geomorph::plotOutliers`, then warp grids for the most extreme shapes.
The user is prompted to select a cetain number of extreme shapes to exclude from
a repeated Procrustes alignment. This processes is iterative, until the user indicates that
it should stop or no shapes are requested for removal.

#### read.mmm

A function to read in linear multivariate morphometric (MMM) data from a `csv` or `xlsx` file.
The input spread sheet is assumed to be "long," in the sense that one column includes a list measurements made either by hand or in graphics software such as [ImageJ](https://imagej.net).
This organization is typically convenient for rapid data entry.
The function re-formats the multiple measurements into a list containing the data in a traditional "wide" tabular format, with each measurement in a separate column. It also returns several elements
describing the data and providing data provenance.

#### read.tps

Read shape data from a `tps` file, and extract any metadata embedded in the ID lines.

#### write.provenace

Data provenance from the `provenance` element of the input list will be written to file in [markdown](https://www.markdownguide.org/getting-started/) format.

### Molecular stuff

#### qPCR.plot

This function takes raw qPCR data and produces a convenient plot and table, which can be used to assess the data.


## Data

### Bombus.forewings

Preliminary shape data from bumblebee forewings. These data were imported from a
`tps` file using `read.tps` . (The file was created from raw data using
`create.tps`.) These data are very preliminary. They have not been curated and have not
undergone Procrustes alignment. The main purpose of these data are for trouble shooting
morphometric workflows.

### Bombus.tree

This phylogeny covers 26 bumblebee species (*Bombus*) focusing on those found in northeastern North America. The tree is based on sequence data from five genes, reported by [Cameron et al. (2007)](https://doi.org/10.1111/j.1095-8312.2007.00784.x). The taxa in that study were subset to the species included here. Sequences were realigned using ClustalW and the consensus tree was inferred using RAxML. The `phylo` list also includes data on taxonomy, node support, species names, and convenient short species codes.

### East1916

A dataset reporting the corolla lengths of *Nicotiana* flowers of different breeds, after hybridization, and after several generations of artificial selection for longer or shorter corolla lengths. These data originally appeared in [East (1916)](http://www.genetics.org/content/1/2/164/). 

This dataset is often used in genetics courses to present the effects of selection on quantitative traits. `East1916` organizes the data in a "tidy" long format. `East1916.wide` presents the data in a data frame that most closely resembles the original Table 1 from East (1916).





