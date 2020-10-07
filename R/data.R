#' @title Quantative Genetics Data from East 1916
#'
#' @description A dataset reporting the corolla lengths of \emph{Nicotiana} flowers of different
#'     breeds, after hybridization, and after several generations of artificial selection
#'     for longer or shorter corolla lengths.
#'
#' @usage East1916
#' East1916.wide
#'
#' @details
#'     These data originally appeared in East (1916). This daatset is often used in genetics courses
#'     to present the effects of selection on quantitative traits.
#'
#'     \code{East1916} organizes the data in a "tidy" long format. \code{East1916.wide} presents the data in
#'     a dateframe that most closely resembles the original Table 1 from East (1916).
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut]
#'
#' @references East, E. 1916. Studies on Size Inheritance in \emph{Nicotiana}. \emph{Genetics} 1(2): 164-176
#'     \url{http://www.genetics.org/content/1/2/164/}
#'
#' @examples
#' library(tidyverse)
#'
#' line.info <- data.frame(
#'   shortdes = c('F2','F2','F3-3','F3-3'),
#'   xintercept = c(81,69.88,69.88,76.34),
#'   style = c('dotted','dotted','solid','dotted')
#' )
#'
#' East1916 %>% filter(grepl(') 2',designation)) %>%
#'   mutate(shortdes = paste0(generation,sub("No. \\(383 X 330\\) 2","",designation))) %>%
#'   filter(shortdes %in% c('F2','F3-3')) %>%
#'   ggplot(aes(x=corolla_mm, y=count, fill=shortdes)) +
#'   theme_bw() +
#'   theme(strip.text = element_blank(), legend.position="none") +
#'   facet_grid(shortdes~., ) +
#'   geom_bar(stat='identity') +
#'   scale_fill_manual(values = c("#7FD34E", "#C2DF23")) +
#'   geom_vline(data=line.info, aes(xintercept=xintercept, linetype=style), color = 'gray35', size = 1) +
#'   labs(x='corolla length (mm)')
#'
#' ggsave('East1916.parent.offspring.RS.example.vertical.pdf',
#'        width = 4, height = 5, scale = 1)
#'
"East1916"

#' @title Quantative Genetics Data from East 1916
#'
#' @description A dataset reporting the corolla lengths of \emph{Nicotiana} flowers of different
#'     breeds, after hybridization, and after several generations of artificial selection
#'     for longer or shorter corolla lengths.
#'
#' @usage East1916
#' East1916.wide
#'
#' @details
#'     These data originally appeared in East (1916). This daatset is often used in genetics courses
#'     to present the effects of selection on quantitative traits.
#'
#'     \code{East1916} organizes the data in a "tidy" long format. \code{East1916.wide} presents the data in
#'     a dateframe that most closely resembles the original Table 1 from East (1916).
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut]
#'
#' @references East, E. 1916. Studies on Size Inheritance in \emph{Nicotiana}. \emph{Genetics} 1(2): 164-176
#'     \url{http://www.genetics.org/content/1/2/164/}
#'
"East1916.wide"

#' @title A phylogeny of bumblebee species
#'
#' @description This phylogeny covers 26 bumblebee species (\emph{Bombus})
#'     focusing on those found in northeastern North America.
#'
#' @details This tree is based on sequence data from five genes, reported by Cameron et al. (2007).
#'     The taxa in that study were subset to the species included here.
#'     Sequences were realigned using ClustalW and the consensus tree was inferred using
#'     RAxML. The \code{phylo} list also includes data on taxonomy, node support, species names,
#'     and convenient short specioes codes.
#'
#' \describe{
#'   \item{\code{edge}}{A matrix encoding tree topology.}
#'   \item{\code{Nnode}}{The total number of nodes in the tree.}
#'   \item{\code{tip.label}}{A character vector with tip labels utilized by \code{phytools} plotting functions. These are species names with genus abbreviations.}
#'   \item{\code{edge.length}}{A numerical vector of branch lengths.}
#'   \item{\code{node.label}}{A character vector of clade names.}
#'   \item{\code{node.support}}{A character vector of bootstrap support values for each node.}
#'   \item{\code{code.name}}{A character vector with short codes for species names.}
#'   \item{\code{species.name}}{A character vector with full species names.}
#' }
#'
#' For more information on manipulating phylogenies in R, refer to the \href{https://cran.r-project.org/web/packages/phytools/phytools.pdf}{phytools documentation}.
#'
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut]
#'
#' @references
#' Cameron SA, Hines HM, and Williams PH. 2007. A comprehensive phylogeny of the bumble bees (\emph{Bombus}). \emph{Biological Journal of the Linnean Society} 91: 161-188. \url{https://doi.org/10.1111/j.1095-8312.2007.00784.x}
#' @references
#' Revell LJ. 2011. phytools: an R package for phylogenetic comparative biology (and other things). \emph{Methods in Ecology and Evolution} 3(2): 2041-210X. \url{https://doi.org/10.1111/j.2041-210X.2011.00169.x}
#'
#' @examples
#' # Load the dataset
#' data(Bombus.tree)
#'
#' # # Plot the tree
#' # library("phytools")
#' # plot(Bombus.tree)
#'
#' # # Use phytools to add extra features to the tree
#' # nodelabels(Bombus.tree$node.label, adj=c(1,-0.2), frame="none", cex = 0.8)
#'
#' # # The tree can be copied and modified
#' # btree <- Bombus.tree
#'
#' # # For example, the shorter code names can be used as tip labels
#' # btree$tip.label <- btree$code.name
#' # plot(btree)
#'
#' # # The tree can be pruned to keep only common species
#' # common.species <- c("ferv","bor","tern","imp","bimac","prep","sande","vag","terri","rufo","gris")
#' # btree <- keep.tip(btree, common.species)
#' # plot(btree)
#'
"Bombus.tree"

#' @title Bumblebee forewing shape data
#'
#' @description Preliminary shape data from bumblebee forewings. These data were imported from a
#'     \code{tps} file using \code{\link{read.tps}} . The \code{tps} file was created from raw data using
#'     \code{\link{create.tps}}. These data are very preliminary. They have not been curated and have not
#'     undergone Procrustes alginment. The main purpose of these data are for trouble shooting
#'     morphometric worlkflows.
#'
#' @details
#' \describe{
#'   \item{\code{coords}}{An array of XY coordinates for the shape of multiple specimens (20 landmarks by 2 (XY) by 99 specimens).}
#'   \item{\code{tps.header}}{The tps file header, retained for provenance.}
#'   \item{\code{landmark.number}}{The number of landmarks in the shape data.}
#'   \item{\code{specimen.number}}{The number of specimens in the shape data.}
#' }
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut]
#' @source   Ye Jin "Jane" Lee \email{yjlee21@@colby.edu} [cre]
#'
#' @examples
#' # Load the dataset
#' data(Bombus.forewings)
#'
#' landmark.plot(Bombus.forewings)
#'
#' cat(Bombus.forewings$tps.header)
#'
"Bombus.forewings"

#' @title Pixel-to-metric distance conversions for the dissecting scope in Arey 301A
#'
#' @description Ideally, micrographs are taken with a ruler or other scale in the image.
#'     However, if that's not the case, metric distances can be infered from pixel distances
#'     based on this conversion table. These data are based on measurements made
#'     from a high-quality stage micrometer on the VWR VistaVision dissecting microscope
#'     in Arey 301A at Colby College. Digital photomicrographs were taken with a
#'     Moticam 5 digital camera. Values apply to the camera's high resolution (2592 × 1944)
#'     setting. However, a value for pixels per mm are also included for the
#'     low resolution (1296 × 972) setting. (Try to always save images at high resolution!)
#'
#' @details
#' \describe{
#'   \item{\code{mag}}{Total magnification}
#'   \item{\code{setting}}{Zoom setting}
#'   \item{\code{umpx}}{Microns per pixel}
#'   \item{\code{umpx_sd}}{Standard deviation for the micron/pixel values, based on triplicate measurements}
#'   \item{\code{pxmm}}{Pixels per millimeter at high resolution}
#'   \item{\code{pxmm_lores}}{Pixels per millimeter at low resolution}
#' }
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @examples
#' # Load the dataset
#' data("Arey301A.scope", package = "borealis")
#'
#' Arey301A.scope
#'
"Arey301A.scope"

#' @title Simulated dataset of Mantis forelimb shapes
#'
#' @description A simulated dataset of with 16 landmarks on the femur and tibia for 100 Mantis specimens.
#'     Two species with male and female shape differences are included.
#'
#' @details
#' \describe{
#'   \item{\code{coords}}{An array of XY coordinates for the shape of multiple specimens (16 landmarks by 2 (XY) by 100 specimens).}
#'   \item{\code{landmark.number}}{The number of landmarks in the shape data.}
#'   \item{\code{specimen.number}}{The number of specimens in the shape data.}
#'   \item{\code{metadata}}{A data frame of simulated metadata.}
#'   \item{\code{provenance}}{Data provenance.}
#' }
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @examples
#' # Load the dataset
#' data("mantis", package = "borealis")
#'
#' # Define connecting lines
#' {
#'   x <- 1:16
#'   mantis.lines <- matrix(c(x[-length(x)],x[-1]), ncol = 2)
#'   mantis.lines[10,] <- c(10,1)
#'   mantis.lines[15,] <- c(15,6)
#'   mantis.lines <- rbind(mantis.lines,
#'                         matrix(c(5,11, 6,11, 13,16, 14,16), ncol = 2, byrow = TRUE))
#' }
#'
#' landmark.plot(mantis)
#' names(mantis)
#'
"mantis"

