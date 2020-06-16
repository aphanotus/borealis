#' Morphospace in the ggplot style
#'
#' This function is designed to make pretty plots from geometric morphometric ordinations
#' using the output of \code{geomorph::gm.prcomp} (Adams et al. 2020).
#'
#' Beware that right now, the \code{save.as} feature only works with
#' \code{backtransform.examples = FALSE}.
#'
#' @references  Geomorph: Software for geometric morphometric analyses.
#' R package version 3.2.1. D.C. Adams and M.L. Collyer and A. Kaliontzopoulou. 2020.
#' (\href{https://cran.r-project.org/package=geomorph}{Link})
#'
#' @param x An object of class \code{gm.prcomp}.
#' @param axis1 The PC axis to plot on the horizontal.
#' @param axis2 The PC axis to plot on the vertical.
#' @param group A factor to group samples.
#' @param group.title An optional title for the group legend.
#' @param convex.hulls A logical factor specifying whether to plot convex hulls around groups.
#' @param color The color of points and hulls in each group.
#' @param pt.size The size of individual points.
#' @param pt.alpha The opacity of individual points.
#' @param hull.alpha The opacity of the convex hulls.
#' @param save.as An optional filename to export the plot.
#' @param height The height of the exported plot.
#' @param width The width of the exported plot.
#' @param scale A scale factor for elements of the exported plot.
#' @param units The units for height and width of the exported plot.
#' @param backtransform.examples A logical factor specifying whether to include example shapes at the extremes of each axis.
#' @param shape.method Method used to visualize shape differences, following \code{geomorph::plotRefToTarget}.
#' @param lm.labels A logical factor specifying whether to label landmarks in the example shapes.
#' @param ref.shape A reference of consensus shape.
#' @param bt.links A matrix with two columns indicating landmarks to connect by lines in the example shapes.
#' @param bt.shape.mag The desired magnification to be used on the example shape, in comparison to the reference.
#' @param bt.inset.size The relative size of the example shape insets.
#' @param bt.margin.factor A factor to control size of the margin where example shapes are plotted.
#' @param bt.legend.position A two-element vector specifying the justification and position of the legend in a back-transform plot.
#' @param ref.pt.size Scale factor for reference configuration points (single value or vector of values)
#' @param target.pt.size Scale factor for target configuration points (single value or vector of values)
#' @param viridis.color.option The color pallette to use from the \code{viridis} package.
#'
#' @export
#'
#' @examples
#' # GPA
#' Y.gpa <- gpagen(plethodon$land)
#' # Create a gm.prcomp object
#' PCA <- gm.prcomp(Y.gpa$coords)
#'
#' # Morphospace plots
#' ggGMMplot(PCA, group = plethodon$species, group.title = 'species')
#' ggGMMplot(PCA, group = plethodon$species, group.title = 'species', convex.hulls = TRUE)
#' ggGMMplot(PCA, group = plethodon$species, group.title = 'species', convex.hulls = TRUE,
#'           axis1 = 1, axis2 = 3)
#' ggGMMplot(PCA, group = plethodon$species, group.title = 'species', convex.hulls = TRUE,
#'           viridis.color.option = 'plasma')
#' ggGMMplot(PCA, group = plethodon$species, group.title = 'species', convex.hulls = TRUE,
#'           color = c("royalblue","gray40"))
#'
#' # Plots with example shapes
#' ggGMMplot(PCA, group = plethodon$species, group.title = 'species', convex.hulls = TRUE,
#'           backtransform.examples = TRUE,
#'           ref.shape = Y.gpa$consensus)
#'
#' # Custom links on the example shapes
#' ggGMMplot(PCA, group = plethodon$species, group.title = 'species', convex.hulls = TRUE,
#'           backtransform.examples = TRUE,
#'           ref.shape = Y.gpa$consensus,
#'           bt.links = matrix(c(4,5,5,6,6,7,7,8,8,9,9,10,10,11,2,4,12,4,3,5),
#'                             ncol=2, byrow=TRUE))
#'
#' # No links on the example shapes
#' ggGMMplot(PCA, group = plethodon$species, group.title = 'species', convex.hulls = TRUE,
#'           backtransform.examples = TRUE,
#'           ref.shape = Y.gpa$consensus,
#'           bt.links = "none")
#'
#' # Example shapes displaysed as thin-plate spline warp grids
#' ggGMMplot(PCA, group = plethodon$species, group.title = 'species', convex.hulls = TRUE,
#'           backtransform.examples = TRUE,
#'           ref.shape = Y.gpa$consensus,
#'           shape.method = "TPS",
#'           lm.labels = FALSE)
#'

ggGMMplot <- function (
  x,
  axis1 = 1,
  axis2 = 2,
  group = NULL,
  group.title = NULL,
  convex.hulls = FALSE,
  color = NULL,
  pt.size = 1.5,
  pt.alpha = 0.85,
  hull.alpha = 0.2,
  save.as = NULL,
  height = 5,
  width = 7,
  scale = 1,
  units = "in",
  backtransform.examples = FALSE,
  shape.method = "points",
  lm.labels = TRUE,
  ref.shape = NULL,
  bt.links = NULL,
  bt.shape.mag = 1,
  bt.inset.size = 0.35,
  bt.margin.factor = 2,
  bt.legend.position = c(1,1),
  ref.pt.size = 1,
  target.pt.size = 0.75,
  viridis.color.option = "viridis"

) {

  # Check that the input is a PCA object
  if (!any(grepl("prcomp",class(x)))) {
    return(cat("Error: 'x' must be an object of class 'prcomp' or 'gm.prcomp'."))
  } else {
    if (backtransform.examples & !any(grepl("gm.prcomp",class(x)))) {
      return(cat("Warning: To include backtransformed example shapes, 'x' must be an object of class 'gm.prcomp'."))
      backtransform.examples <- FALSE
    }
  }

  # Load packages
  require(geomorph)
  require(ggplot2)
  require(ggpubr)
  require(grid)
  require(ggplotify)

  # Check that the requested axes are appropriate
  if (!(is.numeric(axis1) & (axis1 %% 1 == 0) & (axis1 > 0) & (axis1 < dim(x$x)[2])))  {
    cat("Axis 1 is out of bounds. Defaulting to axis1 = 1.")
    axis1 <- 1
  }
  if (!(is.numeric(axis2) & (axis2 %% 1 == 0) & (axis2 > 0) & (axis2 < dim(x$x)[2]))) {
    cat("Axis 2 is out of bounds. Defaulting to axis2 = 2.")
    axis2 <- 2
  }
  pcx <- data.frame(
    comp1 = x$x[,axis1],
    comp2 = x$x[,axis2]
  )

  # Check the color options
  if (is.null(color)) {
    if (is.null(group)) {
      color <- "black"
    } else {
      if(require("viridis")) {
        if (!(viridis.color.option %in% c("magma", "A", "inferno", "B", "plasma", "C", "viridis", "D", "cividis", "E"))) {
          cat("Warning: Color option is not recognized. See '?viridis::scale_color_viridis' for details. Defaulting to 'viridis'. ")
          viridis.color.option <- "viridis"
        }
        color <- viridis::viridis(length(unique(group)), option = viridis.color.option)
      }
      else {
        color <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = TRUE)],
                        length(unique(group)) )
      }
    }
  }

  pc.contributions <- pcvar(x, dimensions = dim(x$x)[2])[c(axis1,axis2)]

  # The base PCA plot
  base.plot <- ggplot(pcx, aes(x=comp1, y=comp2, color=group, fill = group)) +
    theme_minimal() +
    geom_point(size = pt.size, alpha = pt.alpha) +
    scale_color_manual(values=color, name=group.title) +
    # scale_color_viridis(discrete = TRUE, option = viridis.color.option, name=group.title) +
    guides(fill = "none") +
    labs(x = paste0('PC',axis1,' (',pc.contributions[1],')'),
         y = paste0('PC',axis2,' (',pc.contributions[2],')')
    )
  # Add convex hulls
  if (convex.hulls) {
    base.plot <- base.plot +
      stat_chull(alpha = hull.alpha, geom = "polygon", show.legend = FALSE) +
      scale_fill_manual(values=color)
      # scale_fill_viridis(discrete=TRUE, option = viridis.color.option)
  }

  # Exit scenario: no backtransform & an output file is requested
  if (!backtransform.examples & !is.null(save.as)) {
    if (!(grepl("\\.pdf",save.as, ignore.case = TRUE) |
          grepl("\\.png",save.as, ignore.case = TRUE) |
          grepl("\\.jpg",save.as, ignore.case = TRUE) ) ) {
      cat("Warning: Output file format is not recognized. Suported formats are pdf, png, and jpg. Defaulting to jpg. ")
      save.as <- paste0(save.as,'.jpg')
    }
    ggsave(filename = save.as, plot = base.plot,
           height = height, width = width, scale = scale, units = units)
    return(base.plot)
  }

  # Add backtransform examples
  if (backtransform.examples) {

    # Check that a reference shape is provided. If not, return the base plot
    if (is.null(ref.shape)) {
      cat("Error: A reference shape is required to include back-transform examples. Try 'ref.shape = gpa$consensus'.")
      return(base.plot)
    }

    # If no links are provided to connect landmarks, then outline them...
    if (is.null(bt.links)) {
      bt.links <- grDevices::chull(ref.shape)
      bt.links <- matrix(c(bt.links,bt.links[-1],bt.links[1]), ncol=2, byrow = FALSE)
    }
    # Unless the input is "none", in which case don't show any links
    else { if (any(bt.links == "none")) { bt.links <- NULL } }

    # ggplotify::as.grob wants variables to global
    ref.shape <<- ref.shape
    x <<- x
    shape.method <<- shape.method
    lm.labels <<- lm.labels
    bt.links <<- bt.links
    bt.shape.mag <<- bt.shape.mag
    ref.pt.size <<- ref.pt.size
    target.pt.size <<- target.pt.size
    axis1.shape.index <<- grep(paste0("comp",axis1,"$"),names(x$shapes))
    axis2.shape.index <<- grep(paste0("comp",axis2,"$"),names(x$shapes))

    # Create the extreme shape examples
    axis1min <- as.grob( ~ plotRefToTarget(
      ref.shape, x$shapes[[axis1.shape.index]][["min"]],
      method = shape.method, links=bt.links, label = lm.labels, mag = bt.shape.mag,
      gridPars = gridPar(pt.size = ref.pt.size, tar.pt.size = target.pt.size) ) )
    axis1max <- as.grob( ~ plotRefToTarget(
      ref.shape, x$shapes[[axis1.shape.index]][["max"]],
      method = shape.method, links=bt.links, label = lm.labels, mag = bt.shape.mag,
      gridPars = gridPar(pt.size = ref.pt.size, tar.pt.size = target.pt.size) ) )
    axis2min <- as.grob( ~ plotRefToTarget(
      ref.shape, x$shapes[[axis2.shape.index]][["min"]],
      method = shape.method, links=bt.links, label = lm.labels, mag = bt.shape.mag,
      gridPars = gridPar(pt.size = ref.pt.size, tar.pt.size = target.pt.size) ) )
    axis2max <- as.grob( ~ plotRefToTarget(
      ref.shape, x$shapes[[axis2.shape.index]][["max"]],
      method = shape.method, links=bt.links, label = lm.labels, mag = bt.shape.mag,
      gridPars = gridPar(pt.size = ref.pt.size, tar.pt.size = target.pt.size) ) )

    # Clean-up
    # Don't sure why this doesn't work, but right now there variable end up in .GlobalEnv
    # rm(ref.shape, x, bt.links, bt.shape.mag)
    # rm(axis1.shape.index, axis2.shape.index)

    # Prepare to write out the plot
    if (!is.null(save.as)) {
      if (!(grepl("\\.pdf",save.as, ignore.case = TRUE) |
            grepl("\\.png",save.as, ignore.case = TRUE) |
            grepl("\\.jpg",save.as, ignore.case = TRUE) ) ) {
        cat("Warning: Output file format is not recognized. Suported formats are pdf, png, and jpg. Defaulting to jpg. ")
        save.as <- paste0(save.as,'.jpg')
      }
      if (grepl("\\.pdf",save.as, ignore.case = TRUE)) {
        pdf(file = save.as, height = height, width = width)
      }
      if (grepl("\\.png",save.as, ignore.case = TRUE)) {
        png(file = save.as, bg = "transparent", height = height, width = width, units = units)
      }
      if (grepl("\\.jpg",save.as, ignore.case = TRUE)) {
        png(file = save.as, height = height, width = width, units = units)
      }
    }

    # Use 'grid' tools to build the plot
    if (bt.margin.factor < 0) { bt.margin.factor <- 1 }
    grid.newpage()
    grid.draw(
      base.plot +
        theme(legend.justification=bt.legend.position, legend.position=bt.legend.position) +
        xlim(c(min(x$x[,axis1])*bt.margin.factor, max(x$x[,axis1])*bt.margin.factor)) +
        ylim(c(min(x$x[,axis2])*bt.margin.factor, max(x$x[,axis2])*bt.margin.factor))
    )
    pushViewport(viewport(x=.15, y=.5, width=bt.inset.size, height=bt.inset.size))
    grid.draw(axis1min)
    upViewport()
    pushViewport(viewport(x=.85, y=.5, width=bt.inset.size, height=bt.inset.size))
    grid.draw(axis1max)
    upViewport()

    pushViewport(viewport(x=.5, y=.15, width=bt.inset.size, height=bt.inset.size))
    grid.draw(axis2min)
    upViewport()
    pushViewport(viewport(x=.5, y=.85, width=bt.inset.size, height=bt.inset.size))
    grid.draw(axis2max)
    upViewport()

    # Close the output file.
    if (!is.null(save.as)) { dev.off() }

  } # End back-transform steps
  else {
    return(base.plot)
  }

} # End of function










