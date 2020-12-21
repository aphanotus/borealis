#' Scaling plots in the ggplot style
#'
#' Make pretty plots of the relationship between two variables, tailored for use
#' in examining allometric scaling relationships.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param x A vector of numerical values to plot on the horizontal axis.
#' @param y A vector of numerical values to plot on the vertical axis.
#' @param group A factor to group samples.
#' @param group.title An optional title for the group legend.
#' @param convex.hulls A logical factor specifying whether to plot convex hulls around groups.
#' @param viridis.color.option The color palette to use from the \code{viridis} package.
#' @param color The color of points and hulls in each group.
#' @param pt.size The size of individual points.
#' @param pt.alpha The opacity of individual points.
#' @param hull.alpha The opacity of the convex hulls.
#' @param save.as An optional file name to export the plot.
#' @param height The height of the exported plot.
#' @param width The width of the exported plot.
#' @param fixed.aspect A logical factor specifying whether to enforce an equal aspect ratio.
#' @param label.groups A logical factor specifying whether to label groups.
#' @param groups.trendlines A logical factor specifying whether to include trend lines for each group.
#' @param include.legend A logical factor specifying whether to include a legend.
#' @param isometry.line A logical factor specifying whether to include a slope illustrating slope of 1.
#' @param isometry.line.color The color of isometry line.
#' @param verbose A logical factor specifying whether to output a table of slopes.
#'
#' @export
#'
#' @examples
#' data(plethodon, package = "geomorph")
#'
#' pletho.links <- matrix(c(4,5,5,6,6,7,7,8,8,9,9,10,10,11,2,4,12,2,3,5),
#'                        ncol = 2, byrow = TRUE)
#' landmark.plot(plethodon$land[,,1], links = pletho.links )
#'
#' distance <- function(x1,y1,x2,y2) { d <- sqrt((x1-x2)^2+(y1-y2)^2); return(d) }
#'
#' head.length <- apply(plethodon$land, 3, function(xy) {
#'   distance(xy[7,1],xy[7,2], xy[12,1],xy[12,2])
#' } )
#' jaw.length <- apply(plethodon$land, 3, function(xy) {
#'   distance(xy[3,1],xy[3,2], xy[5,1],xy[5,2])
#' } )
#'
#' gg.scaling.plot(
#'   x = log10(head.length), y = log10(jaw.length),
#'   group=plethodon$species, group.title = "species",
#'   xlab = "head length (log10 mm)", ylab = "jaw length (log10 mm)",
#'   include.legend = TRUE,
#'   isometry.line = TRUE
#' )
#'

gg.scaling.plot <- function(
  x,
  y,
  group = NULL,
  group.title = NULL,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  convex.hulls = TRUE,
  viridis.color.option = "viridis",
  color = NULL,
  pt.size = 1.5,
  pt.alpha = 0.85,
  hull.alpha = 0.2,
  save.as = NULL,
  height = 5,
  width = 7,
  fixed.aspect = TRUE,
  label.groups = TRUE,
  groups.trendlines = FALSE,
  include.legend = FALSE,
  isometry.line = FALSE,
  isometry.line.color = "gray35",
  verbose = TRUE,
  ...
)
{
  if (!require(ggplot2)) { stop("Package missing. First, try running `install.packages('ggplot2')`")}
  if (!require(magrittr)) { stop("Package missing. First, try running `install.packages('magrittr')`")}
  if (!require(dplyr)) { stop("Package missing. First, try running `install.packages('dplyr')`")}

  # Sub functions
  slope <- function(y,x) { lm(y~x)$coefficients[2] }
  slope.p <- function(y,x) { tmp <- summary(lm(y~x)); if (!is.na(tmp$coefficients[1,4])) { tmp$coefficients[2,4] } else { 1 } }
  slope.ci <- function(y,x) { c(confint(lm(y~x), parm = "x")) }

  df <- data.frame(
    x = x,
    y = y,
    group = as.factor(group)
  )

  df <- filter(df, !is.na(x) & !is.na(y))

  slope.info <- df %>%
    group_by(group) %>%
    summarise(n = n(),
              slope=signif(slope(y, x),3),
              slope.p=signif(slope.p(y, x),3),
              ci.lo = signif(slope.ci(y, x)[1],3),
              ci.hi = signif(slope.ci(y, x)[2],3),
              spans.zero = dplyr::between(0,ci.lo,ci.hi),
              x = mean(x),
              y = mean(y),
              .groups = "drop")

  # Check the color options
  if (is.null(color)) {
    if (is.null(group)) {
      color <- "black"
    } else {
      if(require("viridis")) {
        if (!(viridis.color.option %in% c("magma", "A", "inferno", "B", "plasma", "C", "viridis", "D", "cividis", "E"))) {
          warning("Warning: Color option is not recognized. See '?viridis::scale_color_viridis' for details. Defaulting to 'viridis'. ")
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

  base.plot <- ggplot(df, aes(x=x, y=y, group=group, color=group, fill=group)) +
    theme_minimal() +
    ggtitle(title) +
    geom_point(size=pt.size, alpha=pt.alpha) +
    scale_color_manual(values=color, name=group.title) +
    # scale_color_viridis( discrete=TRUE, group.title) +
    scale_fill_manual(values=color, name=group.title) +
    scale_x_continuous(xlab) +
    scale_y_continuous(ylab)

  if (convex.hulls) {
    if (require("ggpubr")) {
      base.plot <- base.plot +
        stat_chull(alpha = hull.alpha, geom = "polygon", show.legend = FALSE)
    } else {
      warning("Package missing. First, try running `install.packages('ggpubr')`")
    }
  }

  if (!include.legend) {
    base.plot <- base.plot + theme(legend.position="none")
  }

  if (label.groups) {
    if (require("ggrepel")) {
      base.plot <- base.plot +
        geom_text_repel(data=slope.info,
                        aes(x=x, y=y, group=group,
                            label=paste0(group,
                                         ifelse(is.na(slope),"",paste0('\nk=',slope)),
                                         ifelse(slope.p<0.05,paste0('\np=',slope.p),'') ) ),
                        hjust=0, size = 2.5, lineheight=0.825, alpha=0.7,
                        force = 1, max.iter = 10000)
    } else {
      warning("Package missing. First, try running `install.packages('ggrepel')`")
    }
  }

  if (groups.trendlines) {
    base.plot <- base.plot + geom_smooth(method = lm, se = FALSE)
  }

  if (fixed.aspect) {
    base.plot <- base.plot + coord_fixed()
  }

  if (isometry.line) {
    base.plot <- base.plot + geom_abline(slope=1, intercept = lm(y~x)$coefficients[1], color=isometry.line.color)
  }

  if (!is.null(save.as)) {
    if (!(grepl("\\.pdf",save.as, ignore.case = TRUE) |
          grepl("\\.png",save.as, ignore.case = TRUE) |
          grepl("\\.jpg",save.as, ignore.case = TRUE) ) ) {
      warning("Warning: Output file format is not recognized. Suported formats are pdf, png, and jpg. Defaulting to jpg. ")
      save.as <- paste0(save.as,'.jpg')
    }
    ggsave(filename = save.as, plot = base.plot, height = height, width = width, ...)
  }

  sigstar <- function (x) {symnum(x, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "** ", "*  ", ".  ", "   "), na = '   ') }

  slope.info$sig <- sigstar(slope.info$slope.p)
  slope.info <- slope.info %>%
    select(group, n, slope, p=slope.p, sig, ci.lo, ci.hi, spans.zero ) %>%
    arrange(desc(slope)) %>%
    as.data.frame()

  output <- list(
    slopes = slope.info,
    plot = base.plot
  )

  if (verbose) {
    return(output)
  } else {
    return(base.plot)
  }

} # End of function
