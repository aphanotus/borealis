#' Generalized Procrustes alignment with interactive outlier detection and removal
#'
#' GPA is performed by a call to \code{\link[geomorph]{gpagen}}.
#' If \code{outlier.analysis = TRUE} then the function will display the results of
#' \code{\link[geomorph]{plotOutliers}}, then warp grids for the most extreme shapes.
#' The user is prompted to select a certain number of extreme shapes to exclude from
#' a repeated Procrustes alignment. This processes is iterative, until the user indicates that
#' it should stop or no shapes are requested for removal.
#'
#' @return Returns a list with Procrustes-aligned coordinates in \code{coords}, centroid size in \code{Csize}.
#'     All other elements from \code{\link[geomorph]{gpagen}} are in the the element \code{gpagen}.
#'     Any other list elements in the input are retained.
#'     Data provenance is also retained in \code{provenance} and updated with details of the GPA..
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @references Adams and M.L. Collyer and A. Kaliontzopoulou. 2020. Geomorph: Software for geometric morphometric analyses. R package version 3.2.1. D.C. (\href{https://cran.r-project.org/package=geomorph}{Link})
#'
#' @param A A list or 3-dimensional array containing XY shape coordinates for multiple specimens.
#' @param curves An optional matrix defining which landmarks should be treated as semilandmarks by \code{gpagen}.
#' @param show.plot.gpa A logical value specifying whether to plot the Procrustes-aligned landmarks.
#' @param outlier.analysis A logical value specifying whether to perform interactive outlier analysis.
#'     Alternatively, a numeric vector can be provided indicating the index positions of specimens to remove.
#'     A character vector of specimen names can also be provided, indicating the specimens to remove.
#' @param display.outliers The number of outlier shapes to display in the interactive outlier analysis.
#' @param output.gdf A logical value specifying whether to return output with
#'     a list structure that includes an element, \code{gdf}, containing a
#'     \code{geomorph.data.frame} of shape coordinates and metadata (the default).
#'
#' @export
#'
#' @examples
#' data("Bombus.forewings", package = "borealis")
#'
#' fw.gpa <- align.procrustes(Bombus.forewings)
#'
#' fw.gpa <- align.procrustes(Bombus.forewings, show.plot.gpa = FALSE)
#' plot(fw.gpa$gpagen)
#'
#' fw.gpa <- align.procrustes(Bombus.forewings, outlier.analysis = TRUE)
#' fw.gpa <- align.procrustes(Bombus.forewings, outlier.analysis = 57)
#' fw.gpa <- align.procrustes(Bombus.forewings,
#'                            outlier.analysis = c("FJ190828-002","DRA190718-001"),
#'                            print.progress = FALSE )
#'
#' names(fw.gpa$provenance)
#' cat(fw.gpa$provenance$align.procrustes)
#'

align.procrustes <- function (
  A,
  curves = NULL,
  show.plot.gpa = TRUE,
  outlier.analysis = FALSE,
  display.outliers = 4,
  output.gdf = TRUE,
  ...
)
{
  # Don't bother running anything if geomorph isn't installed!
  if (!require(geomorph)) {
    stop("Please run  install.packages('geomorph'). ")
  }

  shape.data <- NULL
  output <- NULL
  provenance <- NULL

  # Vet the input
  if (class(A)[1] %in% c("gpagen","list")) {
    if (any(grepl("coords",names(A)))) {
      shape.data <- A$coords
      output <- A[-grep("coords",names(A))]
    } else {
      if (any(grepl("land",names(A)))) {
        shape.data <- A$land
        output <- A[-grep("land",names(A))]
      } else {
        stop("Error: Input is not a recognized type. (See the help entry: `?align.procrustes`.)")
      }
    }
    if (is.null(provenance) & any(grepl("provenance",names(A)))) { provenance <- A$provenance }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shape.data <- A
      if (is.null(provenance)) {
        warning("Warning: No data provenance provided.\n")
      } else {
        output$provenance <- provenance
      }
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: `?align.procrustes`.)")
    }
  }

  # Do the GPA
  GPA <- gpagen(shape.data, curves = curves, ...)
  new.prov.details <- paste0(
    paste0("## Generalized Procrustes analysis\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::align.procrustes` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n")
  )

  if(!is.null(curves)) {
    new.prov.details <- paste0(
      new.prov.details,
      paste0("The following matrix was used to define semilandmarks:\n\n|     |  LM |     |\n|:---:|:---:|:---:|\n| "),
      paste0(apply(curves,1,function(x) {paste0(sprintf("% 3d",x), collapse = " | ")}), collapse = " |\n| "),
      " |\n\n"
    )
  }

  # Outlier analysis
  names.of.outliers.removed <- NULL
  identified.outliers <- FALSE

  # Allow outliers to be named in the argument as an index number
  if (is.numeric(outlier.analysis) | is.character(outlier.analysis)) {
    if (is.numeric(outlier.analysis)) {
      if (length(outlier.analysis)>0) {
        names.of.outliers.removed <- dimnames(shape.data)[[3]][outlier.analysis]
        shape.data <- shape.data[,,-outlier.analysis]
      }
    } else {
      # Allow outliers to be named in the argument as a specimen ID
      if (is.character(outlier.analysis)) {
        if (length(outlier.analysis)>0) {
          names.of.outliers.removed <- outlier.analysis
          x <- which(dimnames(shape.data)[[3]] %in% names.of.outliers.removed)
          cat(x)
          shape.data <- shape.data[,,-x]
        }
      }
    }
    new.prov.details <- paste0(new.prov.details,"The following ",length(names.of.outliers.removed)," specimens were removed as outliers: \n- ",paste0(names.of.outliers.removed, collapse = "\n- "),"\n\n")
    GPA <- gpagen(shape.data, curves = curves, ...)
    if (show.plot.gpa) {
      plot(GPA)
    }
    outlier.analysis <- FALSE
    identified.outliers <- TRUE
  }

  if (!outlier.analysis & !identified.outliers) {
    if (show.plot.gpa) {
      plot(GPA)
    }
  }

  # Interactive outlier analysis
  if (outlier.analysis) {

    # Initial GPA Plot
    if (show.plot.gpa) {
      plot(GPA)
      # Pause if the user requests both the GPA plot and outlier analysis
      invisible(readline(prompt="Press any key to continue."))
    }

    # Outlier Procrustes distances
    rounds.of.outlier.analysis <- 0
    outliers <- plotOutliers(GPA$coords)
    h <- ceiling(sqrt(display.outliers))
    w <- ifelse( ( h^2 - display.outliers) >= h, h-1, h )

    readline(prompt="Press any key to continue.")

    continue.loop <- TRUE
    ###################
    while (continue.loop) {

      # Plot the most extreme outliers to the consensus GPA shape
      par(mfrow=c(h,w))
      for (i in 1:display.outliers) {
        plotRefToTarget(GPA$consensus,GPA$coords[,,outliers[i]])
        title(main = paste0('outlier #',i,'  [',outliers[i],']\n',
                           dimnames(shape.data)[[3]][outliers[i]]),
              col.main = 'darkred', font.main = 1)
      }
      par(mfrow=c(1,1))

      # Remove the extreme outlier. I think this is the one where I entered zeros for missing data
      rm.outliers <- readline(prompt="Remove how many of the most extreme outliers? ('Enter' or '0' for none.) ")
      if (rm.outliers %in% c(""," ","  ")) {
        rm.outliers <- NULL
      }
      else {
        if (!(rm.outliers %in% dimnames(shape.data)[[3]])) {
          rm.outliers <- as.integer(rm.outliers)
          if (rm.outliers > 0) {
            rm.outliers <- 1:rm.outliers
          } else {
            rm.outliers <- NULL
          }
        }
      }
      if (!is.null(rm.outliers)) {
        names.of.outliers.removed <- c(names.of.outliers.removed, dimnames(shape.data)[[3]][outliers[rm.outliers]])
        shape.data <- shape.data[,,-outliers[rm.outliers]]
        rounds.of.outlier.analysis <- rounds.of.outlier.analysis + 1

        # Repeat GPA
        GPA <- gpagen(shape.data, curves = curves, ...)

        if (show.plot.gpa) {
          plot(GPA)
          invisible(readline(prompt="Press any key to continue."))
        }

        outliers <- plotOutliers(GPA$coords)

        # Ask to continue
        if (continue.loop) {
          x <- readline("Would you like to perform another round of outlier analysis? (y/n)")
          continue.loop <- (tolower(x) %in% c("","y","yes","ya","yep"))
        }

      } else {
        continue.loop <- FALSE
      } # End if/else  (rm.outliers > 0)

    } # End while-loop
    ###################

    if (!is.null(names.of.outliers.removed)) {
      new.prov.details <- paste0(new.prov.details,"The following ",length(names.of.outliers.removed)," specimens were removed after ",rounds.of.outlier.analysis," rounds of outlier analysis: \n- ",paste0(names.of.outliers.removed, collapse = "\n- "),"\n\n")
    }

  } # End outlier analysis

  # Prep the output
  output$coords <- GPA$coords
  output$Csize <- GPA$Csize
  output$gpagen <- GPA
  # output$gpagen <- output$gpagen[-c(1:2)]
  if (!is.null(provenance) & !any(grepl("provenance",names(output)))) {
    output$provenance <- provenance
  }
  output$provenance$align.procrustes <- new.prov.details

  if (any(grepl("specimen.number",names(output))) & (!is.null(names.of.outliers.removed))) {
    output$specimen.number <- dim(output$coords)[3]
    if (any(grepl("metadata",names(output)))) {
      output$metadata <- output$metadata[-which(output$metadata[,1] %in% names.of.outliers.removed),]
    }
  }

  if(output.gdf) {
    output <- listed.gdf(output)
  }

  return(output)

} # End of function
