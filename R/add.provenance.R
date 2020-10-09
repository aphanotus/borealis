#' Add data provenance to a list
#'
#' This function will add a custom data provenance entry in the markdown-based format used
#'     by the \code{borealis} package. While designed for use with shape data structures,
#'     the function should be compatible with any list (or an atomic vector that can be coerced
#'     by \code{\link{base::as.list}}.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param x A list object
#' @param name A string identifying the data provenance entry (defaults to \code{title} without white space)
#' @param title A string titling the data provenance entry (defaults to \code{name})
#' @param user A string identifying the user (defaults to \code{Sys.getenv("LOGNAME")})
#' @param date A string for the date and time associated with the provenance (defaults to the current system date and time)
#' @param text A string containing the main text of the data provenance entry
#' @param markdown A logical argument specifying whether markdown formatting should be applied
#'
#' @export
#'
#' @examples
#'
#' pleth <- add.provenance(
#'   plethodon,
#'   name="acquisition",
#'   title = "Loaded example dataset",
#'   text = "Started a **training analysis** using the `plethodon` dataset,
#'   which is included in the excellent
#'   [`geomorph` package](https://cran.r-project.org/web/packages/geomorph/)" )
#'
#' str(pleth)
#'
#' cat(pleth$provenance$acquisition)
#'

add.provenance <- function (x, name = NULL, title = NULL, user = NULL, date = NULL, text = NULL, markdown = TRUE)
{
  # Vet the input data object
  if (is.list(x)) {
    output <- x
  } else {
    if (is.vector(x)) {
      output <- list(x = x)
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: `?add.provenance`.)")
    }
  }

  # Vet the key arguments
  if (is.null(name) & is.null(title)) {
    stop("Error: either `name` or `title` must be provided. (See the help entry: `?add.provenance`.)")
  } else {
    if (is.null(name)) { name <- make.names(tolower(title)) }
    else { name <- make.names(name) }
    if (is.null(title)) { title <- name }
  }
  if (is.null(user)) { user <- Sys.getenv("LOGNAME") }
  if (is.null(date)) { date <- format(Sys.time(), "%A, %d %B %Y, %X") }
  if (is.null(text)) {
    warning("No value provided for the argument `text`. (See the help entry: `?add.provenance`.)")
  }

  # Construct new provenance entry
  new.provenance <- ifelse(markdown,"## ","")
  new.provenance <- paste0(
    new.provenance,
    paste0(title,"\n\n"),
    paste0("Performed by user `",user,"` on ",date,"\n\n")
  )
  if (!markdown) { new.provenance <- gsub("\n\n","\n",new.provenance) }
  new.provenance <- paste0(
    new.provenance,
    paste0(text,"\n")
  )
  if (markdown) { new.provenance <- paste0(new.provenance,"\n") }

  # Make sure there's a `provenance` element in the list
  if (!any(grepl("provenance",names(output)))) {
    output$provenance <- list()
  }

  output$provenance$add.provenance <- new.provenance
  x <- length(names(output$provenance))
  names(output$provenance)[x] <- name

  # Output
  return(output)

} # End of function


