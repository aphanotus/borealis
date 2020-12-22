#' Add information to the names in a phylip alignment file
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param input.phy A phylip-format file containing an alignment with ID names
#'     to update.
#' @param input.csv A comma-separated values file with exactly two columns.
#'     This file must include a header row, but the column names are not used.
#'     The first column must include the existing ID names in the phylip alignment.
#'     The second column should contain information to be added to the alignment ID names.
#' @param output.filename The file name to export. If no argument is
#'     provided to \code{output.filename} then the output will over-write the input
#'     in \code{input.phy}.
#' @param separator A character string to separate the terms used in the names.
#'     This string should not be a space, since the phylip format uses spaces to
#'     distinguish names from sequence data.
#' @param prepend A logical value indicating whether or not new information should be
#'     added to the beginning (TRUE, the default) or the end of the existing names.
#' @param strict.species.names A logical value indicating whether or not to
#'     treat the the new information being added as a species name, and enforce
#'     a strict genus-specific epithet format (e.g. \emph{Homo sapiens neanderthalensis}
#'     would become \code{Homo_sapiens}).
#'
#' @export
#'

phy.names <- function (
  input.phy = NULL,
  input.csv = NULL,
  output.filename = NULL,
  separator = "_",
  prepend = TRUE,
  strict.species.names = TRUE
)
{
  # Load packages
  if (!require(phylotools)) { stop("Package missing. First, try running `install.packages('phylotools')`")}
  if (!require(dplyr)) { stop("Package missing. First, try running `install.packages('dplyr')`")}
  if (!require(magrittr)) { stop("Package missing. First, try running `install.packages('magrittr')`")}

  # Determine the input file names
  if (is.null(input.phy)) { input.phy <- file.choose() }
  if (!grepl('.phy$',input.phy)) {
    warning(paste("The file indicated by `input.phy`",input.phy,"must be phylip format - Proceeding anyway..."))
  }
  if (is.null(input.csv)) { input.csv <- file.choose() }
  if (!grepl('.csv$',input.csv)) {
    warning(paste("The file indicated by `input.csv`",input.csv,"must be CSV format - Proceeding anyway..."))
  }

  # Default output file name
  if (is.null(output.filename)) {
    if (grepl('.phy$',input.phy)) { output.filename <- input.phy }
    else {
      output.filename <- paste0(input.phy,".phy")
    }
  }

  # Check the existence of files
  if (!file.exists(input.phy)) {
    stop(paste("The file indicated by `input.phy`",input.phy,"does not exist."))
  }
  if (!file.exists(input.csv)) {
    stop(paste("The file indicated by `input.csv`",input.csv,"does not exist."))
  }

  new.info <- read.csv(input.csv, header = TRUE, stringsAsFactors = FALSE)
  new.info[,1] <- trimws(new.info[,1])
  new.info[,2] <- trimws(new.info[,2])

  # Remove any spaces from the new info
  new.info[,2] <- gsub(" ",separator,new.info[,2])

  if (strict.species.names) {
    if (!require(stringr)) { stop("Package missing. First, try running `install.packages('stringr')`")}
    new.info[,2] <- apply(str_split_fixed(new.info[,2],separator,3)[,1:2], 1, paste0, collapse=separator)
  }

  if (prepend) {
    new.info$id <- paste0(new.info[,2],separator,new.info[,1])
  } else {
    new.info$id <- paste0(new.info[,1],separator,new.info[,2])
  }

  phy.header <- read.delim(input.phy, header = FALSE, sep = "\n")[1,]
  alignment <- read.phylip(input.phy, clean_name = FALSE)
  original.order <- order(alignment$seq.name)

  # Remove any rows from new.info that duplicate ID names
  x <- which(!duplicated(new.info[,1]))
  new.info <- new.info[x,]

  # Remove rows from new.info if the ID doesn't appear in the alignment
  x <- which(new.info[,1] %in% alignment$seq.name)
  new.info <- new.info[x,]

  # Any ID names in the alignment that aren't in the CSV?
  if (any(!(alignment$seq.name %in% new.info[,1]))) {
    x <- which(!(alignment$seq.name %in% new.info[,1]))
    stop(paste(alignment$seq.name[x],"does not appear in",input.csv))
  }

  # Sort by the ID name column
  new.info <- new.info[order(new.info[,1]),]

  # Sort by the name column
  alignment <- alignment[order(alignment$seq.name),]

  alignment$seq.name <- new.info$id

  # Return to the original alignment sequence order
  alignment <- alignment %>%
    arrange(original.order) %>%
    as.data.frame()

  # Write out the new phylip file
  write(phy.header, output.filename)
  write(paste0(alignment$seq.name," ",alignment$seq.text),
        output.filename, append = TRUE)

} # End of function
