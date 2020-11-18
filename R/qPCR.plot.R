#' Plot raw qPCR data
#'
#' This function takes raw real-time PCR data and produces a convenient plot and table,
#' which can be used to assess the quality of the data.
#'
#' The function takes information on the standard concentrations (unitless, but assumed
#' to be log10 of template molecules per microliter) and Cq values measured by the
#' real-time PCR instrument. A linear regression provides intercept and slope, from which
#' PCR efficiency is calculated. The correlation coefficient is also displayed.
#' Potential outlier values in the standard Cq values are highlighted on the plot in red.
#' These are flagged as technical replicates more than 1.5 cycles from the median.
#' efficiency values outside the range of 1.8 to 2.2 (90% - 110%) are also highlighted,
#' as are correlation coefficients (R^2) less than 0.9.
#'
#' If they are provided, the minimum Cq values for negative controls (NRT and NTC) are
#' plotted as horizontal lines (gray and blue, respectively).
#'
#' If Cq values are provided for unknown samples, they are plotted on the line and the
#' corresponding starting concentrations are provided in the output table.
#'
#' Any additional arguments are passed to \code{plot}.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param std.conc A numeric vector providing the concentrations of the standards used.
#' @param std.cq A numeric vector of Cq values measured for the standard samples.
#' @param unk.cq A numeric vector of Cq values measured for unknown samples.
#' @param nrt.cq A numeric vector of Cq values measured for no-reverse transcription control (NRT) samples.
#' @param ntc.cq A numeric vector of Cq values measured for no-template control (NTC) samples.
#' @param replicate.number The number of technical replicates in the standards (Default value is 3).
#' @param outlier.method A character specifying the method to use in determining outliers among the standards.
#'     The default is \code{absolute} which flags values more than 1.5 cycles from the median.
#'     Alternatively \code{IQR} will flags replicates more than 1.5 times the interquartile range from the median Cq value.
#' @param unk.replicates Specifies whether \code{unk.cq} contains technical replicates (or previously averaged) values.
#'     The default value is \code{FALSE}. If set to \code{TRUE}, then the function assumes
#'     the same \code{replicate.number} used for standards.
#'     Alternatively, a single numeric value can specify a different number of technical
#'     replicates for the unknown samples.
#' @param error.bars A logical factor specifying whether to include error bars on the unknowns.
#'     Only applies if there are replicates for the unknowns.
#' @param unk.ids An optional character vector with ID names for the unknown samples.
#'     Alternative, if set to \code{TRUE}, then \code{unk.cq} values will be numbered in order.
#'     If \code{unk.replicates} is \code{TRUE} or set to a numeric, then one only one
#'     value should be provided in the \code{unk.ids} vector for each sample.
#' @param main An overall title for the plot.
#' @param xlab A title for the x axis.
#' @param ylab A title for the y axis.
#'
#' @export
#'
#' @examples
#' std.conc <- c(rep(6,3),rep(5,3),rep(4,3),rep(3,3))
#' std.cq <- c(25.74, 26.33, 25.03,  28.42, 28.90, 27.30,
#'             32.15, 32.31, 30.61,  34.46, 34.04, 34.29)
#' unk.cq <- c(28.531, 29.331, 29.466,  29.597, 30.168, 30.258,
#'             30.535, 31.206, 31.279,  32.332, 33.096, 32.114)
#' nrt.cq <- c(32.76, 32.80, 31.51)
#' ntc.cq <- c(36.08, 35.56, 37.62)
#'
#' qPCR.plot(std.conc, std.cq, unk.cq, nrt.cq, ntc.cq, main="TFX (FAM)")
#'
#' qPCR.plot(std.conc, std.cq, unk.cq, nrt.cq, ntc.cq, main="TFX (FAM)",
#'           unk.ids = TRUE)
#'
#' qPCR.plot(std.conc, std.cq, unk.cq, nrt.cq, ntc.cq, main="TFX (FAM)",
#'           unk.replicates = TRUE,
#'           unk.ids = c("A","B","C","control"))
#'
#' # It's often convenient to scan in values copied from a spreadsheet, e.g.
#' # std.conc <- scan()
#'
#' # If no inputs are provided, the function simulates data and analysis,
#' # which can be useful when training people new to real-time PCR.
#' qPCR.plot()
#'

qPCR.plot <- function(std.conc=NULL, std.cq=NULL, unk.cq=NULL, nrt.cq=NULL, ntc.cq=NULL,
                      replicate.number = 3,
                      outlier.method = c("absolute","IQR"),
                      unk.replicates = FALSE,
                      error.bars = TRUE,
                      unk.ids = NULL,
                      main = NULL,
                      xlab = 'log10 template / ul',
                      ylab = 'Cq',
                      ...)
{ # Begin the function

  # Sub-function
  robust.rep <- function(x, times, ...) {
    if (is.null(times)) { return(NULL) } else {
      if (is.na(times)) { return(NULL) } else {
        if (times < 1) { return(NULL) } else {
          rep(x=x, times=times, ...)
        }
      }
    }
  }

  # If no data is provided, simulate something plausible
  if (is.null(std.conc) | is.null(std.cq)) {
    DataProvided <- FALSE
    warning('No data provided. Simulating values.')
    std.conc <- 4:6
    std.conc <- sort(robust.rep(std.conc,replicate.number))
    a <- -10/3 # slope
    b <- 40 # intercept
    noise.value <- 1 # standard deviation to add as noise to simulated replicates
    std.cq <- a*std.conc + b + rnorm(n=length(std.conc), mean=0, sd=noise.value)
    unk.cq <- sort(rep(runif(3,min=min(std.cq),max=max(std.cq)),replicate.number)) + rnorm(n=length(std.conc), mean=0, sd=noise.value)
  } else {
    DataProvided <- TRUE
  }

  # Linear model
  if (length(std.cq)==length(std.conc)) {
    model <- lm(std.cq~std.conc)
    # se.e <- sqrt(sum((std.cq - model$fitted.values)^2)/length(std.cq)) # Standard error of the estimate
  } else {
    stop("`std.conc` and `std.cq` must be equal in length. (See the help entry: `?qPCR.plot`.)")
  }
  if (length(std.cq) %% replicate.number != 0) {
    x <- length(std.cq) %% replicate.number
    s <- paste0("`std.conc` and `std.cq` must be multiples of `replicate.number`\n",
                "`replicate.number.` = ",replicate.number,"\n",
                "`length(std.cq) %% replicate.number` = ",x,"\n",
                "(See the help entry: `?qPCR.plot`=)")
    stop(s)
  }

  # Predict values for unknowns
  if (!is.null(unk.cq)) { predicted.conc <- (unk.cq - model$coefficients[1]) / model$coefficients[2] }

  # Title
  title.text <- ifelse(DataProvided,ifelse(is.null(main),'',main),'Simulated Data')

  # Find outliers, flagged as technical replicates more than 1.5 cycles from the median
  n <- sort(robust.rep(1:(length(std.cq)/replicate.number),replicate.number))
  if (outlier.method[1] == "absolute") {
    outliers <- unlist(by(std.cq,n,function(x){abs(x - median(x,na.rm=TRUE)) > 1.5}))
  } else {
    if (outlier.method[1] == "IQR") {
      outliers <- unlist(by(std.cq,n,function(x){abs(x - median(x,na.rm=TRUE)) > 1.5*IQR(x, na.rm = TRUE)}))
    } else {
      warning("")
      outliers <- unlist(by(std.cq,n,function(x){abs(x - median(x,na.rm=TRUE)) > 1.5}))
    }
  }

  # Find bounds for the plot
  if (is.null(unk.cq)) { xvalues <- std.conc ; yvalues <- std.cq }
    else { xvalues <- c(std.conc,predicted.conc) ; yvalues <- c(std.cq,unk.cq) }
  if (!is.null(nrt.cq)) { yvalues <- c(yvalues,mean(nrt.cq, na.rm=TRUE)) }
  if (!is.null(ntc.cq)) { yvalues <- c(yvalues,mean(ntc.cq, na.rm=TRUE)) }
  x.min <- min(xvalues,na.rm=TRUE) ; x.max <- max(xvalues,na.rm=TRUE)
  y.min <- min(yvalues,na.rm=TRUE) ; y.max <- max(yvalues,na.rm=TRUE)

  # Plot
  plot(x = std.conc, y = std.cq,
       type = "n",
       main=title.text,
       xlab=xlab, ylab=ylab,
       xlim=c(x.min,x.max), ylim=c(y.min,y.max),
       ... )
  points(x = std.conc[!outliers], y = std.cq[!outliers])
  points(x = std.conc[outliers], y = std.cq[outliers], pch=16, col='darkred')

  # Add negative control baselines
  min.control.cq <- NULL
  if (!is.null(nrt.cq)) {
    abline(h=min(nrt.cq, na.rm=TRUE), col='gray75')
    min.control.cq <- min(nrt.cq)
  }
  if (!is.null(ntc.cq)) {
    abline(h=min(ntc.cq, na.rm=TRUE), col='darkblue')
    min.control.cq <- min(c(min.control.cq,ntc.cq))
  }

  # Annotate with stats
  label.table <- data.frame(row.names = c('efficiency','slope','intercept','R^2') )
  label.table$value <- c(
    10^(-1/model$coefficients[2]),
    model$coefficients[2],
    model$coefficients[1],
    cor(std.cq,std.conc, use='complete.obs')^2
  )

  # Flag stats outside desired ranges
  label.table['efficiency','OK'] <- (label.table['efficiency','value'] < 2.2) & (label.table['efficiency','value'] > 1.8)
  label.table['slope','OK'] <- (label.table['slope','value'] < (-1/log10(2.2))) & (label.table['slope','value'] > (-1/log10(1.8)))
  label.table['intercept','OK'] <- (label.table['intercept','value'] < 60) & (label.table['intercept','value'] > 5)
  label.table['R^2','OK'] <- (label.table['R^2','value'] < 1) & (label.table['R^2','value'] > 0.9)
  abline(model, col=ifelse(label.table['slope','OK'],'gray65','darkred'))

  # Position text
  label.table$x1 <- x.max-(x.max-x.min)/3
  label.table$x2 <- x.max-(x.max-label.table$x1)/3
  label.table[2:4,'value'] <- signif(label.table[2:4,'value'],4)
  label.table['efficiency','value'] <- paste0(signif(50*label.table['efficiency','value'],3),'%')
  rect(xleft = label.table[1,'x1']-(x.max-x.min)*0.025, xright = x.max+(x.max-x.min)*0.025,
       ytop = y.max+(y.max-y.min)*0.025, ybottom = y.max-((y.max-y.min)/20)*5.25,
       col='white', border = NA)

  # Loop to write the text
  for (i in 1:length(row.names(label.table))) {
    name.i <- row.names(label.table)[i]
    text(x=label.table[i,'x1'], y=y.max-((y.max-y.min)/20)*(i-1),
         adj = c(0,1), cex=0.85, labels=paste0(name.i,':'),
         col = ifelse(label.table[i,'OK'],'black','darkred'))
    text(x=label.table[i,'x2'], y=y.max-((y.max-y.min)/20)*(i-1),
         adj = c(0,1), cex=0.85, labels=label.table[i,'value'],
         col = ifelse(label.table[i,'OK'],'black','darkred'))
  }

  # Plot unknowns
  unk.cq.range <- NULL
  unk.conc.ci <- NULL
  if (!is.null(unk.cq)) {
    if (is.logical(unk.replicates)) {
      if (unk.replicates) {
        unk.replicates <- replicate.number
      }
    }
    if (is.numeric(unk.replicates)) {
      if (length(unk.cq) %% unk.replicates == 0) {
        # Find the mean of unk.cq for each replicate
        n <- sort(robust.rep(1:(length(unk.cq)/unk.replicates),unk.replicates))
        avg <- c(by(unk.cq, n, mean, na.rm = TRUE))
        x <- c(by(unk.cq,n,function(x){max(abs(x - mean(x,na.rm=TRUE)))}))
        unk.cq.range <- data.frame(
          x0 = NA,
          y0 = avg-x,
          y1 = avg+x
        )
        inv.lm <- lm(std.conc ~ std.cq)
        x <- predict(inv.lm, data.frame(std.cq=avg), interval="confidence")
        unk.conc.ci <- data.frame(
          x0 = x[,2],
          x1 = x[,3],
          y0 = avg
        )
        unk.cq <- avg
      } else {
        x <- length(unk.cq) %% unk.replicates
        s <- paste0("`unk.cq` length must a multiple of `unk.replicates`\n",
                    "`unk.replicates` = ",unk.replicates,"\n",
                    "`length(unk.cq) %% unk.replicates` = ",x,"\n",
                    "Plotting all values of `unk.cq`. ",
                    "(See the help entry: `?qPCR.plot`)")
        warning(s)
      }
    }

    # Predict concentrations for unknown Cq values
    predicted.conc <- (unk.cq - model$coefficients[1]) / model$coefficients[2]
    above.zero <- robust.rep(TRUE,length(unk.cq))
    if (!is.null(min.control.cq)) {
      above.zero <- (unk.cq < min.control.cq)
      points(x = predicted.conc[!above.zero], y = unk.cq[!above.zero], pch=8, col="midnightblue")
    }
    points(x = predicted.conc[above.zero], y = unk.cq[above.zero], pch=16)

    # Add error bars
    if (error.bars & !(is.null(unk.conc.ci))) {
      unk.cq.range$x0 <- predicted.conc
      with(unk.cq.range, segments(x0=x0, y0=y0, y1=y1, col = "gray50") )
      with(unk.conc.ci, segments(x0=x0, x1=x1, y0=y0, col = "gray50") )
    }

    # Add unknown IDs
    if (!is.null(unk.ids)) {
      if (is.logical(unk.ids)) {
        if (unk.ids) {
          unk.ids <- 1:length(unk.cq)
        }
      } else {
        if (length(unk.ids) != length(unk.cq) ) {
          if (length(unk.ids) > length(unk.cq) ) { unk.ids <- unk.ids[1:length(unk.cq)] }
          warning("Length of `unk.ids` does not equal length of `unk.cq`. (See the help entry: `?qPCR.plot`.)")
        }
      }
      text(x = predicted.conc, y = unk.cq, labels = unk.ids,
           pos = 4, cex = 0.85, col = "grey25")
    }

  } # End  if (!is.null(unk.cq))
  # End of Plot unknowns section

  # Highlight outliers in stats table
  if (any(outliers)) {
    text(x=label.table[i,'x1'], y=y.max-((y.max-y.min)/20)*(i),
         adj = c(0,1), cex=0.85, labels='Potential outliers', col = 'darkred')
  }

  # Format output
  output <- list(
    stds = data.frame(
      conc = std.conc,
      cq = std.cq,
      outlier = outliers
    )
  )

  output$outlier.method = outlier.method[1]

  if(!is.null(min.control.cq)) {
    output$controls <- data.frame(
      type = c(robust.rep("NTC",length(ntc.cq)),robust.rep("NRT",length(nrt.cq))),
      cq   = c(ntc.cq,nrt.cq)
    )
  } else {
    above.zero <- robust.rep(NA,length(unk.cq))
  }

  if (!is.null(unk.cq)) {
    output$unknowns <- data.frame(
      cq = unk.cq,
      conc = predicted.conc,
      above.zero = above.zero
    )
  }

  output$diagnostics = label.table[,1:2]

  return(output)
}

