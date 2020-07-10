#' Plot raw qPCR data
#'
#' This function takes raw realtime PCR data and produces a convenient plot and table,
#' which can be used to assess the quality of the data.
#'
#' The function takes information on the standard concentrations (unitless) and Cq values
#' measured by the realtime PCR instrument. A linear regression provides intercept and
#' slope, from which PCR efficiency is calculated. The correlation coefficient is also
#' displayed.
#'
#' Potential outlier values in the standard Cq values are highlighted on the plot in red.
#' These are flagged as technical replicate more than 1.5 cycles from the median.
#' Efficiency values outside the range of 1.8 to 2.2 (90% - 110%) are also highlighted,
#' as are correlation coefficients (R^2) less than 0.9.
#'
#' If they are provided, the mean negative control values (NRT and NTC) are ploted as horizontal lines
#' (gray and blue, respectively).
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param std.conc A numeric vector providing the concentrations of the standards used.
#' @param std.cq A numeric vector of Cq values measured for the standard samples.
#' @param unk.cq A numeric vector of Cq values measured for unknown samples.
#' @param nrt.cq A numeric vector of Cq values measured for no-reverse transcription control (NRT) samples.
#' @param ntc.cq A numeric vector of Cq values measured for no-template control (NTC) samples.
#' @param main An overall title for the plot.
#' @param xlab A title for the x axis.
#' @param ylab A title for the y axis.
#'
#' @export
#'
#' @examples
#' std.conc <- c(rep(6,3),rep(5,3),rep(4,3),rep(3,3))
#' std.cq <- c(26.74, 27.33, 25.83, 29.42, 29.90, 28.10, 33.15, 33.31, 31.41, 34.56, 34.64, 34.89)
#' unk.cq <- c(28.031, 29.331, 29.466, 29.597, 30.168, 30.258, 30.535, 31.706, 31.779)
#' nrt.cq <- c(32.76, 32.80, 31.51)
#' ntc.cq <- c(37.08, 36.56, 38.62)
#'
#' qPCR.plot(std.conc, std.cq, unk.cq, nrt.cq, ntc.cq, main="TFX (FAM)")
#'
#' # In practice, it's usually convenient to scan in values copied from a spreadsheet
#' # e.g.
#' # std.conc <- scan()
#'
#' # If no inputs are provided, the function simulates data and analysis,
#' # which can be useful when training people new to realtime PCR.
#' qPCR.plot()
#'

qPCR.plot <- function(std.conc=NULL, std.cq=NULL, unk.cq=NULL, nrt.cq=NULL, ntc.cq=NULL,
                      main=NULL,
                      xlab='log10 template / ul',
                      ylab='Cq')
{
  # If no data is provided, simulutate something plausible
  if (is.null(std.conc) | is.null(std.cq)) {
    DataProvided <- FALSE
    warning('No data provided. Simulating values.')
    std.conc <- 4:6
    std.conc <- sort(rep(std.conc,3))
    a <- -10/3 # slope
    b <- 40 # intercept
    noise.value <- 1 # standard deviation to add as noise to simulated replicates
    std.cq <- a*std.conc + b + rnorm(n=length(std.conc), mean=0, sd=noise.value)
    unk.cq <- sort(rep(runif(3,min=min(std.cq),max=max(std.cq)),3)) + rnorm(n=length(std.conc), mean=0, sd=noise.value)
  } else {
    DataProvided <- TRUE
  }
  # Linear model
  model <- lm(std.cq~std.conc)
  # Predict values for unknowns
  if (!is.null(unk.cq)) { predicted.conc <- (unk.cq - model$coefficients[1]) / model$coefficients[2] }
  # Plot
  title.text <- ifelse(DataProvided,ifelse(is.null(main),'',main),'Simulated Data')
  # Find outliers, flagged as technical replicates more than 1.5 cycles from the median
  if (length(std.cq) %% 3 == 0) {
    n <- sort(rep(1:(length(std.cq)/3),3))
    outliers <- unlist(by(std.cq,n,function(x){abs(x - median(x,na.rm=TRUE)) > 1.5}))
  }

  if (is.null(unk.cq)) { xvalues <- std.conc ; yvalues <- std.cq }
    else { xvalues <- c(std.conc,predicted.conc) ; yvalues <- c(std.cq,unk.cq) }
  if (!is.null(nrt.cq)) { yvalues <- c(yvalues,mean(nrt.cq, na.rm=TRUE)) }
  if (!is.null(ntc.cq)) { yvalues <- c(yvalues,mean(ntc.cq, na.rm=TRUE)) }
  x.min <- min(xvalues,na.rm=TRUE) ; x.max <- max(xvalues,na.rm=TRUE)
  y.min <- min(yvalues,na.rm=TRUE) ; y.max <- max(yvalues,na.rm=TRUE)
  plot(std.cq~std.conc, main=title.text, xlab=xlab, ylab=ylab, xlim=c(x.min,x.max), ylim=c(y.min,y.max) )
  points(std.cq[outliers]~std.conc[outliers], pch=16, col='darkred')
  # Add negative control baselines
  if (!is.null(nrt.cq)) { abline(h=mean(nrt.cq, na.rm=TRUE), col='gray75') }
  if (!is.null(ntc.cq)) { abline(h=mean(ntc.cq, na.rm=TRUE), col='darkblue') }
  # Annotate with stats
  label.table <- data.frame(row.names = c('Efficiency','Slope','Intercept','R^2') )
  label.table$value <- c(
    10^(-1/model$coefficients[2]),
    model$coefficients[2],
    model$coefficients[1],
    cor(std.cq,std.conc, use='complete.obs')^2
  )
  # Flag stats outside desired ranges
  label.table['Efficiency','OK'] <- (label.table['Efficiency','value'] < 2.2) & (label.table['Efficiency','value'] > 1.8)
  label.table['Slope','OK'] <- (label.table['Slope','value'] < (-1/log10(2.2))) & (label.table['Slope','value'] > (-1/log10(1.8)))
  label.table['Intercept','OK'] <- (label.table['Intercept','value'] < 60) & (label.table['Intercept','value'] > 5)
  label.table['R^2','OK'] <- (label.table['R^2','value'] < 1) & (label.table['R^2','value'] > 0.9)
  abline(model, col=ifelse(label.table['Slope','OK'],'gray65','darkred'))
  # Position text
  label.table$x1 <- x.max-(x.max-x.min)/3
  label.table$x2 <- x.max-(x.max-label.table$x1)/3
  label.table[2:4,'value'] <- signif(label.table[2:4,'value'],4)
  label.table['Efficiency','value'] <- paste0(signif(50*label.table['Efficiency','value'],3),'%')
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
  if (!is.null(unk.cq)) {
    predicted.conc <- (unk.cq - model$coefficients[1]) / model$coefficients[2]
    points(unk.cq~predicted.conc, pch=16)
  }
  # Highlight outliers
  if (any(outliers)) {
    text(x=label.table[i,'x1'], y=y.max-((y.max-y.min)/20)*(i),
         adj = c(0,1), cex=0.85, labels='Potential outliers', col = 'darkred')
  }
  return(label.table[,1:2])
}

