ggAddExtras <- function(xlab=NA, ylab=NA, main=NA){
  dots <- eval.parent(quote(list(...)))
  extras <- list()
  if("xlab"%in%names(dots) || is.null(xlab) || !is.na(xlab)){
    if("xlab"%in%names(dots)){
      extras[[length(extras)+1]] <- ggplot2::xlab(dots$xlab)
    }
    else{
      extras[[length(extras)+1]] <- ggplot2::xlab(xlab)
    }
  }
  if("ylab"%in%names(dots) || is.null(ylab) || !is.na(ylab)){
    if("ylab"%in%names(dots)){
      extras[[length(extras)+1]] <- ggplot2::ylab(dots$ylab)
    }
    else{
      extras[[length(extras)+1]] <- ggplot2::ylab(ylab)
    }
  }
  if("main"%in%names(dots) || is.null(main) || !is.na(main)){
    if("main"%in%names(dots)){
      extras[[length(extras)+1]] <- ggplot2::ggtitle(dots$main)
    }
    else{
      extras[[length(extras)+1]] <- ggplot2::ggtitle(main)
    }
  }
  if("xlim"%in%names(dots)){
    extras[[length(extras)+1]] <- ggplot2::xlim(dots$xlim)
  }
  if("ylim"%in%names(dots)){
    extras[[length(extras)+1]] <- ggplot2::ylim(dots$ylim)
  }
  return(extras)
}

autoplot.acf <- function(object, ci=0.95, ...){
  if (requireNamespace("ggplot2")){
    if (!inherits(object, "acf")){
      stop("autoplot.acf requires a acf object, use object=object")
    }

    data <- data.frame(Lag=object$lag,ACF=object$acf)
    if (data$Lag[1] == 0 & object$type == "correlation"){
      data <- data[-1,]
    }

    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x = ~Lag, xend = ~Lag, y = 0, yend = ~ACF),
                         data=data)
    p <- p + ggplot2::geom_hline(yintercept = 0)

    #Add data
    p <- p + ggplot2::geom_segment(lineend = "butt")

    #Add ci lines (assuming white noise input)
    ci <- qnorm((1 + ci)/2)/sqrt(object$n.used)
    p <- p + ggplot2::geom_hline(yintercept=c(-ci, ci), colour="blue", linetype="dashed")

    #Prepare graph labels
    if(object$series == "X"){
      ylab <- "CCF"
      ticktype <- "ccf"
      main <- paste("Series:",object$snames)
    }
    else if(object$type == "partial"){
      ylab <- "PACF"
      ticktype <- "acf"
      main <- paste("Series:",object$series)
    }
    else if(object$type == "correlation"){
      ylab <- "ACF"
      ticktype <- "acf"
      main <- paste("Series:",object$series)
    }
    else{
      ylab <- NULL
    }

    #Change ticks to be seasonal and prepare default title
    if(!is.null(object$series)){
      seriesname <- object$series
      if(object$series == "X"){
        seriesname <- strsplit(object$snames, " ")[[1]][1]
      }
      x <- eval.parent(parse(text=seriesname))
      freq <- frequency(x)
      msts <- is.element("msts",class(x))
    } else{
      freq <- 1
      msts <- FALSE
    }

    # Add seasonal x-axis
    if(msts)
    {
      periods <- attributes(x)$msts
      periods <- periods[periods != freq]
      minorbreaks <- periods * seq(-20:20)
    }
    else
      minorbreaks <- NULL
    p <- p + ggplot2::scale_x_continuous(breaks = seasonalaxis(freq,
      length(data$Lag), type=ticktype, plot=FALSE), minor_breaks=minorbreaks)
    p <- p + ggAddExtras(ylab=ylab, xlab="Lag", main=main)
    return(p)
  }
}

ggAcf <- function(x, lag.max = NULL,
                  type = c("correlation", "covariance", "partial"),
                  plot = TRUE, na.action = na.contiguous, demean=TRUE, ...){
  cl <- match.call()
  if(plot==TRUE){
    cl$plot=FALSE
  }
  cl[[1]] <- quote(Acf)
  object <- eval.parent(cl)
  if(plot==TRUE){
    return(autoplot(object,  ...))
  }
  else{
    return(object)
  }
}

ggPacf <- function(x, ...){
  ggAcf(x, type="partial", ...)
}

ggCcf <- function(x, y, lag.max=NULL, type=c("correlation","covariance"),
                  plot=TRUE, na.action=na.contiguous, ...){
  cl <- match.call()
  if(plot==TRUE){
    cl$plot <- FALSE
  }
  cl[[1]] <- quote(Ccf)
  object <- eval.parent(cl)
  object$snames <- paste(substitute(x), "&", substitute(y))
  if(plot==TRUE){
    return(autoplot(object, ...))
  }
  else{
    return(object)
  }
}

autoplot.mpacf <- function(object, ...){
  if (requireNamespace("ggplot2")){
    if (!inherits(object, "mpacf")){
      stop("autoplot.mpacf requires a mpacf object, use object=object")
    }
    if(!is.null(object$lower)){
      data <- data.frame(Lag=1:object$lag, z=object$z, sig=(object$lower<0 & object$upper>0))
      cidata <- data.frame(Lag=rep(1:object$lag,each=2) + c(-0.5,0.5), z=rep(object$z, each=2), upper=rep(object$upper, each=2), lower=rep(object$lower, each=2))
      plotpi <- TRUE
    }
    else{
      data <- data.frame(Lag=1:object$lag, z=object$z)
      plotpi <- FALSE
    }
    #Initialise ggplot object
    p <- ggplot2::ggplot()
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0), size=0.2)

    #Add data
    if(plotpi){
      p <- p + ggplot2::geom_ribbon(ggplot2::aes_(x = ~Lag, ymin = ~lower, ymax = ~upper), data=cidata, fill="grey50")
    }
    p <- p + ggplot2::geom_line(ggplot2::aes_(x = ~Lag, y = ~z), data=data)
    if(plotpi){
      p <- p + ggplot2::geom_point(ggplot2::aes_(x = ~Lag, y = ~z, colour = ~sig), data=data)
    }

    #Change ticks to be seasonal
    freq <- frequency(object$x)
    msts <- is.element("msts",class(object$x))

    # Add seasonal x-axis
    if(msts)
    {
      periods <- attributes(object$x)$msts
      periods <- periods[periods != freq]
      minorbreaks <- periods * seq(-20:20)
    }
    else
      minorbreaks <- NULL

    p <- p + ggplot2::scale_x_continuous(breaks = seasonalaxis(frequency(object$x), length(data$Lag), type="acf", plot=FALSE),
                              minor_breaks=minorbreaks)

    if(object$type=="partial"){
      ylab <- "PACF"
    }
    else if(object$type=="correlation"){
      ylab <- "ACF"
    }

    p <- p + ggAddExtras(ylab=ylab)

    return(p)
  }
}

ggtaperedacf <- function(x, lag.max=NULL, type=c("correlation", "partial"),
                         plot=TRUE, calc.ci=TRUE, level=95, nsim=100, ...){
  cl <- match.call()
  if(plot==TRUE){
    cl$plot=FALSE
  }
  cl[[1]] <- quote(taperedacf)
  object <- eval.parent(cl)
  if(plot==TRUE){
    return(autoplot(object, ...))
  }
  else{
    return(object)
  }
}

ggtaperedpacf <- function(x, ...){
  ggtaperedacf(x, type="partial", ...)
}

autoplot.Arima <- function (object, type = c("both", "ar", "ma"), ...){
  if (requireNamespace("ggplot2")){
    if (is.Arima(object)){
      #Detect type
      type <- match.arg(type)
      q <- p <- 0
      if (length(object$model$phi) > 0) {
        test <- abs(object$model$phi) > 1e-09
        if (any(test)){
          p <- max(which(test))
        }
      }
      if (length(object$model$theta) > 0) {
        test <- abs(object$model$theta) > 1e-09
        if (any(test)) {
          q <- max(which(test))
        }
      }

      if (type == "both") {
        if (p == 0)
          type <- "ma"
        else if (q == 0)
          type <- "ar"
      }
    }
    else if (inherits(object, "ar")){
      type <- "ar"
      p <- length(arroots(object)$roots)
      q <- 0
    }
    else{
      stop("autoplot.Arima requires an Arima object, use object=object")
    }

    #Check if no roots
    emptyplot <- ((p == 0 & q == 0) | (type == "ar" & (p == 0)) | (type == "ma" & (q == 0)))

    if (type == "both") {
      if (requireNamespace("grid")){
        type <- c("ar", "ma")
      }
      else{
        warning("Cannot do plots side by side, install grid package")
      }
    }

    #Initialise general ggplot object
    p <- ggplot2::ggplot()
    p <- p + ggplot2::coord_fixed(ratio = 1)
    p <- p + ggplot2::annotate("path", x=cos(seq(0,2*pi,length.out=100)),
                               y=sin(seq(0,2*pi,length.out=100)))
    p <- p + ggplot2::geom_vline(xintercept = 0)
    p <- p + ggplot2::geom_hline(yintercept = 0)
    p <- p + ggAddExtras(xlab = "Real", ylab="Imaginary")

    if(emptyplot)
      return(p + ggAddExtras(main = "No AR or MA roots"))

    allroots <- vector("list", length(type))

    for (i in 1:length(type)){
      if (type[i] == "ma"){
        allroots[[i]] <- data.frame(roots = 1/maroots(object)$roots)
      }
      else if (type[i] == "ar"){
        allroots[[i]] <- data.frame(roots = 1/arroots(object)$roots)
      }
      else{
        stop(paste("Unknown type:", type[i]))
      }
      allroots[[i]]$UnitCircle <- factor(ifelse((abs(1/allroots[[i]]$roots) > 1), "Within", "Outside"))
    }

    #Add data
    if (length(type)==1){
      p <- p + ggplot2::geom_point(ggplot2::aes_(x=~Re(roots), y=~Im(roots), colour=~UnitCircle), data=allroots[[1]], size=3)
      p <- p + ggAddExtras(main = paste("Inverse",toupper(type[1]),"roots"))
      return(p)
    }
    else{
      gridlayout <- matrix(seq(1, length(type)), ncol = length(type), nrow = 1)
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(gridlayout), ncol(gridlayout))))

      for (i in 1:length(type)){
        m <- p + ggplot2::geom_point(ggplot2::aes_(x=~Re(roots), y=~Im(roots), colour=~UnitCircle), data=allroots[[i]], size=3)
        m <- m + ggAddExtras(main = paste("Inverse",toupper(type[i]),"roots"))

        matchidx <- as.data.frame(which(gridlayout == i, arr.ind = TRUE))

        print(m, vp = grid::viewport(layout.pos.row = matchidx$row,
                               layout.pos.col = matchidx$col))
      }
    }
  }
}

autoplot.ar <- function(object, ...){
  autoplot.Arima(object, ...)
}

autoplot.decomposed.ts <- function (object, ...){
  if (requireNamespace("ggplot2")){
    data <- data.frame(datetime=rep(time(object$x),4), y=c(object$x, object$trend, object$seasonal, object$random),
                       decomposed=factor(rep(c("observed","trend","seasonal","random"),each=NROW(object$x)),
                                         levels=c("observed","trend","seasonal","random")))

    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x=~datetime, y=~y), data=data)

    #Add data
    p <- p + ggplot2::geom_line(na.rm=TRUE)
    p <- p + ggplot2::facet_grid(decomposed ~ ., scales="free_y", switch="y")

    p <- p + ggAddExtras(main = paste("Decomposition of",object$type,"time series"), xlab=NULL,
                         ylab="")

    return(p)
  }
}

autoplot.ets <- function (object, ...){
  if (requireNamespace("ggplot2")){
    if (!is.ets(object)){
      stop("autoplot.ets requires an ets object, use object=object")
    }

    names <- c(y="observed", l="level", b="slope", s1="season")
    data <- cbind(object$x, object$states[,colnames(object$states)%in%names(names)])
    cn <- c("y",c(colnames(object$states)))
    colnames(data) <- cn <- names[stats::na.exclude(match(cn, names(names)))]

    #Convert to longform
    data <- data.frame(datetime=rep(time(data),NCOL(data)), y=c(data),
                       parts=factor(rep(cn, each=NROW(data)), levels=cn))

    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x=~datetime, y=~y), data=data, ylab="")

    #Add data
    p <- p + ggplot2::geom_line(na.rm=TRUE)
    p <- p + ggplot2::facet_grid(parts ~ ., scales="free_y", switch="y")

    p <- p + ggAddExtras(xlab = NULL, ylab = "", main = paste("Decomposition by",object$method,"method"))
    return(p)
  }
}

autoplot.forecast <- function (object, include, plot.conf=TRUE, shadecols=c("#596DD5","#D5DBFF"), fcol="#0000AA", flwd=0.5, ...){
  if (requireNamespace("ggplot2")){
    if (!is.forecast(object)){
      stop("autoplot.forecast requires a forecast object, use object=object")
    }
    if(is.null(object$lower) | is.null(object$upper) | is.null(object$level)) {
      plot.conf <- FALSE
    }
    else if(!is.finite(max(object$upper))) {
      plot.conf <- FALSE
    }

    if (!is.null(object$model$terms) && !is.null(object$model$model)){
      #Initialise original dataset
      mt <- object$model$terms
      yvar <- deparse(mt[[2]]) # Perhaps a better way to do this
      xvar <- attr(mt,"term.labels")
      vars <- c(yvar=yvar, xvar=xvar)
      data <- object$model$model
      colnames(data) <- names(vars)[match(colnames(data), vars)]
      if(!is.null(object$model$lambda)){
        data$yvar <- InvBoxCox(data$yvar, object$model$lambda)
      }
    }
    else if (!is.null(object$x)){
      data <- data.frame(yvar=c(object$x))
      vars <- c(yvar="y")
    }
    else if (!is.null(object$residuals) && !is.null(object$fitted)){
      data <- data.frame(yvar=c(object$residuals+object$fitted))
      vars <- c(yvar="y")
    }
    else{
      stop("Could not find data")
    }

    #Initialise ggplot object
    p <- ggplot2::ggplot()

    # Cross sectional forecasts
    if (!is.element("ts",class(object$mean))){
      if (length(xvar) > 1){
        stop("Forecast plot for regression models only available for a single predictor")
      }
      if(NCOL(object$newdata)==1){ # Make sure column has correct name
        colnames(object$newdata) <- xvar
      }
      flwd <- 2*flwd # Scale for points

      #Data points
      p <- p + ggplot2::geom_point(ggplot2::aes_(x=~xvar, y=~yvar), data=data)
      p <- p + ggplot2::labs(y=vars["yvar"], x=vars["xvar"])

      #Forecasted intervals
      if (plot.conf){
        levels <- NROW(object$level)
        interval <- data.frame(xpred=rep(object$newdata[[1]],levels),lower=c(object$lower),upper=c(object$upper),level=object$level)
        interval<-interval[order(interval$level,decreasing = TRUE),] #Must be ordered for gg z-index
        p <- p + ggplot2::geom_linerange(ggplot2::aes_(x=~xpred, ymin=~lower, ymax=~upper, colour=~level), data=interval, size=flwd)
        if(length(object$level)<=5){
          p <- p + ggplot2::scale_colour_gradientn(breaks=object$level, colours = shadecols, guide="legend")
        }
        else{
          p <- p + ggplot2::scale_colour_gradientn(colours = shadecols, guide="colourbar")
        }
      }

      #Forecasted points
      predicted <- data.frame(object$newdata, object$mean)
      colnames(predicted) <- c("xpred", "ypred")
      p <- p + ggplot2::geom_point(ggplot2::aes_(x=~xpred, y=~ypred), data=predicted, color=fcol, size=flwd)

      #Line of best fit
      coef <- data.frame(int=0,m=0)
      i <- match("(Intercept)",names(object$model$coefficients))
      if (i!=0){
        coef$int <- object$model$coefficients[i]
        if (NROW(object$model$coefficients)==2){
          coef$m <- object$model$coefficients[-i]
        }
      }
      else{
        if (NROW(object$model$coefficients)==1){
          coef$m <- object$model$coefficients
        }
      }
      p <- p + ggplot2::geom_abline(intercept = coef$int, slope=coef$m)
    }
    else{
      # Time series objects (assumed)

      #Data points
      if(!is.null(time(object$x))){
        timex <- time(object$x)
      }
      else if (!is.null(time(object$model$residuals))){
        timex <- time(object$model$residuals)
      }
      data <- data.frame(yvar = as.numeric(data$yvar), datetime = as.numeric(timex))
      if(!missing(include))
        data <- tail(data, include)
      p <- p + ggplot2::scale_x_continuous()
      p <- p + ggplot2::geom_line(ggplot2::aes_(x=~datetime, y=~yvar), data=data) +
        ggplot2::labs(y=vars["yvar"], x="Time")

      #Forecasted intervals
      predicted <- data.frame(xvar = time(object$mean), yvar = object$mean)
      colnames(predicted) <- c("datetime","ypred")
      if (plot.conf){
        levels <- NROW(object$level)
        interval <- data.frame(datetime=rep(predicted$datetime,levels),lower=c(object$lower),upper=c(object$upper),level=rep(object$level,each=NROW(object$mean)))
        interval <- interval[order(interval$level,decreasing = TRUE),] #Must be ordered for gg z-index
        p <- p + ggplot2::geom_ribbon(ggplot2::aes_(x=~datetime, ymin=~lower, ymax=~upper, group=~-level, fill=~level),data=interval)
        if(min(object$level)<50){
          scalelimit <- c(1,99)
        }
        else{
          scalelimit <- c(50,99)
        }
        if(length(object$level)<=5){
          p <- p + ggplot2::scale_fill_gradientn(breaks=object$level, colours=shadecols, limit=scalelimit, guide="legend")
        }
        else{
          p <- p + ggplot2::scale_fill_gradientn(colours=shadecols, limit=scalelimit)
        }
        #Negative group is a work around for missing z-index
      }

      #Forecasted points
      p <- p + ggplot2::geom_line(ggplot2::aes_(x=~datetime,y=~ypred), data=predicted, color=fcol, size=flwd)
    }

    p <- p + ggAddExtras(main=paste("Forecasts from ",object$method,sep=""))
    return(p)
  }
}

autoplot.mforecast <- function (object, plot.conf=TRUE, gridlayout=NULL, ...){
  if (requireNamespace("ggplot2") & requireNamespace("grid")){
    if (!is.mforecast(object)){
      stop("autoplot.mforecast requires a mforecast object, use object=object")
    }

    K <- NCOL(object$x)
    if (K<2){
      warning("Expected at least two plots but forecast required less.")
    }

    #Set up vector arguments
    if (missing(plot.conf)){
      plot.conf <- rep(TRUE, K)
    }

    #Set up grid
    if (is.null(gridlayout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      gridlayout <- matrix(seq(1, K), ncol = 1, nrow = K)
    }

    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(gridlayout), ncol(gridlayout))))

    for (i in 1:K){
      partialfcast <- list(x=object$x[,i],mean=object$mean[[i]],method=object$method,
                           upper=object$upper[[i]], lower=object$lower[[i]], level=object$level, newdata=object$newdata)
      if (!is.null(object$model) &   inherits(object$model, "mlm")){
        partialfcast$model <- mlmsplit(object$model,index=i)
      }
      matchidx <- as.data.frame(which(gridlayout == i, arr.ind = TRUE))
      print(autoplot(structure(partialfcast,class="forecast"),
                     plot.conf=plot.conf[i], ...) + ggAddExtras(ylab=colnames(object$x)[i]),
            vp = grid::viewport(layout.pos.row = matchidx$row,
                          layout.pos.col = matchidx$col))
    }
  }
}

ggtsdisplay <- function(x, plot.type=c("partial","scatter","spectrum"),
                        points=TRUE, lag.max, na.action=na.contiguous, theme=NULL, ...){
  if (requireNamespace("ggplot2") & requireNamespace("grid")){
    plot.type <- match.arg(plot.type)

    if(!is.ts(x)){
      x <- ts(x)
    }
    if(missing(lag.max)){
      lag.max <- round(min(max(10*log10(length(x)), 3*frequency(x)), length(x)/3))
    }

    dots <- list(...)
    labs <- match(c("xlab", "ylab", "main"), names(dots), nomatch=0)

    #Set up grid for plots
    gridlayout <- matrix(c(1,2,1,3), nrow=2)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(gridlayout), ncol(gridlayout))))

    #Add ts plot with points
    matchidx <- as.data.frame(which(gridlayout == 1, arr.ind = TRUE))
    tsplot <- do.call(ggplot2::autoplot, c(object=quote(x), dots[labs]))
    if(points){
      tsplot <- tsplot + ggplot2::geom_point()
    }
    if(is.null(tsplot$labels$title)){ #Add title if missing
      tsplot <- tsplot + ggplot2::ggtitle(substitute(x))
    }
    if(!is.null(theme)){
      tsplot <- tsplot + theme
    }
    print(tsplot,
          vp = grid::viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))

    #Prepare Acf plot
    acfplot <- do.call(ggAcf, c(x=quote(x), lag.max=lag.max, na.action=na.action, dots[-labs])) + ggplot2::ggtitle(NULL)
    if(!is.null(theme)){
      acfplot <- acfplot + theme
    }

    #Prepare last plot (variable)
    if(plot.type == "partial"){
      lastplot <- ggPacf(x, lag.max=lag.max, na.action=na.action) + ggplot2::ggtitle(NULL)
      #Match y-axis
      acfplotrange <- ggplot2::ggplot_build(acfplot)$panel$ranges[[1]]$y.range
      pacfplotrange <- ggplot2::ggplot_build(lastplot)$panel$ranges[[1]]$y.range
      yrange <- range(c(acfplotrange, pacfplotrange))
      acfplot <- acfplot + ggplot2::ylim(yrange)
      lastplot <- lastplot + ggplot2::ylim(yrange)
    }
    else if(plot.type == "scatter"){
      scatterData <- data.frame(y = x[2:NROW(x)], x = x[1:NROW(x)-1])
      lastplot <- ggplot2::ggplot(ggplot2::aes_(y = ~y, x = ~x), data=scatterData) +
        ggplot2::geom_point() + ggplot2::labs(x = expression(Y[t-1]), y = expression(Y[t]))
    }
    else if(plot.type == "spectrum"){
      specData <- spec.ar(x, plot=FALSE)
      specData <- data.frame(spectrum = specData$spec, frequency = specData$freq)
      lastplot <- ggplot2::ggplot(ggplot2::aes_(y = ~spectrum, x = ~frequency), data=specData)+
        ggplot2::geom_line() + ggplot2::scale_y_log10()
    }
    if(!is.null(theme)){
      lastplot <- lastplot + theme
    }

    #Add ACF plot
    matchidx <- as.data.frame(which(gridlayout == 2, arr.ind = TRUE))
    print(acfplot,
          vp = grid::viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))

    #Add last plot
    matchidx <- as.data.frame(which(gridlayout == 3, arr.ind = TRUE))
    print(lastplot,
          vp = grid::viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))
  }
}

gglagplot <- function(x, lags = 1, set.lags = 1:lags, diag=TRUE, diag.col="gray", do.lines = TRUE, colour = TRUE, continuous = TRUE, labels = FALSE, seasonal = TRUE, ...){
  if (requireNamespace("ggplot2")){
    if(frequency(x)>1){
      linecol = cycle(x)
    }
    else{
      seasonal=FALSE
      continuous=TRUE
    }
    x <- as.matrix(x)

    #Prepare data for plotting
    n <- NROW(x)
    data <- data.frame()
    for(i in 1:NCOL(x)){
      for(lag in set.lags){
        sname <- colnames(x)[i]
        if(is.null(sname)){
          sname <- deparse(match.call()$x)
        }
        data <- rbind(data, data.frame(lagnum = 1:(n-lag), freqcur = ifelse(rep(seasonal,n-lag),linecol[(lag+1):n],(lag+1):n), orig = x[(lag+1):n,i], lagged = x[1:(n-lag),i], lag = rep(lag, n-lag), series = rep(sname, n-lag)))
      }
    }
    if(!continuous){
      data$freqcur <- factor(data$freqcur)
    }

    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x=~orig, y=~lagged), data=data)

    if(diag){
      p <- p + ggplot2::geom_abline(colour=diag.col, linetype="dashed")
    }

    if(labels){
      linesize = 0.25
    }
    else{
      linesize = 0.5
    }
    plottype <- if(do.lines){
      ggplot2::geom_path
    }
    else{
      ggplot2::geom_point
    }
    if(colour){
      p <- p + plottype(ggplot2::aes_(colour=~freqcur), size=linesize)
    }
    else{
      p <- p + plottype(size=linesize)
    }

    if(labels){
      p <- p + ggplot2::geom_text(ggplot2::aes_(label=~lagnum))
    }
    #Ensure all facets are of size size (if extreme values are excluded in lag specification)
    if(max(set.lags)>NROW(x)/2){
      axissize <- rbind(aggregate(orig ~ series, data=data, min),aggregate(orig~ series, data=data, max))
      axissize <- data.frame(series = rep(axissize$series, length(set.lags)), orig = rep(axissize$orig, length(set.lags)), lag = rep(set.lags, each=NCOL(x)))
      p <- p + ggplot2::geom_blank(ggplot2::aes_(x=~orig, y=~orig), data=axissize)
    }
    #Facet
    if(NCOL(x)>1){
      p <- p + ggplot2::facet_wrap(series~lag, scales = "free", labeller = function(labels) list(unname(unlist(do.call("Map", c(list(paste, sep=", lag "), lapply(labels, as.character)))))))
    }
    else{
      p <- p + ggplot2::facet_wrap(~lag, labeller = function(labels) lapply(labels, function(x) paste0("lag ",as.character(x))))
    }
    p <- p + ggplot2::theme(aspect.ratio=1)
    if(colour){
      if(continuous){
        p <- p + ggplot2::guides(colour = ggplot2::guide_colourbar(title=ifelse(seasonal, "season", "time")))
      }
      else{
        p <- p + ggplot2::guides(colour = ggplot2::guide_legend(title=ifelse(seasonal, "season", "time")))
      }
    }

    p <- p + ggAddExtras(ylab = NULL, xlab = NULL)

    return(p)
  }
}

gglagchull <- function(x, lags = 1, set.lags = 1:lags, diag=TRUE, diag.col="gray", ...){
  if (requireNamespace("ggplot2")){
    x <- as.matrix(x)

    #Prepare data for plotting
    n <- NROW(x)
    data <- data.frame()
    for(i in 1:NCOL(x)){
      for(lag in set.lags){
        sname <- colnames(x)[i]
        if(is.null(sname)){
          sname <- substitute(x)
        }
        data <- rbind(data, data.frame(orig = x[(lag+1):n,i], lagged = x[1:(n-lag),i], lag = rep(lag, n-lag), series = rep(sname, n-lag))[grDevices::chull(x[(lag+1):n,i], x[1:(n-lag),i]),])
      }
    }

    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x=~orig, y=~lagged), data=data)
    if(diag){
      p <- p + ggplot2::geom_abline(colour=diag.col, linetype="dashed")
    }
    p <- p + ggplot2::geom_polygon(ggplot2::aes_(group=~lag,colour=~lag,fill=~lag), alpha=1/length(set.lags))
    p <- p + ggplot2::guides(colour = ggplot2::guide_colourbar(title="lag"))
    p <- p + ggplot2::theme(aspect.ratio=1)

    #Facet
    if(NCOL(x)>1){
      p <- p + ggplot2::facet_wrap(~series, scales = "free")
    }

    p <- p + ggAddExtras(ylab = "lagged", xlab = "original")

    return(p)
  }
}

ggmonthplot <- function (x, labels = NULL, times = time(x), phase = cycle(x), ...){
  if (requireNamespace("ggplot2")){
    if (!inherits(x, "ts")){
      stop("ggmonthplot requires a ts object, use x=object")
    }

    data <- data.frame(y=as.numeric(x),year=factor(trunc(time(x))),season=as.numeric(phase))
    avgLines <- stats::aggregate(data$y, by=list(data$season), FUN=mean)
    colnames(avgLines) <- c("season", "avg")
    data <- merge(data, avgLines, by="season")

    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x=~interaction(year, season), y=~y, group=~season), data=data, na.rm=TRUE)

    #Remove vertical break lines
    p <- p + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

    #Add data
    p <- p + ggplot2::geom_line()

    #Add average lines
    p <- p + ggplot2::geom_line(ggplot2::aes_(y=~avg), col="#0000AA")

    #Create x-axis labels
    xfreq <- frequency(x)
    if(xfreq==4){
      xbreaks <- c("Q1","Q2","Q3","Q4")
      xlab <- "Quarter"
    }
    else if (xfreq==7){
      xbreaks <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                   "Friday", "Saturday")
      xlab <- "Day"
    }
    else if(xfreq==12){
      xbreaks <- month.abb
      xlab <- "Month"
    }
    else{
      xbreaks <- 1:frequency(x)
      xlab <- "Season"
    }

    midYear <- sort(levels(data$year))[length(levels(data$year))%/%2]
    p <- p + ggplot2::scale_x_discrete(breaks=paste(midYear,".",1:xfreq,sep=""), labels=xbreaks)

    #Graph labels
    p <- p + ggAddExtras(ylab = deparse(substitute(x)), xlab = xlab)
    return(p)
  }
}

ggseasonplot <- function (x, year.labels=FALSE, year.labels.left=FALSE, type=NULL, col=NULL, continuous=FALSE, labelgap=0.04, ...){
  if (requireNamespace("ggplot2")){
    if (!inherits(x, "ts")){
      stop("autoplot.seasonplot requires a ts object, use x=object")
    }
    if(!is.null(type)){
      message("Plot types are not yet supported for seasonplot()")
    }

    # Check data are seasonal
    s <- frequency(x)
    if(s <= 1)
      stop("Data are not seasonal")

    data <- data.frame(y=as.numeric(x),year=trunc(time(x)),time=as.numeric(round(time(x)%%1,digits = 6)))
    data$year <- if(continuous){
      as.numeric(data$year)
    }
    else{
      as.factor(data$year)
    }
    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x=~time, y=~y, group=~year, colour=~year), data=data, na.rm=TRUE)
    #p <- p + ggplot2::scale_x_continuous()

    #Add data
    p <- p + ggplot2::geom_line()

    if(!is.null(col)){
      if(continuous){
        p <- p + ggplot2::scale_color_gradientn(colours=col)
      }
      else{
        ncol <- length(unique(data$year))
        if(length(col)==1){
          p <- p + ggplot2::scale_color_manual(guide="none", values=rep(col, ncol))
        }
        else{
          p <- p + ggplot2::scale_color_manual(values=rep(col, ceiling(ncol/length(col)))[1:ncol])
        }
      }
    }

    if(year.labels){
      yrlab <- stats::aggregate(time ~ year, data=data, FUN = max)
      yrlab <- cbind(yrlab, offset=labelgap)
    }
    if(year.labels.left){
      yrlabL <- stats::aggregate(time ~ year, data=data, FUN = min)
      yrlabL <- cbind(yrlabL, offset=-labelgap)
      if(year.labels){
        yrlab <- rbind(yrlab, yrlabL)
      }
    }
    if(year.labels | year.labels.left){
      yrlab <- merge(yrlab, data)
      yrlab$time <- yrlab$time+yrlab$offset
      p <- p + ggplot2::guides(colour=FALSE)
      p <- p + ggplot2::geom_text(ggplot2::aes_(x=~time, y=~y, label=~year), data=yrlab)
    }

    # Add seasonal labels
    if(s == 12)
    {
      labs <- month.abb
      xLab <- "Month"
    }
    else if(s == 4)
    {
      labs <- paste("Q",1:4,sep="")
      xLab <- "Quarter"
    }
    else if(s == 7)
    {
      labs <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
      xLab <- "Day"
    }
    else
    {
      labs <- NULL
      xLab <- "Season"
    }
    p <-  p + ggplot2::scale_x_continuous(breaks=sort(unique(data$time)), minor_breaks=NULL, labels=labs)

    #Graph title and axes
    p <- p + ggAddExtras(main=paste("Seasonal plot:", deparse(substitute(x))), xlab=xLab, ylab=NULL)
    return(p)
  }
}

autoplot.splineforecast <- function (object, plot.conf=TRUE, ...){
  p <- autoplot.forecast(object, plot.conf=plot.conf, ...)
  fit <- data.frame(datetime=as.numeric(time(object$fitted)),y=as.numeric(object$fitted))
  p <- p + ggplot2::geom_point(ggplot2::aes_(x=~datetime,y=~y),data=fit,size=2)
  p <- p + ggAddExtras(ylab=deparse(object$model$call$x))
  return(p)
}

autoplot.stl <- function (object, labels = NULL, ...){
  if (requireNamespace("ggplot2")){
    if (!inherits(object, "stl")){
      stop("autoplot.stl requires a stl object, use x=object")
    }
    if(is.null(labels)){
      labels <- colnames(object$time.series)
    }

    data <- object$time.series
    cn <- c("data",labels)
    data <- data.frame(datetime=rep(time(data),NCOL(data)+1), y=c(rowSums(data),data),
                       parts=factor(rep(cn, each=NROW(data)), levels=cn))

    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x=~datetime, y=~y), data=data)

    #Add data
    p <- p + ggplot2::geom_line(ggplot2::aes_(x=~datetime, y=~y), data=subset(data,data$parts!="remainder"), na.rm=TRUE)
    p <- p + ggplot2::geom_segment(ggplot2::aes_(x = ~datetime, xend = ~datetime, y = 0, yend = ~y),
                                   data=subset(data,data$parts=="remainder"), lineend = "butt")
    p <- p + ggplot2::facet_grid("parts ~ .", scales="free_y", switch="y")
    p <- p + ggplot2::geom_hline(ggplot2::aes_(yintercept = ~y), data=data.frame(y = 0, parts = "remainder"))

    # Add axis labels
    p <- p + ggAddExtras(xlab="Time", ylab="")

    # Make x axis contain only whole numbers (e.g., years)
    p <- p + ggplot2::scale_x_continuous(breaks=unique(round(pretty(data$datetime))))

    return(p)
  }
}

autoplot.ts <- function(object, ...){
  if(requireNamespace("ggplot2")){
    if(!is.ts(object)){
      stop("autoplot.ts requires a ts object, use object=object")
    }

    # Create data frame with time as a column labelled x
    # and time series as a column labelled y.
    data <- data.frame(y = as.numeric(object), x = as.numeric(time(object)))

    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(y=~y, x=~x), data=data)

    #Add data
    p <- p + ggplot2::geom_line()

    # Add labels
    p <- p + ggAddExtras(xlab="Time", ylab=deparse(substitute(object)))

    # Make x axis contain only whole numbers (e.g., years)
    p <- p + ggplot2::scale_x_continuous(breaks=unique(round(pretty(data$x))))
    return(p)
  }
}

autoplot.mts <- function(object, facets=FALSE, ...){
  if(requireNamespace("ggplot2")){
    if(!stats::is.mts(object)){
      stop("autoplot.mts requires a mts object, use x=object")
    }
    data <- data.frame(y=as.numeric(c(object)), x=rep(as.numeric(time(object)),NCOL(object)),
                       series=factor(rep(colnames(object), each=NROW(object)), levels=colnames(object)))
    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(y=~y, x=~x), data=data)
    if(facets){
      p <- ggplot2::ggplot(ggplot2::aes_(y=~y, x=~x, group=~series), data=data)
      p <- p + ggplot2::geom_line() + ggplot2::facet_grid(series~., scales = "free_y")
    }
    else{
      p <- ggplot2::ggplot(ggplot2::aes_(y=~y, x=~x, group=~series, colour=~series), data=data)
      p <- p + ggplot2::geom_line()
    }

    p <- p + ggAddExtras(xlab="Time", ylab=deparse(substitute(object)))
    return(p)
  }
}

fortify.ts <- function(model, data, ...)
{
  # Use ggfortify version if it is loaded
  # to prevent cran errors
  if(exists("ggfreqplot"))
  {
    tsp <- attr(model, which = "tsp")
    dtindex <- time(model)
    if (any(tsp[3] == c(4, 12)))
      dtindex <- zoo::as.Date.yearmon(dtindex)
    model <- data.frame(Index = dtindex, Data = as.numeric(model))
    return(ggplot2::fortify(model))
  }
  else
  {
    model <- cbind(x = as.numeric(time(model)), y = as.numeric(model))
    as.data.frame(model)
  }
}

fortify.forecast <- function(model, data=as.data.frame(model), PI=TRUE, ...){
  # Use ggfortify version if it is loaded
  # to prevent cran errors
  if(exists("ggfreqplot"))
  {
    n <- length(model$x)
    h <- length(model$mean)
    out <- matrix(NA, nrow=n+h, ncol=4+2*length(model$level))
    out[1:n,2] <- model$x
    out[1:n,3] <- model$fitted
    forecasted <- as.data.frame(model)
    out[n+(1:h),4:NCOL(out)] <- as.matrix(forecasted)
    colnames(out) <- c("Index","Data","Fitted",colnames(forecasted))
    out <- as.data.frame(out)
    tsp <- attr(model$x, which = "tsp")
    dtindex <- seq(from = tsp[1], length = n+h, by = 1/tsp[3])
    if (any(tsp[3] == c(4, 12)))
      dtindex <- zoo::as.Date.yearmon(dtindex)
    out[,1] <- dtindex
    return(ggplot2::fortify(out))
  }
  Hiloc <- grep("Hi ", names(data))
  Loloc <- grep("Lo ", names(data))
  if(PI & !is.null(model$level)){
    if(length(Hiloc)==length(Loloc)){
      if(length(Hiloc)>0){
        return(data.frame(x=rep(as.numeric(time(model$mean)), length(Hiloc)+1),
                          y=c(rep(NA,NROW(data)*(length(Hiloc))),data[,1]),
                          level=c(as.numeric(rep(gsub("Hi ","",names(data)[Hiloc]), each=NROW(data))), rep(-Inf,NROW(data))),
                          ymax=c(unlist(data[,Hiloc]),rep(NA,NROW(data))), ymin=c(unlist(data[,Loloc]),rep(NA,NROW(data)))))
      }
    }
    else{
      warning("missing intervals detected, plotting point predictions only")
    }
  }
  return(data.frame(x=as.numeric(time(model$mean)), y=as.numeric(model$mean), level=rep(-Inf,NROW(model$mean))))
}

StatForecast <- ggplot2::ggproto("StatForecast", ggplot2::Stat,
  required_aes = c("x","y"),
  compute_group = function(data, scales, params, plot.conf=TRUE, h=NULL,
                           level=c(80,95), fan=FALSE, robust=FALSE, lambda=NULL,
                           find.frequency=FALSE, allow.multiplicative.trend=FALSE, ...) {
    tspx <- recoverTSP(data$x)
    if(is.null(h)){
      h <- ifelse(tspx[3] > 1, 2 * tspx[3], 10)
    }
    tsdat <- ts(data = data$y, start = tspx[1], frequency = tspx[3])
    fcast <- forecast(tsdat, h=h, level=level, fan=fan, robust=robust,
                      lambda=lambda, find.frequency=find.frequency,
                      allow.multiplicative.trend=allow.multiplicative.trend)
    fcast <- fortify(fcast, PI=plot.conf)
    suppressWarnings(fcast <- cbind(fcast,data[1,!colnames(data)%in%colnames(fcast)]))
    fcast
  }
)

GeomForecast <- ggplot2::ggproto("GeomForecast", ggplot2::Geom,
  required_aes = c("x","y"),
  default_aes = ggplot2::aes(colour = "#868FBD", fill = "grey60", size = .5,
    linetype = 1, weight = 1, alpha = 1),
  draw_key = function(data, params, size){
    lwd <- min(data$size, min(size) / 4)

    grid::rectGrob(
      width = unit(1, "npc") - unit(lwd, "mm"),
      height = unit(1, "npc") - unit(lwd, "mm"),
      gp = grid::gpar(
        col = data$colour,
        fill = alpha(data$colour, data$alpha),
        lty = data$linetype,
        lwd = lwd * .pt,
        linejoin = "mitre"
      ))
  },
  handle_na = function(self, data, params){
    data
  },
  setup_data = function(data, params){
    if(any(is.finite(data$level))){ # if there are finite confidence levels (point forecasts are non-finite)
      data$group <- -as.numeric(factor(interaction(data$group, data$level))) # multiple group levels
      levels <- suppressWarnings(as.numeric(data$level))
      if(min(levels[is.finite(levels)])<50){
        data$scalefill <- scales::rescale(levels, from = c(1,99))
      }
      else{
        data$scalefill <- scales::rescale(levels, from = c(50,99))
      }
    }
    data
  },

  draw_group = function(data, panel_scales, coord){
    col <- data$colour[1]
    altcol <- col2rgb(col)
    altcol <- rgb2hsv(altcol[[1]],altcol[[2]],altcol[[3]])

    if(any(is.finite(data$level))){
      plot.ci <- TRUE
      altcol1 <- colorspace::hex(colorspace::HSV(altcol[1]*360, 7/12, 5/6))
      altcol2 <- colorspace::hex(colorspace::HSV(altcol[1]*360, 1/6, 1))
      intervalpred <- transform(data[,-match("y", colnames(data))], colour = NA,
                                fill = scales::gradient_n_pal(c(altcol1,altcol2))(data$scalefill[1]))
    }
    else{
      plot.ci <- FALSE
      if(any(c("ymax","ymin")%in%colnames(data))){
        data <- data[,-match(c("level","ymax","ymin"), colnames(data))]
      }
      linecol <- colorspace::hex(colorspace::HSV(altcol[1]*360, 1, 2/3))
      pointpred <- transform(data, group = -1, fill = NA, colour = linecol)
    }
    #Draw forecasted points and intervals
    ggplot2:::ggname("geom_forecast",
      grid::grobTree(if(plot.ci)GeomRibbon$draw_group(intervalpred, panel_scales, coord),
               if(!plot.ci)GeomLine$draw_panel(pointpred, panel_scales, coord)
    ))
  }
)

geom_forecast <- function(mapping = NULL, data = NULL, stat = "forecast",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, plot.conf=TRUE, h=NULL, level=c(80,95), fan=FALSE,
                          robust=FALSE, lambda=NULL, find.frequency=FALSE,
                          allow.multiplicative.trend=FALSE, series, ...) {
  if(is.forecast(mapping)){
    if(stat=="forecast"){
      stat <- "identity"
    }
    plot.conf <- plot.conf & !is.null(mapping$level)
    data <- fortify(mapping, PI=plot.conf)
    mapping <- ggplot2::aes_(x = ~x, y = ~y)
    if(plot.conf){
      mapping$level <- quote(level)
      mapping$group <- quote(-level)
      mapping$ymin <- quote(ymin)
      mapping$ymax <- quote(ymax)
    }
  }
  else if(is.mforecast(mapping)){
    #Convert mforecast to list of forecast
    #return lapply of geom_forecast with params on list
    stop("mforecast objects not yet supported. Try calling geom_forecast() for several forecast objects")
  }
  else if(is.ts(mapping)){
    data <- data.frame(y = as.numeric(mapping), x = as.numeric(time(mapping)))
    mapping <- ggplot2::aes_(y=~y, x=~x)
  }
  if(!missing(series)){
    data <- transform(data, series=series)
  }
  if(stat=="forecast"){
    ggplot2::layer(
      geom = GeomForecast, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(plot.conf=plot.conf, h=h, level=level, fan=fan, robust=robust,
                    lambda=lambda, find.frequency=find.frequency,
                    allow.multiplicative.trend=allow.multiplicative.trend,
                    na.rm = na.rm, ...)
    )
  }
  else{
    ggplot2::layer(
      geom = GeomForecast, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }
}
