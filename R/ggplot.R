autoplot.acf <- function (object, ci=0.95, main=NULL, xlab=NULL, ylab=NULL, ...){
  if (requireNamespace("ggplot2")){
    if (!inherits(object, "acf")){
      stop("autoplot.acf requires a acf object, use object=object")
    }
    
    data <- data.frame(Lag=object$lag,ACF=object$acf)
    if (data$Lag[1] == 0){
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
    
    #Change ticks to be seasonal
    p <- p + ggplot2::scale_x_continuous(breaks = (1:NROW(data)%/%4)*4)
    
    #Graph title
    if (is.null(main)){
      main <- paste("Series:",object$series)
    }
    p <- p + ggplot2::ggtitle(main)
    
    #Graph labels
    if (!is.null(xlab)){
      p <- p + ggplot2::xlab(xlab)
    }
    if (is.null(ylab)){
      if(object$type == "correlation"){
        ylab <- "ACF"
      }
      else{
        ylab <- "Partial ACF"
      }
    }
    p <- p + ggplot2::ylab(ylab)
    return(p)
  }
}

autoplot.Arima <- function (object, type = c("both", "ar", "ma"), main=NULL, xlab="Real", ylab="Imaginary", ...){
  if (requireNamespace("ggplot2")){
    if (!is.Arima(object)){
      stop("autoplot.Arima requires an Arima object, use object=object")
    }
    
    #Prepare data
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
    if ((p == 0 & q == 0) | (type == "ar" & (p == 0)) | (type == "ma" & (q == 0))){
      stop("No roots to plot")
    }
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
      p <- p + ggplot2::labs(title = paste("Inverse",toupper(type[1]),"roots"),
                             x = xlab, y = ylab)
      return(p)
    }
    else{
      gridlayout <- matrix(seq(1, length(type)), ncol = length(type), nrow = 1)
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(gridlayout), ncol(gridlayout))))
      
      for (i in 1:length(type)){
        m <- p + ggplot2::geom_point(ggplot2::aes_(x=~Re(roots), y=~Im(roots), colour=~UnitCircle), data=allroots[[i]], size=3)
        m <- m + ggplot2::labs(title = paste("Inverse",toupper(type[i]),"roots"),
                               x = xlab, y = ylab)
        
        matchidx <- as.data.frame(which(gridlayout == i, arr.ind = TRUE))
        
        print(m, vp = grid::viewport(layout.pos.row = matchidx$row,
                               layout.pos.col = matchidx$col))
      }
    }
  }
}

autoplot.decomposed.ts <- function (object, main=NULL, xlab=NULL, ylab=NULL, ...){
  if (requireNamespace("ggplot2")){
    data <- data.frame(datetime=rep(time(object$x),4), y=c(object$x, object$trend, object$seasonal, object$random),
                       decomposed=factor(rep(c("observed","trend","seasonal","random"),each=NROW(object$x)),
                                         levels=c("observed","trend","seasonal","random")))
    
    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x=~datetime, y=~y), data=data)
    
    #Add data
    p <- p + ggplot2::geom_line(na.rm=TRUE)
    p <- p + ggplot2::facet_grid(decomposed ~ ., scales="free_y", switch="y")
    
    #Graph title
    if (is.null(main)){
      main <- paste("Decomposition of",object$type,"time series")
    }
    p <- p + ggplot2::ggtitle(main)
    
    #Graph labels
    if (!is.null(xlab)){
      p <- p + ggplot2::xlab(xlab)
    }
    if (!is.null(ylab)){
      p <- p + ggplot2::ylab(ylab)
    }
    else{
      p <- p + ggplot2::ylab("")
    }
    
    return(p)
  }
}

autoplot.ets <- function (object, main=NULL, xlab=NULL, ylab=NULL, ...){
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
    
    #Graph title
    if (is.null(main)){
      main <- paste("Decomposition by",object$method,"method")
    }
    p <- p + ggplot2::ggtitle(main)
    
    #Graph labels
    if (!is.null(xlab)){
      p <- p + ggplot2::xlab(xlab)
    }
    if (!is.null(ylab)){
      p <- p + ggplot2::ylab(ylab)
    }
    else{
      p <- p + ggplot2::ylab("")
    }
    return(p)
  }
}

autoplot.forecast <- function (object, plot.conf=TRUE, shadecols=c("#868FBD","#BEC1D4"), fcol="#0000FF", flwd=1, main=NULL, xlab=NULL, ylab=NULL, ...){
  if (requireNamespace("ggplot2")){
    if (!is.forecast(object)){
      stop("autoplot.forecast requires a forecast object, use object=object")
    }
    if(is.null(object$lower) | is.null(object$upper) | is.null(object$level) | (!is.finite(max(object$upper)))){
      plot.conf=FALSE
    }
    
    if (!is.null(object$model$terms) && !is.null(object$model$model)){
      #Initialise original dataset
      mt <- object$model$terms
      yvar <- deparse(mt[[2]]) # Perhaps a better way to do this
      xvar <- attr(mt,"term.labels")
      vars <- c(yvar=yvar, xvar=xvar)
      data <- object$model$model
      colnames(data) <- names(vars)[match(colnames(data), vars)]
    }
    else if (!is.null(object$x)){
      data <- data.frame(yvar=object$x)
      vars <- c(yvar="y")
    }
    else if (!is.null(object$residuals) && !is.null(object$fitted)){
      data <- data.frame(yvar=object$residuals+object$fitted)
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
        if(length(object$level)<=5){
          p <- p + ggplot2::scale_fill_gradientn(breaks=object$level, colours=shadecols, guide="legend")
        }
        else{
          p <- p + ggplot2::scale_fill_gradientn(colours=shadecols)
        }
        #Negative group is a work around for missing z-index
      }
      
      #Forecasted points
      p <- p + ggplot2::geom_line(ggplot2::aes_(x=~datetime,y=~ypred), data=predicted, color=fcol, size=flwd)
    }
    
    #Graph title
    if (is.null(main)){
      main <- paste("Forecasts from ",object$method,sep="")
    }
    p <- p + ggplot2::ggtitle(main)
    
    #Graph labels
    if (!is.null(xlab)){
      p <- p + ggplot2::xlab(xlab)
    }
    if (!is.null(ylab)){
      p <- p + ggplot2::ylab(ylab)
    }
    
    return(p)
  }
}

autoplot.mforecast <- function (object, plot.conf=TRUE, main=NULL, xlab=NULL, ylab=NULL, gridlayout=NULL, ...){
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
    if (missing(main)){
      main <- rep(paste("Forecasts from",object$method), K)
    }
    if (missing(xlab)){
      xlab <- rep(NULL, K)
    }
    if (missing(ylab)){
      ylab <- colnames(object$x)
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
                     plot.conf=plot.conf[i], main=main[i], xlab=xlab[i], ylab=ylab[i], ...),
            vp = grid::viewport(layout.pos.row = matchidx$row,
                          layout.pos.col = matchidx$col))
    }
  }
}

ggseasonplot <- function (x, year.labels=FALSE, year.labels.left=FALSE, type=NULL, main=NULL, xlab="Season", ylab="", col=NULL, labelgap=0.04, ggplot=TRUE, ...){
  if (requireNamespace("ggplot2")){
    if (!inherits(x, "ts")){
      stop("autoplot.seasonplot requires a ts object, use x=object")
    }
    if(!is.null(type)){
      message("Plot types are not yet supported for seasonplot()")
    }
    if (missing(main)){
      main <- paste("Seasonal plot:", deparse(substitute(x)))
    }
    if (!ggplot){
      cl <- match.call()
      cl[[1]] <- quote(seasonplot)
      return(eval(cl))
    }
    
    data <- data.frame(y=as.numeric(x),year=factor(trunc(time(x))),time=as.numeric(round(time(x)%%1,digits = 6)))
    
    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(x=~time, y=~y, group=~year, colour=~year), data=data, na.rm=TRUE)
    p <- p + ggplot2::scale_x_continuous()
    
    #Add data
    p <- p + ggplot2::geom_line()
    
    if(!is.null(col)){
      ncol <- length(unique(data$year))
      if(length(col)==1){
        p <- p + ggplot2::scale_color_manual(guide="none", values=rep(col, ncol))
      }
      else{
        p <- p + ggplot2::scale_color_manual(values=rep(col, ceiling(ncol/length(col)))[1:ncol])
      }
    }
      
    
    if(year.labels){
      if(year.labels.left){
        yrlab <- aggregate(time ~ year, data=data, FUN = min)
        offset <- -labelgap
      }
      else{
        yrlab <- aggregate(time ~ year, data=data, FUN = max)
        offset <- +labelgap
      }
      yrlab <- merge(yrlab, data)
      p <- p + ggplot2::guides(colour=FALSE)
      p <- p + ggplot2::geom_text(ggplot2::aes_(x=~time, y=~y, label=~year), colour = col, nudge_x=offset, data=yrlab)
    }
    
    #Graph title
    p <- p + ggplot2::ggtitle(main)
    
    #Graph labels
    p <- p + ggplot2::xlab(xlab)
    p <- p + ggplot2::ylab(ylab)
    return(p)
  }
}

autoplot.splineforecast <- function (object, plot.conf=TRUE, main=NULL, xlab=NULL, ylab=NULL, ...){
  if (is.null(ylab)){
    ylab <- deparse(object$model$call$x)
  }
  p <- autoplot.forecast(object, plot.conf=plot.conf, main=main, xlab=xlab, ylab=ylab, ...)
  fit <- data.frame(datetime=as.numeric(time(object$fitted)),y=as.numeric(object$fitted))
  p <- p + ggplot2::geom_point(ggplot2::aes_(object=~datetime,y=~y),data=fit,size=2)
  return(p)
}

autoplot.stl <- function (object, labels = NULL, main=NULL, xlab="Time", ylab="", ...){
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
    
    #Graph title
    p <- p + ggplot2::ggtitle(main)
    
    #Graph labels
    p <- p + ggplot2::xlab(xlab)
    p <- p + ggplot2::ylab(ylab)
    return(p)
  }
}

autoplot.ts <- function(object, main=NULL, xlab="Time", ylab=substitute(object), ...){
  if(requireNamespace("ggplot2")){
    if(!is.ts(object)){
      stop("autoplot.ts requires a ts object, use object=object")
    }
    data <- data.frame(y = as.numeric(object), x = as.numeric(time(object)))
    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(y=~y, x=~x), data=data)
    
    #Add data
    p <- p + ggplot2::geom_line()
    
    #Graph title
    p <- p + ggplot2::ggtitle(main)
    
    #Graph labels
    p <- p + ggplot2::xlab(xlab)
    p <- p + ggplot2::ylab(ylab)
    return(p)
  }
}

autoplot.mts <- function(object, main=NULL, xlab="Time", ylab=substitute(object), ...){
  if(requireNamespace("ggplot2")){
    if(!is.mts(object)){
      stop("autoplot.mts requires a mts object, use x=object")
    }
    data <- data.frame(y=as.numeric(c(object)), x=rep(as.numeric(time(object)),NCOL(object)),
                       series=rep(colnames(object), each=NROW(object)))
    #Initialise ggplot object
    p <- ggplot2::ggplot(ggplot2::aes_(y=~y, x=~x, group=~series, colour=~series), data=data)
    
    #Add data
    p <- p + ggplot2::geom_line()
    
    #Graph title
    p <- p + ggplot2::ggtitle(main)
    
    #Graph labels
    p <- p + ggplot2::xlab(xlab)
    p <- p + ggplot2::ylab(ylab)
    return(p)
  }
}

fortify.ts <- function(model, data){
  model <- cbind(x = as.numeric(time(model)), y = as.numeric(model))
  as.data.frame(model)
}

fortify.forecast <- function(model, data=as.data.frame(model), CI=TRUE){
  Hiloc <- grep("Hi ", names(data))
  Loloc <- grep("Lo ", names(data))
  if(CI){  
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
  return(data.frame(x=as.numeric(time(model$mean)), y=data[,1], level=rep(-Inf,NROW(data))))
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
    fcast <- fortify(fcast, CI=plot.conf)
    suppressWarnings(fcast <- cbind(fcast,data[1,!colnames(data)%in%colnames(fcast)]))
    fcast
  }
)

GeomForecast <- ggplot2::ggproto("GeomForecast", ggplot2::Geom,
  required_aes = c("x","y"),
  default_aes = ggplot2::aes(colour = "#BEC1D4", fill = "grey60", size = .5,
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
    data$group <- -as.numeric(factor(interaction(data$group, data$level)))
    if(any(is.finite(data$level))){
      data$scalefill <- scales::rescale(suppressWarnings(as.numeric(data$level)))
    }
    data
  },
  
  draw_group = function(data, panel_scales, coord){
    col <- data$colour[1]
    altcol <- col2rgb(col)
    altcol <- rgb2hsv(altcol[[1]],altcol[[2]],altcol[[3]])
    
    if(all(is.finite(data$level))){
      plot.ci <- TRUE
      altcol1 <- colorspace::hex(colorspace::HSV(altcol[1]*360, altcol[2], altcol[3]*(2/3)))
      altcol2 <- colorspace::hex(colorspace::HSV(altcol[1]*360, altcol[2], 1-(1-altcol[3])*(1/3)))
      intervalpred <- transform(data[,-match("y", colnames(data))], colour = NA,
                                fill = scales::gradient_n_pal(c(altcol1,altcol2))(data$scalefill[1]))
    }
    else{
      plot.ci <- FALSE
      if(any(c("ymax","ymin")%in%colnames(data))){
        data <- data[,-match(c("level","ymax","ymin"), colnames(data))]
        linecol <- "#FFFFFF"
      }
      else{
        linecol <- colorspace::hex(colorspace::HSV(altcol[1]*360, altcol[2], altcol[3]*(2/3)))
      }
      pointpred <- transform(data, group = -1, fill = NA, colour = linecol)
    }
    #Draw forecasted points and intervals
    ggplot2:::ggname("geom_forecast", 
      grid::grobTree(if(plot.ci)GeomRibbon$draw_group(intervalpred, panel_scales, coord),
               if(!plot.ci)GeomLine$draw_panel(pointpred, panel_scales, coord)
    ))
  }
)

stat_forecast <- function(mapping = NULL, data = NULL, geom = "forecast",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, plot.conf=TRUE, h=NULL, level=c(80,95), fan=FALSE,
                       robust=FALSE, lambda=NULL, find.frequency=FALSE, 
                       allow.multiplicative.trend=FALSE, ...) {
  if(!is.null(mapping)){
    if(is.forecast(mapping)){
      message("forecast/fit object detected, using geom_forecast()")
      cl <- match.call()
      cl[[1]] <- quote(geom_forecast)
      return(eval(cl))
    }
    else if(is.ts(mapping)){
      data <- data.frame(y = as.numeric(mapping), x = as.numeric(time(mapping)))
      #Initialise ggplot object
      mapping <- ggplot2::aes_(y=~y, x=~x)
    }
    else if(!"uneval"%in%class(mapping)){
      fcast <- forecast(mapping, h=h, level=level, fan=fan, robust=robust,
                        lambda=lambda, find.frequency=find.frequency,
                        allow.multiplicative.trend=allow.multiplicative.trend)
      return(geom_forecast(fcast))
    }
  }
  ggplot2::layer(
    stat = StatForecast, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(plot.conf=plot.conf, h=h, level=level, fan=fan, robust=robust,
                  lambda=lambda, find.frequency=find.frequency,
                  allow.multiplicative.trend=allow.multiplicative.trend,
                  na.rm = na.rm, ...))
}

geom_forecast <- function(mapping = NULL, data = NULL, stat = "forecast",
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = TRUE, plot.conf=TRUE, h=NULL, level=c(80,95), fan=FALSE,
                          robust=FALSE, lambda=NULL, find.frequency=FALSE, 
                          allow.multiplicative.trend=FALSE, series, ...) {
  if(is.forecast(mapping)){
    if(stat=="forecast"){
      stat <- "identity"
    }
    data <- fortify(mapping, CI=plot.conf)
    mapping <- ggplot2::aes_(x = ~x, y = ~y, level = ~level, group = ~-level)
    if(plot.conf){
      mapping$ymin <- quote(ymin)
      mapping$ymax <- quote(ymax)
    }
    if(!missing(series)){
      data <- transform(data, series=series)
      mapping$colour <- quote(series)
    }
  }
  if(is.mforecast(mapping)){
    #Convert mforecast to list of forecast
    #return lapply of geom_forecast with params on list
    stop("mforecast objects not yet supported. Try calling geom_forecast() for several forecast objects")
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