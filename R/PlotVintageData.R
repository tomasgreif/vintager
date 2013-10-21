#' Plot vintage data
#'
#' This function plots results of \code{GetVintageData} function. All \code{Slicers} in results have
#' to be included in plot definition.
#'
#' @param data Result of \code{GetVintageData} function.
#' @param x Data to be displayed on \code{x} axis. Default is distance
#' @param y Data to be displayed on \code{y} axis. Default is \code{event_weight_csum_pct}.
#' @param cond Variable to be used for conditioning (in-chart group).
#' @param facets Formulat to be used for facetting.
#' @export 


PlotVintageData <- function(data=NULL,x="distance",y="event_weight_csum_pct",cond=NULL,facets=NULL) {
  
  require(ggplot2)
  
  `%ni%` <- Negate(`%in%`)

  DisplayVars <- names(data)[!(names(data) %in% c("distance","vintage_unit_weight","vintage_unit_count","event_weight",
                                                                "event_weight_pct","event_weight_csum","event_weight_csum_pct",
                                                                "rn"))]  
  for (col in names(data)) {
    if (class(data[[col]])[1] %in% c("Date",'POSIXct','POSIXt')) {
      data[[col]] <- as.factor(data[[col]])
    }
  }
  
  BasicPlot <- ggplot(data, aes_string(x=x, y=y))
  
  if (x %ni% names(data)) stop (paste("Variable",x,"is not in data frame."))
  if (y %ni% names(data)) stop (paste("Variable",y,"is not in data frame."))
  if (length(cond)>1) stop (paste("Only one variable can be specified for conditioning."))
  if (length(cond)==1) {
    if (cond %ni% names(data)) stop (paste("Conditioning variable",cond,"is not in data frame."))
  }
  if(!is.null(facets) ) {
    # add sanity check
  }

  if (!is.null(cond)) {
    BasicPlot <- BasicPlot + geom_line(aes_string(group=cond, colour=cond))
  } else {
    BasicPlot <- BasicPlot + geom_line()
  }
    
  
  if(!is.null(facets)) {
    if (substr(facets,1,1) == "~" & !grepl("+",facets,fixed=TRUE)) {
      BasicPlot <- BasicPlot + facet_wrap(as.formula(facets))            
    } else {
      BasicPlot <- BasicPlot + facet_grid(as.formula(facets))      
    }
  }
  
  BasicPlot + 
    ylab("Vintage measure") +
    ggtitle("Vintage Analysis") 
    #scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
}
