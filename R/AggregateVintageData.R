#' Aggregate vintage data
#'
#' This function aggregates already calculated vintage data by removing or keeping selected \code{Slicers}.
#'
#' @param VintageData Result of function \code{GetVintageData} or data frame with the same structure
#' @param Slicers Vector of slicers which should be included or excluded.
#' @param Type Indicating inclusion or exclusion of \code{Slicers}. Default is to include specified slicers. Possible values
#' are \code{include} and \code{exclude}.
#' @param TimeAggregationUnit Defines how distance should be aggregated. By default, distance is not modified at all.
#' When specified, single integer has to be used. For example using value 3 and distance values in \code{VintageData}
#' \code{0,1,2,3,4,5,6,7,8,9} the result will be aggregated to \code{0,1,2,3} (\code{(0,1,2)->0, (3,4,5)->1, (6,7,8)->2, (9)->3}).
#' @param SQLModifier Optional \code{WHERE} clause to filter \code{VintageData} before aggretation
#' @export 

AggregateVintageData <- function(VintageData=NA,Slicers=NA,Type='include',TimeAggregationUnit=NA,SQLModifier=NA) {
  
  if (!is.data.frame(VintageData)) {
    stop("Object specified as VintageData is not data frame.")
  }
  
  RawColumns <- c('vintage_unit_weight','vintage_unit_count','event_weight','event_weight_pct','event_weight_csum','event_weight_csum_pct')

  print (RawColumns %in% names(VintageData))
}
