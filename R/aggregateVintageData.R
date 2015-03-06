#'Aggregate vintage data
#'
#'This function aggregates already calculated vintage data by removing or
#'keeping selected \code{Slicers}.
#'
#'@param vintageData Result of function \code{getVintageData} or data frame with
#'  the same structure. At least the following columns have to be present:
#'  \code{distance, vintage_unit_weight, vintage_unit_count, event_weight,
#'  event_weight_pct, event_weight_csum, event_weight_csum_pct}
#'@param slicers Vector of slicers which should be included or excluded.
#'@param type Indicating inclusion or exclusion of \code{slicers}. Default is to
#'  include specified slicers. Possible values are \code{include} and
#'  \code{exclude}.
#'@param timeAggregationUnit Defines how distance should be aggregated. By
#'  default, distance is not modified at all. When specified, single integer has
#'  to be used. For example using value 3 and distance values in
#'  \code{vintageData} \code{0,1,2,3,4,5,6,7,8,9} the result will be aggregated
#'  to \code{0,1,2,3} (\code{(0,1,2)->0, (3,4,5)->1, (6,7,8)->2, (9)->3}).
#'@param sqlModifier Optional \code{WHERE} clause to filter \code{vintageData}
#'  before aggretation
#'@param verbose If true, then additional diagnostic messages are displayed.
#'  Default is \code{FALSE}.
#' @examples 
#'# Aggregations ##########################
#'
#'# Show data by region
#'aggregateVintageData(vintageData, slicers = 'region')
#'# Show data by origination_month
#'aggregateVintageData(vintageData, slicers = 'origination_month')
#'# By product and region
#'aggregateVintageData(vintageData, slicers = c('region','product'))
#'#Everything but origination_month
#'aggregateVintageData(vintageData, slicers = 'origination_month', type = 'exclude')
#'#Everything but origination_month
#'aggregateVintageData(vintageData, slicers = 'origination_month', type = 'exclude')
#'#Exclude all slicers, only distance will be displayed
#'aggregateVintageData(vintageData, type = 'include')
#'#Exclude all slicers and aggregate also distance (set high timeAggregationUnit parameter)
#'aggregateVintageData(vintageData, type = 'include', timeAggregationUnit = 1000)
#'#Keep only product dimension, only product "Giga Loan"
#'aggregateVintageData(vintageData, slicers = 'product', sqlModifier = "product = 'Giga Loan'")
#'#Keep only product dimension, only product "Giga Loan" and region "West"
#'aggregateVintageData(vintageData, slicers = 'product', sqlModifier = "product = 'Giga Loan' and region = 'West'")
#'# Show diagnostic messages
#'aggregateVintageData(vintageData, slicers = 'region', verbose = TRUE)
#'
#'@export

aggregateVintageData <- function(vintageData = NULL, slicers = NA, type='include',
                                 timeAggregationUnit=1, sqlModifier=NA,
                                 verbose=FALSE) {
  
  if (!is.data.frame(vintageData)) {
    stop("Object specified as vintageData is not data frame.")
  }
  print(names(vintageData))
  rawColumns <- c('distance', 'vintage_unit_weight','vintage_unit_count',
                  'event_weight', 'event_weight_pct','event_weight_csum',
                  'event_weight_csum_pct')

  if (!all(rawColumns %in% names(vintageData))) {
    stop("All columns available in result of getVintageData have to be present
         in provided data frame.")
  } else {
    if(verbose) message("All required columns are available in vintageData.\n")
  }
  

  if (!all(is.na(slicers))) {
    if (!all(slicers %in% names(vintageData))) {
      stop("Some slicer does not exist in vintageData.")
    } else {
      if(verbose) message("All slicers found in VintageData.\n")
    }
    
  }
  
  availableSlicers <- names(vintageData)[!(names(vintageData) 
                                           %in% c(rawColumns,'rn'))]

  
  if(type=='include') {
      if(verbose) message("Specified Slicers will be included and all others excluded.\n")
      usedSlicers <- slicers
      if(verbose) message("Slicers required in results: ", usedSlicers,"\n")
    } else if (type == 'exclude') {
      if(verbose) message("Specified slicers will be excluded and all other included.\n")
      usedSlicers <- availableSlicers[!(availableSlicers %in% slicers)]
      if(verbose) message("Slicers required in results: ", usedSlicers,"\n")
    } else {
      stop("Type can be one of 'include' and 'exclude'.")
  }

  aggregationSql <- paste(
    "select ",
        if(!all(is.na(usedSlicers))) paste(paste(usedSlicers," ",collapse=","),","), 
        "distance/", timeAggregationUnit,"as distance,",
        "
        sum(vintage_unit_weight) as vintage_unit_weight,
        sum(vintage_unit_count) as vintage_unit_count,
        sum(event_weight) as event_weight,
        sum(event_weight)/sum(vintage_unit_weight) as event_weight_pct ,
        sum(event_weight_csum) as event_weight_csum,
        sum(event_weight_csum)/sum(vintage_unit_weight) as event_weight_csum_pct,
        sum(1) as rn
    from"
        , deparse(substitute(vintageData)), 
    "where",
        if(!is.na(sqlModifier)) sqlModifier else "1 = 1",
    "group by",
        if(!all(is.na(usedSlicers))) paste(paste(usedSlicers," ",collapse=","),","), 
        "distance"
    )
  
  if(verbose) message("Used SQL: \n", aggregationSql,"\n")

  sqldf(aggregationSql, drv='SQLite')
}
