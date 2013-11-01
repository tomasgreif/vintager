#' Aggregate vintage data
#'
#' This function aggregates already calculated vintage data by removing or keeping selected \code{Slicers}.
#'
#' @param VintageData Result of function \code{GetVintageData} or data frame with the same structure. At least the following columns
#' have to be present: \code{distance, vintage_unit_weight, vintage_unit_count, event_weight, event_weight_pct, event_weight_csum, event_weight_csum_pct}
#' @param Slicers Vector of slicers which should be included or excluded.
#' @param Type Indicating inclusion or exclusion of \code{Slicers}. Default is to include specified slicers. Possible values
#' are \code{include} and \code{exclude}.
#' @param TimeAggregationUnit Defines how distance should be aggregated. By default, distance is not modified at all.
#' When specified, single integer has to be used. For example using value 3 and distance values in \code{VintageData}
#' \code{0,1,2,3,4,5,6,7,8,9} the result will be aggregated to \code{0,1,2,3} (\code{(0,1,2)->0, (3,4,5)->1, (6,7,8)->2, (9)->3}).
#' @param SQLModifier Optional \code{WHERE} clause to filter \code{VintageData} before aggretation
#' @param Verbose If true, then additional diagnostic messages are displayed. Default is \code{FALSE}.
#' @examples \dontrun{
#' Setup  ########################
#'
#'VintageUnitSQL <- "select id, origination_date as vintage_unit_date, product, region, 
#'date_trunc('month',origination_date) 
#'as origination_month from big_portfolio"
#'PerformanceEventSQL <- "select id, repayment_date as event_date from big_portfolio"
#'Connection <- c('user','password','database','host','port')
#'
#'# Get source data ######################
#'VintageData <- GetVintageData(VintageUnitSQL,PerformanceEventSQL,Connection=Connection)
#'# Now we have vintage curves by product, region and origination_month
#'
#'# Aggregations ##########################
#'
#'# Show data by region
#'AggregateVintageData(VintageData,Slicers='region')
#'# Show data by origination_month
#'AggregateVintageData(VintageData,Slicers='origination_month')
#'# By product and region
#'AggregateVintageData(VintageData,Slicers=c('region','product'))
#'#Everything but origination_month
#'AggregateVintageData(VintageData,Slicers='origination_month',Type='exclude')
#'#Everything but origination_month
#'AggregateVintageData(VintageData,Slicers='origination_month',Type='exclude')
#'#Exclude all slicers, only distance will be displayed
#'AggregateVintageData(VintageData,Type='include')
#'#Exclude all slicers and aggregate also distance (set high TimeAggregationUnit parameter)
#'AggregateVintageData(VintageData,Type='include',TimeAggregationUnit=1000)
#'#Keep only product dimension, only product "Giga Loan"
#'AggregateVintageData(VintageData,Slicers='product',SQLModifier="product = 'Giga Loan'")
#'#Keep only product dimension, only product "Giga Loan" and region "West"
#'AggregateVintageData(VintageData,Slicers='product',SQLModifier="product = 'Giga Loan' and region = 'West'")
#'# Show diagnostic messages
#'AggregateVintageData(VintageData,Slicers='region',Verbose=TRUE)
#' }
#' @export 

AggregateVintageData <- function(VintageData=NA,Slicers=NA,Type='include',TimeAggregationUnit=1,SQLModifier=NA, Verbose=FALSE) {
  
  if (!is.data.frame(VintageData)) {
    stop("Object specified as VintageData is not data frame.")
  }
  
  RawColumns <- c('distance', 'vintage_unit_weight','vintage_unit_count','event_weight',
                  'event_weight_pct','event_weight_csum','event_weight_csum_pct')

  if (!all(RawColumns %in% names(VintageData))) {
    stop("All columns available in result of GetVintageData have to be present in provided data frame.")
  } else {
    if(Verbose) cat("All required columns are available in VintageData.\n")
  }
  

  if (!all(is.na(Slicers))) {
    if (!all(Slicers %in% names(VintageData))) {
      stop("Some Slicer does not exist in VintageData.")
    } else {
      if(Verbose) cat("All Slicers found in VintageData.\n")
    }
    
  }
  
  AvailableSlicers <- names(VintageData)[!(names(VintageData) %in% c(RawColumns,'rn'))]

  
  if(Type=='include') {
      if(Verbose) cat("Specified Slicers will be included and all others excluded.\n")
      UsedSlicers <- Slicers
      if(Verbose) cat("Slicers required in results: ", UsedSlicers,"\n")
    } else if (Type == 'exclude') {
      if(Verbose) cat("Specified Slicers will be excluded and all other included.\n")
      UsedSlicers <- AvailableSlicers[!(AvailableSlicers %in% Slicers)]
      if(Verbose) cat("Slicers required in results: ", UsedSlicers,"\n")
    } else {
      stop("Type can be one of 'include' and 'exclude'.")
  }

  AggregationSQL <- paste(
    "select ",
        if(!all(is.na(UsedSlicers))) paste(paste(UsedSlicers," ",collapse=","),","), 
        "distance/",TimeAggregationUnit,"as distance,",
        "
        sum(vintage_unit_weight) as vintage_unit_weight,
        sum(vintage_unit_count) as vintage_unit_count,
        sum(event_weight) as event_weight,
        sum(event_weight)/sum(vintage_unit_weight) as event_weight_pct ,
        sum(event_weight_csum) as event_weight_csum,
        sum(event_weight_csum)/sum(vintage_unit_weight) as event_weight_csum_pct,
        sum(1) as rn
    from"
        , deparse(substitute(VintageData)), 
    "where",
        if(!is.na(SQLModifier)) SQLModifier else "1=1",
    "group by",
        if(!all(is.na(UsedSlicers))) paste(paste(UsedSlicers," ",collapse=","),","), 
        "distance"
    )
  
  if(Verbose) cat("Used SQL: \n", AggregationSQL,"\n")

  sqldf(AggregationSQL, drv='SQLite')
}
