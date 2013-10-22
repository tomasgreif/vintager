#' Print Vintage Data
#'
#' This function prints results of \code{GetVintageData} function. All \code{Slicers} in results have
#' to be included in plot definition. Data are pivoted by \code{distance} column.
#'
#' @param data Result of \code{GetVintageData} function.
#' @param Columns Columns to be printed. Any combination of the following possible statistics: \code{vintage_unit_weight, vintage_unit_count, event_weight, event_weight_pct, event_weight_csum, event_weight_csum_pct}.
#' For every statistic one table will be printed.
#' @param Result  Type of result. By default, data are printed. If \code{xls} is used than data are written to XLS file.
#' @param File Name of XLS file where data should be stored. Makes sense only when \code{Result} is set to \code{xls}. If no name
#' is given and \code{Result} is \code{xls} then name \code{VintageData.xls} will be used as default.
#' @examples \dontrun{
#' PrintVintageData(VintageData,"event_weight_csum_pct",Result='xls')
#' PrintVintageData(VintageData,c("event_weight, event_weight_csum_pct"),Result='xls')
#' PrintVintageData(VintageData,Result='xls',File='MyVintageAnalysis')
#' }
#' @export 


PrintVintageData <- function(VintageData,
                             Columns=c("vintage_unit_weight","vintage_unit_count","event_weight","event_weight_pct","event_weight_csum","event_weight_csum_pct"),
                             Result='print',File="VintageData.xls",Digits=getOption("digits")) {
  
  RawColumns <- c('vintage_unit_weight','vintage_unit_count','event_weight','event_weight_pct','event_weight_csum','event_weight_csum_pct')
  RawNames <-   c('VintageUnitWeight','VintageUnitCount','EventWeight','EventWeightPct','EventWeightCsum','EventWeightCsumPct')
  
  DisplayVars <- names(VintageData)[!(names(VintageData) %in% c("distance","vintage_unit_weight","vintage_unit_count","event_weight",
                                                                "event_weight_pct","event_weight_csum","event_weight_csum_pct",
                                                                "rn"))]    
  
  Out <- lapply(Columns, function(x) dcast(VintageData,as.formula(paste(paste(DisplayVars,collapse="+"),"~ distance")), value.var=x))  
  names(Out) <- RawNames[(RawColumns %in% Columns)]
  
  if (Result=='xls') {
    require(WriteXLS)
    WriteXLS("Out",ExcelFileName=File)
    cat("Data written to:",File)
  } else if (Result == 'print') {
    if (is.list(Out) & !is.data.frame(Out)) {
      lapply(Out, function(x, Digits) {
        cf <- format(x,digits=Digits) 
        cf[is.na(x)] <- ""
        cf })
    } else {
      cf <- format(Out,digits=Digits) 
      cf[is.na(Out)] <- ""
      cf      
    }

  }
    
}

