#' Print Vintage Data
#' 
#' This function prints results of \code{getVintageData} function. All
#' \code{Slicers} in results have to be included in plot definition. Data are
#' pivoted by \code{distance} column.
#' 
#' @param vintageData Result of \code{getVintageData} function.
#' @param columns Columns to be printed. Any combination of the following
#'   possible statistics: \code{vintage_unit_weight, vintage_unit_count,
#'   event_weight, event_weight_pct, event_weight_csum, event_weight_csum_pct}. 
#'   For every statistic one table will be printed.
#' @param result  Type of result. By default, data are printed. If \code{xls} is
#'   used than data are written to XLS file.
#' @param file Name of XLS file where data should be stored. Makes sense only
#'   when \code{Result} is set to \code{xls}. If no name is given and
#'   \code{Result} is \code{xls} then name \code{VintageData.xls} will be used
#'   as default.
#' @param digits Number of significant digits to be used when printing (only
#'   valid for \code{Result='print'})
#' @param stacked If \code{TRUE} then all tables will be saved on single sheet
#'   in XLS. Default is \code{FALSE}.
#' @examples \dontrun{
#' printVintageData(vintageData,"event_weight_csum_pct", result = 'xls')
#' printVintageData(vintageData,c("event_weight, event_weight_csum_pct"), result = 'xls')
#' printVintageData(vintageData, result = 'xls', file = 'MyVintageAnalysis')
#' }
#' @export


printVintageData <- function(vintageData,
                             columns=c("vintage_unit_weight","vintage_unit_count",
                                       "event_weight","event_weight_pct","event_weight_csum",
                                       "event_weight_csum_pct"),
                             result = 'print',
                             file = "vintageData.xls",
                             digits = getOption("digits"),
                             stacked = FALSE) {
  
  rawColumns <- c('vintage_unit_weight','vintage_unit_count','event_weight',
                  'event_weight_pct','event_weight_csum','event_weight_csum_pct')
  rawNames <-   c('VintageUnitWeight','VintageUnitCount','EventWeight',
                  'EventWeightPct','EventWeightCsum','EventWeightCsumPct')
  
  displayVars <- names(vintageData)[!(names(vintageData) %in% 
                      c("distance","vintage_unit_weight","vintage_unit_count","event_weight",
                        "event_weight_pct","event_weight_csum","event_weight_csum_pct", "rn"))]    

  if (length(displayVars) == 0) {
    out <- lapply(columns, function(x) dcast(vintageData, as.formula("1 ~ distance"), value.var=x))              
  } else {
    out <- lapply(columns, function(x) dcast(VintageData, as.formula(paste(paste(displayVars,collapse="+"),
                                                                           "~ distance")), value.var=x))      
  }

  
  names(out) <- rawNames[(rawColumns %in% columns)]
  
  if (result=='xls') {
    if (stacked) {

      dflc <- lapply(out, function(x) {
        as.data.frame(sapply(x, function(y) {as.character(y)}), stringsAsFactors = FALSE)
      })

      out <- rbind.fill.matrix(lapply(names(dflc),
                                      function(x) {
                                        rbind(
                                          c(x,rep('',ncol(dflc[[x]])-1)),
                                          names(dflc[[x]]),
                                          setNames(dflc[[x]], NULL),
                                          '')
                                      }
      ))
      
      out[is.na(out)] <- ''
      out <- as.data.frame(out)
      WriteXLS("out", ExcelFileName = file, col.names = FALSE)  
    } else {
      WriteXLS("out", ExcelFileName = file)  
    }
    
    message("Data written to:", file)
  } else if (result == 'print') {
    if (is.list(out) & !is.data.frame(out)) {
      lapply(out, function(x, digits) {
        cf <- format(x,digits = digits) 
        cf[is.na(x)] <- ""
        cf }, digits)
    } else {
      cf <- format(out, digits = digits) 
      cf[is.na(out)] <- ""
      cf      
    }

  }
    
}

