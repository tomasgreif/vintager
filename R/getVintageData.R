#' Get data in vintage analysis format
#' 
#' This function prepares data stored in PostgreSQL or Oracle database for
#' vintage analysis. According to Statsoft vintage analysis is: "Vintage
#' Analysis. A vintage is a group of credit accounts that all originated within
#' a specific time period, usually a year. Vintage analysis is used in credit
#' scoring and refers to the process of monitoring groups of accounts and
#' comparing performance across past groups. The comparisons take place at
#' similar loan ages, allowing for the detection of deviation from past
#' performance. Typically, a graphical representation is used for this purpose,
#' such as one showing the relationship between months on the books and the
#' percentage of delinquent accounts across multiple vintages." In this function
#' concept of vintage is generalized in such way that any objects sharing same
#' properties can form a vintage.
#' 
#' If successful, dataset with the following columns is returned: \tabular{ll}{ 
#' Column \tab Description\cr \code{[Slicers]} \tab As defined in 
#' \code{VintageUnitSQL}.\cr \code{distance} \tab Distance between vintage unit 
#' date and event date measured in time interval (currently month, quarter or 
#' year).\cr \code{vintage_unit_weight} \tab sum of vintage unit weights for 
#' given vintage at given distance. Note that one vintage can have different 
#' values for different distances when no slicer is based on vintage unit date 
#' and with granularity equal to time grouping of events (see examples to 
#' understand this better). In case when no weight is defined that values are 
#' equal to values in column \code{vintage_unit_count}.\cr 
#' \code{vintage_unit_count} \tab number (of rows) for given vintage at 
#' distance. Using same logic as vintage unit weight.\cr \code{event_weight} 
#' \tab Sum of event weights for given vintage at distance. If no weight is 
#' defined, than 1 is used (equal to row count)\cr \code{event_weight_pct} \tab 
#' \code{event_weight / vintage_unit_weight}\cr \code{event_weight_csum} \tab 
#' running total of event weights for given vintage ordered by distance. If no 
#' slicer is based on vintage date and with granularity equal to time grouping 
#' of events, than running total cannot be reproduced as running total of 
#' event_weight! See examples to understand this better.\cr 
#' \code{event_weight_csum_pct} \tab \code{event_weight_csum / 
#' vintage_unit_weight}\cr \code{rn} \tab Column to numerically indicate order 
#' of last \code{Sclicer}. Useful when last \code{Slicer} is time-orderable, but
#' data type is not number/date. \cr } This function is tested with PostgreSQL 
#' 9.1, but any version with window functions support should work.
#' 
#' @param vintageUnitSql Valid SQL \code{SELECT} statement. \cr If 
#'   \code{sqlModifier} is not used than result has to have the following 
#'   structure (Note: optional elements are denoted as \code{[...]}): 
#'   \tabular{ll}{ Column \tab Description\cr \code{id} \tab Unique 
#'   identificator of row. Is considered to be a primary key of
#'   \code{performanceEventSql}. Duplicates will result in incorrect results as 
#'   join between vintage units and events will be multiplicated. Any (reasonable) data type 
#'   possible.\cr \code{vintage_unit_date} \tab Has to be of type date. It is 
#'   not necessary to round dates to months/quarters/years.\cr 
#'   \code{[vintage_unit_weight]} \tab Optional weight. When no weight is 
#'   defined than every row is considered to be one unit (every row in result 
#'   represents one unit). Has to be of numeric type (integer, numeric, 
#'   float).\cr \code{[Slicers]} \tab Any number of columns used to form 
#'   vintages. There are no constraints on data types. Any name except \code{id,
#'   vintage_unit_date, vintage_unit_weight} can be used. Every unique 
#'   combination of \code{Slicers} will be considered as one vintage. When there
#'   are no \code{Slicers} then only one vintage will be created. \cr }
#'   
#' @param performanceEventSql Valid SQL \code{SELECT} statement. Result has to 
#'   have the following structure (Note: optional elements are denoted as 
#'   \code{[...]}): \tabular{ll}{ Column \tab Description\cr \code{id} \tab 
#'   Identificator of vintage unit to which event belongs. Has to be of same 
#'   data type as column \code{id} in \code{vintageUnitSql}. \cr 
#'   \code{event_date} \tab Date when event occured. Has to be of type date. It 
#'   is not necessary to round dates to months/quarters/years.\cr 
#'   \code{[event_weight]} \tab Optional weight. When no weight is defined than 
#'   every row is considered to be one unit (every row in result represents one 
#'   event). Has to be of numeric type (integer, numeric, float).\cr } Note: it 
#'   does not make sense to include any other columns as these are not used by 
#'   the function. Currently, only single column key is supported.
#' @param timeGroup Aggregation of vintage data. Defines how distance between 
#'   \code{vintage_unit_date} and \code{event_date} is measured. Possible values
#'   are \code{month, quarter, year}. In PostgreSQL,  distance calculation is performed by 
#'   custom function named \code{time_distance}. Code to create this 
#'   function is stored in \code{exec/time_distance.sql}. For Oracle, standard
#'   date functions are used.
#' @param timeExpansion Defines how time expansion is performed. By default, 
#'   vintages will be generated up to last existing point in events. E.g. when 
#'   maximum distance in data is 10 than every vintage will have 10 
#'   observations. There are three other options, using \code{now} or date in 
#'   \code{yyyy-mm-dd} format or \code{local}. \code{now} will be internally 
#'   replaced by current date. \code{local} will use maximum event date 
#'   available for every vintage. If any of these option is used than value of 
#'   parameter will be used as last available point in data. Thus, number of 
#'   points for every vintage will be expanded. Note that this might return 
#'   unexpected results if this date is earlier than last point in events.
#' @param con Connection to PostgreSQL or Oracle database. This is an 
#' connection object as produced by \code{dbConnect(...)} function.
#' @param result Type of results to return. By default (\code{data}), vintage 
#'   data are returned. The other option is to use \code{sql} - this will return
#'   SQL statement to get vintage data.
#' @param distanceFunctionSchema Name of database schema where 
#'   \code{time_distance} function is available. This is valid for PostgreSQL only.
#' @param sqlModifier This will constraint result of \code{vintageUnitSql} to selected 
#'   columns and/or rows. Vector with 1 or 2 elements. In the first element, 
#'   required columns are specified, the second can contain additional 
#'   \code{WHERE} condition. If used than \code{vintageUnitSql} is wrapped into 
#'   \code{SELECT id, vintage_unit_date [, First Element] FROM (VintageUnitSQL) 
#'   x [WHERE Second Element]}. When only \code{WHERE} clause should be used and 
#'   columns should remain unchanged  than first element has to be asterisk 
#'   ('*'). If first element is empty string or NA than only columns \code{id} 
#'   and \code{vintage_unit_date} will be used from results of 
#'   \code{vintageUnitSql}.
#' @param verbose Prints additional diagnostics messages when \code{TRUE}. 
#'   Default is \code{FALSE}.
#' @param debug Prints low level diagnostic messages. Usefull mainly for package
#'   developer. Default is \code{FALSE}

getVintageData <- function(vintageUnitSql, performanceEventSql, timeGroup='month',
                           timeExpansion = 'none', con, result = 'data',
                           sqlModifier = NULL, distanceFunctionSchema = NULL, 
                           verbose = FALSE, debug = FALSE
                            ) {

  # Dispatch to the correct function based on connection class
  # Report error otherwise
  if (class(con) == 'PostgreSQLConnection') {
    getVintageDataPg(vintageUnitSql = vintageUnitSql, 
                     performanceEventSql = performanceEventSql, 
                     timeGroup = timeGroup,
                     timeExpansion = timeExpansion, 
                     con = con, 
                     result = result,
                     sqlModifier = sqlModifier, 
                     distanceFunctionSchema = distanceFunctionSchema, 
                     verbose = verbose, 
                     debug = debug)
      
  } else if (class(con) == 'OraConnection') {
    getVintageDataOra(vintageUnitSql = vintageUnitSql, 
                     performanceEventSql = performanceEventSql, 
                     timeGroup = timeGroup,
                     timeExpansion = timeExpansion, 
                     con = con, 
                     result = result,
                     sqlModifier = sqlModifier, 
                     distanceFunctionSchema = distanceFunctionSchema, 
                     verbose = verbose, 
                     debug = debug)    
  } else {
    stop('Only PostgreSQL and Oracle is supported. Let me know if your
         database system supports window function and I will try to add
         support.')
  }
}
