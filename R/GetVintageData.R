#' Get data in vintage analysis format
#'
#' This function prepares data stored in PostgreSQL database for vintage analysis.
#' According to Statsoft vintage analysis is: "Vintage Analysis. A vintage is a group of credit accounts that all originated within a specific time period, 
#' usually a year. Vintage analysis is used in credit scoring and refers to the process of monitoring groups of accounts and comparing performance across 
#' past groups. The comparisons take place at similar loan ages, allowing for the detection of deviation from past performance. Typically, a graphical 
#' representation is used for this purpose, such as one showing the relationship between months on the books and the percentage of delinquent accounts 
#' across multiple vintages." In this function concept of vintage is generalized in such way that any objects sharing same properties can form a vintage.
#' 
#' If successful, dataset with the following columns is returned: 
#' \tabular{ll}{
#' Column \tab Description\cr
#' \code{[Slicers]} \tab As defined in \code{VintageUnitSQL}.\cr
#' \code{distance} \tab Distance between vintage unit date and event date measured in time interval (currently month, quarter or year).\cr
#' \code{vintage_unit_weight} \tab sum of vintage unit weights for given vintage at given distance. Note that one vintage can
#' have different values for different distances when no slicer is based on vintage unit date and with 
#' granularity equal to time grouping of events (see examples to understand this better). In case when no
#' weight is defined that values are equal to values in column \code{vintage_unit_count}.\cr
#' \code{vintage_unit_count} \tab number (of rows) for given vintage at distance. Using same logic as vintage unit weight.\cr
#' \code{event_weight} \tab Sum of event weights for given vintage at distance. If no weight is defined, than 1 is used (equal to 
#' row count)\cr
#' \code{event_weight_pct} \tab \code{event_weight / vintage_unit_weight}\cr
#' \code{event_weight_csum} \tab running 
#' total of event weights for given vintage ordered by distance. If no slicer is based on vintage date 
#' and with granularity equal to time grouping of events, than running total cannot be reproduced as running 
#' total of event_weight! See examples to understand this better.\cr
#' \code{event_weight_csum_pct} \tab \code{event_weight_csum / vintage_unit_weight}\cr
#' \code{rn} \tab Column to numerically indicate order of last \code{Sclicer}. Useful when last \code{Slicer} is time-orderable, but data type is not number/date. \cr
#' }
#' This function is tested with PostgreSQL 9.1, but any version with window functions support should work.
#'
#' @param VintageUnitSQL Valid SQL \code{SELECT} statement. \cr 
#' If \code{SQLModifier} is not used than result has to have the following structure (Note: optional elements are denoted as \code{[...]}):
#' \tabular{ll}{
#' Column \tab Description\cr
#' \code{id} \tab Unique identificator of row. Is considered to be a key to \code{PerformanceEventSQL}. Duplicates will result in incorrect results
#' as join between vintage units and events will be multiplicated. Any data type possible.\cr
#' \code{vintage_unit_date} \tab Has to be of type date. It is not necessary to round dates to months/quarters/years.\cr
#' \code{[vintage_unit_weight]} \tab Optional weight. When no weight is defined than every row is 
#' considered to be one unit (every row in result represents one unit). Has to be of numeric type (integer, numeric, float).\cr
#' \code{[Slicers]} \tab Any number of columns used to form vintages. There are no constraints on data types. Any
#' name except \code{id, vintage_unit_date, vintage_unit_weight} can be used. Every unique combination of \code{Slicers} 
#' will be considered as one vintage. When there are no \code{Slicers} then only one vintage will be created. \cr
#' }
#' 
#' @param PerformanceEventSQL Valid SQL \code{SELECT} statement. Result has to have the following structure (Note: optional 
#' elements are denoted as \code{[...]}):
#' \tabular{ll}{
#' Column \tab Description\cr
#' \code{id} \tab Identificator of vintage unit to which event belongs. Has to be of same data type as column \code{id} in \code{VintageUnitSQL}. \cr
#' \code{event_date} \tab Date when event occured. Has to be of type date. It is not necessary to round dates to months/quarters/years.\cr
#' \code{[event_weight]} \tab Optional weight. When no weight is defined than every row is 
#' considered to be one unit (every row in result represents one event). Has to be of numeric type (integer, numeric, float).\cr
#' }
#' Note: it does not make sense to include any other columns as these are not used by the function.
#' Currently, only single column key is supported.
#' @param TimeGroup Aggregation of vintage data. Defines how distance between \code{vintage_unit_date} and 
#' \code{event_date} is measured. Possible values are \code{month, quarter, year}. Distance calculation is 
#' performed by custom PostgreSQL function named \code{time_distance}. Code to create this function is stored 
#' in \code{exec/time_distance.sql}
#' @param TimeExpansion Defines how time expansion is performed. By default, vintages will be generated 
#' up to last existing point in events. E.g. when maximum distance in data is 10 than every vintage 
#' will have 10 observations. There are three other options, using \code{now} or date in \code{yyyy-mm-dd} format or \code{local}. 
#' \code{now} will be internally replaced by current date. \code{local} will use maximum event date available for every 
#' vintage. If any of these option is used than value of parameter 
#' will be used as last available point in data. Thus, number of points for every vintage will be expanded. Note 
#' that this might return unexpected results if this date is earlier than last point in events.
#' @param Connection Connection to PostgreSQL database. Vector of exactly 5 elements in the following 
#' order: \code{user, password, database name, host, port}.
#' @param Result Type of results to return. By default (\code{data}), vintage data are returned. 
#' The other option is to use \code{sql} - this will return SQL statement to get vintage data.
#' @param DistanceFunctionSchema Name of database schema where \code{time_distance} function is available.
#' @param SQLModifier This will constraint result of VintageUnitSQL to selected columns and/or rows. 
#' Vector with 1 or 2 elements. In the first element, required columns are specified, the second can 
#' contain additional \code{WHERE} condition. If used than VintageUnitSQL is wrapped into 
#' \code{SELECT id, vintage_unit_date [, First Element] FROM (VintageUnitSQL) x [WHERE Second Element]}.
#' When only WHERE clause should be used and columns should remain unchanged  than first element has to be asterisk ('*').
#' If first element is empty string or NA than only columns \code{id} and \code{vintage_unit_date} will be used 
#' from results of \code{VintageUnitSQL}.
#' @param Verbose Prints additional diagnostics messages when \code{TRUE}. Default is \code{FALSE}.
#' @param Debug Prints low level diagnostic messages. Usefull mainly for package developer. Default is \code{FALSE}
#' @examples \dontrun{
#' Setup  ########################
#'
#' VintageUnitSQL <- "select id, origination_date as vintage_unit_date, product from small_portfolio"
#' PerformanceEventSQL <- "select id, repayment_date as event_date from small_portfolio"
#' Connection <- c('user','password','database','host','port')
#' 
#' # Get data ######################
#' 
#' # Hello vintage example
#' GetVintageData(VintageUnitSQL,PerformanceEventSQL,Connection=Connection)
#' 
#' # Aggregate data by quarters
#' GetVintageData(VintageUnitSQL,PerformanceEventSQL,TimeGroup='quarter',Connection=Connection)
#'  
#' # Expand maximum allowable date to specific date
#' GetVintageData(VintageUnitSQL,PerformanceEventSQL,TimeExpansion='2013-01-01',Connection=Connection)
#'  
#' # Keep columns in VintageUnitSQL, filter only product A
#' GetVintageData(VintageUnitSQL,PerformanceEventSQL,Connection=Connection,
#'                           SQLModifier = c('*',"product='A'"))
#'  
#' # Do not use any slicers (only vintage_unit_date and id will be used)
#' GetVintageData(VintageUnitSQL,PerformanceEventSQL,Connection=Connection,SQLModifier = NA)
#' 
#' # Add derived column
#' GetVintageData(VintageUnitSQL,PerformanceEventSQL,Connection=Connection,
#'                          SQLModifier = c("vintage_unit_date as granularity, product"))
#' 
#' # Return SQL instead of data
#' GetVintageData(VintageUnitSQL,PerformanceEventSQL,Connection=Connection, Result='sql')
#' }
#' @references http://www.statsoft.com/textbook/statistics-glossary/v/
#' @export 

GetVintageData <- function(VintageUnitSQL,PerformanceEventSQL,TimeGroup='month',TimeExpansion='none',Connection,
                           Result='data',DistanceFunctionSchema=NULL,SQLModifier=NULL,Verbose=FALSE, Debug=FALSE) {

require(RPostgreSQL)
  
options(sqldf.RPostgreSQL.user      = Connection[1], 
        sqldf.RPostgreSQL.password  = Connection[2],
        sqldf.RPostgreSQL.dbname    = Connection[3],
        sqldf.RPostgreSQL.host      = Connection[4], 
        sqldf.RPostgreSQL.port      = Connection[5])

CheckDatabase <- function(Connection) {
    
out <- tryCatch(
        {
          sqldf("select TRUE;")
        },
        error=function(cond) {
          out <- FALSE
        }
        )    
  return(out)
}

if (!CheckDatabase(Connection)) {
  stop("Not valid PostgreSQL connection.") 
} else {
  if(Verbose) cat("PostgreSQL connection is valid. \n")
}

# Add columns selection to VintageUnit if defined
VintageUnitSQLOut <- VintageUnitSQL

if (!is.null(SQLModifier)) {
  if (is.na(SQLModifier[1]) | SQLModifier[1]=='') {
    VintageUnitSQLOut <- paste("select id, vintage_unit_date from (", VintageUnitSQL, ") VintageUnitSQLSource")    
  } else if (SQLModifier[1] == '*') {
    VintageUnitSQLOut <- paste("select * from (", VintageUnitSQL, ") VintageUnitSQLSource")    
  } else {
    RequiredFields <- paste(",",SQLModifier[1])
    VintageUnitSQLOut <- paste("select id, vintage_unit_date", RequiredFields ,"from (", VintageUnitSQL, ") VintageUnitSQLSource")        
  }

  if (length(SQLModifier)==2) {
    VintageUnitSQLOut <- paste(VintageUnitSQLOut,"where",SQLModifier[2])
  }
}

# Test whether VintageUnitSQL contains at least id, vintage_unit_stat, vintage_unit_date

VintageUnitSQLNames <- names(sqldf(paste(VintageUnitSQLOut," limit 1")))

if (!any(VintageUnitSQLNames %in% 'id')) {
  stop ('VintageUnitSQL does not contain column "id".')
} else if (!any(VintageUnitSQLNames %in% 'vintage_unit_date')) {
  stop ('VintageUnitSQL does not contain column "vintage_unit_date".')  
}

# Test whether PerformanceEventSQL contains at least id, event_date
PerformanceEventSQLNames <- names(sqldf(paste(PerformanceEventSQL," limit 1")))

if (!any(PerformanceEventSQLNames %in% 'id')) {
  stop ('VintageUnitSQL does not contain column "id".')
} else if (!any(PerformanceEventSQLNames %in% 'event_date')) {
  stop ('VintageUnitSQL does not contain column "event_date".')  
} 

vGroups <- character(0)
vGroupsNonComma <- character(0)
vGroups <- VintageUnitSQLNames[!(VintageUnitSQLNames  %in% c('id','vintage_unit_date','vintage_unit_weight'))]

if (length(vGroups)==0) {
  if(Verbose) cat("No slicers defined.","\n")
} else {
  if(Verbose) cat("The following slicers will be applied:",vGroups,"\n")
  if(Debug)   cat("Length of vGroups is:", length(vGroups),'\n')
}

VintageUnitSQLOut = paste("with vintage_unit as (",VintageUnitSQLOut,")",sep="")
PerformanceEventSQL = paste(", performance_event as (",PerformanceEventSQL,")")

if (!(TimeGroup  %in% c('month','quarter','year'))) {
  stop('TimeGroup has to be one of month, quarter or year.')
}

if ( TimeGroup == "month" ) {
  if (Verbose) cat("Granularity of performance events will be 1 month. \n")
  vTimeGroupInterval = '1 month'
} else if ( TimeGroup == "quarter" ) {
  if (Verbose) cat("Granularity of performance events will be 1 quarter. \n")
  vTimeGroupInterval = '3 months'
} else if ( TimeGroup == 'year' ) {
  if (Verbose) cat("Granularity of performance events will be 1 year. \n")
  vTimeGroupInterval = '1 year'
}

# Sanity check TimExpansion

if (!(TimeExpansion %in% c('none','now','local') | grepl('[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}?',TimeExpansion))) {
  stop("TimeExpansion has to be one of none, now or date in yyyy-mm-dd format.")
}

if ( TimeExpansion=='none' ) {
  if (Verbose) cat("Used time expansion parameter: none \n")
  TimeExpansionOut="max_vintage_date"
} else if (TimeExpansion=='local') {
  if (Verbose) cat("Used time expansion parameter: local \n")
  TimeExpansionOut="max_vintage_date"  
} else if (TimeExpansion=='now') {
  if (Verbose) cat("Used time expansion parameter: now \n")
  TimeExpansionOut="Now()"  
} else {
  if (Verbose) cat("Used time expansion parameter:", TimeExpansion,"\n")  
  TimeExpansionOut=paste0("'",TimeExpansion,"'::date")
}



  vNonLast <- character(0)
  vLast <- character(0)
  
if (!(length(vGroups) ==0)) {
  vNonLast = paste(paste(vGroups[1:(length(vGroups)-1)],collapse=","),",")
  vLast = tail(vGroups, n=1)
  vGroupsNonComma <- paste(vGroups,collapse=",")    
  vGroups <- paste(paste(vGroups,collapse=","),",")  
  
}

if (length(vLast)==0) {
  vLast="1"
}

DistanceFunctionSchemaOut <- if (!is.null(DistanceFunctionSchema)) paste(DistanceFunctionSchema,'.',sep="")

################################################################################################
# Paste SQL components together
################################################################################################

if(Debug) {
  cat('Length and values of objects in SQL \n (length has to be 1, otherwise it results in SQL output multiplication:\n')
  cat('   VintageUnitSQLOut  : ',length(VintageUnitSQLOut),'\n')
  cat('   PerformanceEventSQL: ',length(PerformanceEventSQL),'\n')
  cat('   vGroups            : ',length(vGroups),' ',vGroups,'\n')
  cat('   vGroups            : ',length(vGroups),' ',vGroupsNonComma,'\n')  
  cat('   TimeGroup          : ',length(TimeGroup),' ',TimeGroup,'\n')
  cat('   TimeExpansionOut   : ',length(TimeExpansionOut),' ',TimeExpansionOut,'\n')
  cat('   vTimeGroupInterval : ',length(vTimeGroupInterval),' ',vTimeGroupInterval,'\n')
  cat('   vNonLast           : ',length(vNonLast),' ',vNonLast,'\n')
  cat('   vLast              : ',length(vLast),' ',vLast,'\n')
}

if (TimeExpansion == 'local' & length(vGroupsNonComma > 0)) {
  localsql <-paste("join (select
                            vug.gid, 
                            max(peg.event_date) as max_vintage_date 
                          from
                            performance_event_gid peg 
                            join vintage_unit_gid vug using (id) 
                          group by 
                            vug.gid) x using (gid) ")
} else {
  localsql <- paste("cross join (select max(event_date) as max_vintage_date from performance_event) x ")
}  


vSQL = paste(VintageUnitSQLOut,PerformanceEventSQL,
  "
  , vintage_unit_gid as (
     select
        vu.id, ",
        if(length(vGroupsNonComma)==0) "1::int" else paste("dense_rank() over(order by ",vGroupsNonComma,")") ," as gid,",
        if('vintage_unit_weight' %in% VintageUnitSQLNames) "vu.vintage_unit_weight " else "1::int ", " as vintage_unit_weight,
        vintage_unit_date
     from
        vintage_unit vu
  )
  
  , vintage_unit_gid_descriptors as (
     select
        distinct gid ", if (length(vGroupsNonComma)!=0) paste(",",vGroupsNonComma) ,"
     from
        vintage_unit
        join vintage_unit_gid using (id)
  )
  
  , performance_event_gid as (
     select
        id,
        gid,
        event_date,",
        if('event_weight' %in% PerformanceEventSQLNames) "event_weight" else "1::int"," as event_weight
     from
        vintage_unit_gid
        join performance_event using (id)
  )

  /* For every group (gid) generate vector of distances */
  ,vintage_descriptors as (
    select
      gid,
      vintage_unit_date, 
      generate_series(date_trunc('",TimeGroup,"',vintage_unit_date),",TimeExpansionOut,",'",vTimeGroupInterval,"')::date as event_date,
      ", DistanceFunctionSchemaOut ,"time_distance(generate_series(date_trunc('",TimeGroup,"',vintage_unit_date),",TimeExpansionOut,",'",vTimeGroupInterval,"')::date, vintage_unit_date,'",TimeGroup,"') as distance
    from(
      select 
        gid,
        vintage_unit_date, 
        max_vintage_date
      from 
        vintage_unit_gid ", localsql ,"group by 
        gid,
        vintage_unit_date,
        max_vintage_date
    ) a
  )
  
  ,vintage_unit_sums as(
      select
        gid,
        vintage_unit_date, 
        sum(vintage_unit_weight) as vintage_unit_weight,
        count(*) as vintage_unit_count
      from
        vintage_unit_gid
      group by 
        gid,
        vintage_unit_date
  )
  
  ,performance_event_sums as(
      select
        vug.gid,
        vug.vintage_unit_date, 
        date_trunc('",TimeGroup,"',peg.event_date)::date as event_date, 
        sum(peg.event_weight) as event_weight
      from
        vintage_unit_gid vug
        join performance_event_gid peg using (id)
      group by 
        vug.gid,
        vug.vintage_unit_date, 
        date_trunc('",TimeGroup,"',peg.event_date)::date
  )
  
  ,vintage_csums as (
      select 
        vd.*,
        vs.event_weight,
        sum(coalesce(event_weight,0)) 
          over(partition by gid, vintage_unit_date order by event_date) as event_weight_csum
      from 
        vintage_descriptors vd
        left join performance_event_sums vs using (gid, vintage_unit_date,event_date)
  )

  ,aggregation as (
     select
        vd.gid,
        vd.distance,
        sum(vintage_unit_weight) vintage_unit_weight,
        sum(vintage_unit_count) vintage_unit_count,
        sum(coalesce(event_weight,0)) as event_weight,
        case when sum(coalesce(vintage_unit_weight,0)) = 0 then null else sum(coalesce(event_weight,0)) / sum(vintage_unit_weight) end as event_weight_pct,
        --sum(coalesce(event_weight,0)) / sum(vintage_unit_weight)  as event_weight_pct,
        sum(coalesce(event_weight_csum,0)) as event_weight_csum,
        case when sum(coalesce(vintage_unit_weight,0)) = 0 then null else sum(coalesce(event_weight_csum,0))/sum(coalesce(vintage_unit_weight,0)) end as event_weight_csum_pct
        --sum(coalesce(event_weight_csum,0))/sum(coalesce(vintage_unit_weight,0))  as event_weight_csum_pct
     from
        vintage_descriptors  vd
        join vintage_unit_sums using  (gid, vintage_unit_date)
        left join vintage_csums using (gid, vintage_unit_date, event_date)
     group by
        vd.gid,
        vd.distance
  ) 


  select 
   ", vGroups ,"
   distance, 
   vintage_unit_weight, 
   vintage_unit_count, 
   event_weight, 
   event_weight_pct, 
   event_weight_csum, 
   event_weight_csum_pct,
   row_number() over(partition by gid) as rn 
  from 
   aggregation 
   join vintage_unit_gid_descriptors using (gid)
 order by
  ",vGroups,"distance"
  ,sep="")

if (Result=="data") {
    sqldf(vSQL) 
  } else if (Result=="sql") {
      print(vSQL)
  }  else {
   stop("Result has to be one of data or sql.")
}

}
