#' Get vintage data for PostgreSQL
#' @inheritParams getVintageData
#' 

getVintageDataPg <- function(vintageUnitSql, performanceEventSql, timeGroup, timeExpansion, con,
                           result, distanceFunctionSchema, sqlModifier, verbose, debug) {


# Add columns selection to VintageUnit if defined
vintageUnitSqlOut <- vintageUnitSql

if (!is.null(sqlModifier)) {
  if (is.na(sqlModifier[1]) | sqlModifier[1]=='') {
    vintageUnitSqlOut <- paste("select id, vintage_unit_date from (", vintageUnitSql, ") vintageUnitSqlSource")    
  } else if (sqlModifier[1] == '*') {
    vintageUnitSqlOut <- paste("select * from (", vintageUnitSql, ") vintageUnitSqlSource")    
  } else {
    requiredFields <- paste(",",sqlModifier[1])
    vintageUnitSqlOut <- paste("select id, vintage_unit_date", requiredFields ,"from (", vintageUnitSql, ") vintageUnitSqlSource")        
  }

  if (length(sqlModifier) == 2) {
    vintageUnitSqlOut <- paste(vintageUnitSqlOut, "where", sqlModifier[2])
  }
}

# Test whether vintageUnitSql contains at least id, vintage_unit_stat, vintage_unit_date

sql <- limitDb(con, vintageUnitSqlOut)
if (debug) message(sql)
vintageUnitSqlNames <- names(dbGetQuery(con, sql))


if (!any(toupper(vintageUnitSqlNames) %in% 'ID')) {
  stop ('vintageUnitSql does not contain column "id".')
} else if (!any(toupper(vintageUnitSqlNames) %in% 'VINTAGE_UNIT_DATE')) {
  stop ('vintageUnitSql does not contain column "vintage_unit_date".')  
}

# Test whether PerformanceEventSQL contains at least id, event_date
sql <- limitDb(con, performanceEventSql)
if(debug) message(sql)
performanceEventSqlNames <- names(dbGetQuery(con, sql))

if (!any(toupper(performanceEventSqlNames) %in% 'ID')) {
  stop ('vintageUnitSql does not contain column "id".')
} else if (!any(toupper(performanceEventSqlNames) %in% 'EVENT_DATE')) {
  stop ('vintageUnitSql does not contain column "event_date".')  
} 

# Define slicers. Slicer is everything except id, vintage_unit_date and vintage_unit_weight
# In other words these are all columns that will be used to form groups for which vintage
# data will be calculated
vGroups <- character(0)
vGroupsNonComma <- character(0)
vGroups <- vintageUnitSqlNames[!(toupper(vintageUnitSqlNames)  %in% c('ID','VINTAGE_UNIT_DATE','VINTAGE_UNIT_WEIGHT'))]

if (length(vGroups)==0) {
  if(verbose) cat("No slicers defined.","\n")
} else {
  if(verbose) cat("The following slicers will be applied:", vGroups, "\n")
  if(debug)   cat("Length of vGroups is:", length(vGroups), '\n')
}

vintageUnitSqlOut = paste("with vintage_unit as (",vintageUnitSqlOut,")",sep="")
performanceEventSql = paste(", performance_event as (", performanceEventSql, ")")

if (!(timeGroup  %in% c('month','quarter','year'))) {
  stop('timeGroup has to be one of month, quarter or year.')
}

if ( timeGroup == "month" ) {
  if (verbose) cat("Granularity of performance events will be 1 month. \n")
  vTimeGroupInterval = '1 month'
} else if (timeGroup == "quarter" ) {
  if (verbose) cat("Granularity of performance events will be 1 quarter. \n")
  vTimeGroupInterval = '3 months'
} else if (timeGroup == 'year' ) {
  if (verbose) cat("Granularity of performance events will be 1 year. \n")
  vTimeGroupInterval = '1 year'
}

# Sanity check TimExpansion

if (!(timeExpansion %in% c('none','now','local') | grepl('[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}?', timeExpansion))) {
  stop("timeExpansion has to be one of none, now or date in yyyy-mm-dd format.")
}

if ( timeExpansion == 'none' ) {
  if (verbose) cat("Used time expansion parameter: none \n")
  timeExpansionOut = "max_vintage_date"
} else if (timeExpansion == 'local') {
  if (verbose) cat("Used time expansion parameter: local \n")
  timeExpansionOut = "max_vintage_date"  
} else if (timeExpansion == 'now') {
  if (verbose) cat("Used time expansion parameter: now \n")
  timeExpansionOut = "Now()"  
} else {
  if (verbose) cat("Used time expansion parameter:", timeExpansion, "\n")  
  timeExpansionOut = paste0("'", timeExpansion,"'::date")
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

distanceFunctionSchemaOut <- if (!is.null(distanceFunctionSchema)) paste(distanceFunctionSchema,'.',sep="") 


################################################################################################
# Paste SQL components together
################################################################################################

if(debug) {
  cat('Length and values of objects in SQL \n (length has to be 1, otherwise it results in SQL output multiplication:\n')
  cat('   vintageUnitSqlOut  : ',length(vintageUnitSqlOut),'\n')
  cat('   PerformanceEventSQL: ',length(performanceEventSql),'\n')
  cat('   vGroups            : ',length(vGroups),' ',vGroups,'\n')
  cat('   vGroups            : ',length(vGroups),' ',vGroupsNonComma,'\n')  
  cat('   TimeGroup          : ',length(timeGroup),' ',timeGroup, '\n')
  cat('   TimeExpansionOut   : ',length(timeExpansionOut),' ',timeExpansionOut,'\n')
  cat('   vTimeGroupInterval : ',length(vTimeGroupInterval),' ',vTimeGroupInterval,'\n')
  cat('   vNonLast           : ',length(vNonLast),' ',vNonLast,'\n')
  cat('   vLast              : ',length(vLast),' ',vLast,'\n')
}

if (timeExpansion == 'local' & length(vGroupsNonComma > 0)) {
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

## Generate vintage time space for every gid. This is different for PostgreSQL and Oracle
#  because PostgreSQL has generate_series() while Oracle has connect by syntax.

if (class(con) == 'PostgreSQLConnection') {
  vintageTimeSpace <- paste0("  /* For every group (gid) generate vector of distances */
    , vintage_descriptors as (
      select
        gid,
        vintage_unit_date, 
        generate_series(date_trunc('",timeGroup,"',vintage_unit_date),",timeExpansionOut,",'",vTimeGroupInterval,"')::date as event_date,
        ", distanceFunctionSchemaOut ,"time_distance(generate_series(date_trunc('",timeGroup,"',vintage_unit_date),",timeExpansionOut,",'",vTimeGroupInterval,"')::date, vintage_unit_date,'", timeGroup,"') as distance
      from(
        select 
          gid,
          vintage_unit_date, 
          max_vintage_date
        from 
          vintage_unit_gid ", localsql ,
       "group by 
          gid,
          vintage_unit_date,
          max_vintage_date
      ) a
    )")
} else if (class(con) == 'OraConnection') {
  vintageTimeSpace <- paste0("
    , vintage_descriptors as (
      select
        gid,
        vintage_unit_date, 
        add_months(trunc(vintage_unit_date, 'month'), level -1) as event_date,
        level - 1 as distance      
  from(
        select 
          gid,
          vintage_unit_date, 
          max_vintage_date
        from 
          vintage_unit_gid ", localsql ,
       "group by 
          gid,
          vintage_unit_date,
          max_vintage_date
      ) a
    connect by level <= months_between(trunc(max_vintage_date, 'mm'), trunc(vintage_unit_date, 'mm')) + 1
           and prior gid = gid
           and prior vintage_unit_date = vintage_unit_date
           and prior dbms_random.value is not null
    )")  
}



vSQL <- paste(vintageUnitSqlOut, performanceEventSql,
  "
  , vintage_unit_gid as (
     select
        vu.id, ",
        if(length(vGroupsNonComma)==0) "cast(1 as integer)" else paste("dense_rank() over(order by ",vGroupsNonComma,")") ," as gid,",
        if('vintage_unit_weight' %in% vintageUnitSqlNames) "vu.vintage_unit_weight " else "cast(1 as integer) ", " as vintage_unit_weight,
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
        if('event_weight' %in% performanceEventSqlNames) "event_weight" else "cast(1 as integer)"," as event_weight
     from
        vintage_unit_gid
        join performance_event using (id)
  )",

  vintageTimeSpace,
  
  ",vintage_unit_sums as(
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
        cast(date_trunc('", timeGroup,"',peg.event_date) as date) as event_date, 
        sum(peg.event_weight) as event_weight
      from
        vintage_unit_gid vug
        join performance_event_gid peg using (id)
      group by 
        vug.gid,
        vug.vintage_unit_date, 
        date_trunc('", timeGroup, "',peg.event_date)::date
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

if (result=="data") {
    dbGetQuery(con, vSQL) 
  } else if (result=="sql") {
      cat(vSQL)
  }  else {
   stop("Result has to be one of data or sql.")
}

}
