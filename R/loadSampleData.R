#' Loads sample data
#'
#' Loads sample data to PostgreSQL or Oracle database
#'
#' @param con Connection to PostgreSQL or Oracle database as produced by \code{dbConnect(...)}
#' @param dataSchema Schema where sample tables should be created. Default is \code{public} for 
#'        PostgreSQL and user schema for Oracle.
#' @examples  \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' pgCon <- dbConnect(drv, user = 'usr', dbname="db", password = 'secret', host = 'localhost', port = 5432)
#' loadSampleData(con)
#' }
#' @export 

loadSampleData <- function(con, schema = NULL) {
  
  if(!is.null(schema)) {
    small_tbl <- paste0(schema, '.', 'small_portfolio')
    big_tbl   <- paste0(schema, '.', 'big_portfolio')    
  } else {
    small_tbl <- 'small_portfolio'
    big_tbl   <- 'big_portfolio'
  }
  
  small <- get(data(smallPortfolio, package="vintager", envir = environment()))
  big   <- get(data(bigPortfolio  , package="vintager", envir = environment()))
  
  if(class(con) == 'PostgreSQLConnection') {
    small_tbl <- tolower(small_tbl)
    big_tbl   <- tolower(big_tbl)
  } else if (class(con) == 'OraConnection') {
    small_tbl <- toupper(small_tbl)
    big_tbl   <- toupper(big_tbl)
    names(small) <- toupper(names(small))  
    names(big) <- toupper(names(big))    
  }

  dbWriteTable(con, small_tbl, small, row.names = FALSE)
  dbWriteTable(con, big_tbl,   big  , row.names = FALSE)
  
  if (class(con) == 'OraConnection') {
    dbSendQuery(con, 'alter table SMALL_PORTFOLIO modify (ORIGINATION_DATE date)')
    dbSendQuery(con, 'alter table SMALL_PORTFOLIO modify (REPAYMENT_DATE date)')  
    dbSendQuery(con, 'alter table BIG_PORTFOLIO modify (ORIGINATION_DATE date)')
    dbSendQuery(con, 'alter table BIG_PORTFOLIO modify (REPAYMENT_DATE date)')
    dbSendQuery(con, 'update SMALL_PORTFOLIO set ORIGINATION_DATE = trunc(ORIGINATION_DATE)')
    dbSendQuery(con, 'update SMALL_PORTFOLIO set REPAYMENT_DATE = TRUNC(REPAYMENT_DATE)')  
    dbSendQuery(con, 'update BIG_PORTFOLIO set ORIGINATION_DATE = TRUNC(ORIGINATION_DATE)')
    dbSendQuery(con, 'update BIG_PORTFOLIO set REPAYMENT_DATE = TRUNC(REPAYMENT_DATE)')    
    dbCommit(con)
  }
}