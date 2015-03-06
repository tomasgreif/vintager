#' Creates time_distance function and load sample data
#'
#' Creates time_distance function in given portfolio and optionally loads sample data.
#'
#' @param functionSchema Schema where \code{time_distance} function will be created. Default is \code{public}.
#' @param con Connection to PostgreSQL database as produced by \code{dbConnect(...)}
#' @param loadData When true than tables \code{small_portfolio} and \code{big_portfolio} with sample data will be created.
#' @param dataSchema Schema where sample tables should be created. Default is \code{public}.
#' @examples  \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' pgCon <- dbConnect(drv, user = 'usr', dbname="db", password = 'secret', host = 'localhost', port = 5432)
#' createTimeDistanceFunction(con = pgCon, loadData = TRUE)
#' }
#' @export 

createTimeDistanceFunction <- function(functionSchema="public", con, 
                                       loadData = FALSE, dataSchema = "public") {
  
timeDistanceFunction <- paste0("
CREATE OR REPLACE FUNCTION ", functionSchema, ".time_distance(to_date date, from_date date, granularity varchar)
  RETURNS integer AS
$BODY$

declare
  distance 	int;

begin

   if granularity = 'month' then
         distance := (extract(months from age(date_trunc('month',to_date), date_trunc('month',from_date))))::int + 12*(extract(years from age(date_trunc('month',to_date), date_trunc('month',from_date))))::int;
   elsif granularity = 'quarter' then
       distance := trunc(((extract(months from age(date_trunc('quarter',to_date), date_trunc('quarter',from_date)))) + 12*(extract(years from age(date_trunc('quarter',to_date), date_trunc('quarter',from_date)))))/3)::integer;
   elsif granularity = 'year' then
       distance := ((trunc(extract(years from age(date_trunc('month',to_date), date_trunc('month',from_date)))) ))::integer;
   else
       distance:= -1;
   end if;

	return distance;

end;
$BODY$
  LANGUAGE plpgsql IMMUTABLE COST 100;
")

sqldf(timeDistanceFunction, connection = con)

if(loadData) {
  sqldf(paste("drop table if exists ", dataSchema,".small_portfolio;
        CREATE TABLE ", dataSchema,".small_portfolio
        (
          id serial NOT NULL,
          product character varying,
          origination_date date,
          repayment_date date,
          CONSTRAINT id_small_pkey PRIMARY KEY (id)
        );",sep=""), connection = con)

  sqldf(paste("drop table if exists ", dataSchema,".big_portfolio;
        CREATE TABLE ", dataSchema,".big_portfolio
        (
          id serial NOT NULL,
          origination_date date,
          balance numeric,
          product character varying,
          region character varying,
          repayment_date date,
          CONSTRAINT id_big_pkey PRIMARY KEY (id)
        );",sep=""), connection = con)
  
  data(bigPortfolio, package="vintager", envir = environment())
  data(smallPortfolio, package="vintager",envir = environment())
  sqldf(paste("insert into ", dataSchema,".", bigPortfolio, collapse="", sep=""), connection = con)
  sqldf(paste("insert into ", dataSchema,".", smallPortfolio, collapse="", sep=""), connection = con)
}

}