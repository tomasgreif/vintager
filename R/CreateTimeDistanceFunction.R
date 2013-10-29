#' Creates time_distance function and load sample data
#'
#' Creates time_distance function in given portfolio and optionally loads sample data.
#'
#' @param FunctionSchema Schema where \code{time_distance} function will be created. Default is \code{public}.
#' @param Connection Connection to PostgreSQL database. Vector of exactly 5 elements in the following 
#' order: \code{user, password, database name, host, port}.
#' @param LoadData When true than tables \code{small_portfolio} and \code{big_portfolio} with sample data will be created.
#' @param DataSchema Schema where sample tables should be created. Default is \code{public}.
#' @examples  \dontrun{
#' Connection <- c('user','password','database','host','port')
#' CreateTimeDistanceFunction(Connection=Connection,LoadData=TRUE)
#' }
#' @export 

CreateTimeDistanceFunction <- function(FunctionSchema="public",Connection,LoadData=FALSE,DataSchema="public") {

  options(sqldf.RPostgreSQL.user      = Connection[1], 
          sqldf.RPostgreSQL.password  = Connection[2],
          sqldf.RPostgreSQL.dbname    = Connection[3],
          sqldf.RPostgreSQL.host      = Connection[4], 
          sqldf.RPostgreSQL.port      = Connection[5])
  

TimeDistanceFunction <- paste("
CREATE OR REPLACE FUNCTION ",FunctionSchema,".time_distance(to_date date, from_date date, granularity varchar)
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
  ",sep="")
sqldf(TimeDistanceFunction)

if(LoadData) {
  sqldf(paste("drop table if exists ",DataSchema,".small_portfolio;
        CREATE TABLE ",DataSchema,".small_portfolio
        (
          id serial NOT NULL,
          product character varying,
          origination_date date,
          repayment_date date,
          CONSTRAINT id_small_pkey PRIMARY KEY (id)
        );",sep=""))

  sqldf(paste("drop table if exists ",DataSchema,".big_portfolio;
        CREATE TABLE ",DataSchema,".big_portfolio
        (
          id serial NOT NULL,
          origination_date date,
          balance numeric,
          product character varying,
          region character varying,
          repayment_date date,
          CONSTRAINT id_big_pkey PRIMARY KEY (id)
        );",sep=""))
  data(BigPortfolio,package="pgvint",envir = environment())
  data(SmallPortfolio,package="pgvint",envir = environment())
  sqldf(paste("insert into ",DataSchema,".",BigPortfolio,collapse="",sep=""))
  sqldf(paste("insert into ",DataSchema,".",SmallPortfolio,collapse="",sep=""))
}

}