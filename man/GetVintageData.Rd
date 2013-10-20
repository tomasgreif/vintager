\name{GetVintageData}
\alias{GetVintageData}
\title{Get data in vintage analysis format}
\usage{
  GetVintageData(VintageUnitSQL, PerformanceEventSQL,
    TimeGroup = "month", TimeExpansion = "none",
    Connection, Result = "data",
    DistanceFunctionSchema = NULL, SQLModifier = NULL)
}
\arguments{
  \item{VintageUnitSQL}{Valid SQL statement. If
  \code{SQLModifier} is not used than at least the
  following columns have to be in the result: \code{id,
  vintage_unit_date}. If vintage units should be weighted
  than column \code{vintage_unit_weight} has to be as
  column in result. Result can contain any number of
  additional columns. These columns are referred as
  \code{Slicers}. All columns in result will be used to
  form vintages - every unique combination of
  \code{Slicers} (all columns except \code{id,
  vintage_unit_date, vintage_unit_weight}) will be
  considered as one vintage.}

  \item{PerformanceEventSQL}{Valid SQL statement. At least
  the following columns have to be in the result: \code{id,
  event_date}. If event should be weighted than column
  event_weight has to be in result. It does not make sense
  to include any other columns as these are not used by the
  function. Column named id will be used as a key to
  results of \code{VintageUnitSQL}. Currently, only single
  column key is supported.}

  \item{TimeGroup}{Aggregation of vintage data. Defines how
  distance between vintage_unit_date and event_date is
  measured. Possible values are \code{month, quarter,
  year}. Distance calculation is performed by custom
  PostgreSQL function named \code{time_distance}. Code to
  create this function is stored in
  \code{XXX/time_distance.sql}}

  \item{TimeExpansion}{Defines how time expansion is
  performed. By default, vintages will be generated up to
  last existing point in events. E.g. when maximum distance
  in data is 10 than every vintage will have 10
  observations. There are two other options, using
  \code{now} or date in \code{yyyy-mm-dd} format.
  \code{now} will be internally replaced by current date.
  If any of these option is used than value of parameter
  will be used as last available point in data. Thus,
  number of points for every vintage will be expanded. Note
  that this might return unexpected results if this date is
  earlier than last point in events.}

  \item{Connection}{Connection to PostgreSQL database.
  Vector of exactly 5 elements in the following order:
  \code{user, password, database name, host, port}.}

  \item{Result}{Type of results to return. By default
  (\code{data}), vintage data are returned. The other
  option is to use \code{sql} - this will return SQL
  statement to get vintage data.}

  \item{DistanceFunctionSchema}{Name of database schema
  where \code{time_distance} function is available.}

  \item{SQLModifier}{This will constraint result of
  VintageUnitSQL to selected columns and/or rows. Vector
  with 1 or 2 elements. In the first element, required
  columns are specified, the second can contain additional
  \code{WHERE} condition. If used than VintageUnitSQL is
  wrapped into \code{SELECT id, vintage_unit_date [, First
  Element] FROM (VintageUnitSQL) x [WHERE Second Element]}.
  When only WHERE clause should be used than first element
  has to be asterisk ('*'). If first element is empty
  string or NA than only columns \code{id} and
  \code{vintage_unit_date} will be used from results of
  \code{VintageUnitSQL}.}
}
\description{
  This function prepares data for vintage analysis.
  Currently it supports only data stored in PostgreSQL
  database.
}
\details{
  If successful, dataset with the following columns is
  returned: \tabular{ll}{ Column \tab Description\cr
  \code{[Slicers]} \tab Additional colums used for
  conditioning of vintages as used in
  \code{VintageUnitSQL}.\cr \code{distance} \tab Distance
  between vintage unit date and event date measured in time
  interval (currently month, quarter or year).\cr
  \code{vintage_unit_weight} \tab sum of vintage unit
  weights for given vintage at distance. Note that one
  vintage can have different values for different distances
  when no slicer is based on vintage unit date and with
  granularity equal to time grouping of events (see
  examples to understand this better). In case when no
  weight is defined that values are equal as in column
  vintage_unit_count\cr \code{vintage_unit_count} \tab
  number (of rows) for given vintage at distance. Using
  same logic as vintage unit weight.\cr \code{event_weight}
  \tab Sum of event weights for given vintage at distance.
  If no weight is defined, than 1 is used (equal to row
  count)\cr \code{event_weight_pct} \tab event_weight_pct /
  vintage_unit_weight\cr \code{event_weight_csum} \tab
  running total of event weights for given vintage ordered
  by distance. If no slicer is based on vintage date and
  with granularity equal to time grouping of events, than
  running total cannot be reproduced as running total of
  event_weight! See examples to understand this better.\cr
  \code{event_weight_csum_pct} \tab a\cr \code{rn} \tab
  a\cr } This function is tested with PostgreSQL 9.1, but
  any version with window functions support should work.
}

