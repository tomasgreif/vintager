\name{CreateTimeDistanceFunction}
\alias{CreateTimeDistanceFunction}
\title{Creates time_distance function and load sample data}
\usage{
  CreateTimeDistanceFunction(FunctionSchema = "public",
    Connection, LoadData = FALSE, DataSchema = "public")
}
\arguments{
  \item{FunctionSchema}{Schema where \code{time_distance}
  function will be created. Default is \code{public}.}

  \item{Connection}{Connection to PostgreSQL database.
  Vector of exactly 5 elements in the following order:
  \code{user, password, database name, host, port}.}

  \item{LoadData}{When true than tables
  \code{small_portfolio} and \code{big_portfolio} with
  sample data will be created.}

  \item{DataSchema}{Schema where sample tables should be
  created. Default is \code{public}.}
}
\description{
  Creates time_distance function in given portfolio and
  optionally loads sample data.
}
\examples{
\dontrun{
Connection <- c('user','password','database','host','port')
CreateTimeDistanceFunction(Connection=Connection,LoadData=TRUE)
}
}

