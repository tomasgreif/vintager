\name{AggregateVintageData}
\alias{AggregateVintageData}
\title{Aggregate vintage data}
\usage{
  AggregateVintageData(VintageData = NA, Slicers = NA,
    Type = "include", TimeAggregationUnit = 1,
    SQLModifier = NA, Verbose = FALSE)
}
\arguments{
  \item{VintageData}{Result of function
  \code{GetVintageData} or data frame with the same
  structure. At least the following columns have to be
  present: \code{distance, vintage_unit_weight,
  vintage_unit_count, event_weight, event_weight_pct,
  event_weight_csum, event_weight_csum_pct}}

  \item{Slicers}{Vector of slicers which should be included
  or excluded.}

  \item{Type}{Indicating inclusion or exclusion of
  \code{Slicers}. Default is to include specified slicers.
  Possible values are \code{include} and \code{exclude}.}

  \item{TimeAggregationUnit}{Defines how distance should be
  aggregated. By default, distance is not modified at all.
  When specified, single integer has to be used. For
  example using value 3 and distance values in
  \code{VintageData} \code{0,1,2,3,4,5,6,7,8,9} the result
  will be aggregated to \code{0,1,2,3} (\code{(0,1,2)->0,
  (3,4,5)->1, (6,7,8)->2, (9)->3}).}

  \item{SQLModifier}{Optional \code{WHERE} clause to filter
  \code{VintageData} before aggretation}

  \item{Verbose}{If true, then additional diagnostic
  messages are displayed. Default is \code{FALSE}.}
}
\description{
  This function aggregates already calculated vintage data
  by removing or keeping selected \code{Slicers}.
}
\examples{
\dontrun{
Setup  ########################

VintageUnitSQL <- "select id, origination_date as vintage_unit_date, product, region,
date_trunc('month',origination_date)
as origination_month from big_portfolio"
PerformanceEventSQL <- "select id, repayment_date as event_date from big_portfolio"
Connection <- c('user','password','database','host','port')

# Get source data ######################
VintageData <- GetVintageData(VintageUnitSQL,PerformanceEventSQL,Connection=Connection)
# Now we have vintage curves by product, region and origination_month

# Aggregations ##########################

# Show data by region
AggregateVintageData(VintageData,Slicers='region')
# Show data by origination_month
AggregateVintageData(VintageData,Slicers='origination_month')
# By product and region
AggregateVintageData(VintageData,Slicers=c('region','product'))
#Everything but origination_month
AggregateVintageData(VintageData,Slicers='origination_month',Type='exclude')
#Everything but origination_month
AggregateVintageData(VintageData,Slicers='origination_month',Type='exclude')
#Exclude all slicers, only distance will be displayed
AggregateVintageData(VintageData,Type='include')
#Exclude all slicers and aggregate also distance (set high TimeAggregationUnit parameter)
AggregateVintageData(VintageData,Type='include',TimeAggregationUnit=1000)
#Keep only product dimension, only product "Giga Loan"
AggregateVintageData(VintageData,Slicers='product',SQLModifier="product = 'Giga Loan'")
#Keep only product dimension, only product "Giga Loan" and region "West"
AggregateVintageData(VintageData,Slicers='product',SQLModifier="product = 'Giga Loan' and region = 'West'")
# Show diagnostic messages
AggregateVintageData(VintageData,Slicers='region',Verbose=TRUE)
}
}

