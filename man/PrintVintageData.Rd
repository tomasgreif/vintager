\name{PrintVintageData}
\alias{PrintVintageData}
\title{Print Vintage Data}
\usage{
  PrintVintageData(VintageData,
    Columns = c("vintage_unit_weight", "vintage_unit_count", "event_weight", "event_weight_pct", "event_weight_csum", "event_weight_csum_pct"),
    Result = "print", File = "VintageData.xls")
}
\arguments{
  \item{data}{Result of \code{GetVintageData} function.}

  \item{Columns}{Columns to be printed. Any combination of
  the following possible statistics:
  \code{vintage_unit_weight, vintage_unit_count,
  event_weight, event_weight_pct, event_weight_csum,
  event_weight_csum_pct}. For every statistic one table
  will be printed.}

  \item{Result}{Type of result. By default, data are
  printed. If \code{xls} is used than data are written to
  XLS file.}

  \item{File}{Name of XLS file where data should be stored.
  Makes sense only when \code{Result} is set to \code{xls}.
  If no name is given and \code{Result} is \code{xls} then
  name \code{VintageData.xls} will be used as default.}
}
\description{
  This function prints results of \code{GetVintageData}
  function. All \code{Slicers} in results have to be
  included in plot definition. Data are pivoted by
  \code{distance} column.
}
\examples{
\dontrun{
PrintVintageData(VintageData,"event_weight_csum_pct",Result='xls')
PrintVintageData(VintageData,c("event_weight, event_weight_csum_pct"),Result='xls')
PrintVintageData(VintageData,Result='xls',File='MyVintageAnalysis')
}
}

