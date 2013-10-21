\name{PlotVintageData}
\alias{PlotVintageData}
\title{Plot vintage data}
\usage{
  PlotVintageData(data = NULL, x = "distance",
    y = "event_weight_csum_pct", cond = NULL,
    facets = NULL)
}
\arguments{
  \item{data}{Result of \code{GetVintageData} function.}

  \item{x}{Data to be displayed on \code{x} axis. Default
  is distance}

  \item{y}{Data to be displayed on \code{y} axis. Default
  is \code{event_weight_csum_pct}.}

  \item{cond}{Variable to be used for conditioning
  (in-chart group).}

  \item{facets}{Formulat to be used for facetting.}
}
\description{
  This function plots results of \code{GetVintageData}
  function. All \code{Slicers} in results have to be
  included in plot definition.
}

