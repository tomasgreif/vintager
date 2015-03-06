# Vintage Analysis in R
`vintager` transforms source data stored in PostgreSQL or Oracle database into vintage analysis format. From detailed list of units (e.g. loans) and events (e.g. repayment), vintage curves are calculated.

## Pre-requisites
* R version 3.02 or higher
* PostgreSQL version 9.1 or higher
* ggplot2, sqldf, RPostgreSQL, devtools, XLSWrite, reshape2

## Installation

### Package installation

    library(devtools)
    install_github("vintager", username="tomasgreif")
    library(vintager)

### Create time_distance function and load sample data
    drv <- dbDriver("PostgreSQL")
    # Use real values!
    pgCon <- dbConnect(drv, user = 'usr', dbname="db", password = 'secret',
                       host = 'localhost', port = 5432)    
    createTimeDistanceFunction(connection = con, loadData = TRUE)

## Usage
`vintager` can work with the following data design:

  TBD

In the basic form, you can just use:

    drv <- dbDriver("PostgreSQL")
    # Use real values!
    pgCon <- dbConnect(drv, user = 'usr', dbname="db", password = 'secret',
                       host = 'localhost', port = 5432)
    
    vintageUnitSQL <- "select * from vintage_units"
    performanceEventSQL <- "select * from performance_events"
    getVintageData(vintageUnitSQL, performanceEventSQL, con = con)

For additional details see help:

    help(getVintageData)
    help(plotVintageData)
    help(printVintageData)
    help(aggregateVintageData)
    help(createTimeDistanceFunction)
