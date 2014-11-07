library(reshape2)
library(plyr)
library(RecordLinkage)

# Pull in import functions
setwd('C:/R/workspace/collapsed_time')
source("import_functions.R")

services <- import_services()
timelog <- import_timelog()

collapsed_time <- aggregate(Hours ~ Services.ID, FUN = sum, data = timelog)

collapsed_history <- merge(services, collapsed_time, "Services.ID", all.x = T)
