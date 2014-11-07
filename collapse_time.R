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

collapsed_history$billable_time <- NA
collapsed_history$concurrent_services <- NA

ptm <- proc.time()
uniques <- unique(collapsed_history[, c("Account.Name", "Quarter.End", "filing.estimate")])
for (i in 1:nrow(uniques)){
    loop_timelog <- timelog[timelog$Account.Name %in% uniques[i,1] & timelog$Billable %in% c("1") &
                              timelog$Date >= uniques[i,2] & timelog$Date <= uniques[i,3] & !is.na(timelog$Date), ]
    if (!is.null(loop_timelog)){
      collapsed_history[collapsed_history$Account.Name %in% uniques[i,1] & 
                          collapsed_history$filing.estimate %in% uniques[i,3],]$billable_time <- sum(loop_timelog$Hours)
      collapsed_history[collapsed_history$Account.Name %in% uniques[i,1] & 
                          collapsed_history$filing.estimate %in% uniques[i,3],]$concurrent_services <- length(unique(loop_timelog$Services.ID))
    }
}
proc.time() - ptm

# code to export 
setwd('C:/R/workspace/collapsed_time/output')
export <- collapsed_history
export <- data.frame(lapply(export, as.character), stringsAsFactors = F)
export[is.na(export)] <- ""
write.csv(export, file = "CollapsedHistoryR.csv", row.names = F, na = "")
