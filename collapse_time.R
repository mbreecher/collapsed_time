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
for (account in unique(collapsed_history$Account.Name)){
  for(deadline in unique(collapsed_history[collapsed_history$Account.Name %in% account,]$filing.estimate)){
    loop_timelog <- timelog[timelog$Account.Name %in% account & timelog$Billable %in% c("1") &
                              timelog$Date >= deadline - 40 & timelog$Date <= deadline & !is.na(timelog$Date), ]
    if (!is.null(loop_timelog)){
      collapsed_history[collapsed_history$Account.Name %in% account & 
                          collapsed_history$filing.estimate %in% deadline,]$billable_time <- sum(loop_timelog$Hours)
      collapsed_history[collapsed_history$Account.Name %in% account & 
                          collapsed_history$filing.estimate %in% deadline,]$concurrent_services <- dim(loop_timelog)[1]
    }
  }
}
proc.time() - ptm

# code to export 
setwd('C:/R/workspace/collapsed_time/output')
export <- collapsed_history
export <- data.frame(lapply(export, as.character), stringsAsFactors = F)
export[is.na(export)] <- ""
write.csv(export, file = "CollapsedHistoryR.csv", row.names = F, na = "")
