library(reshape2)
library(plyr)
library(RecordLinkage)

# Pull in import functions
setwd("C:/R/workspace/shared")
source("import_functions.R")

setwd('C:/R/workspace/collapsed_time')

services <- import_services()
timelog <- import_timelog()

collapsed_time <- aggregate(Hours ~ Services.ID, FUN = sum, data = timelog)

collapsed_history <- merge(services, collapsed_time, "Services.ID", all.x = T)

# #alternatively, use plyr
pts <- proc.time()
count.unique <- function(x) { length(unique(x[!is.na(x)])) } #function to count unique non-NA values

# use ddply to aggregate
collapsed_timelog <- ddply(collapsed_history,
                           .var = c("Account.Name", "Quarter.End", "filing.estimate"),
                           .fun = function(x) {
                             
#                              if (dim(x)[1] > 1){
#                                browser()  
#                              }
                             # Grab the appropriate subset of timelog
                             x_timelog <- subset(timelog, 
                                                 subset = Account.Name %in% x$Account.Name &
                                                   Billable %in% 1 &
                                                   Date >= unique(x$Quarter.End) &
                                                   Date <= unique(x$filing.estimate) &
                                                   !is.na(Date) &
                                                   !is.na(Hours)
                             )
                             
                             # Aggregate using summarise - basically generates a data.frame with just
                             # the variables I name on the 2nd and 3rd lines
                             summarise(x_timelog,
                                       billable_time = sum(Hours),
                                       concurrent_services = count.unique(x_timelog$Services.ID),
                                       divided_billable_time = sum(Hours)/count.unique(x_timelog$Services.ID)
                             )
                            }
)


# Then merging the aggregated results
collapsed_history_time <- merge(x = collapsed_history,
                                y = collapsed_timelog,
                                by = c("Account.Name", "filing.estimate"),
                                all.x = TRUE
)
proc.time() - pts

# code to export 
setwd('C:/R/workspace/collapsed_time/output')
export <- collapsed_history_time
export$divided_billable_time[is.na(export$divided_billable_time)] <- NA
export <- data.frame(lapply(export, as.character), stringsAsFactors = F)
export[is.na(export)] <- ""
write.csv(export, file = "CollapsedHistoryR.csv", row.names = F, na = "")
