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

#for each unique account and logged date, use the distance from the year end date to assign a quarter bucket

diy_bucketed_timelog <- ddply(timelog,
                           .var = c("Account.Name", "Date"),
                           .fun = function(x) {
                             
                             # Grab the year end from the services
                             x_services <- subset(services, subset = Account.Name %in% x$Account.Name)
                             x_ye <- unique(x_services$Year.End)
                             x_ye <- as.Date(paste(year(unique(x$Date)),x_ye, sep = "/"))
#                              browser()
                             qd <- as.numeric((x_ye - x$Date)/90) #quarter difference from year end
                             if (qd < 0){p1 <- TRUE}
                             if(abs(qd > 4)){qd <- qd%%4} #get a mod 4 quarter difference
                             if (qd > 0 & qd <= 1){
                                  cq <- 1
                                }else if (qd > 1 & qd <= 2){
                                  cq <- 2
                                }else if (qd > 2 & qd <= 3){
                                  cq <- 3
                                }else{
                                  cq <- 4
                             }
                             
                             result <- c("Q",cq, " ",year(x$Date))    
                           }
)