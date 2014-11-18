library(reshape2)
library(plyr)
library(RecordLinkage)

# Pull in import functions
setwd('C:/R/workspace/shared')
source("import_functions.R")
setwd('C:/R/workspace/collapsed_time')

services <- import_services()
timelog <- import_timelog()

collapsed_time <- aggregate(Hours ~ Services.ID, FUN = sum, data = timelog)

collapsed_history <- merge(services, collapsed_time, "Services.ID", all.x = T)

timelog <- timelog[!is.na(timelog$Account.Name) & timelog$Billable %in% 1, ]

#for each unique account and logged date, use the distance from the year end date to assign a quarter bucket
diy_bucketed_timelog <- ddply(timelog,
                           .var = c("Account.Name", "Date"),
                           .fun = function(x) {
                             
                             # Grab the year end from the services
                             x_services <- subset(services, subset = Account.Name %in% x$Account.Name, na.rm = T)
                             x_ye <- unique(x_services$Year.End)
                             x_ye <- as.Date(paste(year(unique(x$Date)),x_ye, sep = "/"), format = "%Y/%m/%d")

                             qd <- as.numeric((unique(x$Date)-x_ye)/91)%%4 #quarter difference from year end
                             #if(abs(qd > 4)){qd <- qd%%4} #get a mod 4 quarter difference
                             cq <- ceiling(qd)
                             aq <- paste("Q", ceiling(as.numeric(month(unique(x$Date))/3)), year(unique(x$Date)), sep = "")
                             
                             if(!is.na(cq)){
                               if(cq < 0){
                                 data.frame(customer_quarter = paste("Q",cq, " ",year(unique(x$Date)), sep = ""), 
                                            calendar_quarter = aq,
                                            year_end = x_ye,
                                            Hours = sum(x$Hours))  
                               }else{
                                 data.frame(customer_quarter = paste("Q",abs(cq), " ",year(unique(x$Date))+1, sep = ""), 
                                            calendar_quarter = aq,
                                            year_end = x_ye,
                                            Hours = sum(x$Hours))
                               }
                             }else{
                               data.frame(customer_quarter = NA, 
                                          calendar_quarter = aq,
                                          year_end = NA,
                                          Hours = sum(x$Hours))
                             }
                             
                             #browser()
                           }
)

#aggregate diy time by account and quarter
diy_time <- with(diy_bucketed_timelog,
                 aggregate(Hours ~ Account.Name + customer_quarter + calendar_quarter + year_end, FUN = sum))
#customers with no services will be dropped from the above, so we need to grab them separately and bind the result.
  diy_time_no_services <- with(diy_bucketed_timelog[is.na(diy_bucketed_timelog$customer_quarter),],
                   aggregate(Hours ~ Account.Name, FUN = sum))
  #add some columns and order before bind
  diy_time_no_services$customer_quarter <- NA; diy_time_no_services$calendar_quarter <- NA; diy_time_no_services$year_end <- NA
  diy_time_no_services <- diy_time_no_services[,names(diy_time)]
  diy_time <- rbind(diy_time, diy_time_no_services)

#order by account name then customer quarter
diy_time <- diy_time[order(diy_time$Account.Name, diy_time$customer_quarter),]

# code to export 
setwd('C:/R/workspace/collapsed_time/output')
export <- diy_time
write.csv(export, file = "diy_time.csv", row.names = F, na = "")
