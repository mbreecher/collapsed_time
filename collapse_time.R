library(reshape2)
library(plyr)
library(RecordLinkage)

# Pull in import functions
setwd("C:/R/workspace/shared")
source("import_functions.R")
setwd('C:/R/workspace/collapsed_time')
source("diy_periods.R")

services <- import_services()
timelog <- import_timelog()
diy_time <- import_billable() 

collapsed_time <- aggregate(Hours ~ Services.ID, FUN = sum, data = timelog)

collapsed_history <- merge(services, collapsed_time, "Services.ID", all.x = T)

# #alternatively, use plyr
pts <- proc.time()
count.unique <- function(x) { length(unique(x[!is.na(x)])) } #function to count unique non-NA values

#I need customer period to merge diy time
customer_period <- ddply(collapsed_history,
                              .var = c("Account.Name", "filing.estimate", "Service.Name", "Service.Type", "Form.Type", "Quarter.End"),
                              .fun = function(x) {
                                
                                # Grab the year end from the services
                                x_ye <- unique(x$Year.End)
                                x_ye <- as.Date(paste(year(unique(x$filing.estimate)),x_ye, sep = "/"), format = "%Y/%m/%d")
                                
                                qd <- as.numeric((unique(x$filing.estimate)-x_ye)/91)%%4 #quarter difference from year end
                                #if(abs(qd > 4)){qd <- qd%%4} #get a mod 4 quarter difference
                                if(!is.na(qd)){if(qd >= 0){cq <- ceiling(qd)}else{cq <- floor(qd)}}
                                
                                aq <- paste("Q", ceiling(as.numeric(month(unique(x$filing.estimate))/3)), year(unique(x$filing.estimate)), sep = "")
                                
                                if(!is.na(cq) & !(x_ye %in% c("     ")) & !is.na(x_ye)){
                                  if(unique(x$filing.estimate) < x_ye){
                                    data.frame(customer_quarter = paste("Q",cq, " ",year(unique(x$filing.estimate)), sep = ""), 
                                               calendar_quarter = aq,
                                               year_end = x_ye,
                                               Hours = sum(x$Hours))  
                                  }else{
                                    data.frame(customer_quarter = paste("Q",abs(cq), " ",year(unique(x$filing.estimate))+1, sep = ""), 
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

diy_time_simple <- diy_time[,c("Account.Name", "customer_quarter", "calendar_quarter", "Hours")]
names(diy_time_simple) <- c("Account.Name", "customer_quarter", "calendar_quarter", "Billable.Hours")
# diy_time_simple$calendar_quarter <- paste(substr(diy_time_simple$calendar_quarter,3,6), 
#                                           substr(diy_time_simple$calendar_quarter,2,2), sep = "")
export <- merge(customer_period, diy_time_simple, by = c("Account.Name", "customer_quarter", "calendar_quarter"), all = T)

#some cleanup
names(export[c("Hours")]) <- c("Project.Hours")
export[is.na(export$Service.Name),]$customer_quarter <- NA

# code to export 
setwd('C:/R/workspace/collapsed_time/output')
export <- data.frame(lapply(export, as.character), stringsAsFactors = F)
export[is.na(export)] <- ""
write.csv(export, file = "CollapsedHistoryR.csv", row.names = F, na = "")
