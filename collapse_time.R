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

timelog <- timelog[timelog$Date <= Sys.Date() & timelog$Date >= as.Date("2012-06-30"),]

collapsed_time <- aggregate(Hours ~ Services.ID, FUN = sum, data = timelog)
collapsed_history <- merge(services, collapsed_time, "Services.ID", all.x = T)

collapsed_history <- collapsed_history[collapsed_history$filing.estimate <= Sys.Date() & 
                                         collapsed_history$filing.estimate >= as.Date("2012-06-30"),]


pts <- proc.time()
count.unique <- function(x) { length(unique(x[!is.na(x)])) } #function to count unique non-NA values

#I need customer period to merge diy time
customer_period <- ddply(collapsed_history,
                              .var = c("Account.Name", "filing.estimate", "Service.Name", "Service.Type", "Form.Type", "Quarter.End"),
                              .fun = function(x) {
                                
                                # Grab the year end from the services
                                x_ye <- unique(x$Year.End)
                                x_ye <- as.Date(paste(year(unique(x$filing.estimate)),x_ye, sep = "/"), format = "%Y/%m/%d")
                                
                                qd <- as.numeric((unique(x$filing.estimate)-x_ye)/90)%%4 #quarter difference from year end
                                pqd <- as.numeric((unique(x$filing.estimate)-x_ye - 90)/90)%%4 #quarter difference from year end (prior quarter)
                                #if(abs(qd > 4)){qd <- qd%%4} #get a mod 4 quarter difference
                                if(!is.na(qd)){if(qd > 0){cq <- ceiling(qd)}else{cq <- floor(qd)}}
                                if(!is.na(pqd)){if(pqd > 0){pcq <- ceiling(pqd)}else{pcq <- floor(pqd)}}
                                if(cq %in% 0){cq = 4}
                                if(pcq %in% 0){pcq = 4}
                                
                                #actual quarter
                                aq <- paste(year(unique(x$filing.estimate)),"Q", ceiling(as.numeric(month(unique(x$filing.estimate))/3)),  sep = "")
                                #actual reporting quarter
                                arq <- paste(year(unique(x$filing.estimate)-90),"Q", ceiling(as.numeric(month(unique(x$filing.estimate)-90)/3)),  sep = "")
                                
                                if(!is.na(cq) & !(x_ye %in% c("     ")) & !is.na(x_ye)){
                                  if(unique(x$filing.estimate) < x_ye){
                                    data.frame(customer_quarter_work_done = paste(year(unique(x$filing.estimate)),"Q",cq, sep = ""), 
                                               customer_quarter_reported = paste(year(unique(x$filing.estimate) - 90),"Q",pcq, sep = ""), 
                                               calendar_quarter_work_done = aq,
                                               calendar_quarter_reported = arq,
                                               year_end = x_ye,
                                               Hours = sum(x$Hours))  
                                  }else{
                                    data.frame(customer_quarter_work_done = paste(year(unique(x$filing.estimate))+1,"Q",abs(cq), sep = ""), 
                                               customer_quarter_reported = paste(year(unique(x$filing.estimate)-90)+1,"Q",abs(pcq), sep = ""), 
                                               calendar_quarter_work_done = aq,
                                               calendar_quarter_reported = arq,
                                               year_end = x_ye,
                                               Hours = sum(x$Hours))
                                  }
                                }else{
                                  data.frame(customer_quarter_work_done = NA, 
                                             customer_quarter_reported = NA,
                                             calendar_quarter_work_done = aq,
                                             calendar_quarter_reported = arq,
                                             year_end = NA,
                                             Hours = sum(x$Hours))
                                }
                                
                                #browser()
                              }
)

start <- c("Account.Name", "calendar_quarter_reported", "calendar_quarter_work_done", "customer_quarter_reported", "customer_quarter_work_done")

diy_time_simple <- diy_time[,c(start, "Hours")]
names(diy_time_simple) <- c(start, "Billable.Hours")
# diy_time_simple$calendar_quarter <- paste(substr(diy_time_simple$calendar_quarter,3,6), 
#                                           substr(diy_time_simple$calendar_quarter,2,2), sep = "")

export <- merge(customer_period, diy_time_simple, by = start, all = T)

export <- export[,c(start, names(export)[!(names(export) %in% start)])]

#some cleanup
names(export[c("Hours")]) <- c("Project.Hours")

# code to export 
setwd('C:/R/workspace/collapsed_time/output')
export <- data.frame(lapply(export, as.character), stringsAsFactors = F)
export[is.na(export)] <- ""
write.csv(export, file = "CollapsedHistoryR.csv", row.names = F, na = "")
