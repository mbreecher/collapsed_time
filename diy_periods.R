library(reshape2)
library(plyr)
library(RecordLinkage)

# Pull in import functions

import_billable <- function(){
  setwd('C:/R/workspace/shared')
  source("import_functions.R")
  setwd('C:/R/workspace/collapsed_time')
  
  services <- import_services()
  timelog <- import_timelog()
  
  collapsed_time <- aggregate(Hours ~ Services.ID, FUN = sum, data = timelog)
  
  collapsed_history <- merge(services, collapsed_time, "Services.ID", all.x = T)
  
  timelog <- timelog[!is.na(timelog$Account.Name) & timelog$Billable %in% 1, ]
  timelog <- timelog[timelog$Date > as.Date("2012-06-30"),]
  
  #for each unique account and logged date, use the distance from the year end date to assign a quarter bucket
  diy_bucketed_timelog <- ddply(timelog,
                             .var = c("Account.Name", "Date"),
                             .fun = function(x) {
                               
                               # Grab the year end from the services
                               x_services <- subset(services, subset = Account.Name %in% x$Account.Name, na.rm = T)
                               x_ye <- unique(x_services$Year.End)
                               x_ye <- as.Date(paste(year(unique(x$Date)),x_ye, sep = "/"), format = "%Y/%m/%d")
  
                               qd <- as.numeric((unique(x$Date)-x_ye)/90)%%4 #quarter difference from year end
                               pqd <- as.numeric((unique(x$Date)-x_ye - 90)/90)%%4 #quarter difference from year end (prior quarter)
                               #if(abs(qd > 4)){qd <- qd%%4} #get a mod 4 quarter difference
                               if(!is.na(qd)){if(qd >= 0){cq <- ceiling(qd)}else{cq <- floor(qd)}}
                               if(!is.na(pqd)){if(pqd >= 0){pcq <- ceiling(pqd)}else{pcq <- floor(pqd)}}
                               #actual quarter
                               aq <- paste(year(unique(x$Date)),"Q", ceiling(as.numeric(month(unique(x$Date))/3)),  sep = "")
                               #actual reporting quarter
                               arq <- paste(year(unique(x$Date)-90),"Q", ceiling(as.numeric(month(unique(x$Date)-90)/3)),  sep = "")
                               
                               if(!is.na(cq)& !(x_ye %in% c("     ")) & !is.na(x_ye)){
                                 if(unique(x$Date) < x_ye){
                                   data.frame(customer_quarter_work_done = paste(year(unique(x$Date)),"Q",cq, sep = ""), 
                                              customer_quarter_reported = paste(year(unique(x$Date)-90),"Q",pcq, sep = ""), 
                                              calendar_quarter_work_done = aq,
                                              calendar_quarter_reported = arq,
                                              year_end = x_ye,
                                              Hours = sum(x$Hours))  
                                 }else{
                                   data.frame(customer_quarter_work_done = paste(year(unique(x$Date))+1, "Q",abs(cq),sep = ""), 
                                              customer_quarter_reported = paste(year(unique(x$Date)-90)+1, "Q",abs(pcq),sep = ""), 
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
  
  #aggregate diy time by account and quarter
  diy_time <- with(diy_bucketed_timelog,
                   aggregate(Hours ~ Account.Name + customer_quarter_work_done + customer_quarter_reported + 
                               calendar_quarter_work_done + calendar_quarter_reported + year_end, FUN = sum))
  #customers with no services will be dropped from the above, so we need to grab them separately and bind the result.
    diy_time_no_services <- with(diy_bucketed_timelog[is.na(diy_bucketed_timelog$customer_quarter_reported),],
                     aggregate(Hours ~ Account.Name + calendar_quarter_work_done + calendar_quarter_reported , FUN = sum))
    #add some columns and order before bind
    diy_time_no_services$customer_quarter_work_done <- NA; diy_time_no_services$customer_quarter_reported <- NA; diy_time_no_services$year_end <- NA
    diy_time_no_services <- diy_time_no_services[,names(diy_time)]
    diy_time <- rbind(diy_time, diy_time_no_services)
  
  #order by account name then customer quarter
  diy_time <- diy_time[order(diy_time$Account.Name, diy_time$customer_quarter_reported),]
  
  # code to export 
  setwd('C:/R/workspace/collapsed_time/output')
  write.csv(diy_time, file = "diy_time.csv", row.names = F, na = "")
  
  diy_time
}
