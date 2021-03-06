collapsed_time <- function(){
  library(reshape2)
  library(plyr)
  library(RecordLinkage)
  
  # Pull in import functions
  setwd("C:/R/workspace/shared")
  source("import_functions.R")
  setwd('C:/R/workspace/collapsed_time')
  source("diy_periods.R")
  
  #import services and include customer status = none
  services <- import_services(name = "services_for_ps_history_with_status_none.csv")
  timelog <- import_timelog()
  diy_time <- import_billable() 
  
  #initial exclusions. pre-Q2 2012 time and in-progress or not started services
  services <- services[services$Status %in% "Completed",]
  timelog <- timelog[timelog$Date <= Sys.Date() & timelog$Date >= as.Date("2012-06-30"),]
  
  collapsed_time <- aggregate(Hours ~ Services.ID, FUN = sum, data = timelog)
  collapsed_history <- merge(services, collapsed_time, "Services.ID", all.x = T)
  
  collapsed_history <- collapsed_history[collapsed_history$filing.estimate >= as.Date("2012-06-30"),]
  
  
  pts <- proc.time()
  count.unique <- function(x) { length(unique(x[!is.na(x)])) } #function to count unique non-NA values
  
  #I need customer period to merge diy time
  customer_period <- ddply(collapsed_history,
                              .var = c("Services.ID", "CIK", "Account.Name", "filing.estimate", "Service.Name", "Service.Type", "Form.Type", "Quarter.End"),
                              .fun = function(x) {
                                
                                # Grab the year end from the services
                                x_ye <- unique(x$Year.End)
                                x_ye <- as.Date(paste(year(unique(x$filing.estimate)),x_ye, sep = "/"), format = "%Y/%m/%d")
                                
                                qd <- as.numeric((unique(x$filing.estimate)-x_ye)/90)%%4 #quarter difference from year end
                                pqd <- as.numeric((unique(x$filing.estimate)-x_ye - 90)/90)%%4 #quarter difference from year end (prior quarter)
                                #if(abs(qd > 4)){qd <- qd%%4} #get a mod 4 quarter difference
                                if(!is.na(qd)){if(qd > 0){cq <- ceiling(qd)}else{cq <- floor(qd)}}else{cq <- NA}
                                if(!is.na(pqd)){if(pqd > 0){pcq <- ceiling(pqd)}else{pcq <- floor(pqd)}}else{pcq <- NA}
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
  
  export <- merge(customer_period, diy_time_simple, by = start, all = T)
  export <- export[,c(start, names(export)[!(names(export) %in% start)])]
  
  #merge duplicated billable hour counts for customers with more then one concurrent service in a period. Need to correct.
  export$Billable.Hours <- as.numeric(export$Billable.Hours)
  normalized_time <- ddply(export, .var = start,
                           .fun = function(x){
                             count <- length(unique(x$Services.ID))
                             if(count > 1 & !is.na(count)){normalized_time <- x$Billable.Hours/count}else{normalized_time <- x$Billable.Hours}
                             data.frame(services_count = count,
                                        normalized_time <- normalized_time)
                           })
  
  export <- merge(export, normalized_time, by = start, all.x = T )
  names(export)[names(export) %in% c("normalized_time....normalized_time")] <- "normalized_time"
  #some cleanup
  names(export[c("Hours")]) <- c("Project.Hours")
  export <- unique(export)
  
  # code to export 
  #setwd('C:/R/workspace/collapsed_time/output')
  # export <- data.frame(lapply(export, as.character), stringsAsFactors = F)
  #export[is.na(export)] <- ""
  # write.csv(export, file = "CollapsedHistoryR.csv", row.names = F, na = "")
  export
}

#explore orphaned time
# orphans <- timelog[!(timelog$Services.ID %in% export$Services.ID ),]
# orphans_simple <- aggregate(Hours ~ Services.ID, data = orphans, FUN = sum)
# setwd('C:/R/workspace/source')
# complete_services <- read.csv("unfiltered_services.csv", header = T , stringsAsFactors=F)
# merge_orphans <- merge(orphans_simple, complete_services, "Services.ID", all.x = T)
# 
# setwd('C:/R/workspace/collapsed_time/output')
# merge_orphans[is.na(merge_orphans)] <- ""
# write.csv(merge_orphans, file = "merged_orphans.csv", row.names = F, na = "")
