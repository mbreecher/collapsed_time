#I wanted to separate import and cleanup functions to minimize the noise in the aggregation


import_timelog <- function(name = "timelog_for_ps_history.csv"){
    #import and cleanup timelog
    setwd('C:/R/workspace/source')
    timelog <- read.csv(name, header = T , stringsAsFactors=F)
    start_dates <- read.csv("ps_start_dates.csv", header = T , stringsAsFactors=F)
    
    #trim footer information by removing rows without a valid value for services ID
    timelog <- timelog[substr(timelog$Services.ID,0,3) %in% c("a01"), ]
    
    #cleanup names and data values
    names(timelog)[names(timelog) %in% c("User..Full.Name")] <- "User"
    names(timelog)[names(timelog) %in% c("Services..Service.Name")] <- "Service"
    timelog$CIK <- as.numeric(timelog$CIK)
    timelog$Hours <- as.numeric(timelog$Hours)
    timelog$Date <- as.Date(timelog$Date, format = "%m/%d/%Y")
    timelog$Filing.Date <- as.Date(timelog$Filing.Date, format = "%m/%d/%Y")
    timelog$Filing.Deadline <- as.Date(timelog$Filing.Deadline, format = "%m/%d/%Y")
    timelog <- timelog[!is.na(timelog$Hours) && !is.na(timelog$Date),]
    start_dates$Start.Date <- as.Date(start_dates$Start.Date, format = "%m/%d/%Y")
    start_dates$End.Date <- as.Date(start_dates$End.Date, format = "%m/%d/%Y")
    
    #need to remove non-psm time. 
    timelog$is_psm <- NA
    #case 1: Known PSMs
    timelog[timelog$User %in% start_dates[is.na(start_dates$Start.Date) & is.na(start_dates$End.Date), ]$Full.Name, ]$is_psm <- 1 #with no movement in position
    for (psm in start_dates[!is.na(start_dates$Start.Date) | !is.na(start_dates$End.Date), ]$Full.Name) {#need to subset for each psm
      if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$Start.Date)){
        timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date >= start_dates[start_dates$Full.Name == psm, ]$Start.Date, ]$is_psm <- 1
        timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date < start_dates[start_dates$Full.Name == psm, ]$Start.Date, ]$is_psm <- 0
      }
      if (!is.na(start_dates[start_dates$Full.Name %in% psm, ]$End.Date)){
        timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date <= start_dates[start_dates$Full.Name == psm, ]$End.Date, ]$is_psm <- 1 
        timelog[timelog$User %in% psm & !is.na(timelog$User) & timelog$Date > start_dates[start_dates$Full.Name == psm, ]$End.Date, ]$is_psm <- 0
      }
    }
    #case 2: unknown PSMs
    ps_titles <- c("Senior Professional Services Manager", "Professional Services Manager")
    timelog[timelog$User.Title %in% ps_titles & is.na(timelog$is_psm), ]$is_psm <- 1
    
    #now all relevant time is marked, remove 0 and na time from timelog
    timelog <- timelog[timelog$is_psm == 1 & !is.na(timelog$is_psm), ]
    
    #Construct the Period Identifiers for service grouping
    timelog$filingPeriod <- paste(as.numeric(format(timelog$Date, "%Y")), ceiling(as.numeric(format(timelog$Date, "%m"))/3), sep = "")
    timelog$reportingPeriod <- ifelse(substr(timelog$filingPeriod, nchar(timelog$filingPeriod), nchar(timelog$filingPeriod)) == 1,
          paste(as.numeric(format(timelog$Date, "%Y")) -1, 4, sep = ""),
          paste(as.numeric(format(timelog$Date, "%Y")), ceiling(as.numeric(format(timelog$Date, "%m"))/3) - 1, sep = ""))
    
    timelog
}

import_services <- function(name = "services_for_ps_history_R.csv"){
    ##import services report
    setwd('C:/R/workspace/source')
    services <- read.csv(name, header = T , stringsAsFactors=F)
    
    #trim footer information by removing rows without a valid Solution name
    services <- services[substr(services$Solution.Name,0,3) == "SEC"  & !is.na(services$Solution.Name), ] #Remove non-SEC solutions
    
    #cleanup names and data values
    names(services)[names(services) %in% c("Account..Account.Name")] <- "Account.Name"
    names(services)[names(services) %in% c("PSM..Full.Name")] <- "PSM"
    names(services)[names(services) %in% c("Sr.PSM..Full.Name")] <- "Sr.PSM"
    names(services)[names(services) %in% c("CSM..Full.Name")] <- "CSM"
    names(services)[names(services) %in% c("Sr.CSM..Full.Name")] <- "Sr.CSM"
    names(services)[names(services) %in% c("Churned.Effective.Date")] <- "Churn.Date"
    services$Form.Type[services$Form.Type == 'N/A' & !is.na(services$Form.Type)] <- NA
    services$Goodwill.Hours.Available[services$Goodwill.Hours.Available %in% c("0")] <- NA
    services$Quarter.End <- as.Date(services$Quarter.End, format = "%m/%d/%Y")
    services$Filing.Date <- as.Date(services$Filing.Date, format = "%m/%d/%Y")
    services$Filing.Deadline <- as.Date(services$Filing.Deadline, format = "%m/%d/%Y")
    services$Year.End <- format(services$Year.End, format = "%Y-%U")
    
    # logic to calculate filing period and reporting period
    services <- services[!is.na(services$Quarter.End), ] #remove services without quarter ends (can't place them)
    services <- services[!(services$CS.PS %in% c('CS')),] #remove all CS services.
    services <- services[!(services$Service.Type %in% c('Reserve Hours', 'Other', 'Training', '')), ] #Remove Reserve Projects
    
    #calculate filing deadline estimate for all projects
    #for services with form type and registrant type, set reporting offset, then calculate filing.estimate 
    services$reportingOffset <- paste(services$Registrant.Type, services$Form.Type, Sep = "") #placeholder lookup value
    #set offsets for Qs
    services$reportingOffset[services$reportingOffset %in% c("Smaller Reporting Company K-Q ", "Smaller Reporting Company 10-Q ", "Smaller Reporting Company Q-Q ")] <- 45
    services$reportingOffset[services$reportingOffset %in% c("Non-Accelerated Filer K-Q ", "Non-Accelerated Filer 10-Q ", "Non-Accelerated Filer Q-Q ")] <- 45
    services$reportingOffset[services$reportingOffset %in% c("Accelerated Filer K-Q ", "Accelerated Filer 10-Q ", "Accelerated Filer Q-Q ")] <- 40
    services$reportingOffset[services$reportingOffset %in% c("Large Accelerated Filer K-Q ", "Large Accelerated Filer 10-Q ", "Large Accelerated Filer Q-Q ")] <- 40
    #set offsets for Ks
    services$reportingOffset[services$reportingOffset %in% c("Smaller Reporting Company Q-K ", "Smaller Reporting Company 10-K ", "Smaller Reporting Company K-K ")] <- 90
    services$reportingOffset[services$reportingOffset %in% c("Non-Accelerated Filer Q-K ", "Non-Accelerated Filer 10-K ", "Non-Accelerated Filer K-K ")] <- 90
    services$reportingOffset[services$reportingOffset %in% c("Accelerated Filer Q-K ", "Accelerated Filer 10-K ", "Accelerated Filer K-K ")] <- 75
    services$reportingOffset[services$reportingOffset %in% c("Large Accelerated Filer Q-K ", "Large Accelerated Filer 10-K ", "Large Accelerated Filer K-K ", "Large Accelerated Filer 20-F - 20-F ")] <- 60
    #
    services$reportingOffset[!(services$reportingOffset %in% c(40, 45, 60, 75, 90))] = NA
    
    #with report offset, calculate filing deadline estimate
    services$filing.estimate <- NA
    services$filing.estimate <- as.Date(services$filing.estimate)
    services[services$reportingOffset %in% c(40, 45, 60, 75, 90), ]$filing.estimate <- 
      as.Date(services[services$reportingOffset %in% c(40, 45, 60, 75, 90),]$Quarter.End) + 
      as.numeric(services[services$reportingOffset %in% c(40, 45, 60, 75, 90),]$reportingOffset)
    #for services without both form type and registrant type, use the filing date reported (if available)
    services[is.na(services$filing.estimate) & !is.na(services$Filing.Date), ]$filing.estimate <- services[is.na(services$filing.estimate) & !is.na(services$Filing.Date), ]$Filing.Date
    #handle foreign issuer cases
    #case for 6-Ks and 10-Qs
    services[services$Registrant.Type %in% c("Foreign Issuer") & services$Form.Type %in% c("10-Q", "6-K"), ]$filing.estimate <- 
      as.Date(services[services$Registrant.Type %in% c("Foreign Issuer") & services$Form.Type %in% c("10-Q", "6-K"),]$Quarter.End) + 45
    #case for 10-Ks
    services[services$Registrant.Type %in% c("Foreign Issuer") & services$Form.Type %in% c("10-K"), ]$filing.estimate <- 
      as.Date(services[services$Registrant.Type %in% c("Foreign Issuer") & services$Form.Type %in% c("10-K"),]$Quarter.End) + 90
    #take the filing deadline in the system at this point (if available).
    services[is.na(services$filing.estimate) & !is.na(services$Filing.Deadline), ]$filing.estimate <- 
      as.Date(services[is.na(services$filing.estimate) & !is.na(services$Filing.Deadline), ]$Filing.Deadline)
    #anyone with no registrant type set, estimate 40 days for 10-Qs and 60 for a 10-Ks and 20-Fs
    services[services$Registrant.Type %in% c("") & services$Form.Type %in% c("10-K", "Q-K", "K-K", "20-F"), ]$filing.estimate <- 
      as.Date(services[services$Registrant.Type %in% c("") & services$Form.Type %in% c("10-K", "Q-K", "K-K", "20-F", "20-F - 20-F"), ]$Quarter.End) + 60
    services[services$Registrant.Type %in% c("") & services$Form.Type %in% c("10-Q", "K-Q", "Q-Q"), ]$filing.estimate <- 
      as.Date(services[services$Registrant.Type %in% c("") & services$Form.Type %in% c("10-Q", "K-Q", "Q-Q"), ]$Quarter.End) + 40
    #for remaining services, estimate 40 days
    services[is.na(services$filing.estimate), ]$filing.estimate <- 
      as.Date(services[is.na(services$filing.estimate), ]$Quarter.End) + 40
    
    #Construct the Period Identifiers for service grouping
    services$filingPeriod <- paste(as.numeric(format(services$filing.estimate, "%Y")), ceiling(as.numeric(format(services$filing.estimate, "%m"))/3), sep = "")
    services$reportingPeriod <- ifelse(substr(services$filingPeriod, nchar(services$filingPeriod), nchar(services$filingPeriod)) == 1,
                                      paste(as.numeric(format(services$filing.estimate, "%Y")) -1, 4, sep = ""),
                                      paste(as.numeric(format(services$filing.estimate, "%Y")), ceiling(as.numeric(format(services$filing.estimate, "%m"))/3) - 1, sep = ""))
    
    services
}

import_sec <- function(name = "filing_data.csv" ){
  setwd('C:/R/workspace/source')
  facts <- read.csv(name, header = T , stringsAsFactors=F)
  
  #remove extra fields to remove potential for incomplete cases due to something extraneous
  valid_list <- c("accession_number", "name", "cik", "sic", "form", "form_group", 
             "filing_date", "filing_qtr", "filing_month", "facts", "xbrl_software")
  facts <- facts[, names(facts) %in% valid_list]
  facts <- facts[facts$xbrl_software %in% c("WebFilings"), ]
  #remove incomplete cases
  facts <- facts[complete.cases(facts),]
  #cast columns properly
  facts$filing_date <- as.Date(facts$filing_date, format = "%m/%d/%Y")
  facts$facts <- as.numeric(facts$facts)
  facts <- facts[facts$form_group %in% c("tens", "amend"),]
  facts <- facts[rev(order(facts$filing_date)), ]
  facts
}

import_sales_recommendations <- function(name = "sales_recommendations_for_ps_history.csv" ){
  setwd('C:/R/workspace/source')
  sales_rec <- read.csv(name, header = T , stringsAsFactors=F)
  sales_rec$Service.QED <- as.Date(sales_rec$Service.QED, format = "%m/%d/%Y")
  sales_rec <- sales_rec[!is.na(sales_rec$Service.QED),]
  sales_rec$period <- paste(as.numeric(format(sales_rec$Service.QED, "%Y")), ceiling(as.numeric(format(sales_rec$Service.QED, "%m"))/3) , sep = "")
  
  svc_by_qtr <- aggregate(sales_rec$Product, by=list(sales_rec$Account.Name, sales_rec$period), paste, collapse = "\n")
  names(svc_by_qtr) <- c("Account.Name", "period", "Services")
  result <- dcast(svc_by_qtr, Account.Name ~ period)
  for (i in 1:length(names(result)[grep('[1-9]+', names(result), perl = T)])) {
       names(result)[grep('[1-9]+', names(result), perl = T)][i] <- 
         paste(names(result)[grep('[1-9]+', names(result), perl = T)][i] , "Sales Rec", collapse = "\n")
  }
  result
}
