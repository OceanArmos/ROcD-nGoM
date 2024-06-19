library(rstudioapi)
library(data.table)
library(dataRetrieval)
library(httr)
library(lubridate) 
library(ggplot2)
library(scales)
library(EGRET)
library(dplyr)
library(readxl)
library(PeriodicTable)
library(rapportools)

#you can add a list of siteNumbers and loop through the script if wanted
#I did one at a time due to the amount of data generated and time needed for each site
#You can also loop though multiple sites with one parameter of interest

#usgs site of interest
siteNumber <- "08176500"
#list of parameters to run through the script for every site
Param_list <- c("Temperature, water","Temperature, air, deg C","Salinity",
                "Oxygen","pH","Alkalinity","Carbon","Organic carbon",
                "Inorganic carbon","Carbon dioxide","Bicarbonate",
                "Carbonate","Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)",
                "Organic Nitrogen","Inorganic nitrogen (nitrate and nitrite)",
                "Kjeldahl nitrogen","Ammonia and ammonium","Nitrate","Nitrite",
                "Phosphorus","Organic phosphorus","Inorganic phosphorus","Orthophosphate",
                "Silica","Calcium","Magnesium","Sodium","Potassium","Chloride","Sulfate","Iron",
                "Lead","Aluminum","Fluoride","Arsenic","Boron","Cadmium","Selenium")
#empty data frame to store samples after each is collected 
Samples_dt = data.frame()
#empty data frame to store data for each site after each is collected
Sites_dt = data.frame()

#LOOP through all parameters
for (i in 1:length(Param_list)) {
  Param <- Param_list[i]
#Because this is a loop, the first step is to empty important data tables
  #Why? If there is no data for a certain parameter in readWQPSample(), the script will run through with the previous info in Sample_format,
  #emptying the data frame first prevents this
  Sample_format = data.frame()
  Sample_qw = data.frame()
  total_dt = data.frame()
  #enter in your desired start and end date of data
  startDate <- "" 
  endDate <- "2021-12-31"
  #read in the sample format data, this is the format for the WRTDS model, but lacks important metadata.
  Sample_format<- as.data.table(readWQPSample(paste0("USGS-",siteNumber), Param, startDate, endDate))
  
#if there is less than 2 observations at this step, try next parameter
  if (nrow(Sample_format) < 2){
    next
  }
#sometimes the Sample_format data table is not ordered
  Sample_format <- Sample_format[order(as.Date(Sample_format$Date)),]
  #reading in the water quality info from WQP, this gives all of the important meta data 
  Sample_qw <- as.data.table(readWQPqw(paste0("USGS-",siteNumber), Param, startDate, endDate))
  #detecting if multiple fractions (filtration status) for parameter and what they are
  fraction <- Sample_qw[,unique(ResultSampleFractionText)]
  #detecting if multiple units for parameter and what they are
  unit <- Sample_qw[, unique(ResultMeasure.MeasureUnitCode)]
  #detecting if multiple detection statuses for parameter
  #NA means no non-detects
  #"Not detected" means there are non-detects in data, can also be called "detected not quantified" by WQP
  detection_limit <- Sample_qw[, unique(ResultDetectionConditionText)]
  #dealing with non-detects if they are in data
  if ("Not Detected" %in% detection_limit | "Detected Not Quantified" %in% detection_limit){      
    #separating out non detects from WQP portal data
    Sample_qw_nt <- Sample_qw[!is.na(ResultDetectionConditionText)]
    #finding non-detects in sample dataframe
    replace <- Sample_format[is.na(ConcLow)]
    #separating out the two parameters of interest
    replace_value <- replace[,c("Date", "ConcAve")] 
    #renaming columns to match sample_qw dataframe
    colnames(replace_value) <- c("ActivityStartDate","ResultUncen")
    #merging the non-detect info from both dataframes
    Sample_qw_nt <- merge(Sample_qw_nt,replace_value,by="ActivityStartDate")
    #if value is non-detect in sample_qw, it is automatically NA
    #using info from Sample_format to quantify these as half the detection limit
    Sample_qw_nt[,ResultMeasureValue := ResultUncen]
    #the data with concentration above the detection limit
    Sample_qw_dt <- Sample_qw[is.na(ResultDetectionConditionText)]
    #the different units in the data
    unit_reg <- Sample_qw_dt[, unique(ResultMeasure.MeasureUnitCode)]
    #unfortunately, the sample_qw dataframe has an NA value for both the ResultMeasureValue and ResultMeasure.MeasureUnitCode
    #So if there are multiple units in the dataframe, you can't be certain which unit the non-detect is supposed to be
    #therefore, if there is only one unit, we can go ahead and enter in the correct value for the non-detect
    #If there are more than one unit, we keep the original NA values for non detects in the Sample_qw format.
    if (length(unit_reg) == 1){
      Sample_qw_nt[,ResultMeasure.MeasureUnitCode := unit_reg]
      Sample_qw <- rbind(Sample_qw_dt,Sample_qw_nt,fill=T)
    }
  }
  
  #double checking qw info
  #Quality control
  #also checking the meta data associated with the site
  #excluding data not wanted
  Sample_qw <- Sample_qw[!ResultValueTypeName == "Estimated",]
  Sample_qw[, unique(ActivityMediaName)]
  Sample_qw <- Sample_qw[ActivityMediaName == "Water",]
  Sample_qw[, unique(ActivityTypeCode)]
  Sample_qw <- Sample_qw[!ActivityTypeCode == "Quality Control Sample-Field Replicate"]
  Sample_qw[, unique(ActivityMediaSubdivisionName)]
  Sample_qw <- Sample_qw[ActivityMediaSubdivisionName == "Surface Water",]
  Sample_qw[, unique(HydrologicCondition)]
  Sample_qw[, unique(HydrologicEvent)]
  Sample_qw[, unique(ResultSampleFractionText)]
  Sample_qw[, unique(ResultMeasure.MeasureUnitCode)]
  Sample_qw <- Sample_qw[!ResultMeasure.MeasureUnitCode == "NA",]
  #here if the parameter is alkalinity, previous research has shown that there is no difference between filtered and unfiltered alkalinity
  #we set fraction to combined, so daily estimate can be average of both if available
  if (Param == "Alkalinity"){
    fraction <- "Combined"
    Sample_qw[, ResultSampleFractionText := "Combined"]
  }
  #setting fraction to none for temperature
  if (Param == "Temperature, water"){
    fraction <- "None"
    Sample_qw[, ResultSampleFractionText := "None"]
  }
  
  if (Param == "Temperature, air, deg C"){
    fraction <- "None"
    Sample_qw[, ResultSampleFractionText := "None"]
  }
  #more quality control, removing unwanted data
  Sample_qw <- Sample_qw[!ResultSampleFractionText == "Bed Sediment",]
  Sample_qw <- Sample_qw[!ResultMeasure.MeasureUnitCode == "% saturatn"]
  Sample_qw <- Sample_qw[!ResultMeasure.MeasureUnitCode == "%"]
  Sample_qw <- Sample_qw[!ResultMeasure.MeasureUnitCode == "mg/kg"]

#here we go into our unit conversions, all data needs to be in mg/l before WRTDS model
  if ("ug/l" %in% unit){
    #separate out data with unit of ug/l
    #can be multiple units in data
    Sample_qw_ugl <- Sample_qw[ResultMeasure.MeasureUnitCode == "ug/l"]
    #change unit to reflect conversion
    Sample_qw_ugl[,ResultMeasure.MeasureUnitCode := "mg/l"]
    #unit conversion
    Sample_qw_ugl[,ResultMeasureValue := ResultMeasureValue * 0.001]
    #separating out data with units of mg/l
    Sample_qw_dt <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l"]
    #NEED THIS??
  #  Sample_qw[, ResultMeasure.MeasureUnitCode := "mg/l"]
    #combining all data after unit conversion for ug/l data
    Sample_qw <- rbind(Sample_qw_dt, Sample_qw_ugl)
  }
  
  if ("mg/l NO3" %in% unit){      
    #separating out data with this unit
    #you need to separate because we do not want to convert the wrong unit if multiple unit in original dataframe
    Sample_qw_unit <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l NO3"]
    #change unit to reflect conversion
    Sample_qw_unit[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    #unit conversion
    Sample_qw_unit[,ResultMeasureValue := ResultMeasureValue * 0.225897]
    #separating out unit of mg/l
    Sample_qw_dt <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l"]
    #renaming if unit is mg/l to mg/l as N
    Sample_qw_dt[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    #sometimes units also as mg/l as N as well
    Sample_qw_N <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l as N"]
    #merging all converted data
    Sample_qw <- rbind(Sample_qw_dt,Sample_qw_N, Sample_qw_unit)
  }
  #same structure as before, WQP has many different unit codes sometimes for the same thing!
  if ("mg/l asNO3" %in% unit){      
    Sample_qw_unit <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l asNO3"]
    Sample_qw_unit[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    Sample_qw_unit[,ResultMeasureValue := ResultMeasureValue * 0.225897]
    Sample_qw_dt <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l"]
    Sample_qw_dt[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    Sample_qw_N <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l as N"]
    Sample_qw <- rbind(Sample_qw_dt,Sample_qw_N, Sample_qw_unit)
  }
  #for phosphorous data
  if ("mg/l PO4" %in% unit){    
    Sample_qw_unit <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l PO4"]
    Sample_qw_unit[,ResultMeasure.MeasureUnitCode := "mg/l as P"]
    Sample_qw_unit[,ResultMeasureValue := ResultMeasureValue * 0.326138]
    Sample_qw_dt <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l"]
    Sample_qw_dt[,ResultMeasure.MeasureUnitCode := "mg/l as P"]
    Sample_qw_P <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l as P"]
    Sample_qw <- rbind(Sample_qw_dt,Sample_qw_P, Sample_qw_unit)
  }

  if ("mg/l asPO4" %in% unit){      
    Sample_qw_unit <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l asPO4"]
    Sample_qw_unit[,ResultMeasure.MeasureUnitCode := "mg/l as P"]
    Sample_qw_unit[,ResultMeasureValue := ResultMeasureValue * 0.326138]
    Sample_qw_dt <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l"]
    Sample_qw_dt[,ResultMeasure.MeasureUnitCode := "mg/l as P"]
    Sample_qw_P <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l as P"]
    Sample_qw <- rbind(Sample_qw_dt,Sample_qw_P, Sample_qw_unit)
  } 
  
  if ("mg/l asNO2" %in% unit){      
    Sample_qw_unit <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l asNO2"]
    Sample_qw_unit[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    Sample_qw_unit[,ResultMeasureValue := ResultMeasureValue * 0.304457]
    Sample_qw_dt <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l"]
    Sample_qw_dt[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    Sample_qw_N <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l as N"]
    Sample_qw <- rbind(Sample_qw_dt,Sample_qw_N, Sample_qw_unit)
  } 
  
  if ("mg/l NO2" %in% unit){      
    Sample_qw_unit <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l NO2"]
    Sample_qw_unit[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    Sample_qw_unit[,ResultMeasureValue := ResultMeasureValue * 0.304457]
    Sample_qw_dt <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l"]
    Sample_qw_dt[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    Sample_qw_N <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l as N"]
    Sample_qw <- rbind(Sample_qw_dt,Sample_qw_N, Sample_qw_unit)
  } 
  
  if ("mg/l asNH4" %in% unit){      
    Sample_qw_unit <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l asNH4"]
    Sample_qw_unit[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    Sample_qw_unit[,ResultMeasureValue := ResultMeasureValue * 0.776490]
    Sample_qw_dt <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l"]
    Sample_qw_dt[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    Sample_qw_N <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l as N"]
    Sample_qw <- rbind(Sample_qw_dt,Sample_qw_N, Sample_qw_unit)
  }  
  
  if ("mg/l NH4" %in% unit){      
    Sample_qw_unit <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l NH4"]
    Sample_qw_unit[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    Sample_qw_unit[,ResultMeasureValue := ResultMeasureValue * 0.776490]
    Sample_qw_dt <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l"]
    Sample_qw_dt[,ResultMeasure.MeasureUnitCode := "mg/l as N"]
    Sample_qw_N <- Sample_qw[ResultMeasure.MeasureUnitCode == "mg/l as N"]
    Sample_qw <- rbind(Sample_qw_dt,Sample_qw_N, Sample_qw_unit)
  }  
  #converting pH to H ion concentration
  #converting to mg/l first to run through egret
  if (Param == "pH"){
    Sample_qw[,ResultMeasureValue := 10 ^ (-ResultMeasureValue)*mass("H")*1e3]
    Sample_qw[,ResultMeasure.MeasureUnitCode := "mg/l"]
  }
  fraction <- Sample_qw[,unique(ResultSampleFractionText)]
  unit <- Sample_qw[, unique(ResultMeasure.MeasureUnitCode)]
  #after all the quality control and unit conversion
  #skip to next parameter in less than 2 samples remaining
  if (nrow(Sample_qw) < 2){
    next
  }
  #now we separate the fractions
  #different fractions can have different concentrations and need to be separated before running through the model
  #we will loop through all the fractions so in the end we have a dataframe with all together
  for (x in 1:length(fraction)) {
    #separating first fraction
    Sample_fraction <- Sample_qw[ResultSampleFractionText == fraction[x],]
    #now we have a second loop for separating units
    #but with the previous steps there should be only one unit
    #this is essentially a safeguard if you are running many parameters
    #if there are multiple units, they will be separated before running regression and you will get the results for both!
    for (y in 1:length(unit)){
      Sample_units <- Sample_fraction[ResultMeasure.MeasureUnitCode == unit[y],]
      #at this point we are averaging to create a daily concentration for WRTDS model
      Sample_mean <- Sample_units[,.(mean=mean(ResultMeasureValue,na.rm=TRUE)),by=.(Date=ActivityStartDate),]
      #separating out date and detection limit info
      Sample_detect <- Sample_units[,c("ActivityStartDate", "ResultDetectionConditionText")]
      #renaming date to match sample dataframe
      colnames(Sample_detect)[1] <- "Date"
      #only info we still need
      Sample_mean <- merge(Sample_mean,Sample_detect)
      #if we have less than 5 observations here, move to next 
      if (nrow(Sample_mean) < 5){
        next
      }
      #merging our sample format with quality controlled data from WQP (sample_qw)
      Sample_format <- Sample_format[,c(1:13)]
      Sample_format <- unique(Sample_format,by="Date")
      Sample <- merge(Sample_format,Sample_mean,by="Date")
      #remove duplicates
      Sample <- unique(Sample, by = "Date")
      #we want our quality controlled data from WQP as the ConcAve
      #usually ConcAve and mean are same, we are just taking that one extra step for quality assurance
      #the mean data is the one that went through unit conversion, detection limit steps, and excluding all sorts of data with help from WQP meta data
      Sample[, ConcAve := mean]
      #remember that we need our data in the sample format for WRTDS model
      #ConcLow and ConcHigh are always the same as ConcAve if value is above detection limit
      #Uncen = 1 also means data is above detection limit
      #since we defined ConcAve as our mean from the Sample_qw portal, we are going to define ConcLow and ConcHigh as the same
      #Don't worry, we will adjust for any non-detects in next step.
      Sample[,ConcLow := mean]
      Sample[,ConcHigh := mean]
      Sample[,Uncen := 1]
      #now we see if we still have non-detect values (all the quality control steps can potentially remove them)
      detection_limit <- Sample_qw[, unique(ResultDetectionConditionText)]
      #if we do, we adjust our uncen, conclow, and conchigh
      if ("Not Detected" %in% detection_limit | "Detected Not Quantified" %in% detection_limit){
        if (length(unit_reg) == 1){
          #separate out non-detects in data
          Sample_qw_nt <- Sample[!is.na(ResultDetectionConditionText)]
          #the WRTDS model can deal with non-detects
          #for non-detects the ConcLow is set to NA
          Sample_qw_nt$ConcLow <- NA
          #remember that earlier in the script we made sure that the non-detects had the corrent concentration as high the detection limit
         #The sample format already had the ConcAve as half the detection limit
          #ConcHigh represents that detection limit, so we just multiply the ConcAve value by two to get the original ConcHigh value
           Sample_qw_nt$ConcHigh <- Sample_qw_nt$ConcAve * 2
           #Uncen equal to 0 signals to WRTDS that the value is a non-detect, we also retain this info in database
          Sample_qw_nt$Uncen <- 0
          #here we separate the normal "detected" samples
          Sample_norm <- Sample[(is.na(ResultDetectionConditionText))]
          #finally we merge both non-detects and detects together
          Sample <- rbind(Sample_qw_nt,Sample_norm)
        }
      }
      #The model will not run if any sample has a concentration of zero, so deleting these
      Sample[ConcAve == 0] <- NA
  #for the discharge data, we don't want discharge data before the start date of sample data
      #model is more unreliable
      startDate <- min(as.character(Sample$Date)) 
      #end the discharge with last sample
      endDate <- max(as.character(Sample$Date))
      #reading in the discharge data
      #as I did for the database, you can specify the siteNumber here if needed
      Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)
     
      #here is example code if getting discharge data from a non USGS source 
      #  filePath <- "/Users/barmos/dropbox/Research/Database_data/INFO/"
      #fileName <- "Grande_08475000.csv"
      #rio_discharge <- fread("MISS_07373420.csv")
      #Daily <- readUserDaily(filePath,fileName,qUnit = 2)
      
      #if no discharge data, skip
      if (nrow(Daily) < 1) {
        next
      } 
      #We also need to make sure there is discharge data for all the sampled data
      #sometimes the water quality data starts before the discharge
      #need to start sample when discharge is available before running regression
      start <- min(as.character(Daily$Date))
      end <- max(as.character(Daily$Date))
      #making sure discharge and sample data aligns
      #model can produce an error if not
      Sample <- Sample[Date %between% c(start,end)]
#getting specifc site info: lat, long, drainage
      INFO <- readWQPInfo(paste0("USGS-",siteNumber),Param,interactive = FALSE)
      #gathering all data in one place for model
      eList <- mergeReport(INFO, Daily, Sample)
      #model works best with minimum 50 samples
      #these settings can be adjusted as needed
      if (nrow(Sample) < 50){
        next
      }
      if (nrow(Sample) >= 50) {
        eList <- modelEstimation(eList, minNumObs = 50, minNumUncen = 25)
        #below is the model adjustment used for some sites if needed
        #eList <- modelEstimation(eList, minNumObs = 20, minNumUncen = 10 )
      }
 #model output
      dt <- as.data.table(eList$Daily)
      #columns we want from model
      DT <- dt[,c("Date","Q","Month","ConcDay","FluxDay")]
      #original "raw" values we want to keep
      concAve <- Sample[,c("Date","ConcAve","Uncen")]
      #merging raw and model output data
      all_dt <- merge(DT, concAve, all = TRUE, by="Date")
      #calculating the daily flux based on "raw" concentration data in units kg/day
      all_dt[,FluxAve := ConcAve*Q*86.4]
#getting site info needed for database
      all_dt[, data_source := INFO$OrganizationIdentifier]
      all_dt[, siteNumber := INFO$MonitoringLocationIdentifier]
      #for database, changing parameter name for pH to H ion 
      if (Param == "pH"){
        all_dt[,parameter := "H ion"]
      } else {
        all_dt[, parameter := INFO$param.nm]
      }
      #adding important metadata for each site
      all_dt[, conc_unit := unit[y]]
      all_dt[, fraction := fraction[x]]
      #drainage area converting to km2
      all_dt[, drainage_area := as.numeric(INFO$DrainageAreaMeasure.MeasureValue)*2.58998811]
      all_dt[, latitude := as.numeric(INFO$LatitudeMeasure,8)]
      all_dt[,longitude := as.numeric(INFO$LongitudeMeasure,8)]
      
      #time columns
      all_dt[,Year := year(Date)]
      all_dt[, yearMonth := ymd(paste0(Year, "-", Month, "-01"))]
      #yearSeason
      Season <- as.vector(all_dt$Month)
      Season[Season %in% c(12,1,2)] <-  "12"
      Season[Season %in% c(3,4,5)]  <- "03"
      Season[Season%in% c(6,7,8)]  <- "06"
      Season[Season%in% c(9,10,11)] <- "09"
      Season <- as.numeric(Season)
      all_dt[, yearSeason := paste0(all_dt$Year, "-",Season,"-", "01")]
      all_dt$Season <- Season
      #the order for the database so far
      setcolorder(all_dt,c('data_source','siteNumber','latitude','longitude','drainage_area','parameter','conc_unit','fraction','Date','Month','Season','Year','yearMonth','yearSeason',
                           'Q', 'ConcAve','FluxAve','ConcDay','FluxDay'))
      
     #time averaging section
      #yearMonth Discharge
      all_dt[, Q_yearMonth := mean(Q,na.rm = TRUE), by = c("yearMonth","siteNumber")]
      #yearSeason Discharge
      all_dt[, Q_yearSeason := mean(Q,na.rm = TRUE), by= c("yearSeason","siteNumber")]
      #just year
      all_dt[, Q_year := mean(Q,na.rm = TRUE), by = c("Year","siteNumber")]
      #just month
      all_dt[, Q_month := mean(Q,na.rm = TRUE), by = c("Month","siteNumber")]
      #Just season
      all_dt[, Q_Season := mean(Q,na.rm = TRUE), by = c("Season","siteNumber")]
      
      #ConcDay
      #yearMonth
      all_dt[, ConcDay_yearMonth := mean(ConcDay,na.rm = TRUE), by = c("yearMonth","siteNumber")]
      #yearSeason
      all_dt[, ConcDay_yearSeason := mean(ConcDay,na.rm = TRUE), by = c("yearSeason","siteNumber")]
      #year
      all_dt[, ConcDay_year := mean(ConcDay,na.rm = TRUE), by = c("Year","siteNumber")]
      #Month
      all_dt[, ConcDay_month := mean(ConcDay,na.rm = TRUE), by = c("Month","siteNumber")]
      #Season
      all_dt[, ConcDay_season := mean(ConcDay,na.rm = TRUE), by = c("Season","siteNumber")]
      
      #FluxDay
      #yearMonth
      all_dt[, FluxDay_yearMonth := mean(FluxDay,na.rm = TRUE), by = c("yearMonth","siteNumber")]
      #yearSeason
      all_dt[, FluxDay_yearSeason := mean(FluxDay,na.rm = TRUE), by = c("yearSeason","siteNumber")]
      #year
      all_dt[, FluxDay_year := mean(FluxDay,na.rm = TRUE), by = c("Year","siteNumber")]
      #Month
      all_dt[, FluxDay_month := mean(FluxDay,na.rm = TRUE), by = c("Month","siteNumber")]
      #Season
      all_dt[, FluxDay_season := mean(FluxDay,na.rm = TRUE), by = c("Season","siteNumber")]
 
      #Flow normalized ConcDay values
      #we need concentration and discharge pairs
      #creating a variable outside of database for discharge and concDay
      Q_for_cleaning_concDay <- all_dt$Q
      ConcDay_working <- all_dt$ConcDay
      #creating an NA index
      na_index <- is.na(ConcDay_working) | is.na(Q_for_cleaning_concDay)
      #deleting if any zero in either ConcDay or Q
      Q_for_cleaning_concDay[na_index] <- NA
      ConcDay_working[na_index] <- NA
      #adding to database for flow-weighting
      #will be deleted before final product
      all_dt[,Q_for_cleaning_concDay := ..Q_for_cleaning_concDay]
      all_dt[,clean_ConcDay := ..ConcDay_working]

      #calculating the flow weighted concentration
      #yearMonth
      all_dt[, FN_ConcDay_yearMonth := sum(clean_ConcDay*Q_for_cleaning_concDay)/sum(Q_for_cleaning_concDay), by = c("yearMonth","siteNumber")]
      #yearSeason
      all_dt[, FN_ConcDay_yearSeason := sum(clean_ConcDay*Q_for_cleaning_concDay)/sum(Q_for_cleaning_concDay), by = c("yearSeason","siteNumber")]
      #year
      all_dt[, FN_ConcDay_year := sum(clean_ConcDay*Q_for_cleaning_concDay)/sum(Q_for_cleaning_concDay), by = c("Year","siteNumber")]
      #Month
      all_dt[, FN_ConcDay_month := sum(clean_ConcDay*Q_for_cleaning_concDay)/sum(Q_for_cleaning_concDay), by = c("Month","siteNumber")]
      #Season
      all_dt[, FN_ConcDay_season := sum(clean_ConcDay*Q_for_cleaning_concDay)/sum(Q_for_cleaning_concDay), by = c("Season","siteNumber")]
      
      #ConcAve data
      #this is where all of NA values come into play
      #yearMonth
      all_dt[,ConcAve_yearMonth := mean(ConcAve, na.rm = TRUE), by = c("yearMonth","siteNumber")]
      #Need to count how many samples per month, to give reader better understanding of means
      all_dt[,ConcAve_counts_yearMonth := sum(!is.na(ConcAve)), by = c("yearMonth","siteNumber")]
      #yearSeason
      all_dt[,ConcAve_yearSeason := mean(ConcAve, na.rm = TRUE), by = c("yearSeason","siteNumber")]
      #Need to count how many samples per month, to give reader better understanding of means
      all_dt[,ConcAve_counts_yearSeason := sum(!is.na(ConcAve)), by = c("yearSeason","siteNumber")]
      #year
      all_dt[,ConcAve_year := mean(ConcAve, na.rm = TRUE), by = c("Year","siteNumber")]
      #Need to count how many samples per month, to give reader better understanding of means
      all_dt[,ConcAve_counts_year := sum(!is.na(ConcAve)), by = c("Year","siteNumber")]
      #Month
      all_dt[,ConcAve_month := mean(ConcAve, na.rm = TRUE), by = c("Month","siteNumber")]
      #Need to count how many samples per month, to give reader better understanding of means
      all_dt[,ConcAve_counts_month := sum(!is.na(ConcAve)), by = c("Month","siteNumber")]
      #season
      all_dt[,ConcAve_season := mean(ConcAve, na.rm = TRUE), by = c("Season","siteNumber")]
      #Need to count how many samples per month, to give reader better understanding of means
      all_dt[,ConcAve_counts_season := sum(!is.na(ConcAve)), by = c("Season","siteNumber")]
      
      #FluxAve
      all_dt[,FluxAve_yearMonth := mean(FluxAve, na.rm = TRUE), by = c("yearMonth","siteNumber")]
      all_dt[,FluxAve_yearSeason := mean(FluxAve, na.rm = TRUE), by = c("yearSeason","siteNumber")]
      all_dt[,FluxAve_year := mean(FluxAve, na.rm = TRUE), by = c("Year","siteNumber")]
      all_dt[,FluxAve_month := mean(FluxAve, na.rm = TRUE), by = c("Month","siteNumber")]
      all_dt[,FluxAve_season := mean(FluxAve, na.rm = TRUE), by = c("Season","siteNumber")]
      
      #Flow weighting ConcAve
      #same process as before
      Q_for_cleaning_concAve <- all_dt$Q
      ConcAve_working <- all_dt$ConcAve
      na_index <- is.na(ConcAve_working) | is.na(Q_for_cleaning_concAve)
      Q_for_cleaning_concAve[na_index] <- NA
      ConcAve_working[na_index] <- NA
      all_dt[,Q_for_cleaning_concAve := ..Q_for_cleaning_concAve]
      all_dt[,clean_ConcAve := ..ConcAve_working]
      
      #FN ConcAve
      #yearMonth
      all_dt[, FN_ConcAve_yearMonth := sum(clean_ConcAve*Q_for_cleaning_concAve)/sum(Q_for_cleaning_concAve), by = c("yearMonth","siteNumber")]
      #yearSeason
      all_dt[, FN_ConcAve_yearSeason := sum(clean_ConcAve*Q_for_cleaning_concAve)/sum(Q_for_cleaning_concAve), by = c("yearSeason","siteNumber")]
      #year
      all_dt[, FN_ConcAve_year := sum(clean_ConcAve*Q_for_cleaning_concAve)/sum(Q_for_cleaning_concAve), by = c("Year","siteNumber")]
      #Month
      all_dt[, FN_ConcAve_month := sum(clean_ConcAve*Q_for_cleaning_concAve)/sum(Q_for_cleaning_concAve), by = c("Month","siteNumber")]
      #Season
      all_dt[, FN_ConcAve_season := sum(clean_ConcAve*Q_for_cleaning_concAve)/sum(Q_for_cleaning_concAve), by = c("Season","siteNumber")]
      
      #delete columns not needed
      all_dt[,c("Q_for_cleaning_concAve","clean_ConcAve","Q_for_cleaning_concDay","clean_ConcDay")] <- NULL
      #this saves the individual fraction data from model and adds to it for every fraction
      total_dt <- rbind(total_dt,all_dt)
      #resetting sample dataframe to make sure same info isn't run twice
      Sample = data.frame()
    }
    #this saves individual parameters through the loop
    #if an error is encountered, this will save the parameters already run
    #Errors can prevent the model from running, 
    #if encountered you can troubleshoot but may have to skip the specific parameter/fraction and continue looping through the rest
    Samples_dt <- rbind(Samples_dt,total_dt)
  }
}

fwrite(Samples_dt,".csv")

#After running the script and writing the files, I merged the files so that each file had a particular parameter/fraction combo for all sites
#Below is the code I used for quality assurance and identifying potential outliers
#After flagging outliers and deleting them, the data in database went through the time averaging portion of script again to provide best quality time-averaged estimates of parameters
all_dt$outlier_99.5_concAve <- ifelse(test = all_dt$ConcAve > quantile(all_dt$ConcAve, 0.995,na.rm=T), yes = "Outlier", no = NA)
all_dt$outlier_99.5_concDay <- ifelse(test = all_dt$ConcDay > quantile(all_dt$ConcAve, 0.995,na.rm=T), yes = "Outlier", no = NA)
all_dt$outlier_99.5_Q <- ifelse(test = all_dt$Q > quantile(all_dt$Q, 0.995,na.rm=T), yes = "Outlier", no = NA)

all_dt$outlier_0.5_concAve <- ifelse(test = all_dt$ConcAve < quantile(all_dt$ConcAve, 0.005,na.rm=T), yes = "Outlier", no = NA)
all_dt$outlier_0.5_concDay <- ifelse(test = all_dt$ConcDay < quantile(all_dt$ConcAve, 0.005,na.rm=T), yes = "Outlier", no = NA)
all_dt$outlier_0.5_Q <- ifelse(test = all_dt$Q < quantile(all_dt$Q, 0.005,na.rm=T), yes = "Outlier", no = NA)

