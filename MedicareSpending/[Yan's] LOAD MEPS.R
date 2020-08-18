# process medical expenditure panel survey (MEPS)
# 1996 - 2016
# both consolidated dataset and longitudinal dataset

# set up paths
Dir <- "C://Users//yanhe8//Dropbox (Penn)//MEPS_Healthcare"
Data_Dir <- "C://Users//yanhe8//Dropbox (Penn)//MEPS_Healthcare//DATA"
Result_Dir <- "C://Users//yanhe8//Dropbox (Penn)//MEPS_Healthcare"
Code_Dir <- "E://Repositories//HealthcareSpending//MedicareSpending"
MEPS_RAW_Dir <- "Z://MEPS//Raw"
# install pacakges

setwd(Code_Dir)
source("package_check.R") 

#===========================================================================================
# Load MEPS
#===========================================================================================

#setwd(Code_Dir)
setwd(MEPS_RAW_Dir)

# all available meps years
year <- 1996:2017

# specify the file numbers of all MEPS public use files
# (these were acquired from browsing around http://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp)
# notes:
# 1996 files are buggy.
# 2000, 2001, 2002, and 2003 jobs files need a workaround.
# 2001 and 2002 medical conditions files need a workaround.
# 2000, 2001, and 2002 events files need a workaround.
consolidated <- c( 12 , 20 , 28 , 38 , 50 , 60 , 70 , 79 , 89 , 97 , 105 , 113 , 121 , 129 , 138 , 147 , 155 , 163, 171, 181, 192, 201)
longitudinal <- c( NA , 23 , 35 , 48 , 58 , 65 , 71 , 80 , 86 , 98 , 106 , 114 , 122 , 130 , 139 , 148 , 156 , 164, 172, 183, 193, 202)

# specify the most current brr / link file locations
#lf <- "http://meps.ahrq.gov/data_files/pufs/h36brr13ssp.zip"

# create a big table containing the file number of each meps data file available
# mm stands for meps matrix - a data table pointing to each data file
mm <- 
  data.frame(
    year , 
    consolidated , 
    longitudinal
  )

# anything containing the character string 'NA' should be replaced with NA
for ( i in seq( ncol( mm ) ) ) mm[ grepl( "NA" , mm[ , i ] ) , i ] <- NA


# if you only want to download certain years of data,
# subset the mm object here.  some examples:

# only download MEPS years 2007 - 2017
startyear<-2007
endyear<-2017
mm <- subset( mm , year %in% startyear:endyear)

# conversion options #

# it's recommended you keep a version of the .rda files,
# since they work with all subsequent scripts

# do you want to store the sas transport (.ssp) file in the working directory?
ssp <- FALSE

# do you want to save an R data file (.rda) to the working directory?
rda <- TRUE

# do you want to save a stata-readable file (.dta) to the working directory?
dta <- FALSE

# do you want to save a comma-separated values file (.csv) to the working directory?
csv <- TRUE

# end of conversion options #


# Creating Variables. NOTE the list ending in "_RN" indicate the naming of the variables in the final data frame.
# Note: Variable definitions for the Consolidated data are available at http://goo.gl/BDtxYA (this is a short URL)

# Consolidated variables whose name is the same accross years
vars_consol1    <- c("AGE53X","AGELAST", "BORNUSA", "USBORN42", "YRSINUS", "USLIVE42")

# Consolidated variables whose name differs across years and have a suffix
vars_consol2    <- c("PERWT")

# Consolidated variables whose name differs across years 
vars_consol3    <- c("MCREV", "TOTEXP", "TOTMCR", "TOTSLF", "IPNGTD","MCRPB", "MCRPHO",
                     "OBVMCR", "OBDMCR", "OBOMCR", "OBCMCR", "OBNMCR", "OBEMCR", "OBAMCR", "OBTMCR",
                     "OPTMCR", "OPFMCR", "OPDMCR", "OPVMCR", "OPSMCR", "OPOMCR", "OPPMCR",
                     "AMCMCR", "AMNMCR", "AMEMCR", "AMAMCR", "AMTMCR", "ERTMCR", "ERFMCR", "ERDMCR", 
                     "ZIFMCR", "ZIDMCR", "IPTMCR", "IPFMCR", "IPDMCR", "DVTMCR", "DVGMCR", "DVOMCR",
                     "HHAMCR", "HHNMCR", "VISMCR", "OTHMCR", "RXMCR", "RXEXP")

# Consolidated variables whose name differs across years and have a suffix
vars_consol4    <-c("AGE", "MCRPD")

# Defining a function I will use to select the last two digits from a four-digit year
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# begin importing the data for all years specified in the mm (meps matrix) table
# start importing the most current year first.
for ( i in nrow( mm ):1 ) {
  whatyear<-startyear+i-1
  
  # year is the first column, so cycle through all the others (consolidated: j=2, panel: j=3.
  for ( j in 2:ncol( mm ) ) {
    
    # Only if the current table position has something in it do we continue here.
    if ( !is.na( mm[ i , j ] ) ) {
      
      # Reading the consolidated data used for calculating medical expenditures
      if(j==2){
        # Creating the year-specific list of variable names (note that many variables include the last two digits of the
        # four digit year. Year_temp calculates those, the result of which is then added to the variables defined above.
        # Some variables also require an "F" or an "X" behind the year.
        year_temp = substrRight(mm[i,1],2)
        
        # some variables only exist in certain years, so update the var_consoli var name list
        # based on the year temp condition
        vars_consol1_temp = vars_consol1
        vars_consol3_temp = vars_consol3
 
        if (as.integer(year_temp) < 13) {
          vars_consol1_temp = vars_consol1_temp[!(vars_consol1_temp %in% c("BORNUSA", "YRSINUS"))]
        } # here 13 means 2013
        if (as.integer(year_temp) > 12) {
          vars_consol1_temp = vars_consol1_temp[!(vars_consol1_temp %in% c("USBORN42", "USLIVE42"))]
        }
        if (as.integer(year_temp) < 9) {
          vars_consol3_temp = vars_consol3_temp[!(vars_consol3_temp %in% c("MCRPB"))]
        }
        if (as.integer(year_temp) == 7) {
          vars_consol1_temp = vars_consol1_temp[!(vars_consol1_temp %in% c("AGELAST"))]
        } 
        if (as.integer(year_temp) == 17) {
          vars_consol3_temp = vars_consol3_temp[!(vars_consol3_temp %in% c("OBOMCR", "OBCMCR", "OBNMCR", "OBEMCR", 
                                                            "OBAMCR", "OBTMCR", "OPOMCR", "OPPMCR",
                                                            "AMCMCR", "AMNMCR", "AMEMCR", "AMAMCR", 
                                                            "AMTMCR", "ZIFMCR", "ZIDMCR", "DVGMCR", "DVOMCR"))]
        }
        
        # Creating variable lists for consolidated data for use in final data frame
        vars_consol  <- c(vars_consol1_temp,vars_consol2,vars_consol3_temp,vars_consol4)
        
        vars_consol2_temp <- paste(vars_consol2,year_temp,"F",sep="")
        vars_consol3_temp <- paste(vars_consol3_temp,year_temp,sep="")
        vars_consol4_temp <- paste(vars_consol4,year_temp,"X",sep="")
        
        vars_consol_temp  <- c(vars_consol1_temp,vars_consol2_temp,vars_consol3_temp,vars_consol4_temp)
        
        # create the full path to the zipped file on the disk
        u <- paste0( MEPS_RAW_Dir,"//h" , mm[ i , j ] , ".ssp" )     
        
        # read the file in as an R data frame
        temp      <- read.xport( u )
        
        # Selecting the required variables from the whole dataset
        temp_sub  <- temp[vars_consol_temp]
        
        # renaming the columns to get rid of the years and suffixes in the variable names
        names(temp_sub) <- vars_consol
        temp_sub$YEAR<-whatyear
        
        # Add each year of data to the final data frame
        if(i == nrow(mm)) Consolidated_df <- as.data.frame(temp_sub)
        if(i  < nrow(mm)) Consolidated_df <- dplyr::bind_rows(Consolidated_df,temp_sub)
        
        # immediately delete the temp_sub data frame from memory and clear up ram
        rm( temp_sub ) ; gc()
      } 
      
    }
  }
}

# Saving the results to .rda file
setwd(Data_Dir)
save( Consolidated_df , file = "MEPS_Healthcare_Spending_2007_2017.rda" )
write.csv(Consolidated_df,'MEPS_Healthcare_Spending_2007_2017.csv', row.names = F)

# delete data frames from memory and clear up ram
rm( Consolidated_df ) ; rm( temp ) ; rm( mm ) ;gc()

# Cleaning up unnecessary object (these are temporary data frames). In particular, we are deleting
# all objects that are larger than 10,000 bytes large
i=1
for (things in ls()){
  things <- ls()[i]
  size = object.size(get(things))
  name<- ls()[i]
  if (size>10000) rm(list=c(name))   # deleting everything that is more than 10000 bytes large
  if (size<10000) i = i+1
}


