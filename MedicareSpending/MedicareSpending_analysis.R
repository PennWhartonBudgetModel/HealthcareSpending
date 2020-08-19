# Here for the Medicare spending, the following variables will be used:
# TOTMCR: Total Medicare expenditure
# PART B-
# OBVMCR: All Office-based visits Medicare expenditure
# OPTMCR: All Outpatient visits -- Medical provider visits to hospital outpatient departments
# ERTMCT: Emergency room -- Visits to hospital emergency rooms
# HHAMCR: Agency Home health provider
# HHNMCR: Non-angency home health provider
# PART A-
# IPTMCR: Inpatient hospital stay -- Hospitalizations, including zero-night stays.
# PART C-
# DVTMCR: All Dental care
# VISMCR: Vision -- Glasses and/or contact lenses.
# OTHMCR: Other equipment/supplies
# PART D-
# RXMCR: Prescription medication

# clear the environment
rm(list = ls())

# ***edit your working directory here***
Main_Dir <- "C://Users//yanhe8//Dropbox (Penn)//MEPS_Healthcare"
Data_Dir <- "C://Users//yanhe8//Dropbox (Penn)//MEPS_Healthcare//DATA"
Result_Dir <- "C://Users//yanhe8//Dropbox (Penn)//MEPS_Healthcare//Output"
Code_Dir <- "E://Repositories//HealthcareSpending//MedicareSpending"

#installing the relevant packages
setwd(Code_Dir)
source("package_check.R")

#===========================================================================================
#load data
setwd(Data_Dir)
# here since MEPS_ForMedicareSpending_2007_2017_new.rda is Consolidated_df in LOAD MPES.R
load("MEPS_Healthcare_Spending_2007_2017.rda")

#fetch the specific variables you want
df <- Consolidated_df[,c('YEAR','AGE', 'AGELAST', 'PERWT','TOTEXP','TOTSLF','TOTMCR','MCREV', 
                         'OBVMCR', 'OPTMCR', 'ERTMCR', 'IPTMCR', 'DVTMCR', 'HHAMCR', 'HHNMCR', 
                         'VISMCR', 'OTHMCR', 'RXMCR', 'RXEXP', 'RXMCD', "MCDEV")]
#rename them your way
names(df) <- c("Year","Age", "AgeLastRound", "Weight","TotalExp","OOP","Medicare", "Medicare_Yes",
               'OfficeBasedVisits_MCR', 'Outpatient_MCR', 'Emergency_MCR', 'Inpatient_MCR', 
               'Dental_MCR', 'HomeHealthAgency_MCR', 'HomeHealthNonAgency_MCR', 'Vision_MCR', 
               'OtherMedSupply_MCR', 'Prescription_MCR', 'TotalPrescriptionExp', 
               'Prescription_MCD', "Medicaid_Yes")

# create Variables indicating different parts of the Medicare spending
df['MedicarePartA'] = df$Inpatient_MCR
df['MedicarePartB'] = df$OfficeBasedVisits_MCR + df$Outpatient_MCR + df$Emergency_MCR +
  df$HomeHealthAgency_MCR + df$HomeHealthNonAgency_MCR
df['MedicarePartC'] = df$Dental_MCR + df$Vision_MCR + df$OtherMedSupply_MCR
df['MedicarePartD'] = df$Prescription_MCR

#clean the dataframe by removing insignificant/irrelevant observations
df <- subset(df,Weight!=0)
# convert to integer to avoid data format inconsistency
df$Age <- as.integer(df$Age)
# reformat the "Medicare_yes" so that 1 is yes and 0 is no
df$Medicare_Yes[df$Medicare_Yes==2]<-0
df$Medicaid_Yes[df$Medicaid_Yes==2]<-0

#adjust expenditures to account for inflation using CPI (optional)
# price indexes are available at https://meps.ahrq.gov/about_meps/Price_Index.shtml
# https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=2#reqid=19&step=2&isuri=1&1921=survey
years <- c("2007", "2008",  "2009",  "2010",  "2011",  "2012",  "2013", "2014", "2015", "2016", "2017")
GDP_Index   <- c(92.498, 94.264, 94.999, 96.109, 98.112, 100, 101.773, 103.647, 104.688, 105.770, 107.795)
GDP_Index2020 = 113.119
# adjust
for(i in seq(years)){
  df$TotalPrescriptionExp[df$Year==years[i]]<-df$TotalPrescriptionExp[df$Year==years[i]]*(GDP_Index2020/GDP_Index[i])
  df$TotalExp[df$Year==years[i]]<-df$TotalExp[df$Year==years[i]]*(GDP_Index2020/GDP_Index[i])
  df$OOP[df$Year==years[i]]<-df$OOP[df$Year==years[i]]*(GDP_Index2020/GDP_Index[i])
  df$Medicare[df$Year==years[i]]<-df$Medicare[df$Year==years[i]]*(GDP_Index2020/GDP_Index[i])
  df$MedicarePartA[df$Year==years[i]]<-df$MedicarePartA[df$Year==years[i]]*(GDP_Index2020/GDP_Index[i])
  df$MedicarePartB[df$Year==years[i]]<-df$MedicarePartB[df$Year==years[i]]*(GDP_Index2020/GDP_Index[i])
  df$MedicarePartC[df$Year==years[i]]<-df$MedicarePartC[df$Year==years[i]]*(GDP_Index2020/GDP_Index[i])
  df$MedicarePartD[df$Year==years[i]]<-df$MedicarePartD[df$Year==years[i]]*(GDP_Index2020/GDP_Index[i])
  df$Prescription_MCD[df$Year==years[i]]<-df$Prescription_MCD[df$Year==years[i]]*(GDP_Index2020/GDP_Index[i])
}

###Do some extra processing of the data
# For people who are not covered by Medicare, their Medicare spending should be 0
df$Medicare[df$Medicare_Yes == 0] = 0
df$MedicarePartA[df$Medicare_Yes == 0] = 0
df$MedicarePartB[df$Medicare_Yes == 0] = 0
df$MedicarePartC[df$Medicare_Yes == 0] = 0
df$MedicarePartD[df$Medicare_Yes == 0] = 0

# for people who are not covered by Medicaid, their Medicaid on drug should be 0
df$Prescription_MCD[df$Medicaid_Yes == 0] = 0

# generate variables used for calculating the % Medicare enrollment for each age
df['WgtMedicareEnrollee'] = df$Weight
df$WgtMedicareEnrollee[df$Medicare_Yes==0] <- 0

# generate variables used for calculating the % Medicaid enrollment for each age
df['WgtMedicaidEnrollee'] = df$Weight
df$WgtMedicaidEnrollee[df$Medicaid_Yes==0] <- 0

# generate age group
df$Age[(df$Age == -1) & (!is.na(df$AgeLastRound))] = 
  df$AgeLastRound[(df$Age == -1) & (!is.na(df$AgeLastRound))] 
# recode the age -1 with the age from last round

df['AgeGrp'] = df$Age
df$AgeGrp[df$Age >= 65] = 65
# df$AgeGrp[(df$Age >= 60) & (df$Age < 65)] = 60

# local function to do calculate the weighted mean
uyen <- function(df, age_grp, tot_pop = TRUE) {
  age_grp = enquo(age_grp) # create a quosure
  
  output <-  df %>%
    group_by(Year,!!age_grp) %>%
    summarize(total = weighted.mean(TotalExp, Weight, na.rm = TRUE))
  
  output0 <-  df %>%
    group_by(Year,!!age_grp) %>%
    summarize(totalRx = weighted.mean(TotalPrescriptionExp, Weight, na.rm = TRUE))
  
  output1 <-  df %>%
    group_by(Year,!!age_grp) %>%
    summarize(oop = weighted.mean(OOP, Weight, na.rm = TRUE))
  
  output2 <-  df %>%
    group_by(Year,!!age_grp) %>%
    summarize(mcare = weighted.mean(Medicare, Weight, na.rm = TRUE))

  output3 <-  df %>%
    group_by(Year,!!age_grp) %>%
    summarize(mcareA = weighted.mean(MedicarePartA, Weight, na.rm = TRUE))
  
  output4 <-  df %>%
    group_by(Year,!!age_grp) %>%
    summarize(mcareB = weighted.mean(MedicarePartB, Weight, na.rm = TRUE))
  
  output5 <-  df %>%
    group_by(Year,!!age_grp) %>%
    summarize(mcareC = weighted.mean(MedicarePartC, Weight, na.rm = TRUE))
  
  output6 <-  df %>%
    group_by(Year,!!age_grp) %>%
    summarize(mcareD = weighted.mean(MedicarePartD, Weight, na.rm = TRUE))
  
   # total population
  if (tot_pop == TRUE) {
    output7 <- df %>%
      group_by(Year,!!age_grp) %>%
      summarize(TotPop = sum(Weight, na.rm = TRUE))    
  }

   # total Medicare enrollees
   output8 <- df %>%
     group_by(Year,!!age_grp) %>%
     summarize(MedicarePop = sum(WgtMedicareEnrollee, na.rm = TRUE))   

   # total Medicaid enrollees
   output9 <- df %>%
     group_by(Year,!!age_grp) %>%
     summarize(MedicaidPop = sum(WgtMedicaidEnrollee, na.rm = TRUE))  
   
   # Medicaid Spending on Drug 
   output10 <-  df %>%
     group_by(Year,!!age_grp) %>%
     summarize(MedicaidDrug = weighted.mean(Prescription_MCD, Weight, na.rm = TRUE))
   
  output <- left_join(output,output0)
  output <- left_join(output,output1)
  output <- left_join(output,output2)
  output <- left_join(output,output3)
  output <- left_join(output,output4)
  output <- left_join(output,output5)
  output <- left_join(output,output6)
  if (tot_pop == TRUE) {
    output <- left_join(output,output7)
  }
  output <- left_join(output,output8)
  output <- left_join(output,output9)
  output <- left_join(output,output10)
  
  return(output)
}

#subset df to get only people with Medicare coverage
df_mcr <- subset(df, Medicare_Yes==1)
df_mcd <- subset(df, Medicaid_Yes ==1)
df_old <- subset(df,Age>=60)
df_old_mcr <- subset(df_old,Medicare_Yes==1)
df_old_mcd <- subset(df_old,Medicaid_Yes==1)

# output resultant dataframes--of data by age
o1<-uyen(df, Age, TRUE)
o2<-uyen(df_mcr, Age, FALSE)
o3<-uyen(df_mcd, Age, FALSE)

# create workbook
wb <- createWorkbook()
addWorksheet(wb, "All")
addWorksheet(wb, "Only people with Medicare")
addWorksheet(wb, "Only people with Medicaid")

# put our dataframes into the worksheet
writeData(wb, 1, o1)
writeData(wb, 2, o2)
writeData(wb, 3, o3)

# save worksheet
setwd(Result_Dir)
saveWorkbook(wb, "PerCapitaMedExpenditure.xlsx", overwrite = TRUE)

###################################################
# output result dataframes--of data by age group
o1<-uyen(df_old, AgeGrp, TRUE)
o2<-uyen(df_old_mcr, AgeGrp, FALSE)
o3<-uyen(df_old_mcd, AgeGrp, FALSE)

# create workbook
wb <- createWorkbook()
addWorksheet(wb, "All")
addWorksheet(wb, "Only people with Medicare")
addWorksheet(wb, "Only people with Medicaid")

# put our dataframes into the worksheet
writeData(wb, 1, o1)
writeData(wb, 2, o2)
writeData(wb, 3, o3)

# save worksheet
setwd(Result_Dir)
saveWorkbook(wb, "PerCapitaMedExpenditure_oldPeople.xlsx", overwrite = TRUE)

