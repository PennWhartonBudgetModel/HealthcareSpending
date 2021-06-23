library(tidyverse) 
library(ggplot2)
library(openxlsx)
library(readxl)

setwd("E:/Repositories/HealthcareSpending/Healthcare expenditure")
# meps <- read.csv("F:/Interfaces/meps_person.csv.gz")
meps <- read.csv("F:/Interfaces/meps.csv.gz")
meps <- meps[meps$Year>=2008 & meps$FinalWeight>0,]

# create excel workbook
# wb <- createWorkbook()
# Do more data processing-mainly labeling of some demographic variables
# Age-group, gender, race, education, region, Hispanic, Income level, family size, coverage type
meps["AgeGroup"] <- 0
meps[meps$Age>=18 & meps$Age<35, "AgeGroup"] <- 1
meps[meps$Age>=35 & meps$Age<45, "AgeGroup"] <- 2
meps[meps$Age>=45 & meps$Age<55, "AgeGroup"] <- 3
meps[meps$Age>=55 & meps$Age<65, "AgeGroup"] <- 4
meps[meps$Age>=65, "AgeGroup"] <- 5

# Poverty level
meps["PovertyLevel"] <- 0
meps[meps$PercentOfFPL>=18 & meps$PercentOfFPL<35, "PovertyLevel"] <- 1
meps[meps$PercentOfFPL>=35 & meps$PercentOfFPL<45, "PovertyLevel"] <- 2
meps[meps$PercentOfFPL>=45 & meps$PercentOfFPL<55, "PovertyLevel"] <- 3
meps[meps$PercentOfFPL>=55 & meps$PercentOfFPL<65, "PovertyLevel"] <- 4
meps[meps$PercentOfFPL>=65, "PovertyLevel"] <- 5

meps["AgeGroup"] <- factor(meps$AgeGroup, labels = c("Under18", "18-34","35-44", "45-54", "55-64", "65 and Over"))
meps["Gender"] <- factor(meps$Gender, labels = c("Male", "Female"))
meps["Race"] <- factor(meps$Race, labels = c("White Non-Hisp", "Black Non-Hisp", "Hispanic", "Asian Non-Hisp", "Other"))
meps["Education"] <- factor(meps$Race, labels = c("Under High School", "High School/GED/Other degree", "Some College", "Bachelor", "Master or more"))
meps["MaritalStatus"] <- factor(meps$MaritalStatus, labels = c("NIU(<16)", "Married", "Widowed", "Divorced", "Separated", "Never Married"))
meps["EmploymentStatus"] <- factor(meps$EmploymentStatus, labels = c("NIU(<16)", "Unemployed/Unknown", "Employed"))
meps["RegionMEPS"] <- factor(meps$RegionMEPS, labels = c("Northeast", "Midwest", "South", "West"))
meps["PovertyStatus"] <- factor(meps$PovertyStatus, labels = c("<100%FPL", "100-124%FPL", "125-199%FPL", "200-399%FPL", ">=400%FPL"))
meps["CoverageType"] <- factor(meps$CoverageType, labels = c("Uninsured", "Any Private", "Public only"))

# create the OOP share variable
meps['OOPShare'] <- meps$TotalOOPExp/meps$TotalHealthExp
# there are some NAs from obs where both OOPexp and total exp are 0

for (service in c("Outpatient", "ER", "Hospitalization", "Dental", "Homehealth",
                  "Rx", "MedEquip")) {
  meps[paste0(service,'OOPShare')] <- 
    meps[paste0(service, 'OOPExp')]/meps[paste0(service, 'TotalExp')]
}

meps['TotalTotalExp'] <- meps$TotalHealthExp
save(meps, file='meps.rda')

## 1) the enrollment rate of Medicare/Medicaid/PHI/Uninsured among different groups
demolist = c('AgeGroup','Gender','Race','MaritalStatus','Education','EmploymentStatus','PovertyStatus','RegionMEPS')

df_process1 <- function(demo1) {
  meps["byvar"] <- meps[demo1]
  
  mcr <- meps %>%
    group_by(Year, byvar) %>%
    summarise(value = weighted.mean(CoveredByMedicare, FinalWeight,na.rm = T)*100)
  mcr["instype"] <- "Medicare"
  
  mcd <- meps %>%
    group_by(Year, byvar) %>%
    summarise(value = weighted.mean(CoveredByMedicaid, FinalWeight,na.rm = T)*100)
  mcd["instype"] <- "Medicaid"
  
  phi <- meps %>%
    group_by(Year, byvar) %>%
    summarise(value = weighted.mean(CoveredByPrivateInsurance, FinalWeight,na.rm = T)*100)
  phi["instype"] <- "Private"
  
  unins <- meps %>%
    group_by(Year, byvar) %>%
    summarise(value = weighted.mean(NoInsuranceCoverage, FinalWeight,na.rm = T)*100)
  unins["instype"] <- "No Insurance"
  
  df <- mcr %>% bind_rows(mcd) %>% bind_rows(phi) %>% bind_rows(unins) 
  # %>% filter(byvar %in% input$demolevels1)
  
  df["instype"] <- factor(df$instype, levels = c("Medicare", "Medicaid", "Private", 
                                                 "No Insurance"))
  df["Demo"] <- demo1
  return(df)
}

df1 <- data.frame()
for (var in demolist) {
  temp <- df_process1(var)
  df1 <- df1 %>% bind_rows(temp)
}

df1 %>% 
  filter(Demo == 'Gender') %>%
  ggplot(aes(x = Year, y = value, group=byvar, color = byvar)) +
  geom_line()+
  scale_x_continuous(breaks=seq(2008,2018,1)) +
  facet_wrap(~instype, scales = "free_y") +
  theme_bw() +
  labs(title= 'Enrollment Rate', x = "Year", colour = 'Gender')

# 2) The OOP share of different health services among different demographics (line plots)
df_process2 <- function(demo2) {
  
  meps["group"] <- meps[demo2]
  
  oop <- meps %>%
    filter(TotalHealthExp>0) %>%
    group_by(Year, group) %>%
    summarise(OOPShare = weighted.mean(OOPShare, FinalWeight,na.rm = T)*100)
  oop['service'] <- "Total"
  
  for (service in c("Outpatient", "ER", "Hospitalization", "Dental", "Homehealth",
                    "Rx", "MedEquip")) {
    meps['tot'] <- meps[,paste0(service, "TotalExp")]
    meps['oop'] <- meps[,paste0(service, "OOPShare")]
    
    temp <- meps %>%
      filter(tot>0) %>%
      group_by(Year, group) %>%
      summarise(OOPShare = weighted.mean(oop, FinalWeight,na.rm = T)*100)
    temp['service'] <- service
    oop <- oop %>% bind_rows(temp)
  }
  
  oop[oop$service=='Total', "service"] <- "All"
  oop["service"] <- factor(oop$service,levels = c("All","Hospitalization", "Outpatient",
                                                  "ER", "Dental", "Homehealth", "Rx",
                                                  "MedEquip"))
  oop["Demo"] <- demo2
  # oop <- oop %>%
  #   filter(group %in% input$demolevels2)
  return (oop)
}

df2 <- data.frame()
for (var in append(demolist, 'CoverageType')) {
  temp <- df_process2(var)
  df2 <- df2 %>% bind_rows(temp)
}

df2 %>%
  filter(Demo=='Gender') %>%
  ggplot(aes(x = Year, y = OOPShare, group=group, color = group)) +
  geom_line()+
  facet_wrap(~service, scales = "free_y") +
  scale_x_continuous(breaks=seq(2008,2018,1)) +
  theme_bw()+
  labs(title= 'OOP Share', x = "Year", colour = 'Gender')

## 3) The bar plots of population and spending
df_prep <- function(exp, grp, pay) {
  
  df <- meps[meps[exp]>0, c('Year', "FinalWeight", exp, grp)]
  df['group'] <- df[, grp]
  df['expenditure'] <- df[,exp]
  
  df <- df %>%
    group_by(Year, group) %>%
    summarise(Population = sum(FinalWeight),
              Spending = sum(FinalWeight*expenditure)) %>%
    group_by(Year) %>%
    mutate(TotalPop = sum(Population),
           TotalExp = sum(Spending),
           PopShare = Population/TotalPop*100,
           ExpShare = Spending/TotalExp*100) %>%
    select(Year, group, PopShare, ExpShare) %>%
    pivot_longer(cols=c("PopShare", "ExpShare"),  # reshape from wide to long
                 names_to = 'var',
                 values_to = 'value') 
  df$var <- factor(df$var, levels =c("PopShare", "ExpShare"))
  df['PaymentType'] <- pay
  return(df)
}

# process data for graph 3
df_process3 <- function(service, demo3) {
  
  expvarlist <- c("TotalExp", "MedicareExp", "MedicaidExp", 
                  "PHIandTricareExp", "OtherExp", "OOPExp")
  varlist <- lapply(service, paste0, expvarlist)[[1]]
  
  tot <- df_prep(varlist[1], demo3, "Total")
  mcr <- df_prep(varlist[2], demo3, "Medicare")
  mcd <- df_prep(varlist[3], demo3, "Medicaid")
  phi <- df_prep(varlist[4], demo3, "PHI/Tricare")
  oth <- df_prep(varlist[5], demo3, "Other insurance")
  oop <- df_prep(varlist[6], demo3, "OOP")
  
  df <- tot %>% bind_rows(mcr) %>% bind_rows(mcd) %>% bind_rows(phi) %>%
    bind_rows(oth) %>% bind_rows(oop)
  
  df["PaymentType"] <- factor(df$PaymentType, levels = c("Total", "Medicare", "Medicaid", "PHI/Tricare", "Other insurance", "OOP"))
  df["service"] <- service
  df["Demo"] <- demo3
  # df <- df %>%
  #   filter(group %in% input$demolevels3)
  return (df)
}

healthservice <- c('Total','Hospitalization', 'Outpatient', 'ER','Dental', 
                   'Homehealth', 'Rx', 'MedEquip')

df3 <- data.frame()
for (demo in demolist) {
  for (service in healthservice) {
    temp <- df_process3(service, demo)
    df3 <- df3 %>% bind_rows(temp)
  }
}

df3[df3$service=="Total", "service"] <- "All"

df3 %>%
  filter(Year==2018 & Demo=="Gender" & service=="All") %>%
  ggplot(aes(x = var, y = value, fill = group)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels=c("PopShare" = "Pop",
                            "ExpShare" = "Exp")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45)) +
  geom_text(aes(label = paste0(round(value,1), '%')), size = 2, position = position_stack(vjust = 0.5)) +
  facet_grid(~PaymentType) +
  labs(title= "Share of population and corresponding spending", y="Share", x = "")

# save the three processed data
save(df1, file='df1.rda')
save(df2, file='df2.rda')
save(df3, file='df3.rda')
