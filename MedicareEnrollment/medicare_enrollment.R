library(readxl)
library(tidyverse)
setwd("Z:/CMS/Interfaces/2021-05-05-yanhe/MedicareEnrollment")

total.enroll <- read_excel("total_enrollment.xlsx", sheet = "MA vs tradtional")
colnames(total.enroll) <- c('year', 'total.enroll', 'enroll.pct.growth', 'org.enroll',
                            'org.pct.growth', 'org.share', 'ma.enroll',
                            'ma.pct.growth', 'ma.share')
total.enroll['year'] <- as.numeric(total.enroll$year)

# read all the enrollment time series
# create the names
AB.name <- c('AorB.total', 'AorB.aged', 'AorB.disabled',
             'AandB.total', 'AandB.aged', 'AandB.disabled',
             'A.total', 'A.aged', 'A.disabled',
             'B.total', 'B.aged', 'B.disabled')
byage.name <- c('total.enroll', 'count.under18', 'share.under18',
                'count.18to24', 'share.18to24',
                'count.25to34', 'share.25to34','count.35to44', 'share.35to44',
                'count.45to54', 'share.45to54','count.55to64', 'share.55to64',
                'count.65to74', 'share.65to74','count.75to84', 'share.75to84',
                'count.85to94', 'share.85to94','count.over95', 'share.over95')

# A/B by age and disabled status--all, traditional, MA
mcr.ab <- read_excel("total_enrollment.xlsx", sheet = "AB")
colnames(mcr.ab) <- c('year', AB.name)
mcr.ab['year'] <- as.numeric(mcr.ab$year)
mcr.age <- read_excel("total_enrollment.xlsx", sheet = "by age")
colnames(mcr.age) <- c('year', byage.name)
mcr.age['year'] <- as.numeric(mcr.age$year)

# traditional Medicare enrollments
org.ab <- read_excel("total_enrollment_org.xlsx", sheet = "AB")
colnames(org.ab) <- c('year', paste(AB.name, "org",sep="."))
org.age <- read_excel("total_enrollment_org.xlsx", sheet = "by age")
colnames(org.age) <- c('year', paste(byage.name, "org",sep="."))

# Medicare Advantage enrollments
ma.ab <- read_excel("total_enrollment_MA.xlsx", sheet = "AB")
colnames(ma.ab) <- c('year', paste(AB.name, "MA",sep="."))
ma.age <- read_excel("total_enrollment_MA.xlsx", sheet = "by age")
colnames(ma.age) <- c('year', paste(byage.name, "MA",sep="."))
# ma.age[ma.age=='*'] <- NA
# ma.age[ma.age=='???'] <- NA

# read enrollment by demographic
demo.names <- c('AorB.total', 'AandB.total', 'A.total', 'B.total')
demo.group <- c('total', 'under65', 'over65', 'under18', '18to24', '25to34',
                '35to44', '45to54', '55to64', '65to74', '75to84',
                '85to94', 'over95', 'male', 'female', 'Non-hispanic.White',
                'Black', 'Asian.or.Pacific', 'Hispanic', 'American.Indian', 
                'Other.race', 'Unknown.race')

mcr.demo <- read_excel("total_enrollment_by_demo.xlsx", sheet = "2013")
mcr.demo['year'] <- 2013
mcr.demo[,1] <- demo.group
for (yr in seq(2014,2019,1)) {
  temp <- read_excel("total_enrollment_by_demo.xlsx", sheet = as.character(yr))
  temp['year'] <- yr
  temp[,1] <- demo.group
  mcr.demo <- rbind(mcr.demo, temp)
}
colnames(mcr.demo) <- c('group', demo.names, 'year')

# traditional Medicare
org.demo <- read_excel("total_enrollment_org_by_demo.xlsx", sheet = "2013")
org.demo['year'] <- 2013
org.demo[,1] <- demo.group
for (yr in seq(2014,2019,1)) {
  temp <- read_excel("total_enrollment_org_by_demo.xlsx", sheet = as.character(yr))
  temp['year'] <- yr
  temp[,1] <- demo.group
  org.demo <- rbind(org.demo, temp)
}
colnames(org.demo) <- c('group',paste(demo.names, "org",sep="."), 'year')

# Medicare Advantage
ma.demo <- read_excel("total_enrollment_MA_by_demo.xlsx", sheet = "2013")
ma.demo['year'] <- 2013
ma.demo[,1] <- demo.group
for (yr in seq(2014,2019,1)) {
  temp <- read_excel("total_enrollment_MA_by_demo.xlsx", sheet = as.character(yr))
  temp['year'] <- yr
  temp[,1] <- demo.group
  ma.demo <- rbind(ma.demo, temp)
}
colnames(ma.demo) <- c('group',paste(demo.names, "MA",sep="."), 'year')

# read total enrollment by state
state.name <- c('state', 'state.pop', 'total.enroll', 'total.enroll.rate', 'org.enroll',
                'org.share', 'ma.enroll', 'ma.enroll.share', 'metro.enroll', 
                'micro.enroll', 'ncb.enroll')
mcr.state <- read_excel('total_enrollment_by_state.xlsx',sheet = "2013")
mcr.state['year'] <- 2013
for (yr in seq(2014,2019,1)) {
  temp <- read_excel("total_enrollment_by_state.xlsx", sheet = as.character(yr))
  temp['year'] <- yr
  mcr.state <- rbind(mcr.state, temp)
}
colnames(mcr.state) <- c(state.name, AB.name, 'year')

# traditional medicare 
org.state <- read_excel('total_enrollment_org_by_state.xlsx',sheet = "2013")
org.state['year'] <- 2013
for (yr in seq(2014,2019,1)) {
  temp <- read_excel("total_enrollment_org_by_state.xlsx", sheet = as.character(yr))
  temp['year'] <- yr
  org.state <- rbind(org.state, temp)
}
colnames(org.state) <- c('state', paste(AB.name, "org",sep="."), 'year')

# medicare advantage
ma.state <- read_excel('total_enrollment_ma_by_state.xlsx',sheet = "2013")
ma.state['year'] <- 2013
for (yr in seq(2014,2019,1)) {
  temp <- read_excel("total_enrollment_ma_by_state.xlsx", sheet = as.character(yr))
  temp['year'] <- yr
  ma.state <- rbind(ma.state, temp)
}
colnames(ma.state) <- c('state', paste(AB.name, "MA",sep="."), 'year')

# read the Medicare part D enrollment data
pd.name <- c('total.enroll', 'partD.enroll', 'stand.alone', 
             'ma.rx','wo.LIS', 'LIS', 'LIS.eligible', 'retiree.rx.subsidy',
             'no.pd.rx.retiree.subsidy')

mcr.pd <- read_excel('total_enrollment_partD.xlsx')
colnames(mcr.pd) <- c('year', pd.name)

# by demo
pd.demo <- read_excel('total_enrollment_partD_by_demo.xlsx', sheet = '2013')
pd.demo['year'] <- 2013
pd.demo[,1] <- demo.group

for (yr in seq(2014,2019,1)) {
  temp <- read_excel("total_enrollment_partD_by_demo.xlsx", sheet = as.character(yr))
  temp['year'] <- yr
  temp[,1] <- demo.group
  # append with other years
  pd.demo <- rbind(pd.demo, temp)
}
colnames(pd.demo) <- c('group', pd.name, 'year')

# by state
pd.state <- read_excel('total_enrollment_partD_by_state.xlsx', sheet = '2013')
pd.state['year'] <- 2013
for (yr in seq(2014,2019,1)) {
  temp <- read_excel("total_enrollment_partD_by_state.xlsx", sheet = as.character(yr))
  temp['year'] <- yr
  pd.state <- rbind(pd.state, temp)
}
colnames(pd.state) <- c('state', pd.name, 'year')

####################################
# merge all the data together ######
by.time <- total.enroll %>% full_join(mcr.age) %>% 
  full_join(mcr.ab) %>% full_join(org.ab) %>% 
  full_join(org.age) %>% full_join(ma.ab) %>% 
  full_join(ma.age) %>% full_join(mcr.pd)
  
by.state <- mcr.state %>% full_join(org.state) %>%
  full_join(ma.state) %>% 
  full_join(pd.state[,-2], by = c('state', 'year'))

by.demo <- mcr.demo %>% full_join(org.demo) %>%
  full_join(ma.demo) %>% full_join(pd.demo)

## in the by.demo data, add the under25 age group row
# create under25 age group
by.demo <- by.demo[!by.demo$group %in% c('under18', '18to24'),]
cols.num <- colnames(by.demo)
cols.num <- cols.num[!cols.num %in% c("group", 'year')]
by.demo[cols.num] <- sapply(by.demo[cols.num],as.numeric)

by.demo.final <- by.demo[by.demo$year==2013,]
agegrp <- c('25to34', '35to44', '45to54', '55to64', '65to74', '75to84',
            '85to94', 'over95')
by.demo.final <- by.demo.final %>%
  bind_rows(by.demo.final[by.demo.final$group=='total',cols.num] -
              colSums(by.demo.final[by.demo.final$group %in% agegrp,cols.num]))
by.demo.final[is.na(by.demo.final$group), 'group'] = 'under25'
# create group over 85
by.demo.final <- by.demo.final %>%
  bind_rows(colSums(
    by.demo.final[by.demo.final$group %in% c('85to94', 'over95'),cols.num]))
by.demo.final[is.na(by.demo.final$group), 'group'] = 'over85'
by.demo.final[is.na(by.demo.final$year), 'year'] = 2013

for (yr in seq(2014,2019,1)) {
  temp <- by.demo[by.demo$year==yr,]
  # create under25 group
  temp <- temp %>%
    bind_rows(temp[temp$group=='total',cols.num] -
                colSums(temp[temp$group %in% agegrp,cols.num]))
  temp[is.na(temp$group), 'group'] = 'under25'
  # create over85 group
  temp <- temp %>%
    bind_rows(colSums(
      temp[temp$group %in% c('85to94', 'over95'),cols.num]))
  temp[is.na(temp$group), 'group'] = 'over85'
  
  temp[is.na(temp$year), 'year'] = yr
  by.demo.final <- rbind(by.demo.final, temp)
}
by.demo <- by.demo.final

# add under25 column in the by.time data
for (suffix in c('', '.org', '.MA')) {
  by.time[paste0('count.under25', suffix)] <- by.time[paste0('total.enroll',suffix)]-(
    by.time[paste0('count.25to34',suffix)]+by.time[paste0('count.35to44',suffix)]+
      by.time[paste0('count.45to54',suffix)]+by.time[paste0('count.55to64',suffix)]+
      by.time[paste0('count.65to74',suffix)]+by.time[paste0('count.75to84',suffix)]+
      by.time[paste0('count.85to94',suffix)]+by.time[paste0('count.over95',suffix)])
  # update the share
  by.time[paste0('share.under25', suffix)] <- 100-(
    by.time[paste0('share.25to34',suffix)]+by.time[paste0('share.35to44',suffix)]+
      by.time[paste0('share.45to54',suffix)]+by.time[paste0('share.55to64',suffix)]+
      by.time[paste0('share.65to74',suffix)]+by.time[paste0('share.75to84',suffix)]+
      by.time[paste0('share.85to94',suffix)]+by.time[paste0('share.over95',suffix)])
  # by.time[paste0('share.under25', suffix)] <- by.time[paste0('count.under25', suffix)]/
  #   by.time[paste0('total.enroll',suffix)]*100
  
  drop.cols <- c(paste0('count.under18',suffix), paste0('count.18to24',suffix), 
                 paste0('share.under18',suffix), paste0('share.18to24',suffix))
  by.time <- by.time[,!names(by.time) %in% drop.cols]
  
  # create over85 age group
  by.time[paste0('count.over85', suffix)] <- by.time[paste0('count.85to94',suffix)]+
    by.time[paste0('count.over95',suffix)]
  by.time[paste0('share.over85', suffix)] <- by.time[paste0('share.85to94',suffix)]+
    by.time[paste0('share.over95',suffix)]
}

setwd("E:/Repositories/HealthcareSpending/MedicareEnrollment")
save(by.time, file = "by_time.rda")
save(by.demo, file = "by_demo.rda")
save(by.state, file = "by_state.rda")

# read the population data
pop <- read.csv('Z:/DatasetProcessor/Census/IntercensalEstimates/Interfaces/2021-04-05-17-29-yanhe8-04c6fdf/Intercensal_estimates.csv')
pop <- pop[pop$Year>=2008 & pop$AgeGroup>=0,]
pop['age.group'] <- 'under25'
for (age in seq(25,75,10)) {
  ub <- age+9
  pop[pop$AgeGroup %in% c(age,age+5), 'age.group'] <- paste0(age,'to',ub)
}
pop[pop$AgeGroup==85, 'age.group'] <- 'over85'
table(pop$age.group)
save(pop, file = "pop.rda")
