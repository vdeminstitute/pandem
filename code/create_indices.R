#This script creates the pandem indices from the raw pandem indicators (raw_data_V6.xlsx) and the v2x_libdem variable from the V-Dem dataset (VDem-v11.1.csv)
rm(list=ls())
library(foreign)
library(openxlsx)
library(dplyr)

#Load raw data
#setwd()
dat <- read.xlsx('datasets/raw_data_V6.xlsx')

#Order data
dat$quarter <- factor(dat$quarter, levels = c("Q1", "Q2", "Q3", "Q4"))
dat <- dat[order(dat$country_name, dat$year, dat$quarter),]

#Define functions to create indices
assign_type1 <- function(x){
  type <- ifelse(x[['discrim']] == 0, 0, NA)
  type <- ifelse(x[['discrim']] == 1, 1, type)
  type <- ifelse(x[['discrim']] == 2, 2, type)
  type <- ifelse(x[['discrim']] == 3, 3, type)
  return(type)
}

assign_type2 <- function(x){
  type <- ifelse(x['ndrights'] == 0, 0, NA)
  type <- ifelse(x['ndrights'] == 1, 3, type)
  return(type)
}

assign_type3 <- function(x){
  type <- ifelse(x[['pomviol']] == 0, 0, NA)
  type <- ifelse(x[['pomviol']] == 1, 1, type)
  type <- ifelse(x[['pomviol']] == 2, 2, type)
  type <- ifelse(x[['pomviol']] == 3, 3, type)
  return(type)
}

assign_type4 <- function(x, timeperiod, year){
  if (timeperiod == 'Q2' & year == 2020) {tmax <- as.Date('2020-07-01')}
  if (timeperiod == 'Q3' & year == 2020) {tmax <- as.Date('2020-10-01')}
  if (timeperiod == 'Q4' & year == 2020) {tmax <- as.Date('2021-01-01')}
  if (timeperiod == 'Q1' & year == 2021) {tmax <- as.Date('2021-04-01')}
  if (timeperiod == 'Q2' & year == 2021) {tmax <- as.Date('2021-07-01')}
  ended <- ifelse(!is.na(x['emend']) & (as.Date(x[['emend']]) < tmax), T, F)
  
  type <- ifelse(x[['emlimit']] %in% 1 | is.na(x[['emlimit']]), 0, NA)
  type <- ifelse(x[['emlimit']] %in% 0 & ended, 1, type)
  type <- ifelse(x[['emlimit']] %in% 0 & !ended, 2, type)
  return(type)
}

assign_type5 <- function(x){
  type <- ifelse(x[['leglimit']] <= 2, 0, NA)
  type <- ifelse(x[['leglimit']] == 3, 2, type)
  type <- ifelse(x[['leglimit']] >= 4, 3, type)
  return(type)
}

assign_type6 <- function(x){
  type <- ifelse(x[['govdis']] == 0, 0, NA)
  type <- ifelse(x[['govdis']] == 1, 1, type)
  type <- ifelse(x[['govdis']] == 2, 2, type)
  type <- ifelse(x[['govdis']] == 3, 3, type)
  return(type)
}

assign_type7 <- function(x){
  melim <- x[['merepfact']]
  merepfact <- x[['merepfact']]
  merepgov <- x[['merepgov']]
  merepoth <- x[['merepoth']]
  mevhar <- x[['mevhar']]
  mephar <- x[['mephar']]
  meinf <- x[['meinf']]
  
  d.temp <- data.frame(melim, merepfact, merepgov, merepoth, mevhar, mephar, meinf)
  type <- apply(d.temp, 1, function(x) ifelse(sum(x)==0, 0, NA))
  type <- ifelse(((melim == 1 | merepfact==1 | meinf == 1) & (merepgov == 0 & merepoth == 0 & mevhar == 0 & mephar == 0 )),
                 1, type)
  type <- ifelse(((melim <= 1 & merepgov==0 & merepoth == 0 & mephar == 0) & (meinf == 2  | mevhar == 1)),
                 2, type)
  type <- ifelse(melim >= 2 | merepgov== 1 | merepoth == 1 | mephar == 1,
                 3, type)
  return(type)
}

######################################################################
#Calculate indices
######################################################################
#Type1: Discrimination
t1_q2_y20 <- assign_type1(subset(dat, quarter == 'Q2' & year == 2020))
t1_q3_y20 <- assign_type1(subset(dat, quarter == 'Q3' & year == 2020))
t1_q4_y20 <- assign_type1(subset(dat, quarter == 'Q4' & year == 2020))
t1_q1_y21 <- assign_type1(subset(dat, quarter == 'Q1' & year == 2021))
t1_q2_y21 <- assign_type1(subset(dat, quarter == 'Q2' & year == 2021))
t1_max <- apply(cbind(t1_q2_y20, t1_q3_y20, t1_q4_y20, t1_q1_y21, t1_q2_y21), 1, max)

######################################################################
#Type2: Non-Derogable Rights
t2_q2_y20 <- assign_type2(subset(dat, quarter == 'Q2' & year == 2020))
t2_q3_y20 <- assign_type2(subset(dat, quarter == 'Q3' & year == 2020))
t2_q4_y20 <- assign_type2(subset(dat, quarter == 'Q4' & year == 2020))
t2_q1_y21 <- assign_type2(subset(dat, quarter == 'Q1' & year == 2021))
t2_q2_y21 <- assign_type2(subset(dat, quarter == 'Q2' & year == 2021))
t2_max <- apply(cbind(t2_q2_y20, t2_q3_y20, t2_q4_y20, t2_q1_y21, t2_q2_y21), 1, max)

######################################################################
#Type3: Police Violence
t3_q2_y20 <- assign_type3(subset(dat, quarter == 'Q2' & year == 2020))
t3_q3_y20 <- assign_type3(subset(dat, quarter == 'Q3' & year == 2020))
t3_q4_y20 <- assign_type3(subset(dat, quarter == 'Q4' & year == 2020))
t3_q1_y21 <- assign_type3(subset(dat, quarter == 'Q1' & year == 2021))
t3_q2_y21 <- assign_type3(subset(dat, quarter == 'Q2' & year == 2021))
t3_max <- apply(cbind(t3_q2_y20, t3_q3_y20, t3_q4_y20, t3_q1_y21, t3_q2_y21), 1, max)

######################################################################
#Type4: Time Limit
t4_q2_y20 <- assign_type4(subset(dat, quarter == 'Q2' & year == 2020), timeperiod = 'Q2', year = 2020)
t4_q3_y20 <- assign_type4(subset(dat, quarter == 'Q3' & year == 2020), timeperiod = 'Q3', year = 2020)
t4_q4_y20 <- assign_type4(subset(dat, quarter == 'Q4' & year == 2020), timeperiod = 'Q4', year = 2020)
t4_q1_y21 <- assign_type4(subset(dat, quarter == 'Q1' & year == 2021), timeperiod = 'Q1', year = 2021)
t4_q2_y21 <- assign_type4(subset(dat, quarter == 'Q2' & year == 2021), timeperiod = 'Q2', year = 2021)
t4_max <- apply(cbind(t4_q2_y20, t4_q3_y20, t4_q4_y20, t4_q1_y21, t4_q2_y21), 1, max)

######################################################################
#Type5: Limits on the Legislature
t5_q2_y20 <- assign_type5(subset(dat, quarter == 'Q2' & year == 2020))
t5_q3_y20 <- assign_type5(subset(dat, quarter == 'Q3' & year == 2020))
t5_q4_y20 <- assign_type5(subset(dat, quarter == 'Q4' & year == 2020))
t5_q1_y21 <- assign_type5(subset(dat, quarter == 'Q1' & year == 2021))
t5_q2_y21 <- assign_type5(subset(dat, quarter == 'Q2' & year == 2021))
t5_max <- apply(cbind(t5_q2_y20, t5_q3_y20, t5_q4_y20, t5_q1_y21, t5_q2_y21), 1, max)

######################################################################
#Type6: Disinformation
t6_q2_y20 <- assign_type6(subset(dat, quarter == 'Q2' & year == 2020))
t6_q3_y20 <- assign_type6(subset(dat, quarter == 'Q3' & year == 2020))
t6_q4_y20 <- assign_type6(subset(dat, quarter == 'Q4' & year == 2020))
t6_q1_y21 <- assign_type6(subset(dat, quarter == 'Q1' & year == 2021))
t6_q2_y21 <- assign_type6(subset(dat, quarter == 'Q2' & year == 2021))
t6_max <- apply(cbind(t6_q2_y20, t6_q3_y20, t6_q4_y20, t6_q1_y21, t6_q2_y21), 1, max)

######################################################################
#Type7: Media Restrictions
t7_q2_y20 <- assign_type7(subset(dat, quarter == 'Q2' & year == 2020))
t7_q3_y20 <- assign_type7(subset(dat, quarter == 'Q3' & year == 2020))
t7_q4_y20 <- assign_type7(subset(dat, quarter == 'Q4' & year == 2020))
t7_q1_y21 <- assign_type7(subset(dat, quarter == 'Q1' & year == 2021))
t7_q2_y21 <- assign_type7(subset(dat, quarter == 'Q2' & year == 2021))
t7_max <- apply(cbind(t7_q2_y20, t7_q3_y20, t7_q4_y20, t7_q1_y21, t7_q2_y21), 1, max)

######################################################################
#Create Time-Series Dataset
######################################################################
n <- length(unique(dat$country_name))
year_var <- c(rep(2020, n), rep(2020, n), rep(2020, n), rep(2021, n), rep(2021, n)) 
quarter_var <- c(rep('Q2', n), rep('Q3', n), rep('Q4', n), rep('Q1', n), rep('Q2', n))
country_name <- rep(unique(dat$country_name), 5)
orderTimes <- rep(1:5, n)

dat.indices <- data.frame(
  year = year_var,
  quarter = quarter_var, 
  country_name=country_name, 
  type1 = c(t1_q2_y20, t1_q3_y20, t1_q4_y20, t1_q1_y21, t1_q2_y21),
  type2 = c(t2_q2_y20, t2_q3_y20, t2_q4_y20, t2_q1_y21, t2_q2_y21),
  type3 = c(t3_q2_y20, t3_q3_y20, t3_q4_y20, t3_q1_y21, t3_q2_y21),
  type4 = c(t4_q2_y20, t4_q3_y20, t4_q4_y20, t4_q1_y21, t4_q2_y21),
  type5 = c(t5_q2_y20, t5_q3_y20, t5_q4_y20, t5_q1_y21, t5_q2_y21),
  type6 = c(t6_q2_y20, t6_q3_y20, t6_q4_y20, t6_q1_y21, t6_q2_y21),
  type7 = c(t7_q2_y20, t7_q3_y20, t7_q4_y20, t7_q1_y21, t7_q2_y21)
)
dat.indices <- dat.indices[order(dat.indices$country_name, dat.indices$year, factor(dat.indices$quarter, levels = c("Q1", "Q2", "Q3", "Q4"))), ]

#Merge with V-Dem data (V11.1)
vdem <- read.csv('datasets/VDem-v11.1.csv')
dat.indices <- merge(dat.indices, vdem[vdem$year==2019, c('country_name', 'v2x_libdem')], all.x = T)
dat.indices$v2x_libdem_2019 <- dat.indices$v2x_libdem
dat.indices <- dat.indices[, names(dat.indices)[!(names(dat.indices) %in% 'v2x_libdem')]]

#Calculate PanDem
dat.indices$pandem <- apply(dat.indices[, paste('type', 1:7, sep = '')], 1, function(x) sum(x)/20)

#Calculate PanBack
dat.indices$panback <- 4*dat.indices$pandem*dat.indices$v2x_libdem*(1-dat.indices$v2x_libdem)

#Merge indices data with indicator data
dat.indices.indicators <- left_join(dat.indices, dat, by = c('country_name', 'year', 'quarter'))

#Reorder columns
dat.indices.indicators <- dat.indices.indicators %>% 
  relocate(year) %>% 
  relocate(quarter, .after = year)  %>% 
  relocate(time, .after = quarter)  %>% 
  relocate(country_name, .after = time)  %>% 
  relocate(v2x_libdem_2019, .after = country_name) %>% 
  relocate(pandem, .after = v2x_libdem_2019) %>% 
  relocate(panback, .after = pandem) 

#Save
write.xlsx(dat.indices.indicators, 'datasets/pandem_TS_v6.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE, headerStyle = createStyle(textDecoration = "Bold"))

######################################################################
#Create Cross-Section Dataset
######################################################################
dat.cs <- dat.indices.indicators %>% 
  dplyr::group_by(country_name) %>% 
  dplyr::summarise(time = first(time),
                    pandem = max(pandem), 
                   panback = mean(panback),
                   v2x_libdem_2019 = first(v2x_libdem_2019)
  )

dat.cs$type1 <- t1_max
dat.cs$type2 <- t2_max
dat.cs$type3 <- t3_max
dat.cs$type4 <- t4_max
dat.cs$type5 <- t5_max
dat.cs$type6 <- t6_max
dat.cs$type7 <- t7_max

dat.cs <- dat.cs %>% 
  relocate(time) %>% 
  relocate(v2x_libdem_2019, .after = country_name)

#Save
write.xlsx(dat.cs, 'datasets/pandem_CS_v6.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE, headerStyle = createStyle(textDecoration = "Bold"))

