# For Prof. Yuan on the Age project
# Author: Zhuoran Dai
# Date: Oct 1st, 2019

# Import libraries needed
library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table)

# Set working directory
setwd("C:/Users/dzrna/Desktop/Yuan Zou/Age")

# Read and sort the dataset using PERMCO and date
crsp <- fread("crsp_permno.csv")
crsp <- crsp %>% arrange(PERMNO,date) %>%
  filter(RET!="C",RET!="B",RET!="")
# Question here: what does the B, C and the NA mean?

# Format the variable class in the dataset to facilitate further analysis, and construct the year variable
crsp$date_y <- year(ymd(crsp$date))
crsp$RET <- as.double(crsp$RET)

# Calculate the volatility of returns of one year after IPO for each PERMCO
volatility_1 <- function(x){
  ret <- x$RET
  SD=sd(ret[1:253])
  return(as.data.frame(SD)) 
}

vol_1 <- crsp %>% group_by(PERMNO) %>%
  do(volatility_1(.)) %>%
  ungroup()

# Read and merge the data set with the information about found date
ipo <- fread("IPO.csv")
ipo$PERMNO <- as.integer(ipo$PERMNO)

# Merge the above two data frames and calculate the age of each firm
vol_ipo <- merge(vol_1,ipo)
# Multiple PERMCO in IPO?
vol_ipo$year <- year(ymd(vol_ipo$`Offer Date`))
vol_ipo$age <- vol_ipo$year-vol_ipo$Found

# Remove rows with missing values
vol_ipo <- vol_ipo[complete.cases(vol_ipo),]
vol_ipo <- distinct(vol_ipo,PERMNO,.keep_all = T)

# Draw a graph showing the relationship between age and volatility
ggplot(vol_ipo,aes(age,SD))+geom_point(colour="red",size=1)

# Store the result
fwrite(vol_ipo,"vol_age.csv")

# Calculate the mean of volatility by age
mean_sd <- by(vol_ipo$SD,vol_ipo$age,mean)

# Collect the result into a data frame
age_mean_sd <- as.integer(names(as.list(mean_sd)))
val_mean_sd <- as.vector(mean_sd)
df1 <- data.frame(age=age_mean_sd,sd=val_mean_sd)
df1 <- df1[complete.cases(df1),]

# Draw a picture for the data frame
ggplot(df1,aes(age,sd))+geom_point(colour="red",size=2.5)+geom_line(size=1)


# Calculate the mean of volatility by year
mean_sd_year <- by(vol_ipo$SD,vol_ipo$year,mean)

year_mean_sd <- as.integer(names(as.list(mean_sd_year)))
val_mean_sd <- as.vector(mean_sd_year)
df4 <- data.frame(year=year_mean_sd,sd=val_mean_sd)
df4 <- df4[complete.cases(df4),]
ggplot(df4,aes(year,sd))+geom_point(colour="red",size=2.5)+geom_line(size=1)

# Calculate the mean of age in each year
mean_age <- by(vol_ipo$age,vol_ipo$year,mean)

year_mean <- as.integer(names(as.list(mean_age)))
val_mean <- as.vector(mean_age)
df2 <- data.frame(year=year_mean,age=val_mean)
df2 <- df2[complete.cases(df2),]

ggplot(df2,aes(year,age))+geom_point(colour="red",size=2.5)+geom_line(size=1)


# calculate the number of ipo in each year
df3 <- count(vol_ipo,year)
ggplot(df3,aes(year,n))+geom_point(colour="red",size=2.5)+geom_line(size=1)

# An altenative interpretation of the age and volatility

# First calculate the age of firms

# First get the date of the latest available data, then calculate how many years
# of volatility can be calculated. Second, for each year calculate the volatility
crsp$date_posix <- ymd(crsp$date)

volatility_y <- function(x){
  firstdate <- first(x,1)$date_posix
  lastdate <- last(x,1)$date_posix
  year_avilable <- as.period(interval(firstdate,lastdate))$year
  i <- 1
  SD_year <- data.frame(volatility=rep(0,year_avilable),age=rep(0,year_avilable))
  while (i < year_avilable){
    start <- (i-1)*365
    end <- i*365
    ret <- x$RET[start:end]
    SD <- sd(ret)
    SD_year$volatility[i] <- SD
    SD_year$age[i] <- i
    i <- i+1
  }
  return(SD_year)
}

vol_y <- crsp %>% group_by(PERMCO) %>%
  do(volatility_y(.)) %>%
  ungroup()

# Drop the rows with NA
vol_y <- vol_y[complete.cases(vol_y),]
vol_y <- filter(vol_y,age!=0)

# Output the data frame
fwrite(vol_y,"age_vol.csv")

# Draw a figure the show the relationship between volatility and age
ggplot(vol_y,aes(age,volatility))+geom_point(colour="red",size=1)