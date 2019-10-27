# This is a project studying how does the age of a company at its year of IPO affects its yearly volatility of stock returns.
# Author: Zhuoran Dai

# Import libraries needed
library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table)
library(plm)

# Set working directory
setwd("~/Age")

# Read and sort the dataset using PERMCO and date
crsp <- fread("crsp_permno.csv")
# This csv file contains daily holding period return for each stock companies issued (identify by PERMNO) from CRSP.
# There are three variables, PERMNO, date, and RET.

# Drop those obs with unavailable returns
crsp <- crsp %>% arrange(PERMNO,date) %>%
  filter(RET!="C",RET!="B",RET!="")

# Format the variable class in the dataset to facilitate further analysis, and construct the year variable
crsp$year <- year(ymd(crsp$date))
crsp$RET <- as.double(crsp$RET)

# Calculate the volatility of returns of one year after IPO for each PERMCO
# Below is a function calculating the yearly volatility of returns using 253 trading days
volatility_1 <- function(x){
  ret <- x$RET
  SD=sd(ret[1:253])
  return(as.data.frame(SD)) 
}

vol_1 <- crsp %>% group_by(PERMNO) %>%
  do(volatility_1(.)) %>%
  ungroup()

# Read and merge the data set with the information about found date. This data is drawn from Jay Ritter's website
# on IPO data. There are three variables, Offer Date, PERMNO, and Found.
ipo <- fread("IPO.csv")
ipo$PERMNO <- as.integer(ipo$PERMNO)

# Merge the above two data frames and calculate the age of each firm
vol_ipo <- inner_join(vol_1,ipo, by="PERMNO")
vol_ipo <- vol_ipo %>% mutate(year=year(ymd(`Offer Date`)),age=year-Found) %>%
  select(-`Offer Date`) %>%
  na.omit() %>%
  distinct(PERMNO, .keep_all = T)
# Draw a graph showing the relationship between age and volatility
ggplot(vol_ipo,aes(age,SD))+geom_point(colour="red",size=1)


# Store the result
fwrite(vol_ipo,"vol_age.csv")


# Calculate the mean of volatility by age
mean_age <- vol_ipo %>% group_by(age) %>%
  summarise(sd=mean(SD)) %>%
  ungroup()
# Draw a graph to show the relationship between age and average volatility: there is negative relationship!
ggplot(mean_age,aes(age,sd))+geom_point(colour="red",size=2.5)+geom_line(size=1)
# Run a regression to further test the relationship
lm_age <- lm(sd ~ age, data = mean_age)
# The slop coefficient is significant!


# Calculate the mean of volatility by year
mean_year <- vol_ipo %>% group_by(year) %>%
  summarise(sd=mean(SD)) %>%
  ungroup()
# Draw a graph to show the relationship between year and average volatility
ggplot(mean_year,aes(year,sd))+geom_point(colour="red",size=2.5)+geom_line(size=1)


# Calculate the mean of age in each year
age_year <- vol_ipo %>% group_by(year) %>%
  summarise(av_age=mean(age)) %>%
  ungroup()
# Draw a graph
ggplot(age_year,aes(year,av_age))+geom_point(colour="red",size=2.5)+geom_line(size=1)


# calculate the number of ipo in each year
num_ipo <- count(vol_ipo,year)
# Draw a graph
ggplot(num_ipo,aes(year,n))+geom_point(colour="red",size=2.5)+geom_line(size=1)



# Next, I calculate the yearly volatility for each PERMNO and for each year available
crsp$date_posix <- ymd(crsp$date)

volatility_y <- function(x){
  firstdate <- first(x,1)$date_posix
  lastdate <- last(x,1)$date_posix
  year_avilable <- as.period(interval(firstdate,lastdate))$year
  i <- 1
  SD_year <- data.frame(volatility=rep(0,year_avilable),age=rep(0,year_avilable))
  while (i < year_avilable){
    start <- (i-1)*253
    end <- i*253
    ret <- x$RET[start:end]
    SD <- sd(ret)
    SD_year$volatility[i] <- SD
    SD_year$age[i] <- i
    i <- i+1
  }
  return(SD_year)
}

vol_y <- crsp %>% group_by(PERMNO) %>%
  do(volatility_y(.)) %>%
  na.omit() %>%
  filter(age!=0)
  ungroup()

# Output the data frame
fwrite(vol_y,"age_vol.csv")

# Draw a figure the show the relationship between volatility and age
ggplot(vol_y,aes(age,volatility))+geom_point(colour="red",size=1)



# I further exclude the influence from the market to calculate the idiosyncratic volatility
# I use the fama-french 3 factors model, and the data was drawn from Kenneth French's website.
ff3 <- fread("F-F_Research_Data_Factors_daily.csv")

# Merge the two datasets
crsp_ff3 <- inner_join(crsp,ff3, by="date")
crsp_ff3 <- crsp_ff3 %>% rename(MKT=Mkt-RF) %>%
  distinct(PERMNO,date, .keep_all= T)

# Run the regression to get the residuals
plm_reg <- plm(RET ~ MKT+SMB+HML,data = crsp_ff3, index = c("PERMNO","date"))
crsp_ff3$residuals <- residuals(plm_reg)
remove(plm_reg)

# Calculate the idiosyncratic volatiliy
volatility_1_i <- function(x){
  ret <- x$residuals
  SD_i=sd(ret[1:253])
  return(as.data.frame(SD_i)) 
}

vol_1_i <- crsp_ff3 %>% group_by(PERMNO) %>%
  do(volatility_1_i(.)) %>%
  ungroup()

# The the rest of the analysis for idiosyncratic volatility can be reproduced just as the one above
