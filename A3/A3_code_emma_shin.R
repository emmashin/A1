#================================
#Exercise 1
#================================

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(fastDummies)
library(fixest)
setwd("~/Duke/Spring 2021/ECON 613/A3")
crime <- read.csv(file = 'crime_long.csv', header = TRUE)
officers <- read.csv(file = 'officers.csv', header = TRUE)
pop <- read.csv(file = 'population.csv', header = TRUE)
colnames(crime)
colnames(officers)
colnames(pop)

#================================
#Exercise 2
#================================

#Total crime per month and plot:
crime2 <- crime %>%
  group_by(crime_month) %>%
  summarise(month_total = sum(crimes))

ggplot(data = crime2, aes(x = crime_month, y = month_total, group=1))+geom_line()

#Merged dataset:
crime3 <-crime %>%
  group_by(crime_month, district, crime_type) %>%
  summarise(crime_count = sum(crimes))
  
crimepop <- left_join(crime3, pop, by = c("crime_month" = "month", "district" = "district"))

#Turn data from long to wide:
crimepop_wide <- spread(crimepop, crime_type, crime_count)

#Add variables:

crimepop_wide <- crimepop_wide %>% 
  mutate(total = sum(drug, other, property, violent))

crimepop_wide <- crimepop_wide %>%
  mutate(totalpr = total/tot_pop,
         violentpr = violent/tot_pop,
         propertypr = property/tot_pop,
         share_white = tot_white/tot_pop,
         share_black = tot_black/tot_pop,
         share_hisp = tot_hisp/tot_pop
        )

#================================
#Exercise 3
#================================

#Pull relevant variables from merged data
cpw <- subset(crimepop_wide, select = c("crime_month", "district", "total", "p50_inc", "share_white", "share_black", "share_hisp"))
off_crime <- left_join(officers, cpw, by = c("month" = "crime_month", "unit" = "district"))

#Run regression
basic <- lm(arrest ~ tenure + total + p50_inc + share_white + share_black + share_hisp, data = off_crime)
summary(basic)

#================================
#Exercise 4
#================================

#Create year and month variables
off_crime <- off_crime %>%
  mutate(year = substr(month,1, 4),
         month = substr(month, 6, 7)
         )

#Create indicator columns for fixed effects
off_crime1 <- dummy_cols(off_crime, select_columns = c("unit", "month", "year"), remove_selected_columns = TRUE)

#Run regression
fixed1 <- lm(arrest ~ . - NUID, data = off_crime1)
summary(fixed1)

#================================
#Exercise 5
#================================

#Original model
fixed2 <- feols(arrest ~ tenure + total + p50_inc + share_white + share_black + share_hisp |unit + month + year + NUID, data = off_crime)
summary(fixed2)
fixedeffects = fixef(fixed2)
print(fixedeffects$unit)
print(fixedeffects$month)
print(fixedeffects$year)

#Between estimator

#Create mean of variables by individual across time
off_crime2 <- off_crime %>%
  group_by(NUID) %>%
  summarise(m_arrest = mean(arrest),
            m_tenure = mean(tenure),
            m_total = mean(total),
            m_p50_inc = mean(p50_inc),
            m_s_white = mean(share_white),
            m_s_black = mean(share_black),
            m_s_hisp = mean(share_hisp)
            )

#Run between estimator
betweenest <- lm(m_arrest ~ m_tenure + m_total + m_p50_inc + m_s_white + m_s_black + m_s_hisp, data = off_crime2)
summary(betweenest)

#Within estimator

#Create demeaned variables
off_crime3 = left_join(off_crime, off_crime2, by = "NUID")
off_crime3 <- off_crime3 %>%
  mutate(dm_arrest = arrest - m_arrest,
         dm_tenure = tenure - m_tenure,
         dm_total = total - m_total,
         dm_p50_inc = p50_inc - m_p50_inc,
         dm_s_white = share_white - m_s_white,
         dm_s_black = share_black - m_s_black,
         dm_s_hisp = share_hisp - m_s_hisp,
         )

#Check to make sure demeaned values make sense
sum(off_crime3$dm_total)

#Run within estimator
withinest <- lm(dm_arrest ~ dm_tenure + dm_total + dm_p50_inc + dm_s_white + dm_s_black + dm_s_hisp, data = off_crime3)
summary(withinest)

#First differences estimator

#Create a time variable that increases by 1 every month
off_crime4 <- off_crime %>%
  mutate(month = as.numeric(month),
  year = as.numeric(year)
  ) %>%
  mutate(t = month + (year - 2007)*12)

#Check to make sure time variable makes sense
min(off_crime4$t)
max(off_crime4$t)
max(off_crime$year)

#Make sure rows are in correct order
off_crime4 %>% arrange(NUID, t)

#Create lagged variables by individual officer
off_crime4 <- off_crime4 %>%
  group_by(NUID) %>%
  mutate(t_tenure = dplyr::lag(tenure, n = 1, default = NA),
         t_arrest = dplyr::lag(arrest, n = 1, default = NA),
         t_total = dplyr::lag(total, n = 1, default = NA),
         t_p50_inc = dplyr::lag(p50_inc, n = 1, default = NA),
         t_white = dplyr::lag(share_white, n = 1, default = NA),
         t_black = dplyr::lag(share_black, n = 1, default = NA),
         t_hisp = dplyr::lag(share_hisp, n = 1, default = NA)
         )

#Create first differences variables
off_crime4 <- off_crime4 %>%
  mutate(fd_tenure = tenure - t_tenure,
         fd_arrest = arrest - t_arrest,
         fd_total = total - t_total,
         fd_p50_inc = p50_inc - t_p50_inc,
         fd_white = share_white - t_white,
         fd_black = share_black - t_black,
         fd_hisp = share_hisp - t_hisp
         )

#Run first differences estimator
fdest <- lm(fd_arrest ~ fd_tenure + fd_total + fd_p50_inc + fd_white + fd_black + fd_hisp, data = off_crime4)
summary(fdest)

#GMM approach

off_crime5 = na.omit(off_crime)
off_crime5$month = as.integer(off_crime5$month)
off_crime5$year = as.integer(off_crime5$year)
x = data.matrix(off_crime5[, c("tenure", "total", "p50_inc", "share_white", "share_black", "share_hisp")])
y = data.matrix(off_crime5$arrest)

gmm_fe = function(param, off_crime5, x, y)
{
  #create x matrix not including fixed effects

  #calculate epsilon: off_crime$arrest = y, x*param[1:6] = x*beta, and the other terms subtract out year, month, unit, and individual fixed effects
  eps = y - x%*%param[1:6] - param[6+off_crime5$year - 2006] - param[11+off_crime5$month] - param[23+off_crime5$unit] - param[48 + off_crime5$NUID]
  #calculate the term to go into the likelihood: x'epsilon
  mom = t(x)%*%eps
  #calculate the likelihood
  like = (t(mom)%*%mom)
  return(like);
}

#note: this code takes way too long to run... because of the huge number of individual fixed effects
start = runif(33637, 0, 1) 
param = start
res  = optim(start,fn=gmm_fe,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),off_crime5 = off_crime5, x=x, y=y)

#I wrote the above code to demonstrate understanding of GMM with fixed effects. But since it wouldn't run I wrote another GMM code excluding fixed effects.

off_crime6 = na.omit(off_crime)
sub = subset(off_crime6, select = -c(NUID, month, unit, arrest, year))
x2 = cbind(data.matrix(sub), c(1))
y2 = data.matrix(off_crime6$arrest)

gmm_nfe = function(param, x2, y2)
{
  #calculate epsilon
  eps2 = off_crime6$arrest - x2%*%param
  #calculate the term to go into the likelihood: x'epsilon
  mom2 = t(x2)%*%eps2
  #calculate the likelihood
  like2 = (t(mom2)%*%mom2)
  return(like2);
}

#note: to make this work without taking forever I had to start with values similar to what I should end up with
start = list(-0.000004, 0.0000002, .00000001, -0.01, -0.01, -0.005, 0.5)
res  = optim(start,fn=gmm_nfe,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x2 = x2, y2 = y2)
res$par