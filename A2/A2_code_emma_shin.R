#================================
#Load data
#================================

library(dplyr)
library(bayesm)
data(margarine)

choicePrice = as.data.frame(margarine$choicePrice)
demos = as.data.frame(margarine$demos)

#================================
#Exercise 1
#================================

#Mean and standard deviation of price by product choice
colMeans(choicePrice)
apply(choicePrice, 2, sd)

#Market share of each product
ms = data.frame(10, 1)
for (j in 1:10)
{
  ms[j] = sum(choicePrice$choice==j)/nrow(choicePrice)
}

#Market share by price bins: below average, above average
trueprice = mat.or.vec(nrow(choicePrice),3)
trueprice[, 1] = choicePrice$choice

for (i in 1:4470)
{
  trueprice[i, 2] = choicePrice[i, trueprice[i, 1]+2]
}

aveprice = mean(trueprice[,2])
aveprice
trueprice[, 3] = as.numeric(trueprice[, 2]>=aveprice)
colnames(trueprice) <- c("choice","price", "above_below")
trueprice <- as.data.frame(trueprice)
abovemkt <- sum(trueprice$above_below)
belowmkt <- nrow(trueprice)-abovemkt

msbybin = data.frame(10, 2)
colnames(msbybin) <- c("aboveaverage", "belowaverage")
for (j in 1:10)
{
  msbybin[j, 1] = sum(trueprice$choice==j&trueprice$above_below==1)/abovemkt
  msbybin[j, 2] = sum(trueprice$choice==j&trueprice$above_below==0)/belowmkt
}

#Mapping between observed attributes and choices
choicePrice_demos <- left_join(choicePrice, demos, by = "hhid")
choicePrice_demos3 <- choicePrice_demos %>%
  group_by(choice) %>%
  summarize(meanIncome = mean(Income),
            mean3_4 = mean(Fs3_4),
            meanFs5 = mean(Fs5.),
            meanFamSize = mean(Fam_Size),
            meancollege = mean(college),
            meanwhtcollar = mean(whtcollar),
            meanretired = mean(retired))

#================================
#Exercise 2
#================================

#We are interested in the effect of price on demand. Propose a model specification. Please include a constant!
  #Proposed model: conditional logit regressing choice on price

#================================
#Write the likelihood and optimize the model.
#================================

ni = nrow(choicePrice)
start    = runif(11,-10,10)

like_fun1 = function(param, choicePrice, ni)
{
  nj = length(unique(choicePrice[,2]))
  ut1 = mat.or.vec(ni,nj)

  #Create x_ij*B
  for (j in 1:nj)
  {
    ut1[,j] = param[j] + param[11]*choicePrice[,j+2]
  }
  
  #Create p_ij
  prob1   = exp(ut1)
  prob1   = sweep(prob1,MARGIN=1,FUN="/",STATS=rowSums(prob1))
  
  # match p_ij to actual choices
  probc1 = NULL
  for (i in 1:ni)
  {
    probc1[i] = prob1[i,choicePrice$choice[i]]
  }
  
  #Eliminate 0/1 values
  probc1[probc1>0.999999] = 0.999999
  probc1[probc1<0.000001] = 0.000001
  
  #Calculate likelihood
  like1 = sum(log(probc1))
  return(-like1)
}

#Optimize
res1      = optim(start,fn=like_fun1,method="BFGS",control=list(trace=6,maxit=1000), choicePrice=choicePrice, ni=ni)
res1$par

#Interpret the coefficient on price:
  #The coefficient on price is negative, meaning that an increase in price for a given product decreases the possibility of that product being chosen.

#================================
#Exercise 3
#================================

#We are interested in the effect of family income on demand. Propose a model specification. Please include a constant!
  #Proposed model: multinomial logit regressing choice on family income

#================================
#Write the likelihood and optimize the model.
#================================

start    = runif(19,-5,5)

like_fun2 = function(param, choicePrice_demos, ni)
{
  income = choicePrice_demos$Income
  nj = 10
  ut2 = mat.or.vec(ni,nj)
  pn1    = param[1:10]
  pn2    = param[11:19]
  
  ut2[, 1]= pn1[1]
  
  #Create x_i*B_j
  for (j in 2:10)
  {
    ut2[,j] = pn1[j]+income*pn2[j-1]
  }
  pn1
  #create p_ij
  prob2   = exp(ut2)
  prob2   = sweep(prob2,MARGIN=1,FUN="/",STATS=rowSums(prob2))
  
  # match p_ij to actual choices
  probc2 = NULL
  for (i in 1:ni)
  {
    probc2[i] = prob2[i,choicePrice_demos$choice[i]]
  }
  
  #Eliminate 0/1 values
  probc2[probc2>0.999999] = 0.999999
  probc2[probc2<0.000001] = 0.000001
  
  #Calculate likelihood
  like2 = sum(log(probc2))
  return(-like2)
}

#Optimization
res2      = optim(start,fn=like_fun2,method="BFGS",control=list(trace=6,maxit=1000), choicePrice_demos=choicePrice_demos, ni=ni)
res2$par

#Interpret the coefficient on family.
  #The coefficient on family is different for each product.
  #For example, the coefficient on product 2 is positive. This means that an increase in income is associated with an increased probability that a family will buy product 2 relative to product 1.
  #However, the coefficient on product 3 is negative. This means that an increase in income is associated with a decreased probability that a family will buy product 3 relative to product 1.


#================================
#Exercise 4
#================================

#Compute and interpret the marginal effects for the first and second models.

#================================
#Marginal effects for conditional logit:
#================================

#Calculate p_ij based on optimized parameters

utcl = mat.or.vec(ni,nj)

for (j in 1:nj)
{
  utcl[,j] = res1$par[j] + res1$par[11]*choicePrice[,j+2]
}
utcl1   = exp(utcl)           
pcl   = sweep(utcl1,MARGIN=1,FUN="/",STATS=rowSums(utcl1))

#Calculate p_ij(indicator - p_ik)B for each row

margcl = mat.or.vec(ni, nj*nj)

for (i in 1:ni)
{
  for (k in 1:nj)
  {
    for (j in 1:nj)
    {
      margcl[i, 10*(k-1)+j] = pcl[i, j]*(as.numeric(j==k)-pcl[i, k])*res1$par[11]
    }
  }  
}

#Take the average across rows to compute an average marginal effect of price k on choice j for all j/k combinations

margclave = mat.or.vec(1, nj*nj)
for (h in 1:nj^2)
{
  margclave[h] = mean(margcl[, h])
}

#Put average marginal effect into table format. k = rows and j = columns

margcltbl = mat.or.vec(10, 10)
for (j in 1:nj)
{
  margcltbl[j,1:10]=margclave[1, (10*(j-1)+1):(10*j)]
}

margcltbl

#================================
#Marginal effects for multinomial logit:
#================================

#Calculate p_ij based on optimized parameters

utml = mat.or.vec(ni,nj)

utml[, 1] = res2$par[1]

for (j in 2:nj)
{
  utml[,j] = res2$par[j] + res2$par[9+j]*choicePrice_demos$Income
}
utml1   = exp(utml)           
pml   = sweep(utml1,MARGIN=1,FUN="/",STATS=rowSums(utml1))

#Calculate p_ij(B_j - B_i) for each row

margml = mat.or.vec(ni, nj+1)

for (i in 1:ni)
{
  margml[i, 11] = as.vector(res2$par[11:19])%*%as.vector(pml[i, 2:10])
  for (j in 1:nj)
  {
    margml[i, j] = pml[i, j]*(res2$par[j]-margml[i, 11])
  }
}

#Take the average across rows to compute an average marginal effect of price k on choice j for all j/k combinations

margmlave = mat.or.vec(1, nj)
for (j in 1:nj)
{
  margmlave[j] = mean(margml[, j])
}

#================================
#Exercise 5
#================================

#In this section, we are interested in testing the properties of IIA. We consider the mixed logit setting.

#We are still interested in the effects of price and family income. 
#Write and optimized the likelihood of the mixed logit. Denote by beta_f the estimated coefficients.

#================================
#Mixed logit version 1:
#================================

ni = nrow(choicePrice)
start    = runif(20,-5,5)

like_fun3 = function(param, choicePrice_demos, ni)
{
  nj = length(unique(choicePrice_demos[,2]))
  ut3 = mat.or.vec(ni,nj)
  
  #Create x_ij*B+w_i*g_j
  
  ut3[, 1] = param[1] + param[11]*choicePrice_demos[, 3]
  
  for (j in 2:nj)
  {
    ut3[,j] = param[j] + param[11]*choicePrice_demos[, j+2] + param[10+j]*choicePrice_demos$Income
  } #param 1-10 = intercepts, param 11 = price coefficient, param 12-20 = income coefficients
  
  #Create p_ij
  prob3   = exp(ut3)
  prob3   = sweep(prob3,MARGIN=1,FUN="/",STATS=rowSums(prob3))
  
  # match p_ij to actual choices
  probc3 = NULL
  for (i in 1:ni)
  {
    probc3[i] = prob3[i,choicePrice_demos$choice[i]]
  }
  
  #Eliminate 0/1 values
  probc3[probc3>0.999999] = 0.999999
  probc3[probc3<0.000001] = 0.000001
  
  #Calculate likelihood
  like3 = sum(log(probc3))
  return(-like3)
}

#Optimize
res3      = optim(start,fn=like_fun3,method="BFGS",control=list(trace=6,maxit=1000), choicePrice_demos=choicePrice_demos, ni=ni)
res3$par

beta_f = res3$par
beta_f

#Consider an alternative specification, where we remove data from one choice.
#Estimate this model as well, and denote beta_r the estimated parameters.

#================================
#Mixed logit version 2 (excludes choice 10):
#================================

choicePrice_demos2 <- as.data.frame(choicePrice_demos %>% filter(choice < 10))

ni2 = nrow(choicePrice_demos2)
start    = runif(18,-5,5)
like_fun4 = function(param, choicePrice_demos2, ni)
{
  nj2 = length(unique(choicePrice_demos2[,2]))
  ut4 = mat.or.vec(ni2,nj2)
  
  #Create x_ij*B+w_i*g_j
  
  ut4[, 1] = param[1] + param[10]*choicePrice_demos2[, 3]
  
  for (j in 2:nj2)
  {
    ut4[,j] = param[j] + param[10]*choicePrice_demos2[, j+2] + param[9+j]*choicePrice_demos2$Income
  } #param 1-9 = intercepts, param 10 = price coefficient, param 11-18 = income coefficients
  
  #Create p_ij
  prob4   = exp(ut4)
  prob4   = sweep(prob4,MARGIN=1,FUN="/",STATS=rowSums(prob4))
  
  # match p_ij to actual choices
  probc4 = NULL
  for (i in 1:ni2)
  {
    probc4[i] = prob4[i,choicePrice_demos2$choice[i]]
  }
  
  #Eliminate 0/1 values
  probc4[probc4>0.999999] = 0.999999
  probc4[probc4<0.000001] = 0.000001
  
  #Calculate likelihood
  like4 = sum(log(probc4))
  return(-like4)
}

#Optimize
res4      = optim(start,fn=like_fun4,method="BFGS",control=list(trace=6,maxit=1000), choicePrice_demos2=choicePrice_demos2, ni=ni)
res4$par

beta_r = res4$par
beta_r

#================================
#Likelihood calculation for version 1:
#================================

ut5 = mat.or.vec(ni2,nj2)

#Create x_ij*B+w_i*g_j

ut5[, 1] = beta_f[1] + beta_f[10]*choicePrice_demos2[, 3]

for (j in 2:nj2)
{
  ut5[,j] = beta_f[j] + beta_f[10]*choicePrice_demos2[, j+2] + beta_f[9+j]*choicePrice_demos2$Income
}

#Create p_ij
prob5   = exp(ut5)
prob5   = sweep(prob5,MARGIN=1,FUN="/",STATS=rowSums(prob5))

# match p_ij to actual choices
probc5 = NULL
for (i in 1:ni2)
{
  probc5[i] = prob5[i,choicePrice_demos2$choice[i]]
}

#Eliminate 0/1 values
probc5[probc5>0.999999] = 0.999999
probc5[probc5<0.000001] = 0.000001

#Calculate likelihood
like5 = sum(log(probc5))
like5


#================================
#Likelihood calculation for version 2:
#================================

ut6 = mat.or.vec(ni2,nj2)

#Create x_ij*B+w_i*g_j

ut6[, 1] = beta_r[1] + beta_r[10]*choicePrice_demos2[, 3]

for (j in 2:nj2)
{
  ut6[,j] = beta_r[j] + beta_r[10]*choicePrice_demos2[, j+2] + beta_r[9+j]*choicePrice_demos2$Income
}

#Create p_ij
prob6   = exp(ut6)
prob6   = sweep(prob6,MARGIN=1,FUN="/",STATS=rowSums(prob6))

# match p_ij to actual choices
probc6 = NULL
for (i in 1:ni2)
{
  probc6[i] = prob6[i,choicePrice_demos2$choice[i]]
}

#Eliminate 0/1 values
probc6[probc6>0.999999] = 0.999999
probc6[probc6<0.000001] = 0.000001

#Calculate likelihood
like6 = sum(log(probc6))
like6

#================================
#Test statistic calculation:
#================================

mmt = -2*(like5-like6)
mmt

#The test statistic I calculate is 275.0719. Based on the Chi-square distribution, this is significant and IIA is violated.