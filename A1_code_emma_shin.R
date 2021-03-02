#================================
#Load data
#================================

rm(list=ls())
library(dplyr)
library(matlib)
library(xtable)
setwd("~/Duke/Spring 2021/ECON 613/Assignment 1/Econ613-master/Assignments/A1/dat")
datstu <- read.csv(file = 'datstu.csv', header = TRUE)
datsss <- read.csv(file = 'datsss.csv', header = TRUE)
datjss <- read.csv(file = 'datjss.csv', header = TRUE)
colnames(datstu)
colnames(datsss)
colnames(datjss)
names(datstu)[names(datstu) == "X"] <- "studentid"

#================================
#Exercise 1
#================================

nrow(datstu) 
#number of students: 340,823

length(unique(datsss$schoolcode)) 
#number of schools: 898

#Find unique school-program combinations

schpgm1 <- datstu[, c("studentid", "schoolcode1", "choicepgm1")]
names(schpgm1) <- c("studentid", "schoolcode", "choicepgm")
schpgm2 <- datstu[, c("studentid", "schoolcode2", "choicepgm2")]
names(schpgm2) <- c("studentid", "schoolcode", "choicepgm")
schpgm3 <- datstu[, c("studentid", "schoolcode3", "choicepgm3")]
names(schpgm3) <- c("studentid", "schoolcode", "choicepgm")
schpgm4 <- datstu[, c("studentid", "schoolcode4", "choicepgm4")]
names(schpgm4) <- c("studentid", "schoolcode", "choicepgm")
schpgm5 <- datstu[, c("studentid", "schoolcode5", "choicepgm5")]
names(schpgm5) <- c("studentid", "schoolcode", "choicepgm")
schpgm6 <- datstu[, c("studentid", "schoolcode6", "choicepgm6")]
names(schpgm6) <- c("studentid", "schoolcode", "choicepgm")

schpgmid <- do.call("rbind", list(schpgm1, schpgm2, schpgm3, schpgm4, schpgm5, schpgm6))
nrow(schpgmid)
pgm <- schpgmid[, c("choicepgm")]
sum(!duplicated(pgm)) 
#number of programs: 33

schpgm <- schpgmid[, c("schoolcode", "choicepgm")]
sum(!duplicated(schpgm)) 
#number of school-program choices: 3,086

sum(is.na(datstu[, c("score")])) 
#number of missing scores: 179,887

#Find number of students who applied to multiple programs in the same school
datstu$sameschool = as.numeric(datstu$schoolcode1==datstu$schoolcode2)
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode1==datstu$schoolcode3))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode1==datstu$schoolcode4))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode1==datstu$schoolcode5))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode1==datstu$schoolcode6))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode2==datstu$schoolcode3))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode2==datstu$schoolcode4))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode2==datstu$schoolcode5))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode2==datstu$schoolcode6))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode3==datstu$schoolcode4))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode3==datstu$schoolcode5))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode3==datstu$schoolcode6))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode4==datstu$schoolcode5))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode4==datstu$schoolcode6))
datstu$sameschool = as.numeric((datstu["sameschool"]==1)|(datstu$schoolcode5==datstu$schoolcode6))
datstu$sameschool[is.na(datstu$sameschool)] <-0
sum(datstu$sameschool) 
#number of students who applied to multiple programs in the same school: 120,071

#Find number of students who applied to fewer than six programs
datstu$lessthan6 = as.numeric(is.na(datstu$schoolcode1))
datstu$lessthan6 = as.numeric((datstu$lessthan6==1)|(is.na(datstu$schoolcode2)))
datstu$lessthan6 = as.numeric((datstu$lessthan6==1)|(is.na(datstu$schoolcode3)))
datstu$lessthan6 = as.numeric((datstu$lessthan6==1)|(is.na(datstu$schoolcode4)))
datstu$lessthan6 = as.numeric((datstu$lessthan6==1)|(is.na(datstu$schoolcode5)))
datstu$lessthan6 = as.numeric((datstu$lessthan6==1)|(is.na(datstu$schoolcode6)))
sum(datstu$lessthan6) 
#students who applied to fewer than six programs: 17,734

#================================
#Exercise 2
#================================

#Create dataset with unique row for each schoolcode
datss1 <- datsss[, c("schoolcode", "sssdistrict", "ssslong", "ssslat")]
datss1 <- unique(datss1)
datss1$district = as.numeric(datss1$sssdistrict=="")
datss1$long = as.numeric(is.na(datss1$ssslong))
datss2 <- datss1 %>%
  group_by(schoolcode) %>%
  filter((long == min(long))&(district == min(district)))

#pull school codes for schools students were admitted to
datstu$schoolcode <- ifelse(datstu$rankplace == 1, datstu$schoolcode1, ifelse(datstu$rankplace == 2, datstu$schoolcode2,ifelse(datstu$rankplace == 3, datstu$schoolcode3,ifelse(datstu$rankplace == 4, datstu$schoolcode4,ifelse(datstu$rankplace == 5, datstu$schoolcode5,ifelse(datstu$rankplace == 6, datstu$schoolcode6,0))))))

#create dataset with cutoff, quality, and size
admit <- datstu[, c("studentid", "score", "schoolcode")]
admit1 <- admit %>%
  group_by(schoolcode) %>%
  summarise(cutoff = min(score), meanscore = mean(score), size = length(studentid))

#merge datasets
datss2 = left_join(datss2, admit1, by = "schoolcode")

write.csv(datss2,"C:\\Users\\Emma S\\Documents\\Duke\\Spring 2021\\ECON 613\\schools.csv", row.names = FALSE)

#================================
#Exercise 3
#================================

datstu1 <- datstu

#add district coordinates, quality, and size to student database for each ranked choice
datstu1 <- left_join(datstu1, datss2 %>% dplyr::select(ssslong, ssslat, cutoff, meanscore), by = c("schoolcode1" = "schoolcode"))

names(datstu1)[names(datstu1) == "ssslong"] <- "ssslong1"
names(datstu1)[names(datstu1) == "ssslat"] <- "ssslat1"
names(datstu1)[names(datstu1) == "cutoff"] <- "cutoff1"
names(datstu1)[names(datstu1) == "meanscore"] <- "meanscore1"


datstu1 <- left_join(datstu1, datss2 %>% dplyr::select(ssslong, ssslat, cutoff, meanscore), by = c("schoolcode2" = "schoolcode"))

names(datstu1)[names(datstu1) == "ssslong"] <- "ssslong2"
names(datstu1)[names(datstu1) == "ssslat"] <- "ssslat2"
names(datstu1)[names(datstu1) == "cutoff"] <- "cutoff2"
names(datstu1)[names(datstu1) == "meanscore"] <- "meanscore2"


datstu1 <- left_join(datstu1, datss2 %>% dplyr::select(ssslong, ssslat, cutoff, meanscore), by = c("schoolcode3" = "schoolcode"))

names(datstu1)[names(datstu1) == "ssslong"] <- "ssslong3"
names(datstu1)[names(datstu1) == "ssslat"] <- "ssslat3"
names(datstu1)[names(datstu1) == "cutoff"] <- "cutoff3"
names(datstu1)[names(datstu1) == "meanscore"] <- "meanscore3"


datstu1 <- left_join(datstu1, datss2 %>% dplyr::select(ssslong, ssslat, cutoff, meanscore), by = c("schoolcode4" = "schoolcode"))

names(datstu1)[names(datstu1) == "ssslong"] <- "ssslong4"
names(datstu1)[names(datstu1) == "ssslat"] <- "ssslat4"
names(datstu1)[names(datstu1) == "cutoff"] <- "cutoff4"
names(datstu1)[names(datstu1) == "meanscore"] <- "meanscore4"


datstu1 <- left_join(datstu1, datss2 %>% dplyr::select(ssslong, ssslat, cutoff, meanscore), by = c("schoolcode5" = "schoolcode"))

names(datstu1)[names(datstu1) == "ssslong"] <- "ssslong5"
names(datstu1)[names(datstu1) == "ssslat"] <- "ssslat5"
names(datstu1)[names(datstu1) == "cutoff"] <- "cutoff5"
names(datstu1)[names(datstu1) == "meanscore"] <- "meanscore5"


datstu1 <- left_join(datstu1, datss2 %>% dplyr::select(ssslong, ssslat, cutoff, meanscore), by = c("schoolcode6" = "schoolcode"))


names(datstu1)[names(datstu1) == "ssslong"] <- "ssslong6"
names(datstu1)[names(datstu1) == "ssslat"] <- "ssslat6"
names(datstu1)[names(datstu1) == "cutoff"] <- "cutoff6"
names(datstu1)[names(datstu1) == "meanscore"] <- "meanscore6"

#add junior high school district coordinates
datstu1 <- left_join(datstu1, datjss, by = "jssdistrict")
names(datstu1)[names(datstu1) == "point_x"] <- "jsslong"
names(datstu1)[names(datstu1) == "point_y"] <- "jsslat"

#calculate distances
datstu1$dist1 = sqrt((69.172*(datstu1$ssslong1 - datstu1$jsslong)*cos(datstu1$jsslat/57.3))^2 +(69.172*(datstu1$ssslat1-datstu1$jsslat))^2)
datstu1$dist2 = sqrt((69.172*(datstu1$ssslong2 - datstu1$jsslong)*cos(datstu1$jsslat/57.3))^2 +(69.172*(datstu1$ssslat2-datstu1$jsslat))^2)
datstu1$dist3 = sqrt((69.172*(datstu1$ssslong3 - datstu1$jsslong)*cos(datstu1$jsslat/57.3))^2 +(69.172*(datstu1$ssslat3-datstu1$jsslat))^2)
datstu1$dist4 = sqrt((69.172*(datstu1$ssslong4 - datstu1$jsslong)*cos(datstu1$jsslat/57.3))^2 +(69.172*(datstu1$ssslat4-datstu1$jsslat))^2)
datstu1$dist5 = sqrt((69.172*(datstu1$ssslong5 - datstu1$jsslong)*cos(datstu1$jsslat/57.3))^2 +(69.172*(datstu1$ssslat5-datstu1$jsslat))^2)
datstu1$dist6 = sqrt((69.172*(datstu1$ssslong6 - datstu1$jsslong)*cos(datstu1$jsslat/57.3))^2 +(69.172*(datstu1$ssslat6-datstu1$jsslat))^2)

write.csv(datstu1,"C:\\Users\\Emma S\\Documents\\Duke\\Spring 2021\\ECON 613\\students.csv", row.names = FALSE)

#================================
#Exercise 4
#================================

#calculate mean and sd for cutoff, quality, and distance for each ranked choice

datstu1 <- datstu1 %>%
  summarise(meancutoff1 = mean(cutoff1, na.rm = TRUE),
            sdcutoff1 = sd(cutoff1, na.rm = TRUE), 
            mmeanscore1 = mean(meanscore1, na.rm = TRUE),
            sdmeanscore1 = sd(meanscore1, na.rm = TRUE),
            meandist1 = mean(dist1, na.rm = TRUE),
            sddist1 = sd(dist1, na.rm = TRUE),

            meancutoff2 = mean(cutoff2, na.rm = TRUE),
            sdcutoff2 = sd(cutoff2, na.rm = TRUE), 
            mmeanscore2 = mean(meanscore2, na.rm = TRUE),
            sdmeanscore2 = sd(meanscore2, na.rm = TRUE),
            meandist2 = mean(dist2, na.rm = TRUE),
            sddist2 = sd(dist2, na.rm = TRUE),

            meancutoff3 = mean(cutoff3, na.rm = TRUE),
            sdcutoff3 = sd(cutoff3, na.rm = TRUE), 
            mmeanscore3 = mean(meanscore3, na.rm = TRUE),
            sdmeanscore3 = sd(meanscore3, na.rm = TRUE),
            meandist3 = mean(dist3, na.rm = TRUE),
            sddist3 = sd(dist3, na.rm = TRUE),

            meancutoff4 = mean(cutoff4, na.rm = TRUE),
            sdcutoff4 = sd(cutoff4, na.rm = TRUE), 
            mmeanscore4 = mean(meanscore4, na.rm = TRUE),
            sdmeanscore4 = sd(meanscore4, na.rm = TRUE),
            meandist4 = mean(dist4, na.rm = TRUE),
            sddist4 = sd(dist4, na.rm = TRUE),

            meancutoff5 = mean(cutoff5, na.rm = TRUE),
            sdcutoff5 = sd(cutoff5, na.rm = TRUE), 
            mmeanscore5 = mean(meanscore5, na.rm = TRUE),
            sdmeanscore5 = sd(meanscore5, na.rm = TRUE),
            meandist5 = mean(dist5, na.rm = TRUE),
            sddist5 = sd(dist5, na.rm = TRUE),

            meancutoff6 = mean(cutoff6, na.rm = TRUE),
            sdcutoff6 = sd(cutoff6, na.rm = TRUE), 
            mmeanscore6 = mean(meanscore6, na.rm = TRUE),
            sdmeanscore6 = sd(meanscore6, na.rm = TRUE),
            meandist6 = mean(dist6, na.rm = TRUE),
            sddist6 = sd(dist6, na.rm = TRUE)

            )

write.csv(datstu1,"C:\\Users\\Emma S\\Documents\\Duke\\Spring 2021\\ECON 613\\datstu1.csv", row.names = FALSE)



#split data into quartiles by student test score
datstu1$quartile <- with(datstu1, cut(score, breaks=quantile(score, probs=seq(0,1, by=0.25), na.rm=TRUE), include.lowest=TRUE))

#within quartiles, recalculate all means and sd
datstu2 <- datstu1[, c("quartile", "cutoff1", "meanscore1", "dist1", "cutoff2", "meanscore2", "dist2", "cutoff3", "meanscore3", "dist3", "cutoff4", "meanscore4", "dist4", "cutoff5", "meanscore5", "dist5", "cutoff6", "meanscore6", "dist6")]
datstu2 <- datstu2 %>%
  group_by(quartile) %>%
  summarise(meancutoff1 = mean(cutoff1, na.rm = TRUE),
            sdcutoff1 = sd(cutoff1, na.rm = TRUE), 
            mmeanscore1 = mean(meanscore1, na.rm = TRUE),
            sdmeanscore1 = sd(meanscore1, na.rm = TRUE),
            meandist1 = mean(dist1, na.rm = TRUE),
            sddist1 = sd(dist1, na.rm = TRUE),
            
            meancutoff2 = mean(cutoff2, na.rm = TRUE),
            sdcutoff2 = sd(cutoff2, na.rm = TRUE), 
            mmeanscore2 = mean(meanscore2, na.rm = TRUE),
            sdmeanscore2 = sd(meanscore2, na.rm = TRUE),
            meandist2 = mean(dist2, na.rm = TRUE),
            sddist2 = sd(dist2, na.rm = TRUE),
            
            meancutoff3 = mean(cutoff3, na.rm = TRUE),
            sdcutoff3 = sd(cutoff3, na.rm = TRUE), 
            mmeanscore3 = mean(meanscore3, na.rm = TRUE),
            sdmeanscore3 = sd(meanscore3, na.rm = TRUE),
            meandist3 = mean(dist3, na.rm = TRUE),
            sddist3 = sd(dist3, na.rm = TRUE),
            
            meancutoff4 = mean(cutoff4, na.rm = TRUE),
            sdcutoff4 = sd(cutoff4, na.rm = TRUE), 
            mmeanscore4 = mean(meanscore4, na.rm = TRUE),
            sdmeanscore4 = sd(meanscore4, na.rm = TRUE),
            meandist4 = mean(dist4, na.rm = TRUE),
            sddist4 = sd(dist4, na.rm = TRUE),
            
            meancutoff5 = mean(cutoff5, na.rm = TRUE),
            sdcutoff5 = sd(cutoff5, na.rm = TRUE), 
            mmeanscore5 = mean(meanscore5, na.rm = TRUE),
            sdmeanscore5 = sd(meanscore5, na.rm = TRUE),
            meandist5 = mean(dist5, na.rm = TRUE),
            sddist5 = sd(dist5, na.rm = TRUE),
            
            meancutoff6 = mean(cutoff6, na.rm = TRUE),
            sdcutoff6 = sd(cutoff6, na.rm = TRUE), 
            mmeanscore6 = mean(meanscore6, na.rm = TRUE),
            sdmeanscore6 = sd(meanscore6, na.rm = TRUE),
            meandist6 = mean(dist6, na.rm = TRUE),
            sddist6 = sd(dist6, na.rm = TRUE)
  )

write.csv(datstu2,"C:\\Users\\Emma S\\Documents\\Duke\\Spring 2021\\ECON 613\\datstu2.csv", row.names = FALSE)


#================================
#Exercise 5
#================================

#create x and y variables
set.seed(27707)
nobs = 10000
nvar = 3
true_par <- c(0.5, 1.2, -0.9, 0.1)
x1 <- runif(10000, min=1, max=3)
x2 <- rgamma(10000, 3, scale = 2)
x3 <- rbinom(10000, 1, 0.3)
intercept <- rep(1, 10000)
error <- rnorm(10000, 2, 1)
y <- 0.5+(1.2*x1)-(0.9*x2)+(0.1*x3)+error
ydum <- ifelse(y>mean(y), 1, 0)
X <- cbind(intercept, x1, x2, x3)

#================================
#Exercise 6
#================================

#calculate correlation
corx1y = ((x1-mean(x1))%*%(y-mean(y)))/sqrt((x1-mean(x1))%*%(x1-mean(x1))*((y-mean(y))%*%(y-mean(y))))
print(corx1y)

#calculate OLS from scratch
beta = inv(t(X)%*%X)%*%t(X)%*%y
print(beta)

#calculate standard errors by hand
e = y - X%*%beta
s = sqrt(t(e)%*%e/(10000-3))
print(s)
s1 = s[1, 1]
se = sqrt(s1^2*(inv(t(X)%*%X)))
print(se)

#================================
#Exercise 7
#================================

#run probit probability

reg1 = glm(ydum~x1+x2+x3,family = binomial(link = "probit"))
summary(reg1)

flike = function(par,x1,x2,x3,ydum)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = ydum*log(pr) + (1-ydum)*log(1-pr)
  return(-sum(like))
}

# test if the function is correct
test_par = reg1$coefficients
flike(test_par,x1,x2,x3,ydum)
logLik(reg1)

ntry = 100
out = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-10,10)
  res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,ydum=ydum)
  out[i0,] = res$par
}

#===========================================
start = runif(4)
res  = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,ydum=ydum,hessian=TRUE)

fisher_info = solve(res$hessian)       # standard formula is -res$hessian but flike is return -like
prop_sigma  = sqrt(diag(fisher_info))
prop_sigma

est = cbind(true_par,summary(reg1)$coefficients[, 1],summary(reg1)$coefficients[, 2],res$par,prop_sigma)
colnames(est) = c("True parameter","R: GLM : est","R: GLM :se","R: own : est","R: own :se")
est

#run logit probability
  
  reg2 = glm(ydum~x1+x2+x3,family = binomial(link = "logit"))
  summary(reg2)
  
  flike = function(par,x1,x2,x3,ydum)
  {
    xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
    pr              = exp(xbeta)/(1+exp(xbeta))
    pr[pr>0.999999] = 0.999999
    pr[pr<0.000001] = 0.000001
    like           = ydum*log(pr) + (1-ydum)*log(1-pr)
    return(-sum(like))
  }
  
  # test if the function is correct
  test_par = reg2$coefficients
  flike(test_par,x1,x2,x3,ydum)
  logLik(reg2)
  
  ntry = 100
  out = mat.or.vec(ntry,4)
  for (i0 in 1:ntry)
  {
    start    = runif(4,-10,10)
    res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,ydum=ydum)
    out[i0,] = res$par
  }
  
  #===========================================
  start = runif(4)
  res  = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,ydum=ydum,hessian=TRUE)
  
  fisher_info = solve(res$hessian)       # standard formula is -res$hessian but flike is return -like
  prop_sigma  = sqrt(diag(fisher_info))
  prop_sigma
  
  est2 = cbind(true_par,summary(reg2)$coefficients[, 1],summary(reg2)$coefficients[, 2],res$par,prop_sigma)
  colnames(est2) = c("True parameter","R: GLM : est","R: GLM :se","R: own : est","R: own :se")
  est2
  
#compute linear probability

  beta1 = inv(t(X)%*%X)%*%t(X)%*%ydum
  print(beta1)
  
  edum = ydum - X%*%beta1
  sdum = sqrt(t(edum)%*%edum/(10000-3))
  print(sdum)
  sdum1 = sdum[1, 1]
  se1 = sqrt(sdum1^2*(inv(t(X)%*%X)))
  print(se1)

#================================
#Exercise 8
#================================

#calculate marginal effects
  
  probit = est[, c("R: own : est")]
  logit = est2[, c("R: own : est")]
  
  fav = mean(dnorm(X%*%probit))
  margprob =  as.matrix(fav * probit)
  xtable(margprob)
  
  fav1 = mean(exp(X%*%logit)/(exp(X%*%logit)+1)^2)
  marglog = as.matrix(fav1*logit)
  xtable(marglog)

#calculate standard errors
  
  eprob = ydum - X%*%probit
  sprob = sqrt(t(eprob)%*%eprob/(10000-3))
  print(sprob)
  sprob1 = sprob[1, 1]
  seprob = sqrt(sprob1^2*(inv(t(X)%*%X)))
  print(seprob)
  
  elog = ydum - X%*%logit
  slog = sqrt(t(elog)%*%elog/(10000-3))
  print(slog)
  slog1 = slog[1, 1]
  selog = sqrt(slog1^2*(inv(t(X)%*%X)))
  print(selog)
