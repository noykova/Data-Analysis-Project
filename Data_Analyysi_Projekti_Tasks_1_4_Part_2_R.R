# Data-analyysin projekti, kesä 2018

#####################################################
# Tehtävä 1                                       ##
#####################################################

#read the data
data <- read.csv(file="smear_physicum.csv", header=TRUE, sep=",")
head(data)
tail(data)
dim(data)

#####################################################
#1.a)                                               #
#####################################################

paivaKA <- function(year,month,day,smear_statistics) {
  
  newdata <- subset(smear_statistics, Year==year & Month==month & Day==day,
                    select=AirTemperature:WindSpeed) 
  
  if (is.data.frame(newdata) && nrow(newdata)==0) {
    ##This will give TRUE for an empty data frame (that is, one with no rows) and false otherwise
    return(print("There is no data for the required date"))
  }
  
  print("Avarage daily SMEAR values on ")
  data_string<-paste(as.character(year),as.character(month),as.character(day), sep = "-")
  print(format(as.Date(data_string), "%A, %d-%b. %Y"))
  print(paste("Average air temperature =", signif(sum(newdata$AirTemperature)/24,4)))
  print(paste("Average relative humidity =", signif(sum(newdata$RelativeHumidity)/24,4)))
  print(paste("Average air pressure=", signif(sum(newdata$AirPressure)/24,4)))
  print(paste("Average wind speed =", signif(sum(newdata$WindSpeed)/24,4)))
  
}

paivaKA(2018,2,1,data)
#[1] "Avarage daily SMEAR values on "
#[1] "Thursday, 01-Feb. 2018"
#[1] "Average air temperature = -1.406"
#[1] "Average relative humidity = 93.28"
#[1] "Average air pressure= 993.2"
#[1] "Average wind speed = 5.65"

paivaKA(2018,4,5,data)
#[1] "There is no data for the required date"

#####################################################
# 1. b)                                             #
#####################################################

#examples of own functions: https://www.statmethods.net/management/userfunctions.html 

paivaKA <- function(year,month,day,smear_statistics,tulosta=TRUE) {
  
  newdata <- subset(smear_statistics, Year==year & Month==month & Day==day,
                    select=AirTemperature:WindSpeed) 
  
  if (is.data.frame(newdata) && nrow(newdata)==0) {
    ##This will give TRUE for an empty data frame (that is, one with no rows) and false otherwise
    return(print("There is no data for the required date"))
  }
  
  
  air_temp_aver<-sum(newdata$AirTemperature)/24
  relat_humid_aver<-sum(newdata$RelativeHumidity)/24  
  air_press_aver<-sum(newdata$AirPressure)/24  
  wind_speed_aver<-sum(newdata$WindSpeed)/24  
  
  if (!tulosta) {
    result<-c(air_temp_aver, relat_humid_aver, air_press_aver, wind_speed_aver)
      return (result)
  } else {
  
  print("Avarage daily SMEAR values on ")
  data_string<-paste(as.character(year),as.character(month),as.character(day), sep = "-")
  print(format(as.Date(data_string), "%A, %d-%b. %Y"))
  print(paste("Average air temperature =", signif(air_temp_aver,4)))
  print(paste("Average relative humidity =", signif(relat_humid_aver,4)))
  print(paste("Average air pressure=", signif(air_press_aver,4)))
  print(paste("Average wind speed =", signif(wind_speed_aver,4)))
  }
  
}

paivaKA(2018,2,1,data)
#[1] "Avarage daily SMEAR values on "
#[1] "Thursday, 01-Feb. 2018"
#[1] "Average air temperature = -1.406"
#[1] "Average relative humidity = 93.28"
#[1] "Average air pressure= 993.2"
#[1] "Average wind speed = 5.65"

a<-paivaKA(2018,2,1,data, tulosta = FALSE)
a
#[1]  -1.405556  93.280554 993.207003   5.650486

#####################################################
#1. c)                                              #
#####################################################

result<-matrix(rep(0,124), nrow=31, ncol=4)
for (i in 1:31) {
  result[i,]<-paivaKA(2018,3,i,data, tulosta = FALSE)
}

#####################################################
# 1.d)                                              #
#####################################################

air_temp_aver<-result[,1]
relat_humid_aver<-result[,2]
air_press_aver<-result[,3] 
wind_speed_aver<-result[,4] 

#Plot 1: The dynamics of 4 SMEAR measurements in March 2018
par( mfrow = c( 2, 2 ) )
plot(1:31,air_temp_aver,xlab = "March",
     ylab = "Daily averages of air temperature in March 2018", type = "l",col="red",
     main = "Air Temperature")

plot(1:31, relat_humid_aver,col="blue", xlab = "March",
     ylab = "Daily averages of relative humidity", type = "l",
     main = "Relative himidity")

plot(1:31, air_press_aver,col="chocolate4", xlab = "March",
     ylab = "Daily averages of air pressure", type = "l",
     main = "Air pressure")

plot(1:31, wind_speed_aver,col="green", xlab = "March",
     ylab = "Daily averages of wind speed", type = "l",
     main = "Wind speed")

par( mfrow = c( 1, 1 ) )

#From these data we see that the average daily temperature in March 2018 is increasing function in time,
#while no clear tendency can be observed for the other 3 measured quantities. 


# http://www.gettinggeneticsdone.com/2011/07/scatterplot-matrices-in-r.html 
# panel.smooth function is built in.
# panel.cor puts correlation in upper panels, size proportional to correlation
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Plot #2: Basic scatterplot matrix of the four daily average measurements in March 2018
# Added loess smoother in lower and correlation in upper
pairs(~air_temp_aver+relat_humid_aver+air_press_aver+wind_speed_aver, data=result,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Scatterplot Matrix of 4 daily average SMEAR measurements in March 2018")

#The highest correlation (corr=0.70) is observed between the air temperature and air pressure in March 2018.
#Some quite signifficant correlation might existbetween the relative himidity and air pressure (corr=0.62), 
#as well as between air temperature and relative himidity (corr=0.53). 

#####################################################
#1 e)                                               #
#####################################################

colnames(result) <- c("air_temp_aver", "relat_humid_aver", "air_press_aver", "wind_speed_aver")

my_model <- lm(air_temp_aver ~ air_press_aver, data = as.data.frame(result))

#without intercept: 
#my_model <- lm(air_temp_aver ~ air_press_aver -1, data = as.data.frame(result))

#Scatterplot of the regression data
plot(air_press_aver, air_temp_aver, main="Scatterplot of the data, March 2018", xlab="Daily air pressure", ylab="Daily air temperature", pch=19) 

#plot points, regression line and residuals
abline(my_model,lwd=2, col="red")
# calculate residuals and predicted values
res <- signif(residuals(my_model), 5)
pre <- predict(my_model) # plot distances between points and the regression line
segments(air_press_aver, air_temp_aver, air_press_aver, pre, col="blue")
# add labels (res values) to points
library(calibrate)
textxy(air_press_aver, air_temp_aver, res, cx=0.7)

print(my_model)

##confidence intervals at level 0.95
confint(my_model, level = 0.95)
#the intersept a belongs to [191.9 440.81]. the estimate is a=316
#b belongs to [-0.4425 -0.1948]. the estimate is b=-0.3187

summary(my_model)

#The results about p-values show that both coefficients are significant. 
#It becomes clear also from the plots that a linear model is good approximation for the dependency 
#between the daily average measurements in March 2018 about air temperature and air pressure. 
#The increasing of air pressure leans to decreasing of air temperature. 

#####################################################
#1.f)                                               #
#####################################################

#I think that for the prediction purposes we can use only similar data. 
#Since we do nit have data about July in 2017 or at least some other summer month in different year, 
#it is impossible to use the fitted linear model (using training data for March 2018; 1.e) 
#for prediction purposes in the summer of 2018.
#We can predict for examplethat if in March 2018 the pressure was air_press_aver=1000, then 
#air_temp_aver = a+b*air_press_ave = 316.3581-0.3187*1000 = 316.3581-0.3187*1000 = -2.3419oC. 

#Another approach would be to assume that the average temperature is lenarly increasing from 
#december 2017 to july 2018 and try to estimate linear regression model using all available data.
#But I am not sure how correct could be such assumption and if the dependency between air pressure and 
#air temperature could be modeled in such way. 

#So, the right answer here would be that knowing the average daily air pressure = 1000 hPa 
#we are not able to predict the temperature in July 2018 using the linear regression model in 1.e). 
# It is not very probable that if the presure is 1000 hPa the temperature in July would be -2.3oC. 


#####################################################
# Tehtävä 2                                        ##
#####################################################

#####################################################
# 2.a)                                              #
#####################################################

#use different color intensity for plotting ovelapping plots, for example
#plot( Sepal.Width ~ Sepal.Length, iris , pch=16, col=rgb(0,0,1, 0.2)) :
#https://mgimond.github.io/ES218/Week04d.html 
#overlapping histogram:
#https://www.r-bloggers.com/overlapping-histogram-in-r/ 

#obtain a matrix with average measured values for January 2018
january<-matrix(rep(0,124), nrow=31, ncol=4)
for (i in 1:31) {
  january[i,]<-paivaKA(2018,1,i,data, tulosta = FALSE)
}
colnames(january) <- c("air_temp_aver", "relat_humid_aver", "air_press_aver", "wind_speed_aver")
jan<-as.data.frame(january)


#obtain a matrix with average measured values for February 2018
february<-matrix(rep(0,112), nrow=28, ncol=4)
for (i in 1:28) {
  february[i,]<-paivaKA(2018,2,i,data, tulosta = FALSE)
}
colnames(february) <- c("air_temp_aver", "relat_humid_aver", "air_press_aver", "wind_speed_aver")
feb<-as.data.frame(february)

#plot overlapping huistogram about air_temp_aver
#rgb(0,0,1) - The function returns a hexadecimal value, #0000FF, which R can convert to a color
#A fourth parameter can be passed to rgb(): the opaqueness value alpha. 
#This is useful when many overlapping points are displayed on a plot. 
#alpha is defined in an interval from 0 to 1 with 1 being completely opaque 


# Histogram Grey Color
#hist(jan$air_temp_aver, col=rgb(0.1,0.1,0.1,0.5),xlim=c(-20,5), ylim=c(0,11), main="Average daily temperature")
#hist(feb$air_temp_aver, col=rgb(0.8,0.8,0.8,0.5), add=T)
#box()

# Histogram Colored (blue and red)
#par(new=TRUE) - general way to let us plot one plot over another
#http://www.shizukalab.com/toolkits/overlapping-histograms
hist(jan$air_temp_aver, breaks = 20, col=rgb(1,1,0,0.7),xlim=c(-25,10), ylim=c(0,4), main="Air Temperature in January and February", xlab="Average daily temperature")
hist(feb$air_temp_aver, breaks=20, col=rgb(0,1,1,0.4), add=T)
#legend("topright", c("January", "February"), fill=c(rgb(1,1,0,0.7), rgb(0,1,1,0.4)))
box()

#####################################################
# b)                                                #
#####################################################

par( mfrow = c( 2, 2 ) )
#air temperature
hist(jan$air_temp_aver, breaks = 20, col=rgb(1,1,0,0.7),xlim=c(-25,10), ylim=c(0,4), main="Air Temperature", xlab="Average daily temperature")
hist(feb$air_temp_aver, breaks=20, col=rgb(0,1,1,0.4), add=T)
#legend("topright", c("January", "February"), fill=c(rgb(1,1,0,0.7), rgb(0,1,1,0.4)))
box()

#relat_humid_aver
hist(jan$relat_humid_aver, breaks = 20, col=rgb(1,1,0,0.7),xlim=c(60,110), ylim=c(0,7), main="Relative Humidity", xlab="Average daily relative humidity")
hist(feb$relat_humid_aver, breaks=20, col=rgb(0,1,1,0.4), add=T)
#legend("topright", c("January", "February"), fill=c(rgb(1,1,0,0.7), rgb(0,1,1,0.4)))
box()

#air_press_aver
hist(jan$air_press_aver, breaks = 20, col=rgb(1,1,0,0.7),xlim=c(980,1040), ylim=c(0,4), main="Air Pressure", xlab="Average daily air pressure")
hist(feb$air_press_aver, breaks=20, col=rgb(0,1,1,0.4), add=T)
#legend("topright", c("January", "February"), fill=c(rgb(1,1,0,0.7), rgb(0,1,1,0.4)))
box()

#wind_speed_aver
hist(jan$wind_speed_aver, breaks = 20, col=rgb(1,1,0,0.7),xlim=c(1,9), ylim=c(0,10), main="Wind Speed", xlab="Average daily wind speed")
hist(feb$wind_speed_aver, breaks=20, col=rgb(0,1,1,0.4), add=T)
#legend("topright", c("January", "February"), fill=c(rgb(1,1,0,0.7), rgb(0,1,1,0.4)))
box()
par( mfrow = c( 1, 1 ) )
legend("bottom", c("January 2018", "February 2018"), fill=c(rgb(1,1,0,0.7), rgb(0,1,1,0.4)))


#From there histograms we see that the average temperatures in February are lower than these in January. 
#The same is valid for relative humidity. 
#On the other hand, the air pressure was higher in February.
#There were more windy days in Janyuary comparing with those in February. 
#It is difficult to compare the corresponding distributions for both months from these histograms. 

#####################################################
# 2.c)                                              #
#####################################################

#find daily average measured values for December 2017
december<-matrix(rep(0,124), nrow=31, ncol=4)
for (i in 1:31) {
  december[i,]<-paivaKA(2017,12,i,data, tulosta = FALSE)
}
colnames(december) <- c("air_temp_aver", "relat_humid_aver", "air_press_aver", "wind_speed_aver")
dec<-as.data.frame(december)

#add column names for march
colnames(result) <- c("air_temp_aver", "relat_humid_aver", "air_press_aver", "wind_speed_aver")
march<-as.data.frame(result)

#merge all 4 matrices for Dec, Jan, Feb, March
total<-rbind(dec, jan, feb, march)

#create new column with dates
dates = seq(from = as.Date("2017-12-01"), to = as.Date("2018-03-31"), by = 'day')
total<-cbind(total, dates)

#screen split to 4 horizontal parts
#http://seananderson.ca/courses/11-multipanel/multipanel.pdf

par(mfrow = c(4, 1))
par(cex = 0.6)
par(mar = c(0.5, 0, 1.25, 0), oma = c(0,0,0,0))
par(tcl = -0.25)
par(mgp = c(4, 0.6, 0))


#Add plot to each part of the screen
plot(total$dates,total$air_temp_aver,xlab = "December 2017 - March 2018",
     ylab = "Daily averages of air temperature in March 2018", type = "l",col="red",
     main = "Air Temperature")

plot(total$dates, total$relat_humid_aver,col="blue", xlab = "December 2017 - March 2018",
     ylab = "Daily averages of relative humidity", type = "l",
     main = "Relative himidity")

plot(total$dates, total$air_press_aver,col="chocolate4", xlab = "December 2017 - March 2018",
     ylab = "Daily averages of air pressure", type = "l",
     main = "Air pressure")

plot(total$dates, total$wind_speed_aver,col="green", xlab = "December 2017 - March 2018",
     ylab = "Daily averages of wind speed", type = "l",
     main = "Wind speed")

#Dynamics of air temperature and relative humidity is very similar during the period December 2017 - February 2018.
#Both curves indicate decreasing in their values at the end of february.
#In March 2018 the temperature increases while relative humidity tends to decrease. 
#We may assume that both measurements are correlated. 
#The air pressure has increased in february, but in march it show lower values. 
#There were big fluctoations in wind speed during the whole period December 2017 - March 2018. 
#The highest values were observed in December and January. 


#####################################################
# Tehtävä 3                                        ##
#####################################################


#A collection of die problems: 
#http://www.madandmoonly.com/doctormatt/mathematics/dice1.pdf

#####################################################
# 3.a)                                              #
#####################################################

prior<-function(x){
  
  result<-rep(0, length(x))
  
  for (i in 1:length(x)) {
    if (x[i]>0 && x[i]<=0.01) {
      result[i]<-200*x[i]
    }
    if (x[i]>0.01 && x[i]<1) {
      result[i]<-(20000/9900)*(1-x[i])
    }
    if (x[i]<=0 || x[i]>=1) {
      result[i]<-0
    } 
  }
  return(result)
  
}

x<-seq(0,1, by=0.001)
y<-prior(x)

plot(x, y, xlab = "x values",
     ylab = "Prior function", type = "l",
     main = "Prior density function")

#In the interval (0,1/100] the prior is linearly increasing function with maximum and theta=0.01 prior=2.
#In the interval (0.01, 1) the prior is linearly decreasing function with minimum at theta=0.99 prior = 0.02
#otherwise when theta<0 or theta>=1 the prior=0. 
#So, the maximal prior is at theta=0.01, where prior density function = 2. 

#####################################################
# 3.b)                                              #
#####################################################

#number of experiments
n<-234321
#number of successes (get 1000 after dice rolling)
s<-1234
#number of failures (not get 1000 after dice rolling)
f<-n-s
#f=233087

#I do not know how to solve the integral f(y) = integral((theta)^s(1-theta)^(n-s)f(theta)d(theta)).
#Therefore I use the fact, that since f(y) is a constant, the posterior p(theta|y) ~ p(theta)f(y|theta)
#The likelihood f(y|theta) = (n s)*(theta^s)*((1-theta)^(n-s))
#maksimum likelihood estimate is when d(f(y|theta))/d(theta) = 0.
#this estimate is theta_est=s/n = 0.005

#The posterior p(theta|y) ~ p(theta)f(y|theta) = (n s)*(theta^s)*((1-theta)^(n-s))*p(theta)
#The log posterior log (p(theta|y)) ~ log((n s)*(theta^s)*((1-theta)^(n-s))*p(theta))=
# = log((n s)) + s*log(theta) + (n-s)*log(1-theta)) + log(p(theta))=
#= log(n!/((n-s)!*s!)) + s*log(theta) + (n-s)*log(1-theta)) + log(p(theta))

#lfactorial =log(factorial) ==> factorial(n)/(factorial(n-s)*factorial(s)) = lfactorial(n)-lfactorial(n-s)-lfactorial(s)
posterior <- lfactorial(n)-lfactorial(n-s)-lfactorial(s)+s*log(x) + (n-s)*log(1-x) + log(y)

lfactorial(n)-lfactorial(n-s)-lfactorial(s)

plot(x, posterior, xlab = "x values",
     ylab = "Posterior function", type = "l",
     main = "Posterior density function")


posteriorUniform <- lfactorial(n)-lfactorial(n-s)-lfactorial(s)+s*log(x) + (n-s)*log(1-x) + log(1/1000)
plot(x, posteriorUniform, xlab = "x values",
     ylab = "Posterior function", type = "l",
     main = "Posterior density function")

#It seems that in this example the uniform prior does not change the maximal posterior distribution, 
#which appen at x=0.01. The uniform prior only adds a constant log(1/1000)=-6.907755.


#Computers cannot store numbers very close to 0, as it is in this example. The log (likelihood) 
#(respectively log (posterior)) solves this small numbers problem. 

#####################################################
# 3.c)                                              #
#####################################################

#probability to pick up one of the 3 dices P(Dj)=1/3, j=1,2,3. 
#probability of rolling an i=1000 with dice Dj is PDj(i). Conditional probability of rolling i
#with dice Dj is P(i|Dj)=PDj(i)
#joint probability of picking die Dj and rolling i=1000 is:
#P(i,Dj) = P(Dj)*P(i|Dj)
#The marginal probability of rolling i=1000:
#P(i) = sumj(P(i,Dj))= sumj(P(Dj)*P(i|Dj)), j=1,2,3
#P(i=1000)=(1/3)*(1/1000)+1/3*(1/180)+(1/3)*(1/200)

prior3<-(1/3)*(1/1000)+1/3*(1/180)+(1/3)*(1/200)
#prior3=0.003851852

posteriorTeller <- lfactorial(n)-lfactorial(n-s)-lfactorial(s)+s*log(x) + (n-s)*log(1-x) + prior3

plot(x, posteriorTeller, xlab = "x values",
     ylab = "Posterior function", type = "h",
     main = "Posterior density function")

#the maximum posterior probability is -1.057004e+03 for x=0.01

# I am doing something wrong, but I do not know what. Most probably the problem is in 
# the unsolved integral. 

#####################################################
# Tehtävä 4                                        ##
#####################################################

#Probability for red light for Fisher = theta
#Probability for green light for Fisher = (1-theta)
#p=P(othrers=red|pedestrians=red)
#q=P(pedestrians=red)
#P(Fisher light = green) = P(other drivers = red & pedestrians=red) = P(othrers=red|pedestrians=red)*P(pedestrians=red)
#P(Fisher=green)=p*q
#theta=1-P(Fisher light = green)=1-p*q 
#Assume that the real value of theta is theta=1-p*q
#In the function Fisherin_valot we have to simulate this binomial distribution 
#with parameters n and theta, where p and q are also provided. 

#####################################################
# 4.a                                               #
#####################################################

Fisherin_valot <- function(n, p,q) {
  theta<-1-p*q
  #set.seed(18062018)
  ##rbinom(n, size, prob), n - # of observations, 
  #size - # of trials, prob - prob of success of each trial  y <- rbinom(n, 1, theta)  # aineisto
  y <- rbinom(n, 1, theta)
  return(y)
}


otos<-Fisherin_valot(n=100,p=0.5,q=0.4)

su_estimaatti <- sum(otos)/100
## Or directly:
# su_estimaatti <- mean(y)
su_estimaatti
#0.82

#Repeat this simulation experiment 100 times. 
n=100
p=0.5
q=0.4
theta=1-p*q
set.seed(1234)
su_estimaatit <- replicate(100,mean(rbinom(n, 1, theta)))
hist(su_estimaatit)
mean(su_estimaatit)
#0.8014
var(su_estimaatit)
# 0.001230343

#Mikali edella asetetut todennakoisyydet kuvaavat Fisherin autoillessaan kohtaamia tilanteita,
#voidaanko punaisten valojen sanoa osuvan Fisherin kohdalle muita autoilijoita useammin?

#Probability for other drivers to stop at red:
#Since it is impossible to have P(others=green| traffic light = green), this means that the conditional 
#probability P(others=red| traffic light = green)=1. 
#Then the joint probability P(others=red & traffic light = green)=
#=P(others=red| traffic light = green)*P(traffic light = green) = 1*(1-q)
#The joint prob. P(others=red & traffic light = red) = p*q
# The prob. for red light for others then is sum of both joint probabilities = 
# 1*(1-q) + p*q = 0.8
#Which is exactly the same as the probability for red light for Fisher (maximum likelihood estimate)
#==> Fisher has no reason to complain. 

#Mika on simulaatioittesi perusteella suurimman uskottavuuden estimaatti ehdottomalle 
#todennakoisyydelle, etta risteykseen saapuessaan Fisher kohtaa punaiset valot?

#su-estimaattoria theta_est = sum(yi)/n , i=1...n = X/n = mean(y) = eli onnistumisten osuutta n toistossa. 
#Tässä satunnaismuuttuja X on onnistumisten lukumäärä, joka noudattaa binomijakauma parametrin n ja theta.  
mean(su_estimaatit)
#theta_est=0.8014

#####################################################
#4.b                                               ##
#####################################################

#approximation to normal distribution: mu= n*theta; SD=sqrt(n*theta*(1-theta))
#n*theta = 80>5, n*(1-theta)=20>5 ==> we are allowed to approximate to normal.

#function to compute confidence interval for normal distribution at level alpha
#Continuity correction: -/+0.5. 

alpha<-0.05
luottamistaso<-0.95

z_luottamusvali <- function(y, luottamistaso){
  n <- length(y);
  #z-arvo luottamustasolla alpha/2
  #z <- qnorm(alpha/2, lower = FALSE);
  #z - arvo luottamistasolla (1-alpha)
  #n*mean(y) = n*theta; theta = max. likelihood estimate of theta
  z<-qnorm(luottamistaso)
  sigma<-sqrt(n*mean(y)*(1-mean(y)));
  error<-z*(sigma/sqrt(n));
  L<-(n*mean(y)-error-0.5)/n;
  #-0.5 - continuity correction 
  U<-(n*mean(y)+error+0.5)/n;
  #+0.5 - continuity correction 
  result<-c(L,U)
  return(result)
}

z_luottamusvali(su_estimaatit,0.95)
#[0.7898379 0.8129621]
#theta_est=0.8014 is involved in this interval. 

#prop.test() by default uses Yates' continuity correction
#I do not know how to use it in this task.
#approx.lower <- prop.test(n*su_estimaatit, length(n*su_estimaatit))$conf.int[1]
#approx.upper <- prop.test(n*su_estimaatit, length(su_estimaatit))$conf.int[2]

su_estimaatit_1000 <- replicate(1000,mean(rbinom(1000, 1, theta)))
hist(su_estimaatit_1000)
mean(su_estimaatit_1000)
var(su_estimaatit_1000)
z_luottamusvali(su_estimaatit_1000,0.95)
#[0.7987419 0.8010581]
#In this case the confidence inteval is more narrow. The reason is that we increase the number of 
#observed data 10 times, even if every time we use less observations for the simulation. 
#Since n increases, the binomial distribution approaches closer normal distribution, where the peek 
#is closer to the true value, and confidence interval is more narrow. 

#####################################################
#4.c                                               ##
#####################################################

su_estimaatit_1 <- replicate(100,mean(rbinom(100, 1, theta)))
z_luottamusvali(su_estimaatit_1,0.95)
#The minimal 95% interval in this case is [0.7910529 0.8141471]

su_estimaatit_2 <- replicate(1000,mean(rbinom(1000, 1, theta)))
z_luottamusvali(su_estimaatit_2,0.95)
#The confidence interval becomes more narrow: [0.7989272 0.8012428]

su_estimaatit_3 <- replicate(10000,mean(rbinom(10000, 1, theta)))
z_luottamusvali(su_estimaatit_3,0.95)
#Even more narrow confidence interval: [0.7999045 0.8001361]

#Conclusion: Increasing the sample size n leads to increasing the 
#accuracy of theta estimate, and consequently the confidence interval becomes 
#more narrow. This is in agreement with the central limit theorem. 
