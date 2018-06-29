# Data-analyysin projekti, kesä 2018

#####################################################
# Tehtävä 5                                        ##
#####################################################


#####################################################
# 1. Read and wrangling the data                   ##
#####################################################


#READ THE ORIGINAL DATA
#First have a look to original data
#Read the empoyees data
data_workers <- read.csv(file="fleed_tyo.csv", header=TRUE, sep=",")
head(data_workers)
tail(data_workers)
dim(data_workers)
str(data_workers)

#Read the empoyers data
data_firms <- read.csv(file="fleed_yritys.csv", header=TRUE, sep=",")
head(data_firms)
tail(data_firms)
dim(data_firms)
str(data_firms)

#MERGE BOTH DATA FRAMES. DATA WRANGLING

#1. Take a subset for year **vuosi**=2 from both original tables. 
data_workers2 <- subset(data_workers, vuosi==2)
dim(data_workers2)
data_firms2 <- subset(data_firms, vuosi== 2)
dim(data_firms2)

#2. Merge both data frames by its intersection=syrtun 
data1<-merge(data_workers2, data_firms2, by="syrtun")
dim(data1)
#1925 x 23
head(data1)

#3. Convert integer values to factors for the investigated categorical variables. 
data1[,'peas']<-factor(data1[,'peas'])
typeof(data1$peas)
#"integer"
class(data1$peas)
#"factor"
str(data1$peas)
levels(data1$peas)

data1[,'a7lkm']<-factor(data1[,'a7lkm'])
str(data1$a7lkm)
levels(data1$a7lkm)

data1[,'a18lkm']<-factor(data1[,'a18lkm'])
str(data1$a18lkm)
levels(data1$a18lkm)

data1[,'SLHKY']<-factor(data1[,'SLHKY'])
str(data1$SLHKY)
levels(data1$SLHKY)

data1[,'sllvy']<-factor(data1[,'sllvy'])
str(data1$sllvy)
levels(data1$sllvy)

#4. remove all missing values
data2<-na.omit(data1)
dim(data2)
#0 x 23 - if we delete all missint values, the corresponding subset is empty.

sum(is.na(data1$peas))
#0
sum(is.na(data1$a7lkm))
#318
sum(is.na(data1$a18lkm))
#321
sum(is.na(data1$sllvy))
#0
sum(is.na(data1$svatva))
#9

#Therefore we delete only the missing values in the columns, 
#directly involved in task description. 
#na.omit deletes all na in the table. 
#!is.na() deletes only the NA in particular column
data<-data1[!is.na(data1$svatva),]
#1916 x 23
data_a<-data[!is.na(data1$a7lkm),]
# 1607   23
data_b<-data[!is.na(data1$a18lkm),]
dim(data_b)
#1604 x 23


#####################################################
# 2. Univariate analysis                           ##
#####################################################
#2.0 First look at the within correlation using ggpairs
library(ggplot2)
library(GGally)
var<-data[,c("svatva", "peas", "sllvy")]
assignInNamespace("ggally_cor", ggally_cor, "GGally")
ggpairs(var, upper = list(continuous = wrap("cor", size = 10)), lower = list(continuous = "smooth"))

#Univariate exploratory data analysis

#2.1. The numerical variable svatva

#use attach to save writing. 
#analyse data with na's removed obly from the column statva. 
attach(data)
summary(svatva)
#Min. 1st Qu.  Median  Mean 3rd Qu.    Max. 
#  0   14000    18000  19434   23000   99000

#stem-and-leaf-plot
stem(svatva)
#histogram
#hist(svatva, breaks=40)
h<-hist(svatva, breaks=seq(0, 101000, by=2500), plot=F)
#str(h)

plot(h, col = heat.colors(length(h$mids))[length(h$count)-rank(h$count)+1],
     ylim = c(0, max(h$count)+5),
     main="Earned income total in state taxation, weight %",
     sub="Counts shown above bar, actual values shown with rug plot")
rug(svatva)
#cex - size of the text on the figure
text(h$mids, h$count, h$count, cex=0.7, cex.main =0.7, cex.sub=0.7, pos=3)
rm(h)

#select observations where svatva > 10000
data[svatva>48500,]
#nothing unusual in these 45 data points. 

#boxplot of svatva

#if notch is TRUE, a notch is drawn in each side of the boxes. 
#If the notches of two plots do not overlap this is 'strong evidence' 
#that the two medians differ
boxplot(svatva, notch=T, horizontal=T,main="Boxplot of total earned income")

#histogram showing probability density instead the frequency 
#purpose - to make it comparable to the histograms with different number of obsevations
#lwd - line width
hist(svatva, freq=F, breaks=seq(0, 101000, by=2500), cex.main =0.9, main="Probability density for total earned income")
#density - computes kernel density estimates. 
#These are smoothed continuous approximation to the histogram. 
lines(density(svatva),lwd=2)
lines(density(svatva, adj=.5),lwd=1)
lines(density(svatva, adj=2),lwd=1.5)

#Compare the actual distribution to the theoretical one using quantile-quantile plot. 
qqnorm(svatva, main="QQ plot for total earned income vs Normal distribution",ylab="Total earned income")
qqline(svatva, col=4)

#QQ plot in log scale - impossible to draw because ylim is infinite. 
#qqnorm(log(svatva), main="QQ plot for total earned income vs lognormal distribution", ylab="log(Total earned income)")

#qqline(log(svatva), col=4)

#Point estimation. Inference of the mean
t.test(svatva, mu=18000, conf.level = 0.99)
#t = 6.059, df = 1915, p-value = 1.645e-09
#alternative hypothesis: true mean is not equal to 18000
#99 percent confidence interval:
#  18823.60 20043.84
#sample estimates:
#  mean of x 
#19433.72 

#2.2. The categorical variables sllvy and family status peas.
#sllvy
#count frequences
table(sllvy)
#compute proportions of different classes
library(ggplot2)
library(GGally)
counts_sllvy <- table(sllvy)
proportions_sllvy<-counts_sllvy / sum(counts_sllvy)
proportions_sllvy

#plot the same proportions as bar plot
bar_sllvy <- ggplot(data, aes(x = sllvy)) 
bar_sllvy <- bar_sllvy + geom_bar()
#summary(bar_sllvy)
ggplot(data, aes(x = sllvy)) + 
  geom_bar(fill = "orange", width = 0.7) + 
  xlab("Company group according its turnover") + ylab("Number of Observations")

#peas
#count frequences
table(peas)
#compute proportions of different classes
counts_peas <- table(peas)
proportions_peas<-counts_peas / sum(counts_peas)
proportions_peas
#plot the same proportions as bar plot
bar_peas <- ggplot(data, aes(x = peas)) 
bar_peas <- bar_peas + geom_bar()
#summary(bar_peas)
ggplot(data, aes(x = peas)) + 
  geom_bar(fill = "orange", width = 0.7) + 
  xlab("Family group") + ylab("Number of Observations")

#####################################################
# 3. Bivariate analysis                            ##
#####################################################

#####################################################
# 3.1 Bivariate analysis:                          ##
#Total earned income vs. company turnover          ##
#####################################################

#Bivariate exploratory data analysis. 

#3.1.1 Visualise the total earned income by companies turnover. 
#Sort svatva and use order to keep the sorted indexes

svatva[order(svatva)][1:100]
sllvy[order(svatva)] [1:100]

#Difficult to make any inference

#use by method to compute some statistics about every level of sllvy
by(svatva,sllvy,range)
#length of the range of income in every company turnover group. 
by(svatva,sllvy,function(x) max(x)-min(x))

#use boxplot to divide svatva with respect to different factors of sllvy
#notch=T - shows if the class medians are significantly different. 
boxplot(svatva~sllvy, notch=T, horizontal=T, xlab="Total earned income", ylab="turnover group")
#note the different size of observations (people) in every group. 

#find out how many observations fail in every group 
by(svatva,sllvy,length)


#numerical conformation of box plots by summaries for every group. 
by(svatva,sllvy,summary)

##3.1.2. One-way analisys of variance (ANOVA)
lm_an<-lm(svatva~sllvy)
summary(lm_an)
#same results can be obtain also using aov method:
summary(aov(lm_an))
coefficients(aov(lm_an))

#####################################################
# 3.2 Bivariate analysis:                          ##
#Total earned income vs. family status             ##
#####################################################

#Bivariate exploratory data analysis. 

#3.2.1 Visualise the total earned income by family status. 
#Sort svatva and use order to keep the sorted indexes
svatva[order(svatva)][1:100]
peas[order(svatva)] [1:100]

#Difficult to make any inference

#use by method to compute some statistics about every level of peas
by(svatva,peas,range)

#length of the range of income in every family group. 
by(svatva,peas,function(x) max(x)-min(x))

#find out how many observations fail in every family group 
#same results as shown in coresponding bar plot during univariate analysis.  
by(svatva,peas,length)

#use boxplot to divide svatva with respect to different family groups
#notch=T - shows if the class medians are significantly different. 
boxplot(svatva~peas, notch=F, horizontal=T, xlab="Total earned income", ylab="Family group")
#note the different size of observations (people) in every group. 

#numerical conformation of box plots by summaries for every family group. 
by(svatva,peas,summary)

##3.2.2. One-way analisys of variance (ANOVA)
lm_an_2<-lm(svatva~peas)
summary(lm_an_2)
#same results can be obtain also using aov method:
summary(aov(lm_an_2))
coefficients(aov(lm_an_2))

#####################################################
# 3.3 Bivariate analysis:                          ##
#Visualization of the two categorical variables    ##
#company turnover and family status                ##
#####################################################

g0<-ggplot(data, aes(x = sllvy, fill = peas)) + geom_bar(position = "dodge")
g0 + xlab("Company turnover")


#####################################################
# 4. Multivariate analysis                         ##
#####################################################

#4.1. Visualization of the total earned income  and the two categorical variables                 ##
#company turnover and family status.
#install.packages("dplyr")
#install.packages("ggpubr")

#first generate frequency tables:
table(sllvy, peas)
summary(svatva) 
summary(sllvy)
summary(peas)
#Since the number of observations in every group is not equal, this is not balanced design. 

#Descriptive statistics: means and sd's
#means
tapply(svatva,list(sllvy,peas),mean)
#sd's
tapply(svatva,list(sllvy,peas),sd)

#4.1.1. Box plot with multiple groups

g1 <- ggplot(data, aes(x = sllvy, y = svatva, col=peas))
g1 + geom_boxplot() + xlab("Company turnover group")+ ylab("Earned total income")

#4.1.2 Line plots with multiple groups
library(dplyr)
library(ggpubr)
# Line plots with multiple groups
# Plot y=svatva by groups (sllvy)
# Color box plot by a second group: peas
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
#ggline(data, x = "sllvy", y = "svatva", color = "peas", add = c("mean_se", "dotplot"), palette = 1:7)

ggline(data, x = "sllvy", y = "svatva", 
       xlab = "Company turnover group", ylab="Earned total income",  
       color = "peas",
       add = "mean_se", palette = 1:7)


#interaction.plot(x.factor = sllvy, trace.factor = peas, response = svatva, 
                 #fun = mean, type = "b", legend = TRUE, 
                 #xlab = "Company turnover group", ylab="Earned total income",
                 #pch=c(1,19), col=1:7)



#4.2. Two way ANOVA test is used to evaluate simultaneously the effect of the two categorical
#variables (sllvy and peas) on a response variable svatva. 

#Apply type II sum of squares method to run ANOVA.  
library(car)
#assume sllvy and peas are independent
anova_ind <- aov(svatva ~ sllvy + peas, data = data)
Anova(anova_ind, type = "II")

#include also inteaction term, apply type III sum of squares method 
anova_inter <- aov(svatva ~ sllvy + peas + sllvy:peas, data = data)
Anova(anova_inter, type = "III")
#''alias'' refers to the variables that are linearly dependent on others.
#produce warning about multicollinearity. 

#compute some summary statistics
model.tables(anova_ind, type="means")
#tapply(anova_ind$fitted.values,list(sllvy,peas),mean)
#tapply(anova_ind$fitted.values,list(sllvy,peas),sd)

#Multiple pairwise-comparison between the means of the groups: 
#Tukey Honest Significant Differences (THS)

TukeyHSD(anova_ind)

#Diagnostic plots to check the assumptions about normally distributed data and variance
#residual analysis
op <- par(mfrow = c(2, 2))
plot(anova_ind)
par(op)



#####################################################
# 5. Bivariate and multivariate analyses           ## 
#in the case of grouped                            ##
#levels of the categorical variabes                ##
#####################################################

#5.1. Group the levels of peas and sllvy
#Group the levels in family status)
#0 - single/child/unknown
data$peas[data$peas=='3']<-'0'
data$peas[data$peas=='9']<-'0'
#1 - head
data$peas[data$peas=='4']<-'1'
#2 - spouse
data$peas[data$peas=='5']<-'2'
#drop levels
data$peas<-droplevels(data$peas)
summary(data$peas)
#Group the levels in company turnover
levels(data$sllvy)
#1 - new group formed from 1,2,3.
data$sllvy[data$sllvy=='2']<-'1'
data$sllvy[data$sllvy=='3']<-'1'
#4 - new group formed from 4,5,6.
data$sllvy[data$sllvy=='5']<-'4'
data$sllvy[data$sllvy=='6']<-'4'
#7 - new group formed from 7,8,9.
data$sllvy[data$sllvy=='8']<-'7'
data$sllvy[data$sllvy=='9']<-'7'
data$sllvy<-droplevels(data$sllvy)
levels(data$sllvy)<-c("1", "2", "3")
summary(data$sllvy)

#5.2.1 Glimpse of the grouped data by plotting all investigated variables against each other
assignInNamespace("ggally_cor", ggally_cor, "GGally")
ggpairs(var, upper = list(continuous = wrap("cor", size = 10)), lower = list(continuous = "smooth"))

#5.2.2 Bivariate plots 
#box plots
boxplot(data$svatva~data$peas, notch=F, horizontal=T, xlab="Total earned income", ylab="Family status")
boxplot(data$svatva~data$sllvy, notch=F, horizontal=T, xlab="Total earned income", ylab="Company turnover group")
#bar plot
g0<-ggplot(data, aes(x = data$sllvy, fill = data$peas)) + geom_bar(position = "dodge")
g0 + xlab("Company turnover")

#5.3. Repeat all steps in multivariate ANOVA
#5.3.1. Visualization of the total earned income  and the two categorical variables                 ##
#company turnover and family status.

# generate frequency tables:
table(data$sllvy, data$peas)
summary(data$svatva) 
summary(data$sllvy)
summary(data$peas)
#Since the number of observations in every group is not equal, this is not balanced design. 

#Descriptive statistics: means and sd's
#means
tapply(data$svatva,list(data$sllvy,data$peas),mean)
#sd's
tapply(data$svatva,list(data$sllvy,data$peas),sd)

#Box plot with multiple groups
g1 <- ggplot(data, aes(x = data$sllvy, y = data$svatva, col=data$peas))
g1 + geom_boxplot() + xlab("Company turnover group")+ ylab("Earned total income")

#5.3.2. Line plots with multiple groups
library(dplyr)
library(ggpubr)

ggline(data, x = "sllvy", y = "svatva", 
       xlab = "Company turnover group", ylab="Earned total income",  
       color = "peas",
       add = "mean_se", palette = 1:7)


#5.3.3 Two way ANOVA test is used to evaluate simultaneously the effect of the two categorical
#variables (sllvy and peas) on a response variable svatva. 
#Apply type II sum of squares method to run ANOVA.  
library(car)
#assume sllvy and peas are independent
anova_ind <- aov(data$svatva ~ data$sllvy + data$peas, data = data)
Anova(anova_ind, type = "II")

#compute some summary statistics
#model.tables(anova_ind, type="means")
#tapply(anova_ind$fitted.values,list(sllvy,peas),mean)
#tapply(anova_ind$fitted.values,list(sllvy,peas),sd)

#Multiple pairwise-comparison between the means of the groups: 
#Tukey Honest Significant Differences (THS)
#TukeyHSD(anova_ind)

#Diagnostic plots to check the assumptions about normally distributed data and variance
#residual analysis
#op <- par(mfrow = c(2, 2))
#plot(anova_ind)
#par(op)

##The model with interaction term
#include also inteaction term, apply type III sum of squares method 
anova_inter <- aov(data$svatva ~ data$sllvy + data$peas + data$sllvy:data$peas, data = data)
Anova(anova_inter, type = "III")
#compute some summary statistics
model.tables(anova_inter, type="means")
#Multiple pairwise-comparison between the means of the groups: 
#Tukey Honest Significant Differences (THS)
TukeyHSD(anova_inter)
#Diagnostic plots to check the assumptions about normally distributed data and variance
#residual analysis
op1 <- par(mfrow = c(2, 2))
plot(anova_inter)
par(op1)

#####################################################
# 6. Graphical overview                            ##
#of some interesting columns.                      ##
# Preliminary analyses.                            ##
#####################################################

#6.1. Some data transformations
##change sukup and suuralue12 to factor
data1[,'sukup']<-factor(data1[,'sukup'])
levels(data1$sukup)
data1[,'SLHKY']<-factor(data1[,'SLHKY'])
levels(data1$SLHKY)
#remove na's in svatva and tyotu
data1<-data1[!is.na(data1$svatva),]
data_p<-data1[!is.na(data1$tyotu),]
#combine group levels in peas, sllvy and SLHKY #
#0 - single/child/unknown
data_p$peas[data_p$peas=='3']<-'0'
data_p$peas[data_p$peas=='9']<-'0'
#1 - head
data_p$peas[data_p$peas=='4']<-'1'
#2 - spouse
data_p$peas[data_p$peas=='5']<-'2'
#drop levels
data_p$peas<-droplevels(data_p$peas)
summary(data_p$peas)
#Group the levels in company turnover
levels(data_p$sllvy)
#1 - new group formed from 1,2,3.
data_p$sllvy[data_p$sllvy=='2']<-'1'
data_p$sllvy[data_p$sllvy=='3']<-'1'
#4 - new group formed from 4,5,6.
data_p$sllvy[data_p$sllvy=='5']<-'4'
data_p$sllvy[data_p$sllvy=='6']<-'4'
#7 - new group formed from 7,8,9.
data_p$sllvy[data_p$sllvy=='8']<-'7'
data_p$sllvy[data_p$sllvy=='9']<-'7'
data_p$sllvy<-droplevels(data_p$sllvy)
levels(data_p$sllvy)<-c("1", "2", "3")
summary(data_p$sllvy)
#Group the levels in company size (#of people)
levels(data_p$SLHKY)
#1 - new group formed from 1,2,3.
data_p$SLHKY[data_p$SLHKY=='2']<-'1'
data_p$SLHKY[data_p$SLHKY=='3']<-'1'
#4 - new group formed from 4,5,6.
data_p$SLHKY[data_p$SLHKY=='5']<-'4'
data_p$SLHKY[data_p$SLHKY=='6']<-'4'
#7 - new group formed from 7,8,9.
data_p$SLHKY[data_p$SLHKY=='8']<-'7'
data_p$SLHKY[data_p$SLHKY=='9']<-'7'
data_p$SLHKY<-droplevels(data_p$SLHKY)
levels(data_p$SLHKY)<-c("1", "2", "3")
summary(data_p$SLHKY)
#remove missing values 
data_p<-data_p[!is.na(data_p$tyotu),]

#6.2 plot chosen variables using the combined groups in peas, sllvy and SLHKY.
var1<-data_p[,c("svatva", "tyotu", "syntyv", "peas", "sllvy", "SLHKY")]
str(var1)
p <- ggpairs(var1, mapping = aes(col=data_p$sukup, alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)), cardinality_threshold=23)
p



#####################################################
# 7. Discussion and future work                    ##
# Comparing vuosi=2 with other subsets (vuosi=10)  ##
#####################################################

#Take year=10. Repeat the same subsetting procedure as for year=2.

#--------------------------------------------------------------
#1. Take a subset for year **vuosi**=10 from both original tables. 
data_workers10 <- subset(data_workers, vuosi==10)
dim(data_workers10)
data_firms10 <- subset(data_firms, vuosi== 10)
dim(data_firms10)

#2. Merge both data frames by its intersection=syrtun 
data1<-merge(data_workers10, data_firms10, by="syrtun")
dim(data1)
#2106 x 23
head(data1)

#3. Convert integer values to factors for the investigated categorical variables. 
data1[,'peas']<-factor(data1[,'peas'])
levels(data1$peas)

data1[,'a7lkm']<-factor(data1[,'a7lkm'])
levels(data1$a7lkm)

data1[,'a18lkm']<-factor(data1[,'a18lkm'])
levels(data1$a18lkm)

data1[,'SLHKY']<-factor(data1[,'SLHKY'])
levels(data1$SLHKY)

data1[,'sllvy']<-factor(data1[,'sllvy'])
levels(data1$sllvy)

#4. remove all missing values
data10<-na.omit(data1)
dim(data10)
#114 x 23 - not empty, but still too small subset.

sum(is.na(data1$peas))
#0
sum(is.na(data1$a7lkm))
#444
sum(is.na(data1$a18lkm))
#446
sum(is.na(data1$sllvy))
#0
sum(is.na(data1$svatva))
#16
sum(is.na(data1$sukup))
#0

#!is.na() deletes only the NA in particular column
data_10<-data1[!is.na(data1$svatva),]
#2090 x 23
data_a_10<-data[!is.na(data1$a7lkm),]
dim(data_a_10)
# 1662   23
data_b_10<-data[!is.na(data1$a18lkm),]
dim(data_b_10)
#1660 x 23
#--------------------------------------------------------

summary(data$svatva)
#Min. 1st Qu.  Median  Mean 3rd Qu.    Max. 
#  0   14000    18000  19434   23000   99000
summary(data_10$svatva)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    17000    25000    26490   33000  100000 

summary(data$sllvy)
#1   2   3   4   5   6   7   8   9 
#110  96  63 161 285 296 239 264 402 
summary(data_10$sllvy)
#1   2   3   4   5   6   7   8   9 
#238  56  72 177 320 310 229 296 392 

summary(data$peas)
#0    1    2   3   4   5   9 
#307 753 427 155 145 122   7 
summary(data_10$peas)
#  0   1   2   3   4   5   9 
# 432 681 388 165 240 174  10 

