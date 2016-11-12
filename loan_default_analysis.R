#load useful libraries
library(ggplot2)
library(plyr)
library(moments)
library(gmodels)
library(GGally)
library(psych)
library(corrgram)
library(stats)
library(plotly)
library(dplyr)
library(gridExtra)

#############################################################################################################
#
#                               CHECKPOINT 1 : DATA CLEANING AND PREPARATION
#
#############################################################################################################


# ***************************************************************************
# Load the file loan.csv into data frame loan.
# ***************************************************************************

#Read the file into R
loan <- read.csv(file.choose(), stringsAsFactors = F)
str(loan)

#Check for invalid rows
which(is.na(loan$member_id))
loan <- loan[-c(which(is.na(loan$member_id))),]
#Overview of data
str(loan)

#SUbset loan data with just driver columns
loan_subset <- subset(loan, select = c("id","member_id","annual_inc","loan_amnt","funded_amnt","int_rate","grade",
                                       "dti","emp_length","purpose","home_ownership","loan_status"))
loan_subset$member_id <- as.character(loan_subset$member_id)

# *************************************************************************** 
# Remove rows with loan_status  = "Fully Paid"
# ***************************************************************************

unique(loan_subset$loan_status)
#Identify and remove the Extra statements
loan_subset[which(loan_subset$loan_status == "Does not meet the credit policy. Status:Charged Off"),"loan_status"] <- "Charged Off"
loan_subset[which(loan_subset$loan_status == "Does not meet the credit policy. Status:Fully Paid"),"loan_status"] <- "Fully Paid"
loan_subset$loan_status <- as.factor(loan_subset$loan_status)
unique(loan_subset$loan_status)
#Removing "Fully Paid"
loan_subset <- loan_subset[c(which(loan_subset$loan_status!="Fully Paid")),]

# ***************************************************************************
# Impute the NA values for all driver variables.
# ***************************************************************************

#---------------
# 1. annual_inc
# --------------
str(loan_subset$annual_inc)
summary(loan_subset$annual_inc)
sum(is.na(loan_subset$annual_inc))
#There are no NA values. No treatment needed

#---------------
# 2. dti
# --------------

str(loan_subset$dti)
summary(loan_subset$dti)
sum(is.na(loan_subset$dti))
# There are  0 NA values. No treatment needed

#---------------
# 3. emp_length
# --------------

str(loan_subset$emp_length)
unique(loan_subset$emp_length)
#Remove values with emp_length as n/a
loan_subset <- loan_subset[c(which(loan_subset$emp_length != "n/a")),]
unique(loan_subset$emp_length)
loan_subset$emp_length <- as.factor(loan_subset$emp_length)
#Check for the levels
levels(loan_subset$emp_length)
summary(loan_subset$emp_length)


#---------------
# 4. funded_amnt
# --------------

str(loan_subset$funded_amnt)
sum(is.na(loan_subset$funded_amnt))
# No NA values. No treatment needed

#---------------
# 5. grade
# --------------

str(loan_subset$grade)
#Converting it into factor variable
unique(loan_subset$grade)
loan_subset$grade <- as.factor(loan_subset$grade)
summary(loan_subset$grade)
#No empty strings or na values. No treatment needed

#---------------
# 6. home_ownership
# --------------
str(loan_subset$home_ownership)
#Converting it into factor variable
unique(loan_subset$home_ownership)
loan_subset$home_ownership <- as.factor(loan_subset$home_ownership)
summary(loan_subset$home_ownership)

#No empty strings or na values. No treatment needed

#---------------
# 7. int_rate
# --------------
str(loan_subset$int_rate)

#Converting the int_rate into numeric type by removing the %
loan_subset$int_rate <- gsub("%","",loan_subset$int_rate)
loan_subset$int_rate <- as.double(loan_subset$int_rate)

summary(loan_subset$int_rate)

sum(is.na(loan_subset$int_rate))
#No na values. no treatment needed

#---------------
# 8. loan_amnt
# --------------

str(loan$loan_amnt)
summary(loan$loan_amnt)
sum(is.na(loan$loan_amnt))

# No na values. no treatment needed

#---------------
# 9. loan_status
# --------------

str(loan$loan_status)
unique(loan$loan_status)
loan_subset$loan_status <- as.factor(loan_subset$loan_status)
plyr::count(loan_subset$loan_status)
#No na values. No treatment needed

#---------------
# 10. purpose
# --------------
str(loan_subset$purpose)
unique(loan_subset$purpose)
loan_subset$purpose <- as.factor(loan_subset$purpose)
plyr::count(loan_subset$purpose)
#No na values. no treatment needed


# ***************************************************************************
# Create a new column named loan_status_1  with three levels current_new, default_new and late such that
# rows with loan_status as âcurrentâ or âin grace periodâ are tagged as  "current_new"
# rows with loan_status as â defaultâ or âcharged offâ are tagged as "default_new"
# rows with loan_status as â late 16- 30 daysâ âlate 31-120 daysâ are tagged as "late"
# ***************************************************************************

loan_subset$loan_status_1 <- ""
loan_subset[which(loan_subset$loan_status %in% c("Current", "In Grace Period")), "loan_status_1"] <- "current_new"
loan_subset[which(loan_subset$loan_status %in% c("Default", "Charged Off")), "loan_status_1"] <- "default_new"
loan_subset[which(loan_subset$loan_status %in% c("Late (16-30 days)", "Late (31-120 days)")), "loan_status_1"] <- "late"

unique(loan_subset$loan_status_1)
loan_subset$loan_status_1 <- as.factor(loan_subset$loan_status_1)
plyr::count(loan_subset$loan_status_1)
# ***************************************************************************
# Create new bin variables for int_rate and emp_length respectively as follows:
#  Create int_rate_grp such that 
#    int_rate < 10 is tagged âLowâ; int_rate (10-18) is tagged âMediumâ; int_rate (>18) is tagged âHighâ
# Create emp_len_grp such that 
#    emp_length (0-4) is tagged as âJuniorâ; 
#    emp_length (5-8) is tagged as âMid-levelâ; emp_length (>8) is tagged as âSeniorâ.
# ***************************************************************************

loan_subset$int_rate_grp <- ""
loan_subset[which(loan_subset$int_rate <= 10),"int_rate_grp"] <- "Low"
loan_subset[which(loan_subset$int_rate > 10 & loan_subset$int_rate <= 18),"int_rate_grp"] <- "Medium"
loan_subset[which(loan_subset$int_rate > 18),"int_rate_grp"] <- "High"
unique(loan_subset$int_rate_grp)
loan_subset$int_rate_grp <- factor(loan_subset$int_rate_grp, levels = c("Low","Medium","High"))
plyr::count(loan_subset$int_rate_grp)

unique(loan_subset$emp_length)
loan_subset$emp_len_grp <- ""
loan_subset[which(loan_subset$emp_length %in% c("< 1 year","1 year","2 years","3 years","4 years")),"emp_len_grp"] <- "Junior"
loan_subset[which(loan_subset$emp_length %in% c("5 years","6 years","7 years","8 years")),"emp_len_grp"] <- "Mid-level"
loan_subset[which(loan_subset$emp_length %in% c("9 years","10+ years")),"emp_len_grp"] <- "Senior"
unique(loan_subset$emp_len_grp)
loan_subset$emp_len_grp <- factor(loan_subset$emp_len_grp, levels = c("Junior","Mid-level","Senior"))
plyr::count(loan_subset$emp_len_grp)

#############################################################################################################
#
#                               CHECKPOINT 2 : EXPLORATORY DATA ANALYSIS
# 
# Summary Statistics Provided for Continuous Variables are :
#      Mean, Standard Deviation, Median, Trimmed Mean, Minimum, Maximum, Skewness, Kurtosis, Standard Error
# Outlier Analysis Done Using
#     qqnorm, qqline, boxplot
# Summary Statistics for Categorical Variables
#     Cross Table
#
#############################################################################################################
#---------------
# 1. annual_inc
# --------------

# Summary Stats
describe(loan_subset$annual_inc)
quantile(loan_subset$annual_inc)
qqnorm(loan_subset$annual_inc,main="Annual Income -- Q-Q Plot before treatment")
qqline(loan_subset$annual_inc, col="red")

length(boxplot.stats(loan_subset$annual_inc)$out)
boxplot(loan_subset$annual_inc, main="Annual Income -- Boxplot before treatment")

#Get distribution of the plot
#plots
plot_before_Treatment <- ggplot(loan_subset, aes(annual_inc)) + 
  geom_histogram(binwidth = 5000, col="black", aes(y=..density.., fill=..count..)) +
  scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C") +
  labs( x="Annual Income", y="#Loans given") +
  ggtitle("Annual Income -- Before Outlier Removal") +
  theme (plot.title=element_text(size = 15, face = "bold", colour = "red")) +
  stat_function(fun=dnorm, col = "red", args=list(mean=mean(loan_subset$annual_inc), sd=sd(loan_subset$annual_inc))) +
  geom_vline(aes(xintercept=mean(loan_subset$annual_inc)),color="orange", linetype="dashed", size=1)

#Getting values to Identify Outliers
IQR_Annual_Inc <- quantile(loan_subset$annual_inc,probs=0.75) - quantile(loan_subset$annual_inc,probs=0.25)
UCV_Annual_Inc <- quantile(loan_subset$annual_inc,probs=0.75) + 1.5*IQR_Annual_Inc
LCV_Annual_Inc <- quantile(loan_subset$annual_inc,probs=0.25) - 1.5*IQR_Annual_Inc
LCV_Annual_Inc <- 0 # since 1.5*IQR will result in negative value


outliers <- loan_subset[which(loan_subset$annual_inc > UCV_Annual_Inc),]
# The number of outliers is very less when compared to the total no of records. Hence, we remove these records. Also unclear if these are genuine outliers
loan_subset <- loan_subset[loan_subset$annual_inc < UCV_Annual_Inc,]

#plot after treatment
plot_after_Treatment <- ggplot(loan_subset, aes(annual_inc)) + 
  geom_histogram(binwidth = 5000, col="black", aes(y=..density.., fill=..count..)) +
  scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C") +
  labs( x="Annual Income", y="#Loans given") +
  ggtitle("Annual Income -- After Outlier Removal") +
  theme (plot.title=element_text(size = 15, face = "bold", colour = "red")) +
  stat_function(fun=dnorm, col = "red", args=list(mean=mean(loan_subset$annual_inc), sd=sd(loan_subset$annual_inc))) +
  geom_vline(aes(xintercept=mean(loan_subset$annual_inc)),color="orange", linetype="dashed", size=1)

boxplot(loan_subset$annual_inc, main="Annual Income -- After Treatment")
qqnorm(loan_subset$annual_inc,main="Annual Income -- Q-Q Plot after treatment")
qqline(loan_subset$annual_inc, col="red")

grid.arrange(plot_before_Treatment,plot_after_Treatment,nrow=2, ncol=1)


#---------------
# 2. dti
# --------------

#Summary Statistics
describe(loan_subset$dti)
quantile(loan_subset$dti)
qqnorm(loan_subset$dti,main="Normal Q-Q Plot for dti")
qqline(loan_subset$dti, col="red")

length(boxplot.stats(loan_subset$dti)$out)
boxplot(loan_subset$dti,main="Boxplot for dti")

#PLots to show the distribution
ggplot(loan_subset, aes(dti)) + 
  geom_histogram(binwidth=1, col="black", aes(y=..density.., fill=..count..)) +
  scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C") +
  labs( x="DTI", y="#COUNT") +
  ggtitle("Debt to Income(DTI) plot") +
  theme (plot.title=element_text(size = 15, face = "bold", colour = "red")) +
  stat_function(fun=dnorm, col = "red", args=list(mean=mean(loan_subset$dti), sd=sd(loan_subset$dti))) + 
  geom_vline(aes(xintercept=mean(loan_subset$dti)),color="orange", linetype="dashed", size=1)



#---------------
# 3. emp_length_grp
# --------------

#This is a categorical data. Outlier treatment doesn't apply.

str(loan_subset$emp_len_grp)
summary(loan_subset$emp_len_grp)
ggplot(loan_subset, aes(emp_len_grp)) + 
  geom_bar(col="blue", fill=c("orange")) +
  labs( x="Loan level", y="#COUNT") +
  ggtitle("Employee Length") +
  theme (plot.title=element_text(size = 15, face = "bold", colour = "red"))

CrossTable(loan_subset$emp_len_grp)
table(loan_subset$emp_len_grp)
#categorical data. no need for outlier treatment

#---------------
# 4. loan amount
# --------------
describe(loan_subset$loan_amnt)

quantile(loan_subset$loan_amnt)
qqnorm(loan_subset$loan_amnt,main="Normal Q-Q Plot for Loan Amt")
qqline(loan_subset$loan_amnt, col="red")

length(boxplot.stats(loan_subset$loan_amnt)$out)
boxplot(loan_subset$loan_amnt,main="Boxplot for Loan Amt")

# Since the outliers are the max amount loaned, they are considered valid.

ggplot(loan_subset, aes(loan_amnt)) + 
  geom_histogram(binwidth=2500, col="black", aes(y=..density.., fill=..count..)) +
  scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C") +
  labs( x="Loan Amount", y="#COUNT") +
  ggtitle("Loan Amount plot") +
  theme (plot.title=element_text(size = 15, face = "bold", colour = "red")) +
  stat_function(fun=dnorm, col = "red", args=list(mean=mean(loan_subset$loan_amnt), sd=sd(loan_subset$loan_amnt))) + 
  geom_vline(aes(xintercept=mean(loan_subset$loan_amnt)),color="orange", linetype="dashed", size=1)

#---------------
# 5. funded_amt
# --------------

describe(loan_subset$funded_amnt)

quantile(loan_subset$funded_amnt)
qqnorm(loan_subset$funded_amnt,main = "Normal Q-Q Plot for Funded Amt")
qqline(loan_subset$funded_amnt, col="red")

length(boxplot.stats(loan_subset$funded_amnt)$out)
boxplot(loan_subset$funded_amnt,main="Boxplot for Funded Amt")
boxplot.stats(loan_subset$funded_amnt)$out
ggplot(loan_subset, aes(funded_amnt)) + 
  geom_histogram(binwidth=2500, col="black", aes(y=..density.., fill=..count..)) +
  scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C") +
  labs( x="Funded Amount", y="#COUNT") +
  ggtitle("Funded Amount plot") +
  theme (plot.title=element_text(size = 15, face = "bold", colour = "red")) +
  stat_function(fun=dnorm, col = "red", args=list(mean=mean(loan_subset$funded_amnt), sd=sd(loan_subset$funded_amnt))) + 
  geom_vline(aes(xintercept=mean(loan_subset$funded_amnt)),color="orange", linetype="dashed", size=1)


#Since the funded amount directly depends on the loan amount and cannot be greater than the loan amount, 
# these are not outliers and are hence retained with original values.


#---------------
# 6. grade
# --------------

str(loan_subset$grade)
summary(loan_subset$grade)

ggplot(loan_subset, aes(loan_subset$grade)) + 
  geom_bar(colour="blue", fill="orange") +
  theme (plot.title=element_text(size = 15, face = "bold", colour = "red")) +
  labs(x="Grade", y="#Count")+ggtitle("Loan Grades distribution")
CrossTable(loan_subset$grade)

#categorical data. no need for outlier treatment

#---------------
# 7. home_ownership
# --------------

str(loan_subset$home_ownership)
summary(loan_subset$home_ownership)
ggplot(loan_subset, aes(loan_subset$home_ownership)) + 
  geom_bar(colour="blue", fill="orange") +
  theme (plot.title=element_text(size = 15, face = "bold", colour = "red")) +
  labs(x="Home Ownership", y="#Count")+ggtitle("Home Ownership distribution")
CrossTable(loan_subset$home_ownership)

#most applicants either have mortgaged or rented residencies

#---------------
# 8. interest rate group
# --------------
str(loan_subset$int_rate_grp)
summary(loan_subset$int_rate_grp)
ggplot(loan_subset, aes(loan_subset$int_rate_grp)) + 
  geom_bar(colour="blue", fill="orange") +
  theme (plot.title=element_text(size = 15, face = "bold", colour = "red")) +
  labs(x="Interest Rate Group", y="#Count")+ggtitle("Interest Group distribution")
CrossTable(loan_subset$int_rate_grp)

#---------------
# 9. loan_status group
# --------------
str(loan_subset$loan_status_1)
summary(loan_subset$loan_status_1)
ggplot(loan_subset, aes(loan_subset$loan_status_1)) +   
  geom_bar(colour="blue", fill="orange") +
  labs(x="Loan Status Group", y="#Count") + 
  theme(plot.title=element_text(size = 15, face = "bold", colour = "red"))+
  ggtitle("Loan Status Group plot")

#very few customers who are late. most of them are defaulters.

#---------------
# 10. purpose
# --------------
str(loan_subset$purpose)
summary(loan_subset$purpose)
ggplot(loan_subset, aes(loan_subset$purpose)) + 
  geom_bar(colour="blue", fill="orange") +
  theme(plot.title=element_text(size = 15, face = "bold", colour = "red"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="Purpose", y="#Count")+ggtitle("Purpose distribution")
#debt consolidation is the most significant.

######################################
#
#     Multi Variate Analysis
#
######################################

# continuous variables are dti, funded_amnt, loan_amnt, annual_inc

cor(loan_subset[sapply(loan_subset, is.numeric)])
ggpairs(data = loan_subset, columns = c("loan_amnt","funded_amnt","dti","annual_inc"))
corrgram(loan_subset)



#Categorical are loan_status_1, emp_len_grp, int_rate_grp

# Distributions
# Across loan Status
loan_Status_annual_inc <- ggplot(data = loan_subset, aes(x = annual_inc, fill = loan_status_1)) + 
  geom_histogram(binwidth=10000, colour="blue", fill="orange") +
  ggtitle("Annual Income across Loan Status") +
  labs(x="Annual Income", y="#Count") + 
  facet_wrap(~loan_status_1)
loan_Status_loan_amnt <- ggplot(data = loan_subset, aes(x = loan_amnt, fill = loan_status_1)) + 
  geom_histogram(binwidth=5000, colour="blue", fill="orange") + 
  labs(x="Loan Amount", y="#Count") + 
  ggtitle("Loan Amount across Loan Status") +
  facet_wrap(~loan_status_1)
loan_status_funded_amnt <- ggplot(data = loan_subset, aes(x = funded_amnt, fill = loan_status_1)) +
  geom_histogram(binwidth=5000, colour="blue", fill="orange") + 
  labs(x="Funded Amount", y="#Count") + 
  ggtitle("Funded Amount across Loan Status") +
  facet_wrap(~loan_status_1)
loan_status_dti <- ggplot(data = loan_subset, aes(x = dti, fill = loan_status_1)) +
  geom_histogram(binwidth=1, colour="blue", fill="orange") + 
  ggtitle("DTI across Loan Status") +
  labs(x="DTI", y="#Count") + 
  facet_wrap(~loan_status_1)


grid.arrange(loan_Status_annual_inc, loan_status_dti, nrow=1, ncol=2)
grid.arrange(loan_Status_loan_amnt, loan_status_funded_amnt, nrow=1, ncol=2)

# Across Interest Rates
int_rate_grp_annual_inc <- ggplot(data = loan_subset, aes(x = annual_inc, fill = int_rate_grp)) + 
  geom_histogram(binwidth=10000, colour="blue", fill="orange") + 
  labs(x="Annual Income", y="#Count") +  
  ggtitle("Annual Income across Interest Rate Group") +
  facet_wrap(~int_rate_grp)
int_rate_grp_loan_amnt <- ggplot(data = loan_subset, aes(x = loan_amnt, fill = int_rate_grp)) + 
  geom_histogram(binwidth=5000, colour="blue", fill="orange") + 
  labs(x="Loan Amount", y="#Count") +
  ggtitle("Loan Amount across Interest Rate Group") +
  facet_wrap(~int_rate_grp)
int_rate_grp_funded_amnt <- ggplot(data = loan_subset, aes(x = funded_amnt, fill = int_rate_grp)) + 
  geom_histogram(binwidth=5000, colour="blue", fill="orange") + 
  labs(x="Funded Amount", y="#Count") + 
  ggtitle("Funded Amount across Interest Rate Group") +
  facet_wrap(~int_rate_grp)
int_rate_grp_dti <- ggplot(data = loan_subset, aes(x = dti, fill = int_rate_grp)) + 
  geom_histogram(binwidth=1, colour="blue", fill="orange") + 
  labs(x="DTI", y="#Count") + 
  ggtitle("DTI across Interest Rate Group") +
  facet_wrap(~int_rate_grp)

grid.arrange(int_rate_grp_annual_inc, int_rate_grp_dti, nrow=1, ncol=2)
grid.arrange(int_rate_grp_loan_amnt, int_rate_grp_funded_amnt, nrow=1, ncol=2)


#############################################################################################################
#
#                               CHECKPOINT 3 : HYPOTHESIS TESTING
# Below Steps followed
# Step 1 : Set up the Hypothesis and determine the level of significance(Type -1 )
# Step 2 : Carry out the sampling exercise and calculate the sample statistics.
# Step 3 : Select the appropriate Test Statistic.
# Step 4 : Set up the decision criteria.
# Step 5 : Test Statistic
# Step 6 : Take a Decision
#
#############################################################################################################

#--------------
# 1. loan Status
#--------------

# Test hypotheses (95 % conf. level) for two levels of loan_status_1 - default_new and current_new
#Get Samples
defaults_sample <- subset(loan_subset,loan_subset$loan_status_1=="default_new")
current_sample <- subset(loan_subset,loan_subset$loan_status_1=="current_new")
#Perform Test Using var.equal=FALSE
t.test(x = defaults_sample$loan_amnt, y = current_sample$loan_amnt, alternative = "two.sided", mu = 0, conf.level = 0.95 )
t.test(x = defaults_sample$funded_amnt, y = current_sample$funded_amnt, alternative = "two.sided", mu = 0, conf.level = 0.95 )
t.test(x = defaults_sample$dti, y = current_sample$dti, alternative = "two.sided", mu = 0, conf.level = 0.95 )
t.test(x = defaults_sample$annual_inc, y = current_sample$annual_inc, alternative = "two.sided", mu = 0, conf.level = 0.95 )


#--------------
# 2. interest rate group
#--------------

# Test hypotheses (95 % conf. level) for two levels of int_rate_grp - high and low
#Get Samples
high_int_sample <- subset(loan_subset,loan_subset$int_rate_grp=="High")
low_int_sample <-  subset(loan_subset,loan_subset$int_rate_grp=="Low")
#Perform T Test using var.equal=False
t.test(x = high_int_sample$loan_amnt, y = low_int_sample$loan_amnt, alternative = "two.sided", mu = 0, conf.level = 0.95 )
t.test(x = high_int_sample$funded_amnt, y = low_int_sample$funded_amnt, alternative = "two.sided", mu = 0, conf.level = 0.95 )
t.test(x = high_int_sample$dti, y = low_int_sample$dti, alternative = "two.sided", mu = 0, conf.level = 0.95 )
t.test(x = high_int_sample$annual_inc, y = low_int_sample$annual_inc, alternative = "two.sided", mu = 0, conf.level = 0.95 )


write.csv(loan_subset,"loan_subset.csv")
save(loan_subset,file = "loan_subset.RData")



