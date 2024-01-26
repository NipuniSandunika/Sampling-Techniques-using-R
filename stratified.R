#install.packages("sampling")
#install.packages("ggplot2")
#install.packages("sampler")
#install.packages("tidyverse")
#install.packages("survey")

#import the data

setwd("C:/Users/nipun/Documents/3rd Year/1st Semester/IS 3001 -  Sampling Techniques/Group Assignment")
getwd()
df=as.data.frame(read.table("C:\\Users\\nipun\\Documents\\3rd Year\\1st Semester\\IS 3001 -  Sampling Techniques\\Group Assignment\\Edited Fuel consumption 2019.csv",sep =",",header = TRUE))
head(df)


#Factories following

df$Make=as.factor(df$Make)
df$Vehicle_Class=as.factor(df$Vehicle_Class)
df$Transmission=as.factor(df$Transmission)
df$Fuel_Type=as.factor(df$Fuel_Type)

summary(df)


##Checking for stratification variable

aggregate(CO2_Emissions~Make,mean,data=df)
aggregate(CO2_Emissions~Vehicle_Class,mean,data=df)
aggregate(CO2_Emissions~Transmission,mean,data=df)
aggregate(CO2_Emissions~Fuel_Type,mean,data=df)

aggregate(CO2_Emissions~Make,sum,data=df)
aggregate(CO2_Emissions~Vehicle_Class,sum,data=df)
aggregate(CO2_Emissions~Transmission,sum,data=df)
aggregate(CO2_Emissions~Fuel_Type,sum,data=df)

#ANOVA test One way

#Ho : Each levels means are equal
#H1 : At least one mean is different from each levels

one.way1 = aov(CO2_Emissions~ Make, data = df)
summary(one.way1)

one.way2 = aov(CO2_Emissions~ Vehicle_Class, data = df)
summary(one.way2)

one.way3 = aov(CO2_Emissions~ Transmission, data = df)
summary(one.way3)

one.way4 = aov(CO2_Emissions~ Fuel_Type, data = df)
summary(one.way4)

#Since p_value < significant level(5%) we reject H0 at 5% level
#Therefore at least one mean is different from others
#And also totals and means of co2 emission are significantly differ across the fuel types
#than other categorical variables,
#We can use "Fuel_Type" as our stratification variable.

##population sample units
Nh = table(df$Fuel_Type)

#finding population variance
std.fuel = tapply(df$CO2_Emissions,df$Fuel_Type,sd)
#sd() function gives sample standard deviation of the strata 

Sh = sqrt((std.fuel)^2*(Nh-1)/(Nh)) #population standard deviation by strata
pop_var_by_strata = Sh^2

#Variances differ heavily between strata and let's assume costs are equal,
#Thus Neymann Allocation
#Finding sample size
#Using Neymann Allocation getting value for v
N = sum(Nh)
product = Sh*Nh
summation = sum(product)
X = summation/product

##Calculating sample sizes n
Y = numeric(0)
for (i in 1:length(Nh)){
  Y[i] = X[i]*((Nh[i])/N)^2*pop_var_by_strata[i]
}
v = sum(Y)
v

#Let's consider e = 3
e = 3
library(sampler)
n_srs =rsampcalc(N,e) #Sample size for SRS
pop_var = var(df$CO2_Emissions)*(N-1)/N  #Variance of the population
n = (n_srs*v)/pop_var
ceiling(n)

#Neymann Allocation
nh = ceiling((product/summation)*n)
nh



##Weights
weight = Nh/nh
df2 = data.frame(weight)
var_name = c("Fuel_Type","Weight")
names(df2)[names(df2)==c("Var1" ,"Freq")] = var_name
df2
df3 = merge(df,df2,by ="Fuel_Type") #merge weights data frame with our data set
head(df3)

###Obtaining random sample from the population

#_______________________1st Sample___________________________________________

set.seed(10000)
samp1 = numeric(0)
U = names(nh)
for (i in 1:length(nh)){
  samp1 = c(samp1,sample((1:N)[df3$Fuel_Type==U[i]],size=nh[i],replace=FALSE))
}
sample1 = df3[samp1,]
#check that we have the correct stratum sample sizes
table(sample1$Fuel_Type)
head(sample1)

##Estimating population values

library(survey)

#Defining the survey design object
strat_design1 = svydesign(ID~1,strata = ~ Fuel_Type,weight = ~Weight,data = sample1)

#Estimating the population mean of Co2 emission
svymean(~CO2_Emissions,strat_design1)

#Estimating the population total of Co2 emission
svytotal(~CO2_Emissions,strat_design1)

#Estimating the population proportion of Cylinders

proportions13 = svymean(~as.numeric(Cylinders == "3"), strat_design1)
proportions14 = svymean(~as.numeric(Cylinders == "4"), strat_design1)
proportions15 = svymean(~as.numeric(Cylinders == "5"), strat_design1)
proportions16 = svymean(~as.numeric(Cylinders == "6"), strat_design1)
proportions18 = svymean(~as.numeric(Cylinders == "8"), strat_design1)
proportions110 = svymean(~as.numeric(Cylinders == "10"), strat_design1)
proportions112 = svymean(~as.numeric(Cylinders == "12"), strat_design1)
proportions116 = svymean(~as.numeric(Cylinders == "16"), strat_design1)


##Actual population values

#Population mean of Co2 emission
mean(df3$CO2_Emissions)

#Population total of Co2 emission
sum(df3$CO2_Emissions)

#Population proportion of Cylinders
table(df3$Cylinders)/N

##Regression Estimation for sample 1
plot(sample1$Fuel_Consumption_Comb,sample1$CO2_Emissions)
line_co1 = line(sample1$Fuel_Consumption_Comb,sample1$CO2_Emissions)
abline(coef(line_co1))

regression_model1 = svyglm(formula = CO2_Emissions~Fuel_Consumption_Comb,design = strat_design1)
regression_model1

r1 = cor(sample1$Fuel_Consumption_Comb,sample1$CO2_Emissions)
r1

#Since intercept = 35.50
#We can consider the data well fitted by a straight not the origin.
#Thus regression estimation works best


#Fitting the linear regression model
y_bar_hat_reg1 =  35.50 + 19.89*mean(df3$Fuel_Consumption_Comb)
y_bar_hat_reg1

mean(df3$CO2_Emissions) #actual population mean of CO2 Emission


##Graphical analysis

library(tidyverse)
library(ggplot2)

#pie charts
pie(nh,main = "Distribution of Fuel Type across the sample 1")

#boxplots
ggplot(sample1,aes(x = Fuel_Type,y = CO2_Emissions)) +
  geom_boxplot() +
  ggtitle("Distribution of Co2 Emission among strata for sample 1") +
  xlab("Fuel Type") +
  ylab("Co2 Emission")

ggplot(sample1,aes(x = Fuel_Type,y = Fuel_Consumption_Comb)) +
  geom_boxplot() +
  ggtitle("Distribution of Fuel_Consumption_Comb among strata for sample 1") +
  xlab("Fuel Type") +
  ylab("Fuel_Consumption_Comb")

#Histogram
ggplot(sample1,aes(x = CO2_Emissions)) +
  geom_histogram(color= "white",fill = "darkblue",binwidth=20) +
  ggtitle("Distribution of Co2 Emission in sample 1") +
  xlab("Co2 Emission")

#bar plots
bar_1 = ggplot(sample1,aes(x = Vehicle_Class))
bar_1+geom_bar(fill="black")+labs(title="Vehicle class distribution for sample 1")
bar_2 = ggplot(sample1,aes(x = Transmission))
bar_2+geom_bar(fill="black")+labs(title="Transmission distribution for sample 1")
bar_3 = ggplot(sample1,aes(x = Make))
bar_3+geom_bar(fill="black")+labs(title="Make distribution for sample 1")

#Contingency table
t1 = table(sample1$Make,sample1$Fuel_Type)
t1


#_______________________2nd Sample___________________________________________

set.seed(230000)
samp2 = numeric(0)
U = names(nh)
for (i in 1:length(nh)){
  samp2 = c(samp2,sample((1:N)[df3$Fuel_Type==U[i]],size=nh[i],replace=FALSE))
}
sample2 = df3[samp2,]
#check that we have the correct stratum sample sizes
table(sample2$Fuel_Type)
head(sample2)

##Estimating population values

library(survey)

#Defining the survey design object
strat_design2 = svydesign(ID~1,strata = ~ Fuel_Type,weight = ~Weight,data = sample2)

#Estimating the population mean of Co2 emission
svymean(~CO2_Emissions,strat_design2)

#Estimating the population total of Co2 emission
svytotal(~CO2_Emissions,strat_design2)

#Estimating the population proportion of Cylinders
proportions23 = svymean(~as.numeric(Cylinders == "3"), strat_design2)
proportions24 = svymean(~as.numeric(Cylinders == "4"), strat_design2)
proportions26 = svymean(~as.numeric(Cylinders == "6"), strat_design2)
proportions28 = svymean(~as.numeric(Cylinders == "8"), strat_design2)
proportions210 = svymean(~as.numeric(Cylinders == "10"), strat_design2)
proportions212 = svymean(~as.numeric(Cylinders == "12"), strat_design2)
proportions216 = svymean(~as.numeric(Cylinders == "16"), strat_design2)


##Actual population values

#Population mean of Co2 emission
mean(df3$CO2_Emissions)

#Population total of Co2 emission
sum(df3$CO2_Emissions)

#Population proportion of Cylinders
table(df3$Cylinders)/N

##Regression Estimation for sample 2
plot(sample2$Fuel_Consumption_Comb,sample2$CO2_Emissions)
line_co2 = line(sample2$Fuel_Consumption_Comb,sample2$CO2_Emissions)
abline(coef(line_co2))

regression_model2 = svyglm(formula = CO2_Emissions~Fuel_Consumption_Comb,design = strat_design2)
regression_model2

r2 = cor(sample2$Fuel_Consumption_Comb,sample2$CO2_Emissions)
r2

#Since intercept = 36.28
#We can consider the data well fitted by a straight not the origin.
#Thus regression estimation works best


#Fitting the linear regression model
y_bar_hat_reg2 =  36.28 + 19.77*mean(df3$Fuel_Consumption_Comb)
y_bar_hat_reg2
mean(df3$CO2_Emissions) #actual population mean of CO2 Emission

##Graphical analysis

library(tidyverse)
library(ggplot2)

#pie charts
pie(nh,main = "Distribution of Fuel Type across the sample 2")


#boxplots
ggplot(sample2,aes(x = Fuel_Type,y = CO2_Emissions)) +
  geom_boxplot() +
  ggtitle("Distribution of Co2 Emission among strata for sample 2") +
  xlab("Fuel Type") +
  ylab("Co2 Emission")

ggplot(sample2,aes(x = Fuel_Type,y = Fuel_Consumption_Comb)) +
  geom_boxplot() +
  ggtitle("Distribution of Fuel_Consumption_Comb among strata for sample 2") +
  xlab("Fuel Type") +
  ylab("Fuel_Consumption_Comb")

#Histogram
ggplot(sample2,aes(x = CO2_Emissions)) +
  geom_histogram(color= "white",fill = "darkblue",binwidth=20) +
  ggtitle("Distribution of Co2 Emission in sample 2") +
  xlab("Co2 Emission")

#bar plots
bar_4 = ggplot(sample2,aes(x = Vehicle_Class))
bar_4+geom_bar(fill="blue")+labs(title="Vehicle class distribution for sample 2")
bar_5 = ggplot(sample2,aes(x = Transmission))
bar_5+geom_bar(fill="blue")+labs(title="Transmission distribution for sample 2")
bar_5 = ggplot(sample2,aes(x = Make))
bar_5+geom_bar(fill="blue")+labs(title="Make distribution for sample 2")

#Contingency table
t2 = table(sample2$Make,sample2$Fuel_Type)
t2
