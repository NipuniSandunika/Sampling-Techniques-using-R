#install.packages("sampling")
#install.packages("ggplot2")
#install.packages("sampler")
#install.packages("tidyverse")
#install.packages("survey")

library(survey)

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


#No of observations
No_Obs = nrow(df)
No_Obs


library(sampler)



sample_Size = rsampcalc(nrow(df),e = 3, ci = 95, p = 0.5)
sample_Size

##sample 1
set.seed(10000)
sample_srs_1 = rsamp(df,sample_Size,over=0,rep=FALSE)
View(sample_srs_1)

##Estimating mean,proportion,total, and their standard error under srs
library(survey)

#defining survey design objects
srs_design = svydesign(id =~1 , weights = NULL, data=sample_srs_1)
srs_design

##Weights.......
weight = No_Obs/sample_Size
weight

######Estimation......

#Estimating the population mean of co2 emission 
svymean(~CO2_Emissions,srs_design)

#Estimating population total of co2 emission
svytotal(~CO2_Emissions,srs_design)

#Estimating population proportion of cylinders
svymean(~as.numeric(Cylinders == "3"),srs_design)
svymean(~as.numeric(Cylinders == "4"),srs_design)
svymean(~as.numeric(Cylinders == "5"),srs_design)
svymean(~as.numeric(Cylinders == "6"),srs_design)
svymean(~as.numeric(Cylinders == "8"),srs_design)
svymean(~as.numeric(Cylinders == "10"),srs_design)
svymean(~as.numeric(Cylinders == "12"),srs_design)
svymean(~as.numeric(Cylinders == "16"),srs_design)

###########Actual population values

#population mean of co2 emission
mean(df$CO2_Emissions)

#we estimated mean as 249.29 and the actual mean is 251.3126.

#population total of co2 emission
sum(df$CO2_Emissions)
#we estimated total as 131872 but actual total is 262873

#population proportion of cylinders
table(df$Cylinders)/No_Obs

#for number of cylinders 3, we got estimated value as 0.017013 but actual value is 0.01434
#for number of cylinders 4, we got estimated value as 0.43667 but actual value is 0.43021
#for number of cylinders 5, we got estimated value as 0 but actual value is 0.00191
#for number of cylinders 6, we got estimated value as 0.35917 but actual value is 0.34704
#for number of cylinders 8, we got estimated value as 0.16068 but actual value is 0.18260
#for number of cylinders 10, we got estimated value as 0.00756 but actual value is 0.00574
#for number of cylinders 12, we got estimated value as 0.018904 but actual value is 0.01721
#for number of cylinders 16, we got estimated value as 0 but actual value is 0.00096


######Regression Estimation.................

cor(df$CO2_Emissions,df$Fuel_Consumption_Comb)
#correlation coefficient is 0.9364927 for co2 emission and 
#combined fuel consumption. Therefore there's a strong positive 
#linear relationship between two variables.

plot(sample_srs_1$Fuel_Consumption_Comb,sample_srs_1$CO2_Emissions,
     type = "p",main = "Co2 emission vs combined fuel consumption",ylab="co2 emission",
     xlab="combined fuel emission")
line_co = line(sample_srs_1$Fuel_Consumption_Comb,sample_srs_1$CO2_Emissions)
abline(coef(line_co))

regression_model = svyglm(formula = CO2_Emissions~Fuel_Consumption_Comb,design = srs_design)
regression_model

#Since intercept = 31.60
#We can consider the data well fitted by a straight not the origin.
#Thus regression estimation works best

#Fitting the linear regression model
y_bar_hat_reg1 = 31.60 + 20.23*mean(df$Fuel_Consumption_Comb)
y_bar_hat_reg1
mean(df$CO2_Emissions) #actual population mean of CO2 Emission

##Graphical analysis

library(tidyverse)
library(ggplot2)

#pie charts
pie(sample_Size,main = "Distribution of Fuel Type across the sample 1")

#boxplots
ggplot(sample_srs_1,aes(x = Fuel_Type,y = CO2_Emissions)) +
  geom_boxplot() +
  ggtitle("Distribution of Co2 Emission among strata for sample 1") +
  xlab("Fuel Type") +
  ylab("Co2 Emission")

ggplot(sample_srs_1,aes(x = Fuel_Type,y = Fuel_Consumption_Comb)) +
  geom_boxplot() +
  ggtitle("Distribution of Fuel_Consumption_Comb among strata for sample 1") +
  xlab("Fuel Type") +
  ylab("Fuel_Consumption_Comb")

#Histogram
ggplot(sample_srs_1,aes(x = CO2_Emissions)) +
  geom_histogram(color= "white",fill = "darkblue",binwidth=20) +
  ggtitle("Distribution of Co2 Emission in sample 1") +
  xlab("Co2 Emission")

#bar plots
bar_1 = ggplot(sample_srs_1,aes(x = Vehicle_Class))
bar_1+geom_bar(fill="black")+labs(title="Vehicle class distribution for sample 1")
bar_2 = ggplot(sample_srs_1,aes(x = Transmission))
bar_2+geom_bar(fill="black")+labs(title="Transmission distribution for sample 1")
bar_3 = ggplot(sample_srs_1,aes(x = Make))
bar_3+geom_bar(fill="black")+labs(title="Make distribution for sample 1")

#Contingency table
t1 = table(sample_srs_1$Make,sample_srs_1$Fuel_Type)
t1




#########Sample 2...........
set.seed(230000)
sample_srs_2 = rsamp(df,sample_Size,over=0,rep=FALSE)
View(sample_srs_2)

##Estimating mean,proportion,total, and their standard error under srs
library(survey)

#defining survey design objects
srs_design2 = svydesign(id =~1 , weights = NULL, data=sample_srs_2)
srs_design2

##Weights.......
weight = No_Obs/sample_Size
weight

######Estimation......

#Estimating the population mean of co2 emission 
svymean(~CO2_Emissions,srs_design2)

#Estimating population total of co2 emission
svytotal(~CO2_Emissions,srs_design2)

#Estimating population proportion of cylinders
svymean(~as.numeric(Cylinders == "3"),srs_design2)
svymean(~as.numeric(Cylinders == "4"),srs_design2)
svymean(~as.numeric(Cylinders == "5"),srs_design2)
svymean(~as.numeric(Cylinders == "6"),srs_design2)
svymean(~as.numeric(Cylinders == "8"),srs_design2)
svymean(~as.numeric(Cylinders == "10"),srs_design2)
svymean(~as.numeric(Cylinders == "12"),srs_design2)
svymean(~as.numeric(Cylinders == "16"),srs_design2)

###########Actual population values

#population mean of co2 emission
mean(df$CO2_Emissions)

#we estimated mean as 249.29 and the actual mean is 251.3126.

#population total of co2 emission
sum(df$CO2_Emissions)
#we estimated total as 131872 but actual total is 262873

#population proportion of cylinders
table(df$Cylinders)/No_Obs

#for number of cylinders 3, we got estimated value as 0.017013 but actual value is 0.01434
#for number of cylinders 4, we got estimated value as 0.43667 but actual value is 0.43021
#for number of cylinders 5, we got estimated value as 0 but actual value is 0.00191
#for number of cylinders 6, we got estimated value as 0.35917 but actual value is 0.34704
#for number of cylinders 8, we got estimated value as 0.16068 but actual value is 0.18260
#for number of cylinders 10, we got estimated value as 0.00756 but actual value is 0.00574
#for number of cylinders 12, we got estimated value as 0.018904 but actual value is 0.01721
#for number of cylinders 16, we got estimated value as 0 but actual value is 0.00096


######Regression Estimation.................

cor(df$CO2_Emissions,df$Fuel_Consumption_Comb)
#correlation coefficient is 0.9364927 for co2 emission and 
#combined fuel consumption. Therefore there's a strong positive 
#linear relationship between two variables.

plot(sample_srs_2$Fuel_Consumption_Comb,type = "p",sample_srs_2$CO2_Emissions,
     main = "Co2 emission vs combined fuel consumption",ylab="co2 emission",
     xlab="combined fuel emission")
line_co = line(sample_srs_2$Fuel_Consumption_Comb,sample_srs_2$CO2_Emissions)
abline(coef(line_co))


regression_model = svyglm(formula = CO2_Emissions~Fuel_Consumption_Comb,design = srs_design2)
regression_model

#Since intercept = 43.1
#We can consider the data well fitted by a straight not the origin.
#Thus regression estimation works best

#Fitting the linear regression model
y_bar_hat_reg2 = 43.1+19.1 *mean(df$Fuel_Consumption_Comb)
y_bar_hat_reg2
mean(df$CO2_Emissions) #actual population mean of CO2 Emission



##Graphical analysis

library(tidyverse)
library(ggplot2)

#pie charts
pie(sample_Size,main = "Distribution of Fuel Type across the sample 2")

#boxplots
ggplot(sample_srs_2,aes(x = Fuel_Type,y = CO2_Emissions)) +
  geom_boxplot() +
  ggtitle("Distribution of Co2 Emission among strata for sample 2") +
  xlab("Fuel Type") +
  ylab("Co2 Emission")

ggplot(sample_srs_2,aes(x = Fuel_Type,y = Fuel_Consumption_Comb)) +
  geom_boxplot() +
  ggtitle("Distribution of Fuel_Consumption_Comb among strata for sample 2") +
  xlab("Fuel Type") +
  ylab("Fuel_Consumption_Comb")

#Histogram
ggplot(sample_srs_2,aes(x = CO2_Emissions)) +
  geom_histogram(color= "white",fill = "darkblue",binwidth=20) +
  ggtitle("Distribution of Co2 Emission in sample 2") +
  xlab("Co2 Emission")

#bar plots
bar_1 = ggplot(sample_srs_2,aes(x = Vehicle_Class))
bar_1+geom_bar(fill="black")+labs(title="Vehicle class distribution for sample 2")
bar_2 = ggplot(sample_srs_2,aes(x = Transmission))
bar_2+geom_bar(fill="black")+labs(title="Transmission distribution for sample 2")
bar_3 = ggplot(sample_srs_2,aes(x = Make))
bar_3+geom_bar(fill="black")+labs(title="Make distribution for sample 2")

#Contingency table
t1 = table(sample_srs_2$Make,sample_srs_2$Fuel_Type)
t1
