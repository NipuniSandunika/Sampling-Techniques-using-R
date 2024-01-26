#install.packages("sampling")
#install.packages("ggplot2")
#install.packages("sampler")
#install.packages("tidyverse")
#install.packages("survey")

#import the data

setwd("C:/Users/nipun/Documents/3rd Year/1st Semester/IS 3001 -  Sampling Techniques/Group Assignment")
getwd()

df= read.csv("Edited Fuel consumption 2019.csv")
backup.df=df
summary(df)

#Factories following
df$Make=as.factor(df$Make)
df$Vehicle_Class=as.factor(df$Vehicle_Class)
df$Transmission=as.factor(df$Transmission)
df$Fuel_Type=as.factor(df$Fuel_Type)
df$Cylinders=as.factor(df$Cylinders)
summary(df)

#We consider CO2_Emissions
mean(df$CO2_Emissions)
sum(df$CO2_Emissions)

#Total number of Population Units
N=nrow(df)

#For our clustering sample we consider Vehicle_Make
vm=unique(df$Vehicle_Class)

#Select 8 random clusters from 15 vehicles
M=length(vm)
m=8
fpc1=(1-m/M)


#_______________________1st Sample___________________________________________
set.seed(10000)
cluster.sample=sample(unique(df$Vehicle_Class),8,replace = FALSE)
cluster.sample

library(dplyr)

cluster_population=filter(df, Vehicle_Class %in% cluster.sample)

#Calculating Sample Sizes
tw=table(cluster_population$Vehicle_Class)
tw=tw[tw!=0]
N1=c()
n1=c()
fpc=c()
s=aggregate(CO2_Emissions~Vehicle_Class,sd,data = cluster_population)

library(sampler)

#sample_Size = rsampcalc(nrow(df),e = 3, ci = 95, p = 0.5)
#calculating sample sizes fpc
for(i in 1:length(cluster.sample)){
  n0=rsampcalc(nrow(df),e = 3, ci = 95, p = 0.5)
  N1[i]=tw[i]
  n1[i]=ceiling(n0/(1+n0/N1[i]))
  fpc[i]=1-n1[i]/N1[i]
}
N1
n1

#Calculating Sample weights
df.sample.unique=data.frame(names(tw),N1,n1,fpc)
df.sample.unique$weights=(df.sample.unique$N1/df.sample.unique$n1)*(M/m)
df.sample.unique

cluster_population[cluster_population$Vehicle_Class==df.sample.unique$names.tw.[1],]

#Adding weights column to cluster data frame
weights=1:nrow(cluster_population)
fpc2=1:nrow(cluster_population)
for (i in 1:length(cluster_population$Vehicle_Class)){
  
  weights[cluster_population$Vehicle_Class==df.sample.unique$names.tw.[i]]=df.sample.unique$weights[i]
  
  fpc2[cluster_population$Vehicle_Class==df.sample.unique$names.tw.[i]]=df.sample.unique$fpc[i]
}
cluster_population$weights=weights
cluster_population$fpc=fpc1
cluster_population$fpc2=fpc2

#2nd stage of clustering
set.seed(10000)

test=cluster_population %>%filter(Vehicle_Class==df.sample.unique$names.tw.[1])
clus1=test[sample(1:df.sample.unique$N1[1],df.sample.unique$n1[1],replace = FALSE),]
clusmain=clus1

for (i in 2:length(df.sample.unique$names.tw.)){
  test=cluster_population %>%filter(Vehicle_Class==df.sample.unique$names.tw.[i])
  clus=test[sample(1:df.sample.unique$N1[i],df.sample.unique$n1[i],replace = FALSE),]
  clusmain=rbind(clusmain,clus)
  
}

#using survey
library(survey)

clus_design= svydesign(id=~Vehicle_Class+ID, weights=~weights, data=clusmain, 
                       fpc = ~fpc + fpc2)
summary(clus_design)


#Population Mean from Cluster Sampling
svymean(~CO2_Emissions,clus_design)

#Population Total from Cluster Sampling
svytotal(~CO2_Emissions,clus_design)

#Proportion of Cylinders
cat("Proportion vehicles with certain number of cylinders")
svymean(~Cylinders,clus_design)


#___Actual Population Parameters

#Population Mean
mean(df$CO2_Emissions)

#Population Total
sum(df$CO2_Emissions)

##Regression Estimation for sample 1
plot(clus1$Fuel_Consumption_Comb,clus1$CO2_Emissions)
line_co1 = line(clus1$Fuel_Consumption_Comb,clus1$CO2_Emissions)
abline(coef(line_co1))

regression_model1 = svyglm(formula = CO2_Emissions~Fuel_Consumption_Comb,design = clus_design)
regression_model1

r1 = cor(clus1$CO2_Emissions,clus1$Fuel_Consumption_Comb)
r1

#Since intercept = 21.14
#We can consider the data well fitted by a straight not the origin.
#Thus regression estimation works best

#Fitting the linear regression model
y_bar_hat_reg1 =  21.14 + 21.37  *mean(df$Fuel_Consumption_Comb)
y_bar_hat_reg1

mean(df$CO2_Emissions) #actual population mean of Co2 Emission

#Graphical Analysis
library(tidyverse)
library(ggplot2)

#Barplots
bar_1 = ggplot(clusmain,aes(x = Vehicle_Class))
bar_1+geom_bar(fill="red")+labs(title="Vehicle class distribution in cluster sample 1")

bar_2 = ggplot(clusmain,aes(x = Cylinders))
bar_2+geom_bar(fill="blue")+labs(title="Vehicle class distribution in cluster sample 1")


#histogram
ggplot(clusmain,aes(x = CO2_Emissions)) +
  geom_histogram(color= "white",fill = "darkblue",binwidth=20) +
  ggtitle("Distribution of Co2 Emission in sample 1") +
  xlab("Co2 Emission")

#Box-Plots
ggplot(clusmain,aes(x = Vehicle_Class,y = CO2_Emissions)) +
  geom_boxplot() +
  ggtitle("Distribution of Co2 Emission among strata for sample 1") +
  xlab("Vehicle class") +
  ylab("Co2 Emission")



#_______________________2nd Sample___________________________________________
set.seed(230000)

cluster.sample=sample(unique(df$Vehicle_Class),8,replace = FALSE)
cluster.sample

library(dplyr)

cluster_population=filter(df, Vehicle_Class %in% cluster.sample)
#Calculating Sample Sizes
tw=table(cluster_population$Vehicle_Class)
tw=tw[tw!=0]
N1=c()
n1=c()
fpc=c()
s=aggregate(CO2_Emissions~Vehicle_Class,sd,data = cluster_population)

library(sampler)

#sample_Size = rsampcalc(nrow(df),e = 3, ci = 95, p = 0.5)
#calculating sample sizes fpc
for(i in 1:length(cluster.sample)){
  n0=rsampcalc(nrow(df),e = 3, ci = 95, p = 0.5)
  N1[i]=tw[i]
  n1[i]=ceiling(n0/(1+n0/N1[i]))
  fpc[i]=1-n1[i]/N1[i]
}

N1
n1

#Calculating Sample weights
df.sample.unique=data.frame(names(tw),N1,n1,fpc)
df.sample.unique$weights=(df.sample.unique$N1/df.sample.unique$n1)*(M/m)
df.sample.unique

cluster_population[cluster_population$Vehicle_Class==df.sample.unique$names.tw.[1],]

#Adding weights column to cluster data frame
weights=1:nrow(cluster_population)
fpc2=1:nrow(cluster_population)
for (i in 1:length(cluster_population$Vehicle_Class)){
  
  weights[cluster_population$Vehicle_Class==df.sample.unique$names.tw.[i]]=df.sample.unique$weights[i]
  
  fpc2[cluster_population$Vehicle_Class==df.sample.unique$names.tw.[i]]=df.sample.unique$fpc[i]
}

cluster_population$weights=weights
cluster_population$fpc=fpc1
cluster_population$fpc2=fpc2

#2nd stage of clustering
set.seed(230000)
test=cluster_population %>% filter(Vehicle_Class==df.sample.unique$names.tw.[1])
clus1=test[sample(1:df.sample.unique$N1[1],df.sample.unique$n1[1],replace = FALSE),]
clusmain=clus1
for (i in 2:length(df.sample.unique$names.tw.)){
  test=cluster_population %>%filter(Vehicle_Class==df.sample.unique$names.tw.[i])
  clus=test[sample(1:df.sample.unique$N1[i],df.sample.unique$n1[i],replace = FALSE),]
  clusmain=rbind(clusmain,clus)
  
}

#using survey
library(survey)

clus_design= svydesign(id=~Vehicle_Class+ID, weights=~weights, data=clusmain, 
                       fpc = ~fpc + fpc2)
summary(clus_design)

#Population Mean from Cluster Sampling
svymean(~CO2_Emissions,clus_design)

#Population Total from Cluster Sampling
svytotal(~CO2_Emissions,clus_design)

#Proportion of Cylinders
cat("Proportion vehicles with certain number of cylinders")
svymean(~Cylinders,clus_design)

#___Actual Population Parameters

#Population Mean
mean(df$CO2_Emissions)

#Population Total
sum(df$CO2_Emissions)

##Regression Estimation for sample 2
plot(clus1$Fuel_Consumption_Comb,clus1$CO2_Emissions)
line_co2 = line(clus1$Fuel_Consumption_Comb,clus1$CO2_Emissions,)
abline(coef(line_co2))

regression_model2 = svyglm(formula = CO2_Emissions~Fuel_Consumption_Comb,design = clus_design)
regression_model2

r2 = cor(clus1$CO2_Emissions,clus1$Fuel_Consumption_Comb)
r2

#Since intercept = 39.08 
#We can consider the data well fitted by a straight not the origin.
#Thus regression estimation works best

#Fitting the linear regression model
y_bar_hat_reg1 =  39.08 + 19.73 *mean(df$Fuel_Consumption_Comb)
y_bar_hat_reg1

mean(df$CO2_Emissions) #actual population mean of Co2 Emission

#Graphical Analysis
library(tidyverse)
library(ggplot2)

#Barplots
bar_1 = ggplot(clusmain,aes(x = Vehicle_Class))
bar_1+geom_bar(fill="red")+labs(title="Vehicle class distribution in cluster sample 2")

bar_2 = ggplot(clusmain,aes(x = Cylinders))
bar_2+geom_bar(fill="blue")+labs(title="Vehicle class distribution in cluster sample 2")


#histogram
ggplot(clusmain,aes(x = CO2_Emissions)) +
  geom_histogram(color= "white",fill = "darkblue",binwidth=20) +
  ggtitle("Distribution of Co2 Emission in sample 2") +
  xlab("Co2 Emission")

#Box-Plots
ggplot(clusmain,aes(x = Vehicle_Class,y = CO2_Emissions)) +
  geom_boxplot() +
  ggtitle("Distribution of Co2 Emission among strata for sample 2") +
  xlab("vehicle class") +
  ylab("Co2 Emission")


