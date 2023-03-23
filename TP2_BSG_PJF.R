#Loading the library and the dataset

library(tidyverse)
library(caTools)
library(ggplot2)
dataset=read.csv('bodyPerformance.csv')

#Explore the dataset
view(dataset)
head(dataset)
glimpse(dataset)
length(dataset)
names(dataset)
summary(dataset)

#Looking for any missing values
colSums(is.na(dataset))

#There is no missing data in this set so no imputation is needed.

#Statistical summary of the quantitative columns only
data_quantitative=dataset[c(3,4,5,6,7,8,9,10,11)]
summary(data_quantitative)

#Scatter Plot (Age vs Sit Up Count)
plot(dataset$age, dataset$sit.ups.counts, main="Age vs Sit Up Count",
     xlab="Age", ylab="Sit Up Count", pch=19)

#The scatter plot shows that from between the age 25 and 35 on average that age range is doing more sit ups then the other ages.

#Scatter Plot (Age vs Sit and Bend Forward Test)
plot(dataset$age, dataset$sit.and.bend.forward_cm, main="Age vs Sit and Ben Forward Test",
     xlab="Age", ylab="Sit and Bend Forward (CM)", pch=19)

#Ignoring the two outliers, which I belive to be mistakes in the entry of the data, we can see that age doesn't really affect the sit and bend forward ability between the different ages.

#Scatter plot (Age vs Broad Jump Distance)
plot(dataset$age, dataset$broad.jump_cm, main="Age vs Broad Jump Test",
     xlab="Age", ylab="Broad Jump Distance (CM)", pch=19)

#Looking at this scatter plot, we can examine that the younger the age the higher the broad jump. As the age progresses the distances decrease.

# install.packages("GGally")
library(GGally)

#Pair Plot
ggpairs(dataset,          # Data frame
        columns = 1:11) # Columns 

#Shows the correlation between all of the quantitative variables.
cor(data_quantitative,use="complete.obs", method="kendall")

#Correlation Matrix Heat map for all quantitative variables
cormat=round(cor(data_quantitative),2)
head(cormat)
library(reshape2)
#install.packages("reshape2")
melted_cormat=melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)

################ - Data Set Graphical Exploration - ################

#Age vs Body fat % to see if there is correlation between thr two variables
plot(dataset$age, dataset$body.fat_., main="Age vs Body Fat %",
     xlab="Age", ylab="Body Fat %", pch=19)

#Height vs Grip force to see if there is a relationship between "size" and the force
plot(dataset$height_cm, dataset$gripForce, main="Height vs Grip Force",
     xlab="Height (CM)", ylab="Grip Force",pch=19)

#Weight vs Grip force to see if there is a relationship between "size" and force
plot(dataset$weight_kg, dataset$gripForce, main="Weight vs Grip Force",
     xlab="Weight (KG)", ylab="Grip Force",pch=19)

#Weight vs Grip force to see if there is a relationship between 
plot(dataset$weight_kg, dataset$systolic, main="Weight vs Systolic",
     xlab="Weight (KG)", ylab="Systolic",pch=19)

#Weight vs Diastolic to see if there is a relationship between weight and blood-pressure/diastolic 
plot(dataset$weight_kg, dataset$diastolic, main="Weight vs Diastolic",
     xlab="Weight (KG)", ylab="Diastolic",pch=19)

