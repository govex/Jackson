## The content of this project is licensed under the Creative Commons Attribution 4.0 International (CC BY 4.0).
## https://creativecommons.org/licenses/by/4.0/

####### Changes over original ########
## add a min-max scalar for values
######################################

########## Results ###########
## Cluster cities -> Montgomery AL, Nashville, Richmond VA, Atlanta
## Entanglement = 0.31
## Divisive Coefficient = 0.96
######################################



## This R script is part of the Jackson Analytics Kickstart Challenge A project.
## The goal is to use demographic, economic, and geographic data to identify cities that are similar to Jackson, Mississippi. 
## Analysis methods include general exploration and hierarchical clustering.

## This file is also designed to be a tutorial for users wanting to replicate and maniupulate the analysis. 

## Download city indicator dataset 
## https://drive.google.com/open?id=0B9FLZ57ziQq5UVZrQnJOYkFpMHc

## Load data
data <- read.csv("~/Downloads/city_indicator_dataset.csv")

## Run basic descriptive statistics on the dataset. 
## This function shows the mean, median, etc, of the observations in each column of the dataset.
summary(data)

## Assign variable names to dataset columns. 
## Make sure the variables are stored in the appropriate data type. 
## Categorical variables (names, regions, etc.) should be stored as factors.
## Number variables should be automatically recognized.

## Scale the features by min max range to compress the feature distances to between 0 and 1
## Based on http://stackoverflow.com/questions/5665599/range-standardization-0-to-1-in-r

library(dummies)
mmscalar <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

low_weight = 5
med_weight = 10
high_weight = 50

x0 <- as.character.factor(data$CityName)
x1 <- mmscalar(data$X2013pop) * high_weight
x2 <- mmscalar(data$Growth) * med_weight
x3 <- mmscalar(data$Median.Income) * med_weight
x4 <- mmscalar(data$Poverty) * med_weight
x5 <-  mmscalar(data$Nonwhite) * med_weight
x6 <-  mmscalar(data$Land.Area.Sq.M) * med_weight
x7 <-  mmscalar(data$Density_pop_sq_km) * low_weight
# na.rm = T passes the NAs through the calculation so they can be imputed later
x8 <-  mmscalar(data$FT_emp, na.rm = T) 
x9 <-  mmscalar(data$FT_pay, na.rm = T)
x10 <- mmscalar(data$PT_emp, na.rm = T)
x11 <- mmscalar(data$PT_pay, na.rm = T)
x12 <- mmscalar(data$FTE, na.rm= T)

## Convert categorical variables into dummy variables
x13 <-(as.factor(data$State_Capital))
x13 <- dummy(data$State_Capital, data = NULL, fun = as.factor)


x15 <-(as.factor(data$Region))
x15 <- dummy(data$Region, data = NULL, fun = as.factor)
x15 <- as.data.frame(x15)
x15 <- x15[,-3]


## Combine into dataframe

library(DMwR)
library(ggplot2)
x.impute <- knnImputation((x <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)), k = 10, scale = T, meth = "weighAvg",
                          distData = NULL)
x.impute.df <- cbind(x.impute, x13,x15)

## The R functions we will use later require data to be stored as data frames, so we need to convert the imputed datasets into data frames. 
x.impute.df <- as.data.frame(x.impute)

library(cluster)
x.diana <- diana(x.impute.df)
x.agnes <- agnes(x.impute.df)


## Cluster Models
## The following functions calculate the similarity between the two cluster methods. 
library(factoextra)
library(dendextend)
dend1 <- as.dendrogram(x.diana)
dend2 <- as.dendrogram(x.agnes)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = TRUE, 
           common_subtrees_color_lines = TRUE, 
           common_subtrees_color_branches = TRUE,  
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

## Hierarchical Cluster Plots

## Reduce font size
par(cex=.3)

plot(x.agnes, labels = data$City)

plot(x.diana, labels = data$City)


