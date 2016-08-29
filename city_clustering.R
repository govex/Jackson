## The content of this project is licensed under the Creative Commons Attribution 4.0 International (CC BY 4.0).
## https://creativecommons.org/licenses/by/4.0/

## This R script is part of the Jackson Analytics Kickstart Challenge A project.
## The goal is to use demographic, economic, and geographic data to identify cities that are similar to Jackson, Mississippi.
## Analysis methods include general exploration and hierarchical clustering.

## This file is also designed to be a tutorial for users wanting to replicate and maniupulate the analysis.

## Download city indicator dataset
## https://drive.google.com/open?id=0B9FLZ57ziQq5UVZrQnJOYkFpMHc

## Assign variable names to dataset columns.
## Make sure the variables are stored in the appropriate data type.
## Categorical variables (names, regions, etc.) should be stored as factors.
## Number variables should be automatically recognized.

## Scale the features by min max range to compress the feature distances to between 0 and 1
## Based on http://stackoverflow.com/questions/5665599/range-standardization-0-to-1-in-r

## Load data
data <- read.csv("~/Downloads/city_indicator_dataset.csv")

## Run basic descriptive statistics on the dataset.
## This function shows the mean, median, etc, of the observations in each column of the dataset.
summary(data)


library(dummies)
mmscalar <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

low_weight = 5
med_weight = 10
high_weight = 50

x0 <- as.character.factor(data$CityName)
x1 <- mmscalar(data$X2013pop) * high_weight
x2 <- mmscalar(data$Growth) * high_weight
x3 <- mmscalar(data$Median.Income) * low_weight
x4 <- mmscalar(data$Poverty) * low_weight
x5 <-  mmscalar(data$Nonwhite) * low_weight
x6 <-  mmscalar(data$Land.Area.Sq.M) * high_weight
x7 <-  mmscalar(data$Density_pop_sq_km) * high_weight
# na.rm = T passes the NAs through the calculation so they can be imputed later
x8 <-  mmscalar(data$FT_emp, na.rm = T)
x9 <-  mmscalar(data$FT_pay, na.rm = T)
x10 <- mmscalar(data$PT_emp, na.rm = T)
x11 <- mmscalar(data$PT_pay, na.rm = T)
x12 <- mmscalar(data$FTE, na.rm= T)
x13 <-(as.factor(data$State_Capital))

## Convert categorical variables into dummy variables
x14 <-(as.factor(data$Region))
x14 <- dummy(data$Region, data = NULL, fun = as.factor)
x14 <- as.data.frame(x14)
x14 <- x14[,-3]


## Combine into dataframe

library(DMwR)
library(ggplot2)
x.impute <- knnImputation((x <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)), k = 10, scale = T, meth = "weighAvg",
                          distData = NULL)
x.impute <- cbind(x.impute, x13,x15)

## The R functions we will use later require data to be stored as data frames, so we need to convert the imputed datasets into data frames.
x.impute.df <- as.data.frame(x.impute)

library(cluster)
x.diana <- diana(x.impute.df)
x.agnes <- agnes(x.impute.df)


## Cluster Models
## The following functions calculate the similarity between the two cluster methods.
library(factoextra)
library(dendextend)

##Dendrograms
dend1 <- as.dendrogram(x.diana)
dend2 <- as.dendrogram(x.agnes)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = TRUE,
           common_subtrees_color_lines = TRUE,
           common_subtrees_color_branches = TRUE,
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

## Hierarchical Clustering
## Reduce font size
par(cex=.3)

plot(x.agnes, labels = data$City)


plot(x.diana, labels = data$City)
