# Chipotle-Cluster-Analysis

## Libraries Used
```
library(readxl)
library(ggplot2)
library(reshape2)
```
## Data Loading
```
my_data <- read_excel("Desktop/GSB 516/Week 1/Clustering/Chipotle Excel SheetIDENTIFIED.xlsx")
```
## Summary 
```
head(my_data)
```

## Data Cleaning and Conversions
```
my_dataclean <- na.omit(my_data) #omit NA's 
head(my_dataclean)

my_dataclean <- my_dataclean[-c(1)] #indicate that data column (column 1) does not have numbers so we can normalize the rest

my_datanormalized <- scale(my_dataclean) #scale the numerical data so each variable (column) is normalized for Euclidean distance calculations
head(my_datanormalized)

my_datanormalized <- as.data.frame(my_datanormalized) #convert to data frame
```
## Setting Seed 
```
set.seed(5)
```

## Cluster analysis 
```
k <- kmeans(my_datanormalized[,c('spending','plan','importantprice')], 5, iter.max = 5000)
k

my_dataclean$cluster <- k$cluster
```      

## Mean Analysis
```
clustermeans <- aggregate(my_dataclean[,c('patronage')],  
                          list(my_dataclean$cluster), mean)
clustermeans #OUTCOME 

clustermeans_DEMOGRAPHIC <- aggregate(my_dataclean[,c('female','income','age')],  
                                        list(my_dataclean$cluster), mean)
clustermeans_DEMOGRAPHIC

clustermeans_PRODUCT <- aggregate(my_dataclean[,c('chipotlevariety','chipotlehealthy','chipotletaste')],  
                          list(my_dataclean$cluster), mean)
clustermeans_PRODUCT

clustermeans_PLACE <- aggregate(my_dataclean[,c('chipotleconvenient','chipotleambience')],  
                                  list(my_dataclean$cluster), mean)
clustermeans_PLACE

clustermeans_PRICE <- aggregate(my_dataclean[,c('chipotleprice')],  
                                list(my_dataclean$cluster), mean)
clustermeans_PRICE

clustermeans_PROMOTION <- aggregate(my_dataclean[,c('wom','sm','walk','billboard')],  
                                list(my_dataclean$cluster), mean)
clustermeans_PROMOTION
````

## Visualization
```
p <- ggplot(clustermeans, aes(cluster, female)) + geom_bar(stat = "identity", color = "blue", fill = "blue") 
# show the plot just created
p
p + coord_flip()

p <- ggplot(clustermeans, aes(cluster, female)) + geom_bar(stat = "identity", color = "darkgreen", fill = "yellow") 
p 

p <- ggplot(clustermeans, aes(cluster, female)) + geom_bar(stat = "identity", color = "blue", fill = "white") + geom_text(aes(label= round(female, digits = 2)), vjust= -.3, size=3.5) + theme_minimal()
p 
p <- ggplot(clustermeans, aes(cluster, age)) + geom_bar(stat = "identity", color = "blue", fill = "white") + geom_text(aes(label= round(age, digits = 1)), vjust= -.3, size=3.5) + theme_minimal()
p 
p <- ggplot(clustermeans, aes(cluster, income)) + geom_bar(stat = "identity", color = "blue", fill = "white") + geom_text(aes(label= round(income, digits = 0)), vjust= -.3, size=3.5) + theme_minimal()
p 
```

## Cluster Means & Pre-Processing
```
clustermeans <- aggregate(my_dataclean[,c('patronage')],  
                          list(my_dataclean$cluster), mean)
clustermeans

clustermeans2 <- aggregate(my_dataclean[,c('importanthealthy','importantvariety', 'importantprice')],  list(my_dataclean$cluster), mean)
clustermeans2
colnames(clustermeans2)[1] <- "cluster"
clustermeans2

clustermeans2.long<-melt(clustermeans2,id.vars = "cluster")
head(clustermeans2.long)

clustermeans3 <- aggregate(my_dataclean[,c('chipotlehealthy','chipotlevariety', 'chipotleprice')],  list(my_dataclean$cluster), mean)
clustermeans3
colnames(clustermeans3)[1] <- "cluster"
clustermeans3

clustermeans3.long<-melt(clustermeans3,id.vars = "cluster")
head(clustermeans3.long)
```

## Visualization 2
```
p <- ggplot(clustermeans2.long, aes(x=variable, y = value, fill = factor(cluster))) + geom_bar(stat = "identity", position="dodge") + scale_fill_discrete(name="Cluster", breaks = c(1,2,3), labels = c("1", "2", "3")) + xlab("") + ylab("Mean")  
p 

p <- ggplot(clustermeans3.long, aes(x=variable, y = value, fill = factor(cluster))) + geom_bar(stat = "identity", position="dodge") + scale_fill_discrete(name="Cluster", breaks = c(1,2,3), labels = c("1", "2", "3")) + xlab("") + ylab("Mean")  
p
```



