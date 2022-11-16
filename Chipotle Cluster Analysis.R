library(readxl)
my_data <- read_excel("Desktop/GSB 516/Week 1/Clustering/Chipotle Excel SheetIDENTIFIED.xlsx")

# assign data frame to my_data
head(my_data)

# get rid of NA rows
my_dataclean <- na.omit(my_data)
head(my_dataclean)

# indicate that data column (column 1) does not have numbers so we can normalize the rest
my_dataclean <- my_dataclean[-c(1)]

# scale the numerical data so each variable (column) is normalized for Euclidean distance calculations
my_datanormalized <- scale(my_dataclean)
head(my_datanormalized)

#convert the results to data frame
my_datanormalized <- as.data.frame(my_datanormalized)



# this sets the starting place to a common seed for the whole class, rather than relying on a random seed, which could produce different results for each student
set.seed(5)

#######################################################################
#His in class example
k <- kmeans(my_datanormalized[,c('female','age','income')], 3, iter.max = 5000)
########################################################################

# run cluster analysis (Our Example):
k <- kmeans(my_datanormalized[,c('spending','plan','importantprice')], 5, iter.max = 5000)
k
#######################################################################



# create a new data column using the resulting cluster that kmeans assigned
my_dataclean$cluster <- k$cluster

#######################################################################

# RUN MEAN ANALYSIS FOR OUTCOME VARIABLE:
clustermeans <- aggregate(my_dataclean[,c('patronage')],  
                          list(my_dataclean$cluster), mean)
clustermeans

#######################################################################

#RUN MEAN ANALYSIS FOR DEMOGRAPHIC VARIABLES:
clustermeans_DEMOGRAPHIC <- aggregate(my_dataclean[,c('female','income','age')],  
                                        list(my_dataclean$cluster), mean)
clustermeans_DEMOGRAPHIC

#######################################################################
#RUN MEAN ANALYSIS FOR DIFFERENT MARKETING MIX VARIABLES:

#PRODUCT:
clustermeans_PRODUCT <- aggregate(my_dataclean[,c('chipotlevariety','chipotlehealthy','chipotletaste')],  
                          list(my_dataclean$cluster), mean)
clustermeans_PRODUCT

#PLACE:
clustermeans_PLACE <- aggregate(my_dataclean[,c('chipotleconvenient','chipotleambience')],  
                                  list(my_dataclean$cluster), mean)
clustermeans_PLACE

#PRICE:
clustermeans_PRICE <- aggregate(my_dataclean[,c('chipotleprice')],  
                                list(my_dataclean$cluster), mean)
clustermeans_PRICE

#PROMOTION:
clustermeans_PROMOTION <- aggregate(my_dataclean[,c('wom','sm','walk','billboard')],  
                                list(my_dataclean$cluster), mean)
clustermeans_PROMOTION


#######################################################################





#rename the column
colnames(clustermeans)[1] <- "cluster"
clustermeans

#install the plot package
install.packages("ggplot2")
library(ggplot2)

#create some bar plots
p <- ggplot(clustermeans, aes(cluster, female)) + geom_bar(stat = "identity", color = "blue", fill = "blue") 
# show the plot just created
p
# flip the plot
p + coord_flip()

#try it in different colors
p <- ggplot(clustermeans, aes(cluster, female)) + geom_bar(stat = "identity", color = "darkgreen", fill = "yellow") 
p 
#try it with even more color and number detail 
p <- ggplot(clustermeans, aes(cluster, female)) + geom_bar(stat = "identity", color = "blue", fill = "white") + geom_text(aes(label= round(female, digits = 2)), vjust= -.3, size=3.5) + theme_minimal()
p 
p <- ggplot(clustermeans, aes(cluster, age)) + geom_bar(stat = "identity", color = "blue", fill = "white") + geom_text(aes(label= round(age, digits = 1)), vjust= -.3, size=3.5) + theme_minimal()
p 
p <- ggplot(clustermeans, aes(cluster, income)) + geom_bar(stat = "identity", color = "blue", fill = "white") + geom_text(aes(label= round(income, digits = 0)), vjust= -.3, size=3.5) + theme_minimal()
p 

clustermeans <- aggregate(my_dataclean[,c('patronage')],  
                          list(my_dataclean$cluster), mean)
clustermeans


#create new plots
install.packages("reshape")
library("reshape2")

#calculate the averages for each variable in our clustermeans variable (female  age income) by cluster

clustermeans2 <- aggregate(my_dataclean[,c('importanthealthy','importantvariety', 'importantprice')],  list(my_dataclean$cluster), mean)
clustermeans2
colnames(clustermeans2)[1] <- "cluster"
clustermeans2

clustermeans2.long<-melt(clustermeans2,id.vars = "cluster")
head(clustermeans2.long)

p <- ggplot(clustermeans2.long, aes(x=variable, y = value, fill = factor(cluster))) + geom_bar(stat = "identity", position="dodge") + scale_fill_discrete(name="Cluster", breaks = c(1,2,3), labels = c("1", "2", "3")) + xlab("") + ylab("Mean")  
p 


clustermeans3 <- aggregate(my_dataclean[,c('chipotlehealthy','chipotlevariety', 'chipotleprice')],  list(my_dataclean$cluster), mean)
clustermeans3
colnames(clustermeans3)[1] <- "cluster"
clustermeans3

clustermeans3.long<-melt(clustermeans3,id.vars = "cluster")
head(clustermeans3.long)

p <- ggplot(clustermeans3.long, aes(x=variable, y = value, fill = factor(cluster))) + geom_bar(stat = "identity", position="dodge") + scale_fill_discrete(name="Cluster", breaks = c(1,2,3), labels = c("1", "2", "3")) + xlab("") + ylab("Mean")  
p




