
# reorganazing neighborhoods

mt$neighborhood <- replace(mt$neighborhood, grep("UPPER WEST SIDE", mt$neighborhood),
                           "UPPER WEST SIDE")

mt$neighborhood <- replace(mt$neighborhood, grep("UPPER EAST SIDE", mt$neighborhood),
                           "UPPER EAST SIDE")

mt$neighborhood <- replace(mt$neighborhood, grep("GREENWICH VILLAGE", mt$neighborhood),
                           "GREENWICH VILLAGE")

mt$neighborhood <- replace(mt$neighborhood, grep("HARLEM", mt$neighborhood),
                           "HARLEM")

mt$neighborhood <- replace(mt$neighborhood, grep("MANHATTAN", mt$neighborhood),
                           "MANHATTAN VALLEY")

mt$neighborhood <- replace(mt$neighborhood, grep("MIDTOWN", mt$neighborhood),
                           "MIDTOWN")

mt$neighborhood <- replace(mt$neighborhood, grep("WASHINGTON HEIGHTS", mt$neighborhood),
                           "WASHINGTON HEIGHTS")


library(ggplot2)


ggplot(mt, aes(x=neighborhood)) +
  geom_bar() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#normalizing
nor <-function(x) { (x -min(x))/(max(x)-min(x))} 

mt.norm <- as.data.frame(lapply(mt[,c(5,6,9,16)], nor))

summary(mt.norm)

#dividing train and test subset
n.points <- 23818
sampling.rate <- 0.8

num.test.set.labels <- n.points*(1 - sampling.rate)


training <- sample(1:n.points, sampling.rate*n.points, replace = FALSE)

train <- subset(mt.norm[training,], select=c(block, lot, zip.code, sale.price))


testing <- setdiff(1:n.points, training)

test <- subset(mt.norm[testing,], select=c(block, lot, zip.code, sale.price))


cl <- mt$neighborhood[training]

true.labels <- mt$neighborhood[testing]



#applying k-nn classifier
library(class)

predicted.labels <- knn(train, test, cl, 1)
  
num.incorrect.labels <- sum(predicted.labels != true.labels)
  
misclassification.rate <- num.incorrect.labels / num.test.set.labels
  
  
num.correct.labels <- sum(predicted.labels == true.labels)
  
accurancy <- num.correct.labels / num.test.set.labels
  
  
print(misclassification.rate)
  
print(accurancy)


#plotting results

library(plyr)
library(ggplot2)


plot.df = data.frame(test, predicted= predicted.labels)


ggplot(plot.df, aes(x = block, y = lot, color = predicted, size=full)) + 
  geom_point(size = 2)



