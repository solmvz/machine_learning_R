
#correlation matrix

#install.packages("corrplot")
#source("http://www.sthda.com/upload/rquery_cormat.r")

mt.cor <- mt.sale[,c(5,6,9,10,11,12,13,14,16)] #keeping only numeric values

cormatrix <- cor(mt.cor, method = c("pearson", "kendall", "spearman"))

rcorr(x, type = c("pearson","spearman"))

cormatrix <- round(cormatrix, 2)

#plotting
rquery.cormat(mt.cor)


# building linear model

plot(log(mt.homes$gross.square.feet),log(mt.homes$sale.price))

#model 1 

model1 <- lm(log(sale.price) ~ log(gross.square.feet) ,data=mt.homes)

summary(model1)

abline(model1,col="green",lwd=2)


#model 2

model2 <- lm(log(sale.price) ~ log(gross.square.feet)+log(land.square.feet) +
               factor(neighborhood),data=mt.homes)

summary(model2)

abline(model2,col="red",lwd=2)


#model 3

model3 <- lm(log(sale.price) ~ log(gross.square.feet) + log(total.units), data = mt.homes)

summary(model3)

abline(model3,col="blue",lwd=2)


legend("bottomright",legend=c("model 1", "model 2", "model 3"), fill=c("green", "red", "blue"), bty="n")

