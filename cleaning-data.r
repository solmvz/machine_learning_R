#loading data set
library(readxl)
manhattan_sales <- read_excel("rollingsales_manhattan.xls",skip = 4)

# building data frame
mt = data.frame(manhattan_sales)
head(mt)
str(mt)
#setting column names to lowercase
names(mt) = tolower(names(mt))

#correcting format
mt$gross.square.feet <- as.numeric(gsub("[^[:digit:]]","",mt$gross.square.feet))

mt$land.square.feet <- as.numeric(gsub("[^[:digit:]]","",mt$land.square.feet))

mt$sale.date <- as.Date(mt$sale.date)

mt$year.built <- as.numeric(as.character(mt$year.built))

mt$zip.code <- as.numeric(mt$zip.code)

mt$year.built <- as.numeric(mt$year.built)


#cleaning data
sum(is.na(mt))
mean(is.na(mt))


#reducing nulls by removing columns "BOROUGH" ,"EASE-MENT" and "APARTMENT NUMBER"
mt = mt[,-c(7, 10, 12, 13)]


#percentage of null values
sum(is.na(mt))/(nrow(mt)**ncol(mt))

#deleting null rows
mt = na.omit(mt)



# detecting outliers

length(which(mt$sale.price==0)) #5775 zero values

length(which(mt$year.built==0)) #3384

length(which(mt$gross.square.feet==0)) #19492


library(ggplot2)

#yearbuilt hist
ggplot(mt) +
  aes(x = year.built) +
  geom_histogram(bins=30L, fill = "#0c4c8a") +
  theme_minimal()

#sale.price
ggplot(mt) +
  aes(x = log(sale.price)) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  stat_count() +
  theme_minimal()

#gross sqft
ggplot(mt) +
  aes(x = log(gross.square.feet)) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  stat_count() +
  theme_minimal()

#box plot
ggplot(mt) +
  aes(x = "", y = log(gross.square.feet)) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

boxplot.stats(mt$year.built)$out



hist(mt$year.built)
hist(mt$land.square.feet)


## keep only the actual sales
mt.sale <- mt[mt$sale.price!=0,]

#plot
ggplot(mt.sale, aes(x=sale.price, y=gross.square.feet)) + geom_point(color="#0c4c8a")
ggplot(mt.sale, aes(x=log(sale.price), y=log(gross.square.feet))) + geom_point(color="#0c4c8a")


## for now, let's look at 1-, 2-, and 3-family homes
mt.homes <- mt.sale[which(grepl("FAMILY",mt.sale$building.class.category)),]

ggplot(mt.homes, aes(x = log(gross.square.feet),y = log(sale.price))) + geom_point(color="#0c4c8a")

mt.homes[which(mt.homes$sale.price>10000),]


## remove outliers that seem like they weren't actual sales
mt.homes$outliers <- (log(mt.homes$sale.price) <= 5) + 0

mt.homes <- mt.homes[which(mt.homes$outliers==FALSE),]

ggplot(mt.homes, aes(x = log(gross.square.feet),y = log(sale.price))) + geom_point(color="#0c4c8a")














