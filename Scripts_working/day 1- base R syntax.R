

library("data.table")



# base R


DT <- mtcars


# this means assign <- 


head(DT)
head(DT, n = 7)
?head

str(DT)

print(DT)

class(DT)


plot(DT$mpg ~ DT$gear)

class(DT$gear)
print(DT$gear)

DT$gear <- as.factor(DT$gear)

str(DT)



meanmgp <- mean(DT$mpg)
n <- 4

clalc <- meanmgp*n


install.packages("data.table")



