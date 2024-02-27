library(data.table)
library(ggplot2)


ft <- fread("Input/feeding_trials_workshop.csv")


ft[, intake := (Start_1 - End_1) + (Start_2 - End_2)]

sumtrials <- function(s1, s2, e1, e2){
  day1 <- s1 - e1
  day2 <- s2 - e2
  sum <- day1 + day2
  return(sum)
  }

sumtrials(s1 = ft$Start_1, s2 = ft$Start_2, e1 = ft$End_1, e2 = ft$End_2)

ft$intake2 <- sumtrials(s1 = ft$Start_1, s2 = ft$Start_2, e1 = ft$End_1, e2 = ft$End_2)

ft[, intake2 := sumtrials(Start_1, Start_2, End_1, End_2)]


