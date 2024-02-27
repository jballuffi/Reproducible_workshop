
#load in packages
library(data.table)
library(ggplot2)

#read in data using the data.table function, fread()
ft <- fread("Input/feeding_trials_workshop.csv")
diets <- fread("Input/Diets.csv")

# the feeding trials are results for individual animal performace on 3 different diets
# each diet was fed to the animal for 2 days, and one performance was measured at the end 
# one goal is to plot performance against protein intake

#using the function class, we can see that the data is classed as a data.table
class(ft)

#if you don't read the data in with fread, and thus have a dataframe. Convert to data.table
mtcars <- as.data.table(mtcars)
class(mtcars)


# syntax for data.table:

# DT[i, j, by]

# [with i, do j, by group]

### once you learn this you are set!

# the i subsets the data by row
# the j calls on a function or action
# the 'by group' allows you to do your j by a categorical variable in your data




# examples of I's ---------------------------------------------------------

#subset new data to just results from diet A
dietA <- ft[Diet == "A"]

#show rows where performance is higher than 36
ft[Performance > 36]

#show data from diet A or diet B. "|" represented or
ft[Diet == "A" | Diet == "B"]

#show data for cases of very high or very low performance
ft[Performance > 36 | Performance < 20]

#show data for diet A and performance greater than 22
ft[Diet == "A" & Performance > 22]

#list some individuals of interest
inds <- c("An_1", "An_2", "An_3")

#show data for just those individuals
ft[ID %in% inds]

#show any cases that have an NA for diet. Should be none
ft[is.na(Diet)]




# examples of the J's -----------------------------------------------------

#calculate the median performance
medper <- ft[, median(Performance)]

#calculate performance for just diet A
ft[Diet == "A", mean(Performance)]

#calculate the minimum, maximum, and median performance
ft[, .(min(Performance), max(Performance), median(Performance))]

#create a new column that categorizes the animals based on performance
#when performance is above or equal to the median, classify the animal as being in good condition
ft[Performance >= medper, status := "good"]

#when performance is below the median, classify the animal as being in poor condition
ft[Performance < medper, status := "poor"]



# examples of by's --------------------------------------------------------

#calculate mean by diet
ft[, mean(Performance), by = Diet]

#caclulate mean and sd by diet, save as separate data
stats <- ft[, .(meanP = mean(Performance), sdP = sd(Performance)), by = Diet]



# #rename col names using data.table function
# setnames(stats, c("V1", "V2"), c("Mean", "sd"))



# calculate intake rates  -------------------------------------------------

# Intake of day one
ft[, Intake_1 := Start_1 - End_1]

#Intake of day two
ft[, Intake_2 := Start_2 - End_2]

#total intake of both days
ft[, total_intake := Intake_1 + Intake_2]



# merge function ----------------------------------------------------------

#merge function 

fulldt <- merge(ft, diets, by.x = "Diet", by.y = "Diet", all.x = TRUE )


#as.data.table


# melt function -----------------------------------------------------------

daydt <- melt(fulldt, #first argument is the DT
              measure.vars = c("Intake_1", "Intake_2"), #what columns you combine
              value.name = "Intake_rate", #what do both of those columns measure? 
              variable.name = "Day") #what is the name of the category

#base R
daydt$Day <- tstrsplit(daydt$Day, "_", keep = 2)

#data.table
daydt[, Day := tstrsplit(Day, "_", keep = 2)]     

#paste0 function
daydt[, ID_date := paste0(ID, "_", Date)]


#rbindlist()

#rbind




# ggplot time -------------------------------------------------------------

library(ggplot2)
library(ggpubr) #use for multi panel function: ggarrange()
library(ggeffects) #adding models to your ggplot
library(data.table)

  themepoints <- theme(axis.title = element_text(size=13),
                       axis.text = element_text(size=10),
                       legend.position = "top",
                       legend.key = element_blank(),
                       legend.title = element_blank(),
                       panel.background = element_blank(),
                       axis.line.x.top = element_blank(),
                       axis.line.y.right = element_blank(),
                       axis.line.x.bottom = element_line(size=.5),
                       axis.line.y.left = element_line(size=.5),
                       panel.border = element_blank(),
                       panel.grid.major = element_line(size = 0.5, color = "grey90"))
  


mod <- lm(mpg ~ hp, mtcars)
summary(mod)

slopemod <- coef(mod)["hp"]
intmod <- coef(mod)["(Intercept)"]


(n <- ggplot(mtcars)+
  geom_point(aes(x = hp, y = mpg))+
  geom_abline(slope = slopemod , intercept = intmod)+
  labs(x = "", y = "miles per gallon", title = "A")+
  themepoints)

class(mtcars$am)
mtcars <- as.data.table(mtcars)
mtcars[, am := as.factor(am)]

mod2 <- lm(mpg ~ hp*am, mtcars)
summary(mod2)

effsmod2 <- as.data.table(ggpredict(mod2, terms = c("hp", "am")))

(m <- ggplot()+
  geom_line(aes(x = x, y = predicted, color = group), data = effsmod2)+
  geom_ribbon(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.5, data = effsmod2)+
  geom_point(aes(x = hp, y = mpg, color = am), size = 2, data = mtcars)+
  labs(x = "horse power", y = "miles per gallon", title = "B")+
  themepoints)


(fullfig <- ggarrange(n, m, o, p, ncol = 1, nrow = 3))


ggplot(mtcars)+
  geom_point(aes(x = hp, y = mpg))+
  geom_abline(slope = slopemod , intercept = intmod)+
  labs(x = "horse power", y = "miles per gallon")+
  facet_wrap(~am, scales = "fixed")+
  themepoints

class(mtcars$am)
mtcars[, unique(am)]



#filepath , plot name, width, height, unit
ggsave("Output/cars.jpeg", width = 5, height = 4, unit = "in")


saveRDS

