library(data.table)
library(ggplot2)


ft <- fread("Input/feeding_trials_workshop.csv")
bio <- fread("Input/biomass.csv")


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




# biomass data ------------------------------------------------------------


ggplot(bio)+
  geom_point(aes(x = Julian_day, y = shrub_biomass))+
  geom_smooth(aes(x = Julian_day, y = shrub_biomass), method = "lm")+
  facet_wrap(~ drainage_class + year + SA, scale = "fixed")


bio[, mean(shrub_biomass), by = .(year, drainage_class, SA)]
bio[, period := max(Julian_day) - min(Julian_day), by = bygroups]

bygroups <- c("year", "drainage_class", "SA")


mod <- lm(bio$shrub_biomass ~ bio$Julian_day)
coef(mod)["bio$Julian_day"]


mod <- bio[, coef(lm(shrub_biomass ~ Julian_day))["Julian_day"], by = bygroups]


get_growth <- function(x, y){
  model <- lm(y ~ x)
  slope <- coef(model)["x"]
  return(slope)
}

get_growth(y = bio$shrub_biomass, x = bio$Julian_day)
bio[, growthrate := get_growth(x = Julian_day, y = shrub_biomass), by = bygroups]



# run by subset -----------------------------------------------------------

bio[, .SD]
bio[, .SD, .SDcols = c("date", "year")]

bio[, print(.SD), by = bygroups]




# lapply ------------------------------------------------------------------

# runs an action/function on a list or vector

# lapply(list, function(x))

biolist <- split(bio, bio$drainage_class)

class(biolist)

nrow(biolist[[1]])

lapply(biolist, nrow)

lapply(biolist, function(x) {x[, sum(shrub_biomass)]})



# read in multi csv -------------------------------------------------------

files <- dir("Input/", "*.csv", full.names = TRUE)

ls.files <- lapply(files, fread)

fulldata <- rbindlist(ls.files, fill = TRUE, use.names = TRUE, idcol = 'origin')

fulldata[, origin := factor(origin, labels = basename(files))]





# how much sampling do I need ---------------------------------------------


bio[, daycount := Julian_day - min(Julian_day), by = bygroups]

bio[, max(daycount)]

effort <- c(1: max(bio$daycount))


sampleeffort <- lapply(effort, function(n){
  bio[daycount < n, get_growth(Julian_day, shrub_biomass), by = bygroups]
})

time <- rbindlist(sampleeffort, idcol = "effort")

ggplot(time)+
  geom_point(aes(x = effort, y = V1))+
  facet_wrap(~ drainage_class + year)
