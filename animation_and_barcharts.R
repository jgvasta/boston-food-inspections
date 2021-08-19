library(stringr)
library(ggplot2)
food <- read.csv("Food.Inspections.Records.csv")
yelp <- read.csv("Food.Inspections.Yelp.Restaurant.csv")

food$resultdttm <- as.Date(food$resultdttm, "%m/%d/%Y")
order(food$resultdttm)
foodsort <- food[order(food$resultdttm),]


# Chart number of failures per year by neighborhood
foodsort$year <- "0"

foodsort[foodsort$resultdttm >= "2010-01-01",]$year <- "2010"
foodsort[foodsort$resultdttm >= "2011-01-01",]$year <- "2011"
foodsort[foodsort$resultdttm >= "2012-01-01",]$year <- "2012"
foodsort[foodsort$resultdttm >= "2013-01-01",]$year <- "2013"
foodsort[foodsort$resultdttm >= "2014-01-01",]$year <- "2014"
foodsort[foodsort$resultdttm >= "2015-01-01",]$year <- "2015"
foodsort[foodsort$resultdttm >= "2016-01-01",]$year <- "2016"
foodsort[foodsort$resultdttm >= "2017-01-01",]$year <- "2017"
foodsort[foodsort$resultdttm >= "2018-01-01",]$year <- "2018"
foodsort[foodsort$resultdttm >= "2019-01-01",]$year <- "2019"
foodsort[foodsort$resultdttm >= "2020-01-01",]$year <- "2020"

# Add BRA_PD to foodsort
land <- read.csv("Land.Parcels.2020.csv")
foodsort$BRA_PD <- land[match(foodsort$CT_ID_10, land$CT_ID_10),41]
# Add count for violations
foodsort$count <- 0
foodsort[foodsort$violstatus == "Fail",]$count <- 1

# Avg violations/year per restaurant
foodfails <- aggregate(count ~ businessname, data = foodsort, sum)
foodfails$Land_Parcel_ID <- foodsort[match(foodfails$businessname, foodsort$businessname),27]
foodfails$yearcount <- yelp[match(foodfails$businessname, yelp$Restaurant.Name),24]
foodfails2 <- foodfails
foodfails2$yearcount <- yelp[match(foodfails2$Land_Parcel_ID, yelp$Land_Parcel_ID),24]
foodfails2$adjyearcount <- foodfails2$yearcount + 1
foodfails2$adj_violperyear <- foodfails2$count / foodfails2 $adjyearcount

foodsort$adj_violperyear <- foodfails2[match(foodsort$businessname, foodfails2$businessname),6]

foodsort$failed <- 0
foodsort$failed <- ifelse(foodsort$result == "HE_Fail", "1", "0")


### ANIMATION ###
library(gganimate)
library(gifski)
library(png)

# Yearly violations by neighborhood animation
plot <- ggplot(data = foodsort, aes(x = count, y = BRA_PD)) + geom_bar(stat = "identity") + 
    theme_minimal() +
    transition_states(
        year,
        transition_length = 2,
        state_length = 1,
    ) + ease_aes("sine-in-out") +
    labs(title = "Number of violations: {closest_state}")
animate(plot, duration = 10, fps = 20, width = 400, height = 400, renderer = gifski_renderer())



# Make bar chart, make it colorful
yearplot <- ggplot(data = foodsort, aes(x = reorder(BRA_PD, -count), y = count, fill = year)) + geom_bar(stat = "identity") + 
    theme_minimal() + coord_flip() + 
    labs(y = "# Inspection Violations", x = "Neighborhood", title = "Food Inspection Violations")
yearplot



# Make a bar chart with fill as percent white population
brapdviols <- aggregate(adj_violperyear~BRA_PD, data = foodsort, mean)
brapdviols <- brapdviols[-c(1),]
vplot <- ggplot(data = brapdviols, aes(x = reorder(BRA_PD, -adj_violperyear), y = adj_violperyear)) + geom_bar(stat = "identity") + 
    theme_minimal() + coord_flip()
vplot

race <- read.csv("MAracetract.csv")

land <- merge(land, race, by = "CT_ID_10", all.x = TRUE)
land$pct_white <- as.numeric(land$pct_white)
raceagg <- aggregate(pct_white ~ BRA_PD, data = land, mean)
brapdviols$pct_white <- raceagg[match(brapdviols$BRA_PD, raceagg$BRA_PD),2]

vplot <- ggplot(data = brapdviols, aes(x = reorder(BRA_PD, -adj_violperyear), y = adj_violperyear, fill = pct_white)) + geom_bar(stat = "identity") + 
    coord_flip()
vplot

