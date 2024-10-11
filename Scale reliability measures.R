# scale reliability measures ----------------------------------------------

library(lavaan)
library(psych)
library(dplyr)


data(PoliticalDemocracy)

?PoliticalDemocracy
View(PolitcalDemocracy)

#creating scales from scale items:

dem_1960 <- c("y1" ," y2", "y3", "y4")
ind_1960 <- c("x1", "x2", "x3")
dem_1965 <- c("y5", "y6", "y7", "y8")

#adding variables to the data:

PoliticalDemocracy <- PoliticalDemocracy %>%
  mutate(
    dem_1960 = y1 + y2 + y3 + y4,   # Summing up columns y1 to y4
    ind_1960 = x1 + x2 + x3,        # Summing up columns x1 to x3
    dem_1965 = y5 + y6 + y7 + y8    # Summing up columns y5 to y8
  )


head(PoliticalDemocracy)


# Compute Cronbach's alpha for all items
alpha(PoliticalDemocracy[, c("dem_1960", "ind_1960", "dem_1965")])
omega(PoliticalDemocracy[, c("dem_1960", "ind_1960", "dem_1965")])


