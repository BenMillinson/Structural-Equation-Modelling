#confirmatory factor analysis----

library(lavaan)
library(lavaanPlot)
library(dplyr)
library(semTools)


# model fit index cut offs ------------------------------------------------


#chi^2: p >0.05

#NNFI=TLI: TLI = >0.95

#CFI: >0.95

#RMSEA: <0.05 with 90% confidence interval below 0.1

#(S)RMR: SRMR <0.08


# understanding the data --------------------------------------------------


?PoliticalDemocracy

#democracy_1960 (y1,y2,y3,y4)- latent variable that loads on observed expert ratings of freedom in the press, 
#freedom of political opposition, fairness in elections, and effectiveness of the elected legislature in 1960.

#industry_1960 (x1,x2,x3) - latent variable that loads on gross national product, inanimate energy consumption,
#labor force in industry in 1960.

#democracy_1965 (y5,y6,y7,y8) - latent variable that loads on observed expert ratings of freedom in the press,
#freedom of political opposition, fairness in elections, and effectiveness of the elected legislature in 1965.



# seeing correlation between scale items ----------------------------------


# Select the individual items for each scale
items_df <- PoliticalDemocracy %>%
  select(y1, y2, y3, y4, x1, x2, x3, y5, y6, y7, y8)

# Compute the correlation matrix for the selected items
correlation_matrix <- cor(items_df, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)


# describing the data -----------------------------------------------------


# Select only numeric columns from PoliticalDemocracy
numeric_columns <- PoliticalDemocracy %>% select_if(is.numeric)

# Use describe() to compute descriptive statistics for numeric columns
psych::describe(numeric_columns)


# specify the model -------------------------------------------------------


cfa_model <- "dem_1960=~y1+y2+y3+y4
ind_1960=~x1+x2+x3
dem_1965=~y5+y6+y7+y8"


# fit the model -----------------------------------------------------------


cfa_model_fit <- sem(cfa_model, data = PoliticalDemocracy)
cfa_model_fit


# extracting information --------------------------------------------------


summary(cfa_model_fit, fit.measures = TRUE, standardized = TRUE)



# model quality -----------------------------------------------------------


AIC(cfa_model_fit)



# visualizing cfa_model_fit -----------------------------------------------


lavaanPlot(model = cfa_model_fit, coefs = T, covs = T, stars = T)

lavaanPlot(model = cfa_model_fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)



# CFA with covarying items ------------------------------------------------

# Specify the CFA model, allowing x1 and x2 to covary
cfa_model_COV <- "
  dem_1960 =~ y1 + y2 + y3 + y4
  ind_1960 =~ x1 + x2 + x3
  dem_1965 =~ y5 + y6 + y7 + y8

  # Specify that x1 and x2 should covary
  x1 ~~ x2
"

# Fit the model using SEM (with the PoliticalDemocracy dataset)
cfa_model_fit_COV <- sem(cfa_model_COV, data = PoliticalDemocracy)

# Extract and print the summary with fit measures and standardized estimates
summary(cfa_model_fit_COV, fit.measures = TRUE, standardized = TRUE)

AIC(cfa_model_fit_COV)

lavaanPlot(cfa_model_fit_COV, coefs = T, covs = T, stars = T)

lavaanPlot(model = cfa_model_fit_COV, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)


# comparing the models ----------------------------------------------------


summary(compareFit(cfa_model_fit, cfa_model_fit_COV)) #(crucifix shows which one is better)


