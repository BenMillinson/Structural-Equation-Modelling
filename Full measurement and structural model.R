# full SEM, using a measurement model (CFA) and structural model in one chunk of code,
# including a mediator model and a serial model
# However, is good practice to report seperate model fit indices for measurement and structurl models


# load packages -----------------------------------------------------------


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


# 1. Full SEM Specification
sem_model1 <- "
  # Measurement model
  dem_1960 =~ y1 + y2 + y3 + y4
  ind_1960 =~ x1 + x2 + x3
  dem_1965 =~ y5 + y6 + y7 + y8
  
  # Structural model (causal relationships between latent variables)
  dem_1960 ~ ind_1960     # Industry in 1960 affects Democracy in 1960
  dem_1965 ~ dem_1960     # Democracy in 1960 affects Democracy in 1965
  
  # Covariance between industry variables
  x1 ~~ x2
"

# 2. Fit the SEM model 
sem_model_fit1 <- sem(sem_model1, data = PoliticalDemocracy)

# 3. get results
summary(sem_model_fit1, fit.measures = TRUE, standardized = TRUE)

# 4. evaluate model quality
AIC(sem_model_fit1)

# 5. visualise model
lavaanPlot(model = sem_model_fit1, coefs = TRUE, covs = TRUE, stars = TRUE)

lavaanPlot(model = sem_model_fit1, 
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), 
           coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)


# slightly different model ------------------------------------------------

# Alternative SEM Specification with new variable names

# 1. Full SEM Specification
sem_model2 <- "
  # Measurement model
  dem_1960 =~ y1 + y2 + y3 + y4
  ind_1960 =~ x1 + x2 + x3
  dem_1965 =~ y5 + y6 + y7 + y8
  
  # Structural model (causal relationships between latent variables)
  dem_1965 ~ ind_1960     # Industry in 1960 affects Democracy in 1965
  dem_1965 ~ dem_1960     # Democracy in 1960 affects Democracy in 1965

  # Covariance between industry variables
  x1 ~~ x2
"

# 2. Fit the SEM model 
sem_model_fit2 <- sem(sem_model2, data = PoliticalDemocracy)

# 3. get results
summary(sem_model_fit2, fit.measures = TRUE, standardized = TRUE)

# 4. evaluate model quality
AIC(sem_model_fit2)

# 5. visualise model
lavaanPlot(model = sem_model_fit2, coefs = TRUE, covs = TRUE, stars = TRUE)

lavaanPlot(model = sem_model_fit2, 
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), 
           coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)



# serial mediation model --------------------------------------------------


# Serial mediation model with available variables
serial_model <- "
  # Measurement models
  dem_1960 =~ y1 + y2 + y3 + y4
  ind_1960 =~ x1 + x2 + x3
  dem_1965 =~ y5 + y6 + y7 + y8

  # Mediation paths (serial mediation)
  dem_1965 ~ b2*dem_1960      # Path from M1 (dem_1960) to M2 (dem_1965)
  dem_1960 ~ b1*ind_1960      # Path from IV (ind_1960) to M1 (dem_1960)

  # Indirect effect via the serial path
  indirect_serial := b1 * b2  # Serial indirect effect
  total_effect := b2 + indirect_serial  # Total effect of ind_1960 on dem_1965
"

# Fit the serial mediation model
serial_fit <- sem(serial_model, data = PoliticalDemocracy)

# Summarize the model fit
summary(serial_fit, fit.measures = TRUE, standardized = TRUE)

# Visualize the model
lavaanPlot(model = serial_fit, coefs = TRUE, covs = TRUE, stars = TRUE)
