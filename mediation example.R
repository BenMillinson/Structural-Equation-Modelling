# code to exemplify 'simple' mediation, direct, and indirect effects


library(lavaan)
library(lavaanPlot)
library(psych)
library(semTools)
library(tidyverse)

set.seed(1)


# explore the data --------------------------------------------------------

#see the data
glimpse(teacher_aggression)

# explore distributions in the data
teacher_aggression %>% 
  select(Acad, Teach_r, Agg, Non_agg) %>% 
  pairs.panels(.)

#getting descriptive statistics
teacher_aggression %>% 
  select(Acad, Teach_r, Agg, Non_agg) %>% 
  psych::describe(.)


# running a model ---------------------------------------------------------


#model1: non_agg -> teach_r -> Acad

model1 <- "
# Direct effect (c')
Acad ~ Non_agg

# Effect of the mediator on the outcome (b)
Acad ~ Teach_r

# Effect of the predictor on the mediator (a)
Teach_r ~ Non_agg
"

# 2. Fit the model to the data

fit1 <- sem(model1, data = teacher_aggression)

# 3. Get the results

summary(fit1, fit.measures = TRUE, standardized = TRUE, ci = TRUE, rsquare = TRUE)

# 4. visualise the model

lavaanPlot(model = fit1, edge_options = list(color = "grey"))


# test of direct and indirect effects -------------------------------------

#where direct effects refers to the immediate impact of one variable on another
#where indirect effects refers to an effect of one variable on another mediated through another variable

#model2: non_agg -> teach_r -> Acad
#        non_agg -> Acad

model2 <- "
# Baseline model
Acad ~ cdash*Non_agg 
Acad ~ b*Teach_r
Teach_r ~ a*Non_agg

# Indirect effect:
indirect := a*b

#direct effect:
direct := cdash+indirect
"

# 2. Fit the model to the data

fit2 <- sem(model1r, data = teacher_aggression, se = "bootstrap", bootstrap = 10)

# 3. Get the results

summary(fit2, fit.measures = TRUE, standardized = TRUE, ci = TRUE) #request confidence intervals 

# 4. visualise the model

lavaanPlot(model = fit2,
           coefs = TRUE,
           stand = TRUE,
           stars = c("regress"),
           covs = TRUE)



# running a more complicated model ----------------------------------------


# 1. Specify the model 

#non-aggressive conduct problems, agressive conduct problem -> relationship with teacher,
#non-aggressive conduct problems, agressive conduct problem -> academic performance,
#relationship with teacher -> academic performance 

model3 <- "
# Direct effect (c'); #'cdash_x*' specifies the direct model
Acad ~ cdash1*Non_agg
Acad ~ cdash2*Agg

# Effect of the mediator on the outcome (b)
Teach_r ~ a1*Non_agg
Teach_r ~ a2*Agg

# Effect of the predictor on the mediator (a)
Acad ~ b*Teach_r 

#test of indirect effects
ind_agg := a2*b
ind_non := a1*b
total := cdash1+cdash2+ind_agg+ind_non
"

# 2. Fit the model to the data

fit3 <- sem(model3, data = teacher_aggression, se = "bootstrap", bootstrap = 1000)

# 3. Get the results

summary(fit3, fit.measures = TRUE, standardized = TRUE, ci = TRUE) #request confidence intervals 

# 4. visualise the model

lavaanPlot(model = fit3,
           coefs = TRUE,
           stand = TRUE,
           stars = c("regress"),
           covs = TRUE)



# running the complicated model with different specifications -------------




# 1. Specify the model 

#non-aggressive conduct problems, agressive conduct problem -> relationship with teacher,
#non-aggressive conduct problems, agressive conduct problem -> academic performance,
#relationship with teacher -> academic performance 

model3 <- "
# Direct effect (c'); #'cdash_x*' specifies the direct model
Acad ~ cdash1*Non_agg
Acad ~ cdash2*Agg

# Effect of the mediator on the outcome (b)
Teach_r ~ a1*Non_agg
Teach_r ~ a2*Agg

# Effect of the predictor on the mediator (a)
Acad ~ b*Teach_r 

#test of indirect effects
ind_agg := a2*b
ind_non := a1*b
total := cdash1+cdash2+ind_agg+ind_non
"

# 2. Fit the model to the data

#use maximum likelihood robust if you have non-normally distributed data,
# I.e. skew values not between 1-,+1 and kurtosis between -2, +2 (Hair, et al., 2022)

fit3 <- sem(model3, data = teacher_aggression, estimator = "MLR", missing = "ML")

# 3. Get the results

summary(fit3, fit.measures = TRUE, standardized = TRUE, ci = TRUE) #request confidence intervals 

# 4. visualise the model

lavaanPlot(model = fit3,
           coefs = TRUE,
           stand = TRUE,
           stars = c("regress"),
           covs = TRUE)


