# Code to show serial and parallel mediation


# load packages -----------------------------------------------------------


library(tidyverse)
library(lavaan)
library(psych)
library(semTools)
library(tidySEM)
library(lavaanPlot)

set.seed(1)


# look at data ------------------------------------------------------------

#rename the data for splicity
df <- mediation2_data

#see the data
glimpse(df)

# explore distributions in the data
df %>% 
  select(anxiety, burn, poscop, negcop) %>% 
  pairs.panels(.)

#getting descriptive statistics
df %>% 
  select(anxiety, burn, poscop, negcop) %>% 
  psych::describe(.)



# parallel mediation example ----------------------------------------------


# 1. specify the model

model1<-'
#direct effect
#poscop -> anxiety: anxiety ~ b1*poscop
#negcop -> anxiety: anxiety ~ b2*poscop
#burn -> poscop: poscop ~ a1*burn
#burn -> negcop: negcop ~ a2*burn

#or more efficiently:
   anxiety ~ c*burn + b1*poscop + b2*negcop
   poscop ~ a1*burn
   negcop ~ a2*burn

#Specify indirect effect:
  indpos := a1*b1 #indirect effect via positive coping
  indneg := a2*b2 #indirect effect via negative coping 

#specify total effect:
total := c+indpos+indneg
    '    

# 2. Fit the model to the data

m1 <- sem(model1, data = df, se = "bootstrap", bootstrap = 500)

# 3. get the results
summary(m1, ci = TRUE, standardized = TRUE, fit.measures = TRUE) #request confidence intervals 
#standardised estimates are 'Std.all'


#requesting bias corrected bootstrapped estimates
#bca.simple corrects skewed data
parameterEstimates(m1,
                   boot.ci.type = "bca.simple",
                   level = .95, ci = TRUE)

# 4. visualise the model

lavaanPlot(model = m1,
           coefs = TRUE,
           stand = TRUE,
           stars = c("regress"),
           covs = TRUE)



# model comparison with nested models -------------------------------------

#with positive coping as floating (i.e. its not a mediator)

# 1. specify the model

model2<-'
#remove a1 and b1 paths from model1 by setting to 0 (0*poscop, 0*burn)

   anxiety ~ c*burn + 0*poscop + b2*negcop
   poscop ~ 0*burn
   negcop ~ a2*burn

#Specify indirect effect 
  #commentout indpos := a1*b1 #indirect effect via positive coping
  indneg := a2*b2 #indirect effect via negative coping
        '

# 2. fit the model to the data
m2 <- sem(model2, data = df, se = "bootstrap", bootstrap = 100)

# 3. get the results
summary(m2, ci = TRUE, standardized = TRUE, fit.measures = TRUE) #request confidence intervals 

# 4. visualise model
lavaanPlot(model = m2,
           coefs = TRUE,
           stand = TRUE,
           stars = c("regress"),
           covs = TRUE)

# 5. compare models

anova(m1, m2, test='Chisq') #does not give model fit indices 

summary(compareFit(m1, m2)) #(crucifix shows which one is better)



# serial mediation example ------------------------------------------------

#where precitors are in a sequence (e.g. x -> y -> z)

#1. specify the model

model_serial<-'
#burnout -> positive coping -> negative coping -> anxiety
#burnout -> anxiety
   anxiety ~ c*burn + b1*negcop
   poscop ~ a1*burn
   negcop ~ d1*poscop

#Specify indirect effect 
  ind := a1*d1*b1 #indirect effect via positive coping AND negative coping
'

# 2. Fit the model to the data
m3 <- sem(model_serial, data = df, se = "bootstrap", bootstrap = 500)

# 3. Get the results

summary(m3, ci = TRUE, standardized = TRUE, fit.measures = TRUE) #request confidence intervals 

# 4. visualise the model

lavaanPlot(model = m3,
           coefs = TRUE,
           stand = TRUE,
           stars = c("regress"),
           covs = TRUE)


# 5. compare with previous models

summary(compareFit(m1, m2, m3))






