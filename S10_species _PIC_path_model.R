#load packages
library(lavaan)
#load dataset
df <- read.csv("aphid_species_predictors.csv")
#inspect dataset
head(df)

# model without Host_use_breadth_evolution

#model specification. Response variable is written first, '~' represents the correlation between
#reponse and predictor variable, predictor variable is written last. Multiple predictor
#variables per response variable can be included.
# '~~' indicates covariance between predictor variables.
model2 <-'
  Speciation_rates ~ Host_use_evolution_PC_1 + Environmental_PC2 + Host_use_evolution_PC_2
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2 + Environmental_PC2
  Host_use_evolution_PC_2 ~~ Environmental_PC2
  Environmental_PC1 ~~ Environmental_PC1'

# fit model
sem2 <- sem(model2, data = df)
# model results
summary(sem2)

# examine alternative models
model3 <-'
  Speciation_rates ~ Host_use_evolution_PC_1 + Host_use_evolution_PC_2
  Host_use_evolution_PC_1 ~ Environmental_PC2
  Host_use_evolution_PC_2 ~ Environmental_PC2
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2
  Environmental_PC1 ~~ Environmental_PC1'

sem3 <- sem(model3, data = df)
summary(sem3)

model4 <-'
  Speciation_rates ~ Environmental_PC2
  Environmental_PC2 ~ Host_use_evolution_PC_1 + Host_use_evolution_PC_2
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2
  Environmental_PC1 ~~ Environmental_PC1'

sem4 <- sem(model4, data = df)
summary(sem4)

model3 <-'
  Speciation_rates ~ Host_use_evolution_PC_1 + Host_use_evolution_PC_2 + Environmental_PC2
  Host_use_evolution_PC_1 ~ Environmental_PC2
  Host_use_evolution_PC_2 ~ Environmental_PC2
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2
  Environmental_PC1 ~~ Environmental_PC1'

sem3 <- sem(model3, data = df)
summary(sem3)

#model including Host_use_breadth_evolution

model2 <-'
  Speciation_rates ~ Host_use_evolution_PC_1 + Environmental_PC2 + Host_use_evolution_PC_2 + Host_use_breadth_evolution
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2 + Environmental_PC2 + Host_use_breadth_evolution
  Host_use_evolution_PC_2 ~~ Environmental_PC2 + Host_use_breadth_evolution
  Environmental_PC2 ~~ Host_use_breadth_evolution
  Environmental_PC1 ~~ Environmental_PC1'

sem2 <- sem(model2, data = df)
summary(sem2)

#alternative models

model2 <-'
  Speciation_rates ~ Host_use_evolution_PC_1 + Environmental_PC2 + Host_use_evolution_PC_2 + Host_use_breadth_evolution
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2 + Environmental_PC2 + Host_use_breadth_evolution
  Host_use_evolution_PC_2 ~~ Environmental_PC2 + Host_use_breadth_evolution
  Environmental_PC2 ~~ Host_use_breadth_evolution
  Environmental_PC1 ~~ Environmental_PC1'

sem2 <- sem(model2, data = df)
summary(sem2)

model2 <-'
  Speciation_rates ~ Host_use_breadth_evolution
  Host_use_breadth_evolution ~ Host_use_evolution_PC_1 + Host_use_evolution_PC_2
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2 + Environmental_PC2
  Host_use_evolution_PC_2 ~~ Environmental_PC2
  Environmental_PC1 ~~ Environmental_PC1'

sem2 <- sem(model2, data = df)
summary(sem2)

model2 <-'
  Speciation_rates ~ Host_use_breadth_evolution + Host_use_evolution_PC_1 + Host_use_evolution_PC_2
  Host_use_breadth_evolution ~ Environmental_PC2
  Host_use_evolution_PC_1 ~ Environmental_PC2
  Host_use_evolution_PC_2 ~ Environmental_PC2
  Environmental_PC1 ~~ Environmental_PC1'

sem2 <- sem(model2, data = df)
summary(sem2)

model2 <-'
  Speciation_rates ~ Host_use_breadth_evolution
  Host_use_breadth_evolution ~ Host_use_evolution_PC_1 + Host_use_evolution_PC_2
  Host_use_evolution_PC_1 ~ Environmental_PC1 + Environmental_PC2
  Host_use_evolution_PC_2 ~ Environmental_PC1 + Environmental_PC2
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2
  Environmental_PC1 ~~ Environmental_PC2'

sem2 <- sem(model2, data = df)
summary(sem2)

model2 <-'
  Speciation_rates ~ Host_use_breadth_evolution
  Host_use_breadth_evolution ~~ Environmental_PC2 + Host_use_evolution_PC_1+ Host_use_evolution_PC_2
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2 + Environmental_PC2
  Host_use_evolution_PC_2 ~~ Environmental_PC2
  Environmental_PC1 ~~ Environmental_PC1'

sem2 <- sem(model2, data = df)
summary(sem2)

model2 <-'
  Speciation_rates ~ Host_use_evolution_PC_1 + Host_use_evolution_PC_2
  Host_use_breadth_evolution ~~ Environmental_PC2 + Host_use_evolution_PC_1 + Host_use_evolution_PC_2
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2 + Environmental_PC2
  Host_use_evolution_PC_2 ~~ Environmental_PC2
  Environmental_PC1 ~~ Environmental_PC1'

sem2 <- sem(model2, data = df)
summary(sem2)

model2 <-'
  Speciation_rates ~ Environmental_PC2
  Host_use_breadth_evolution ~~ Environmental_PC2 + Host_use_evolution_PC_1 + Host_use_evolution_PC_2
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2 + Environmental_PC2
  Host_use_evolution_PC_2 ~~ Environmental_PC2
  Environmental_PC1 ~~ Environmental_PC1'

sem2 <- sem(model2, data = df)
summary(sem2)

### best model
model2 <-'
  Speciation_rates ~ Host_use_evolution_PC_1 + Environmental_PC2 + Host_use_evolution_PC_2 + Host_use_breadth_evolution
  Host_use_breadth_evolution ~ Host_use_evolution_PC_1 + Host_use_evolution_PC_2
  Host_use_evolution_PC_1 ~ Environmental_PC2
  Host_use_evolution_PC_2 ~ Environmental_PC2
  Host_use_evolution_PC_1 ~~ Host_use_evolution_PC_2
Environmental_PC1 ~~ Environmental_PC1'


sem2 <- sem(model2, data = df)
summary(sem2)
