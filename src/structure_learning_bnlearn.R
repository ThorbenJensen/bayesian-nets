# LEARNING NETWORK STRUCTURE OF A BAYESIAN NET WITH DISCRETE VARIABLES

library(bnlearn)
library(titanic)
library(arules)
library(dplyr)

df <- titanic::titanic_train

# learn network for categorical variables ----
bl = matrix(c("Survived", "Sex",
              "Pclass", "Sex",
              "SibSp", "Sex",
              "Survived", "Pclass",
              "Survived", "Embarked"),
            ncol = 2,
            byrow = T)

res <-
  df %>% 
  select(Survived, Pclass, Sex, Embarked, SibSp) %>%
  mutate(Survived = as.factor(Survived)) %>%
  mutate(Pclass = as.factor(Pclass)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(Embarked = as.factor(Embarked)) %>%
  mutate(SibSp = as.factor(SibSp)) %>%
  hc(., blacklist = bl, restart = 100, perturb = 100, score = "aic")

plot(res)

# mixed variable network ----
bl2 = matrix(c("Survived", "Sex",
               "Survived", "Age",
               "Pclass", "Sex",
               "Pclass", "Age",
               "SibSp", "Sex",
               "Survived", "Pclass",
               "Survived", "SibSp"),
             ncol = 2,
             byrow = T)

# discretize continuous data
df2 <-
  df %>%   
  select(Survived, Pclass, Sex, Embarked, SibSp, Age) %>%
  filter(complete.cases(.)) %>%
  mutate(Survived = as.factor(Survived)) %>%
  mutate(Pclass = as.factor(Pclass)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(Embarked = as.factor(Embarked)) %>%
  mutate(SibSp = as.factor(SibSp))

# discretize age
mastercuts <- 
  arules::discretize(df2$Age, method = "interval", 
                     categories = 4, onlycuts = T)
df2$Age <- 
  as.numeric(cut(x = df2$Age, breaks = mastercuts))

df2 <-
  df2 %>%
  mutate(Age = as.factor(Age))
  
# learn structure
df3 <-
  df2 %>%
  filter(complete.cases(.))

res2 <-
  hc(df3, blacklist = bl2, score = "aic")

plot(res2)

# fit weights
fitted <- bn.fit(x = res2, data = df3)


# idea: for each feature, measure increased certainty due to evidence, 
#       weighted by unconditional probability of evidence

# query and test entropy reduction for different evidence
prob <- cpquery(fitted, event = (Survived == 1), evidence = TRUE) 
certainty_baseline <- abs(prob - .5) * 2

# 'gain of certainty' from sex
if_male <- cpquery(fitted, event = (Survived == 1), evidence = (Sex == 'male'))
if_female <- cpquery(fitted, event = (Survived == 1), evidence = (Sex == 'female'))
prob_male <- cpquery(fitted, event = (Sex == 'male'), evidence = TRUE)
prob_female <- cpquery(fitted, event = (Sex == 'female'), evidence = TRUE)

certainty_sex <- 
  (prob_male * abs(if_male - .5) + prob_female * abs(if_female - .5)) * 2

# 'certainty' with evidence Pclass
prob_1 <- cpquery(fitted, event = (Pclass == '1'), evidence = TRUE)
if_1 <- cpquery(fitted, event = (Survived == 1), evidence = (Pclass == '1'))
prob_2 <- cpquery(fitted, event = (Pclass == '2'), evidence = TRUE)
if_2 <- cpquery(fitted, event = (Survived == 1), evidence = (Pclass == '2'))
prob_3 <- cpquery(fitted, event = (Pclass == '3'), evidence = TRUE)
if_3 <- cpquery(fitted, event = (Survived == 1), evidence = (Pclass == '3'))

certainty_class <-
  (prob_1 * abs(if_1 - .5) + prob_2 * abs(if_2 - .5) + prob_3 * abs(if_3 - .5)) * 2


# check if this 'information gain' measurement makes sense. if yes, scale up.
# TODO



