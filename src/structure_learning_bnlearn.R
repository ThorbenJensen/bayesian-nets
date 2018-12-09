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

mastercuts <- 
  arules::discretize(df$Age, method = "interval", 
                     breaks = 3, onlycuts = T)
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
  df3 %>% 
  hc(., blacklist = bl2, score = "aic")

plot(res2)

# fit weights
fitted <- bn.fit(x = res2, data = df3)


# idea: for each feature, measure increased certainty due to evidence, 
#       weighted by unconditional probability of evidence

entropy <- function(p) {
  if (p == 0 | p == 1) return(0)
  e <- -p * log2(p) - (1 - p) * log2(1 - p)
  return(e)
}

# query and test entropy reduction for different evidence
prior <- cpquery(fitted, event = (Survived == 1), evidence = TRUE)
entropy_baseline <- entropy(prior)

# 'gain of certainty' from sex
if_male <- cpquery(fitted, event = (Survived == 1), evidence = (Sex == 'male'))
if_female <- cpquery(fitted, event = (Survived == 1), evidence = (Sex == 'female'))
p_male <- cpquery(fitted, event = (Sex == 'male'), evidence = TRUE)
p_female <- cpquery(fitted, event = (Sex == 'female'), evidence = TRUE)

entropy_sex <-
  (p_male * entropy(if_male)) + (p_female * entropy(if_female))

# 'certainty' with evidence Pclass
p_1 <- cpquery(fitted, event = (Pclass == '1'), evidence = TRUE)
if_1 <- cpquery(fitted, event = (Survived == 1), evidence = (Pclass == '1'))
p_2 <- cpquery(fitted, event = (Pclass == '2'), evidence = TRUE)
if_2 <- cpquery(fitted, event = (Survived == 1), evidence = (Pclass == '2'))
p_3 <- cpquery(fitted, event = (Pclass == '3'), evidence = TRUE)
if_3 <- cpquery(fitted, event = (Survived == 1), evidence = (Pclass == '3'))

entropy_sex <-
  p_1 * entropy(if_1) + p_2 * entropy(if_2) + p_2 * entropy(if_2)

# generalize this 'information gain' approach
# TODO
