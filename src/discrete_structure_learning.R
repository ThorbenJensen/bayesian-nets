# LEARNING NETWORK STRUCTURE OF A BAYESIAN NET WITH DISCRETE VARIABLES

library(bnlearn)
library(titanic)
library(arules)

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
res2 <-
  df2 %>%
  filter(complete.cases(.)) %>%
  hc(., blacklist = bl2, score = "aic")

plot(res2)


data(gaussian.test) %>%
  discretize(gaussian.test, method = 'hartemink', breaks = 4, ibreaks = 20)

  
  
