
library(deal)
library(titanic)
library(dplyr)

# load data ----
df <- titanic::titanic_train

# preprocess data
df2 <-
  df %>%   
  select(Survived, Pclass, Sex, Embarked, SibSp, Age) %>%
  filter(complete.cases(.)) %>%
  mutate(Survived = as.factor(Survived)) %>%
  mutate(Pclass = as.factor(Pclass)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(Embarked = as.factor(Embarked)) %>%
  mutate(SibSp = as.factor(SibSp))

net <- network(df2)
prior <- jointprior(net)
net2 <- learn(net, df2, prior)$nw
best <- autosearch(net2, df2, prior, showban = T)
