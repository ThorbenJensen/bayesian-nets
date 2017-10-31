
library(mgm)
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

type = c("c", "c", "c", "c", "c", "g")
level = c(2, 3, 2, 4, 6, 1)

mgm(df2, type, level)
# error is caused by internal check of 'is.finite(matrix(df2))'
# this is odd, package might not be very flexible

# test with autism data ----
data("autism_data")

fit_d2 <- mgm(data = autism_data$data,
              type = autism_data$type,
              level = autism_data$lev,
              k = 2) # ad most pairwise interacitons

fit_d2$pairwise$wadj

library(qgraph)
qgraph(fit_d2$pairwise$wadj,
       edge.color = fit_d2$pairwise$edgecolor,
       layout = 'spring',
       labels = autism_data$colnames)
# error: plotting libraries not supported
