# LEARNING NETWORK STRUCTURE OF A BAYESIAN NET WITH DISCRETE VARIABLES

library(bnlearn)
library(titanic)
library(arules)
library(dplyr)

df <- titanic::titanic_train

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
  as.factor(cut(x = df2$Age, breaks = mastercuts))

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


# calculate surival probability for current evidence
survival_probability <- function(fit, evi) {
  if (length(evi) == 0) { evi <- TRUE}

  cpquery(fit,
          event = (Survived == '1'),
          evidence = evi,
          method = 'lw')
}

# calculate survival probability, given hypotetical additional evidence
survival_probability_hypothetical <- function(fitted,
                                              evidence_pre,
                                              feature,
                                              feature_value) {
  evidence_post <- evidence_pre
  evidence_post[[feature]] <- feature_value
  evidence_post

  survival_probability(fitted, evidence_post)
}

# calculate independent probabilty for feature_Value
# OPTIONAL: make probability dependent on given evidence
prob_feature_value <- function(fitted, feature, feature_value) {
  event_str <- paste0("(", feature, " == '", feature_value, "')")
  cmd <- paste0("cpquery(fitted, ", event_str, ", evidence = TRUE)")
  eval(parse(text = cmd))
}

# convert probability to certainty (i.e. abolute distance from p=0.5)
prob_to_certainty <- function(probability) {
  abs(probability - 0.5)
}

certainty_after_feature <- function(fitted, evidence_pre, df3, feature) {

  feature_values <- unique(df3[,feature])
  feature_values <- setdiff(feature_values, "")

  # query target properties for each feature value
  feature_probs <-
    sapply(feature_values,
           survival_probability_hypothetical,
           fitted = fitted,
           evidence_pre = evidence_pre,
           feature = feature)

  # convert target properties to 'certainties'
  feature_certainties <- sapply(feature_probs, entropy)

  # query indepentent probabilities for each target value to occur
  prob_feature_values <-
    sapply(feature_values,
         prob_feature_value,
         fitted = fitted,
         feature = feature)

  # average certainty for additional feature, weighted by feature value priors
  (prob_feature_values %*% feature_certainties)[1,1]
}


# start without evidence
evidence_pre <- list()
entropy(survival_probability(fit = fitted, evi = evidence_pre))


# calculate certainty after hypothetically adding the next feature
target <- "Survived"
features <- setdiff(colnames(df3), target)
entropy_after_features <-
  sapply(features,
         certainty_after_feature,
         fitted = fitted,
         evidence_pre = evidence_pre,
         df3 = df3)

print(entropy_after_features)

# assume that most informative feature (e.g. Sex) is already known
evidence_2 <- evidence_pre
evidence_2[['Sex']] <- 'male'
entropy(survival_probability(fit = fitted, evi = evidence_2))

features_2 <- setdiff(features, "Sex")
entropy_after_features_2 <-
  sapply(features_2,
         certainty_after_feature,
         fitted = fitted,
         evidence_pre = evidence_2,
         df3 = df3)

print(entropy_after_features_2)

# assume that 'SibSp' is known, too
evidence_3 <- evidence_pre
evidence_3[['SibSp']] <- '1'
survival_probability(fit = fitted, evi = evidence_3)
prob_to_certainty(survival_probability(fit = fitted, evi = evidence_3))

features_2 <- setdiff(features, "SibSp")
entropy_after_features_2 <-
  sapply(features_2,
         certainty_after_feature,
         fitted = fitted,
         evidence_pre = evidence_2,
         df3 = df3)

print(entropy_after_features_2)
