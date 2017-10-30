# TURORIAL ON BAYESIAN STATISTICS WITH R
# source: https://www.r-bloggers.com/bayesian-network-in-r-introduction/

library(bnlearn)
library(dplyr)

data(coronary)

# greedy learner of network structure
bn_df <- data.frame(coronary)
res <- hc(bn_df)
plot(res)
res2 <- mmhc(bn_df)
plot(res2)

# remove selected edge
res$arcs <- res$arcs[-which((res$arcs[,'from'] == "M..Work" 
                             & res$arcs[,'to'] == "Family")),]
plot(res)

# fit model, given graph and data
fittedbn <- bn.fit(res, data = bn_df)
print(fittedbn$Proteins)

# query network:
cpquery(fittedbn, event = (Proteins == "<3"), evidence = (Smoking == "no"))
cpquery(fittedbn, event = (Proteins == "<3"), 
        evidence = (Smoking == "no" & Pressure == ">140"))
cpquery(fittedbn, event = (Pressure == ">140"), evidence = (Proteins == "<3"))
