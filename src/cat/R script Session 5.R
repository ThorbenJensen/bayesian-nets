
## Workshop on catR - The Psychometrics Centre,
## Cambridge University, June 10th, 2014

##############################
### R script for Session 5 ###
##############################

### Load catR package
require(catR)

### Load 'polyIQ' item bank
bank <- read.table("src/cat/polyIQ.txt",header = TRUE)
bank <- as.matrix(bank)

### CAT design:
## two starting items, most informative at theta = c(-1,1)
startList <- list(nrItems = 2, theta = 0)

## next item selection by 'MFI', EAP ad-interim proficiency estimation
testList <- list(method = "EAP", itemSelect = "MFI")

## stop after 10 items
stopList <- list(rule = "length", thr = 10)

## final proficiency estimation
finalList <- list(method = "ML")


### generation of a CAT response pattern
res <- randomCAT(trueTheta = 0, itemBank = bank, model = "RSM", 
start = startList, test = testList, stop = stopList, final = finalList)

## displaying the 'full' (bruto) output
str(res)

## display of the output
res

## graphical display of the output
plot(res)
plot(res,ci = TRUE)




########################
## Suggested exercise ##
########################

### CAT design:
## three starting items, most informative at theta = c(-1,0, 1)
startList <- list(nrItems = 3, theta = 0)

## next item selection by 'KL', BM ad-interim proficiency estimation
testList <- list(method = "BM", itemSelect = "KL", randomesque = 3)

## stop after 10 items
stopList <- list(rule = "precision", thr = 0.4)

## final proficiency estimation
finalList <- list(method = "WL")


### generation of a CAT response pattern
res <- randomCAT(trueTheta = -1, itemBank = bank, model = "RSM",
                 start = startList, test = testList, stop = stopList, 
                 final = finalList)

res

