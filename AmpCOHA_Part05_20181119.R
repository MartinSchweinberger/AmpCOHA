##################################################################
# Titel:      The historical development of the AmE amplifier system - Part 5
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2018-11-16
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2018. 
#             The historical development of the AmE amplifier system, Part 5,
#             unpublished R script, The University of Queensland.
###############################################################
# clean current workspace
rm(list=ls(all=T))
# set wd
setwd("D:\\Uni\\Projekte\\02-Intensification\\AmpCOHA")
# load packages
library(ggplot2)
library(Rling)
# set options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.print=10000)
# define image directory
imageDirectory<-"images"
# load data
socoha <- read.table("socoha.txt", sep = "\t", header = T)
# inspect data
nrow(socoha); str(socoha); head(socoha)

# inspect colnames
colnames(socoha)

# factorize data
clfct <- c("so", "Adjective", "Priming", "Gradability", "SemanticCategory", "Emotionality")
socoha[clfct] <- lapply(socoha[clfct], factor)
clnm <- c("Decade", "Freq")
socoha[clnm] <- lapply(socoha[clnm], as.numeric)
# inspect data
str(socoha)

################################################################
#               CONDITION INFERENCE TREES
library(partykit)
# create data
citd <- socoha
# set.seed
set.seed(111) 
# apply bonferroni correction (1 minus alpha multiplied by n of predictors)
control = ctree_control(mincriterion = 1-(.05*7))
# create initial conditional inference tree model
citd.ctree <- ctree(so ~ Adjective + Priming +  Freq + 
                      Gradability + SemanticCategory + Emotionality,
                    data = citd)
# plot final ctree
png("images/final_ctree.png",  width = 680, height = 480) 
plot(citd.ctree, gp = gpar(fontsize = 8))
dev.off()
# test prediction accuracy
ptb <- table(predict(citd.ctree), citd$so)
(((ptb[1]+ptb[4])+(ptb[2]+ptb[3]))/sum(table(predict(citd.ctree), citd$so)))*100
##100

# determine baseline
(table(citd$so)[[2]]/sum(table(citd$so)))*100
## 41.08

###############################################################
#                   RANDOM FOREST I
# prepare data
rfd <- socoha
# start with random forest
# set seed
set.seed(222)
# partition data for evaluating rf 
id <- sample(2, nrow(rfd), replace = T, prob = c(.7, .3))
train <- rfd[id == 1, ]
test <- rfd[id == 2,]
# load library
library(randomForest)
# create initial model
socoha_rf1 <- randomForest(so~., data = train)
# inspect model
print(socoha_rf1)

# inspect attibutes
attributes(socoha_rf1)

# start model evaluation
# install package
#source("https://bioconductor.org/biocLite.R"); biocLite(); library(Biobase)
#install.packages("Biobase", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com", 
#                                      "http://cran.rstudio.com/", dependencies=TRUE))
#install.packages("dimRed", dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)

# load caret library
library(caret) # because initially caret did not work, the libraries above had to be installed
# extract prediction for training data
ptrain1 <- predict(socoha_rf1, train)
# inspect predictions
head(ptrain1); head(train$so)

# create confusionMatrix
confusionMatrix(ptrain1, train$so)

# extract prediction for test data
ptest1 <- predict(socoha_rf1, test)
# create confusionMatrix
confusionMatrix(ptest1, test$so)

# determine errorrate of random forest model
plot(socoha_rf1, main = "")

# tune model
socoha_rf2 <- tuneRF(train[, !colnames(train)== "so"], train[, colnames(train)== "so"], 
                     stepFactor = .5, # for most values 1 or 2 appear to be optimal
                     plot = T,
                     ntreeTry = 300,
                     trace = T,
                     improve = .05
)
# create improved model
socoha_rf2 <- randomForest(so~., data = train, 
                           ntree = 100,
                           ntry = 2,
                           importance= T,
                           proximity = T)
# inspect model
print(socoha_rf2)

# predict based on improved model
ptrain2 <- predict(socoha_rf2, train)
# create confusionMatrix
confusionMatrix(ptrain2, train$so)

# extract prediction for test data
ptest2 <- predict(socoha_rf2, test)
# create confusionMatrix
confusionMatrix(ptest2, test$so)

# inspect number of nodes for trees
hist(treesize(socoha_rf2), main = "", col = "lightgray")

# check variable importance
png("images/RandomForestVariableImportance.png",  width = 680, height = 480) 
varImpPlot(socoha_rf2, main = "", pch = 20) 
dev.off()
# left plot (Accuracy): how much accuracy decreases if factor is left out
# left plot (Gini/Pureness): how much more unpure (ambigious) the distributions become if fector is left out
# extract variable importance values
importance(socoha_rf2)

#which variables have been used in the trees
varUsed(socoha_rf2)

# partial dependence plot
partialPlot(socoha_rf2, train, Freq, 1)

partialPlot(socoha_rf2, train, Adjective, 1)

partialPlot(socoha_rf2, train, Gradability, 1)

partialPlot(socoha_rf2, train, SemanticCategory, 1)

partialPlot(socoha_rf2, train, Emotionality, 1)


# extract tree
getTree(socoha_rf2, 1, labelVar = T)

# mds plot
MDSplot(socoha_rf2, test$so)

###############################################################
#                   RANDOM FOREST II
# detach partykit
detach("package:partykit", unload=TRUE)
# load package party
library(party)
# prepare data
rfd <- socoha
# set seed
set.seed(333)

# create initial model
socoha.rf <- cforest(so ~ Decade + Priming +  Freq + 
                       Gradability + SemanticCategory + Emotionality,
                     data = rfd, controls = cforest_unbiased(ntree = 50, mtry = 3))
# determine importance of factors
socoha.varimp <- varimp(socoha.rf, conditional = T)
round(socoha.varimp, 3)

# plot result
png("images/RandemForest2FactorImportance.png",  width = 680, height = 480) 
dotchart(sort(socoha.varimp), pch = 20, main = "Conditional importance of variables")
dev.off()
# load library
library(Hmisc)
# evaluate random forst
socoha.rf.pred <- unlist(treeresponse(socoha.rf))[c(FALSE,TRUE)]
somers2(socoha.rf.pred, as.numeric(rfd$so) - 1)
##     C         Dxy           n     Missing 
## 0.7357789    0.4715579     1954      0
###############################################################
#                     RANDOM FOREST III
# load library
library(party)
# create data
randomforestdata <- socoha

cf1 <- cforest(so ~ . , data= randomforestdata, control=cforest_unbiased(mtry=2,ntree=100)) # fit the random forest
varimp(cf1) # get variable importance, based on mean decrease in accuracy

varimp(cf1, conditional=TRUE) # conditional=True, adjusts for correlations between predict

varimpAUC(cf1)  # more robust towards class imbalance.
png("images/RandemForest3FactorImportance.png",  width = 680, height = 480) 
par(mar = c(5, 8, 4, 2) + 0.1)
plot(y = 1:length(varimpAUC(cf1)), x = varimpAUC(cf1)[order(varimpAUC(cf1))], 
     axes = F, ann = F, pch = 20, xlim = c(-0.01, 0.05), main = "Predictor Importance")
axis(1, at = seq(-0.01, 0.05, 0.005), seq(-0.01, 0.05, 0.005))
axis(2, at = 1:length(varimpAUC(cf1)), names(varimpAUC(cf1))[order(varimpAUC(cf1))], las = 2)
grid()
box()
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
###############################################################
#                  BORUTA
# load library
library(Boruta)
# create dada for boruta
borutadata <- socoha
#                      INITIAL RUN
boruta.ampcoha <- Boruta(so~.,data=borutadata)
print(boruta.ampcoha)

getConfirmedFormula(boruta.ampcoha)

png("images/BorutaAmpaus1.png",  width = 1500, height = 300)
plot(boruta.ampcoha, cex = .75)
dev.off()
plot(boruta.ampcoha)

png("images/BorutaAmpausHistory1.png",  width = 680, height = 480)
plotImpHistory(boruta.ampcoha)
dev.off()
plotImpHistory(boruta.ampcoha)

#                    NEXT RUN
# remove superfluous variables
borutadata$Priming <- NULL
# run2
boruta.ampcoha <- Boruta(so~.,data=borutadata)
print(boruta.ampcoha)

getConfirmedFormula(boruta.ampcoha)

png("images/BorutaAmpaus2.png",  width = 1200, height = 300)
plot(boruta.ampcoha, cex = .75)
dev.off()
plot(boruta.ampcoha)

png("images/BorutaAmpausHistory2.png",  width = 680, height = 480)
plotImpHistory(boruta.ampcoha)
dev.off()
plotImpHistory(boruta.ampcoha)

#                    FINAL RUN
# remove superfluous variables
borutadata$Emotionality <- NULL
# run2
boruta.ampcoha <- Boruta(so~.,data=borutadata)
print(boruta.ampcoha)

getConfirmedFormula(boruta.ampcoha)

png("images/BorutaAmpaus2.png",  width = 1200, height = 300)
plot(boruta.ampcoha, cex = .75)
dev.off()
plot(boruta.ampcoha)

png("images/BorutaAmpausHistory2.png",  width = 680, height = 480)
plotImpHistory(boruta.ampcoha)
dev.off()
plotImpHistory(boruta.ampcoha)

getConfirmedFormula(boruta.ampcoha)
png("images/BorutaAmpaus.png",  width = 1500, height = 750)
par(mar = c(18, 8, 4, 2) + 0.1)
plot(boruta.ampcoha, cex.axis=2, las=2, xlab="", ylab = "", cex = 2, 
     col = c("grey50", "grey50",  "grey50", "grey50", "grey50", "grey90","grey90","grey90"))
abline(v = 3.5, lty = "dashed")
mtext("Predictors", 1, line = 16, at = 7, cex = 3)
mtext("Control", 1, line = 16, at = 2, cex = 3)
mtext("Importance", 2, line = 2.5, at = 2.5, cex = 3, las = 0)
dev.off()
par(mar = c(5, 4, 4, 2) + 0.1)
plot(boruta.ampcoha)

png("images/BorutaAmpausHistory.png",  width = 680, height = 480)
plotImpHistory(boruta.ampcoha)
dev.off()
plotImpHistory(boruta.ampcoha)


###############################################################
#                   END PART 5
###############################################################

