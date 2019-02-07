##################################################################
# Titel:      The historical development of the AmE amplifier system - Part 3
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2018-11-16
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2018. 
#             The historical development of the AmE amplifier system, Part 3,
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
ampcoha <- read.table("ampcoha06_varcon_emogradsem.txt", sep = "\t", header = T)
# inspect data
str(ampcoha); head(ampcoha)

colnames(ampcoha)

# overview of data
idx <- which(colnames(ampcoha) == "Function"|colnames(ampcoha) == "Genre"|colnames(ampcoha) == "Variant"|
               colnames(ampcoha) == "Amplifier"|colnames(ampcoha) == "Emotionality"|colnames(ampcoha) == "Gradability"|
               colnames(ampcoha) == "SemanticCategory")
for(i in idx){
  tb <- with(ampcoha, table(ampcoha[,i]))
  print(tb)
}

ampcoha <- subset(ampcoha, Decade > 1840)
###############################################################
#                SEMANTIC VECTOR SPACE MODEL I
# perpare data
#svmd <- subset(ampcoha, Variant != "0")
svmd <- ampcoha
t3coha <- table(svmd$Adjective, svmd$Variant)
# save row and column names
colnamescoha <- colnames(t3coha)
rownamescoha <- rownames(t3coha)
svsmcoha <- as.matrix(t3coha)
svsmcoha <- t(svsmcoha)
# compute expected values
svsmcoha.exp <- chisq.test(svsmcoha)$expected
# calculate PMI and PPMI
svsmcoha.PMI <- log2(svsmcoha/svsmcoha.exp)
svsmcoha.PPMI <- ifelse(svsmcoha.PMI < 0, 0, svsmcoha.PMI)
# calculate cosine similarity
svsmcoha.tmp1 <- svsmcoha.PPMI
svsmcoha.cos <- cossim(svsmcoha.tmp1)
#round(svsmcoha.cos, 2)
###############################################################
#               CLUSTER SEMANTIC VECTORS
# load library
library(cluster)
# find max value that is not 1
svsmcoha.cos.test <- apply(svsmcoha.cos, 1, function(x){
  x <- ifelse(x == 1, 0, x) } )
maxval <- max(svsmcoha.cos.test)
# create distance matrix
svsmcoha.dist <- 1 - (svsmcoha.cos/maxval)
clustd <- as.dist(svsmcoha.dist)
# create distance matrix
clustd <- dist(svsmcoha.cos, method = "manhattan") # create distance matrix (manhattan method: most popular choice)
# alternative methods
# eucledian - not good when dealing with many dimensions
# manhattan - most popular choice
# method - here the difference between points dominates
# canberra - for count data
# binary - for binary data only!
# minkowski - is not a true distance measure

# find optimal number of clusters
asw <- as.vector(unlist(sapply(2:nrow(svsmcoha)-1, function(x) pam(clustd, k = x)$silinfo$avg.width)))
# determine the optimal number of clusters (max width is optimal)
optclust <- which(asw == max(asw))+1 # optimal number of clusters

# inspect clustering with optimal number of clusters
svsmcoha.clust <- pam(clustd, optclust)
svsmcoha.clust$clustering

# create cluster object
# alternative methods: "single", "ward.D2", "average", "mcquitty", "median", "centroid"
ampcohahclust <- hclust(clustd, method="average") 
# load library
library(graphics)
# set parameters for plotting
opar <- par(mar = c(5, 4, 4, 2) + 0.1)      # make a copy of current settings
par(mar = c(3, 1, 1, 20) + 0.1)
# plot cluster solution
png("images/Clustcoha.png", width = 500, height = 1000) # save plot
par(mar = c(3, 1, 1, 20) + 0.1)
plot(as.dendrogram(ampcohahclust), main = "", xlab = "", ylab = "", cex = 2, horiz=TRUE)
rect.dendrogram(as.dendrogram(ampcohahclust), k = optclust, which = 1, border =  "red", horiz=TRUE)
dev.off()
# load libraries for nicer dendrograms
library(factoextra)
library(dendextend)
# plot with colored clusters
png("images/Clustcoha_fviz.png",  width = 400, height = 1000) # save plot
par(mar = c(3, 1, 1, 20) + 0.1)
fviz_dend(ampcohahclust, k = optclust, cex = .8, horiz =T,  
          k_colors = c("red","black",   "black"), 
          rect_border = c("red","black", "black"), 
          rect_fill = F, main = "", labels_track_height=2, rect = F)
dev.off()
# plot as unrooted tree
png("images/PhyClustAmpcoha.png",  width = 680, height = 480) 
fviz_dend(ampcohahclust, k = optclust, color_labels_by_k = T, type = "phylogenic", repel = TRUE, cex = .9,
          k_colors = c("red","black", "black"))
dev.off()
par(opar)          # restore original settings 
###############################################################
# Unrooted clustering
# library ape
library(ape)
# convert 'hclust' to 'phylo' object
phylo_tree = as.phylo(ampcohahclust)
# get edges
graph_edges = phylo_tree$edge
# library igraph
library(igraph)
# get graph from edge list
graph_net = graph.edgelist(graph_edges)
# extract layout (x-y coords)
graph_layout = layout.auto(graph_net)
# number of observations
nobs = nrow(svsmcoha.cos)
# save plot
png("images/UClustAmpcoha.png",  width = 680, height = 480) 
# start plot
plot(graph_layout[,1], graph_layout[,2], type = "n", axes = FALSE,
     xlab = "", ylab = "")
# draw tree branches
segments(
  x0 = graph_layout[graph_edges[,1],1], 
  y0 = graph_layout[graph_edges[,1],2],
  x1 = graph_layout[graph_edges[,2],1],
  y1 = graph_layout[graph_edges[,2],2],
  col = "gray90", lwd = 2
)
# add labels
text(graph_layout[1:nobs,1], graph_layout[1:nobs,2],
     phylo_tree$tip.label, cex = .9, xpd = TRUE, font = 1)
dev.off()
###############################################################
#                 WARNING
#             DATA REDUCTION
# remove maximizers
#maximizers <- names(which(svsmcoha.clust$clustering == 2))
maximizers <- c("absolutely", "completely", "entirely", "totally", "utterly", "wholly")
maximizers <- maximizers[!maximizers == "0"]
nrow(ampcoha)

ampcoha <- ampcoha[which(!ampcoha$Variant %in% maximizers),]
nrow(ampcoha)

###############################################################
#                  PLOTTING
###############################################################
# prepare data plotting
#create data frame with relevant variables
pd <- data.frame(ampcoha$Decade, ampcoha$Genre, ampcoha$Function, ampcoha$Adjective, ampcoha$Amplified, ampcoha$Variant)
# clean col names
colnames(pd) <- gsub("ampcoha.", "", colnames(pd))
# multiply int * 100 to get percent for pint
pd$Amplified <- as.numeric(ifelse(pd$Amplified == 1, 100, 0))
famp <- names(table(pd$Variant))[which(table(pd$Variant) > 900)]
# reclassify amplifiers - infreq. adjs are collapsed into category other
pd$Variant <- ifelse(pd$Variant %in% famp, pd$Variant, "other")
# create vars for pint
pd$zero <- ifelse(pd$Variant == "0", 100, 0)
pd$other <- ifelse(pd$Variant == "other", 100, 0)
pd$pretty <- ifelse(pd$Variant == "pretty", 100, 0)
pd$really <- ifelse(pd$Variant == "really", 100, 0)
pd$so <- ifelse(pd$Variant == "so", 100, 0)
pd$very <- ifelse(pd$Variant == "very", 100, 0)
# inspect data
str(pd); class(pd); head(pd)

###############################################################
# p0 pseudo plot
p0d <- pd

# start plot: all
p0 <- ggplot(p0d, aes(x = Decade, y = Amplified)) +
  facet_grid(vars(Function), vars(Genre)) +
  geom_smooth(aes(y = Amplified), size=.25, col = "grey30", lty = "longdash", se = F) +
  #  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 30)) +
  labs(x = "Year", y = "Percent of Amplification") #+
###############################################################
# p0 amplified in fiction
p0d <- pd
p0d <- subset(p0d, Function == "Predicative")
p0d <- subset(p0d, Genre == "Fiction")

# start plot: all
p0 <- ggplot(p0d, aes(x = Decade, y = Amplified)) +
  geom_smooth(aes(y = Amplified), size=.25, col = "grey30", lty = "longdash", se = F) +
  #  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Year", y = "Percent of Amplification") #+
ggsave(file = paste(imageDirectory,"AmplifiedFictionPredicative.png",sep="/"))
p0

###############################################################
# p1
p1d <- pd

# start plot: all
p1 <- ggplot(p1d, aes(x = Decade, y = Amplified)) +
  facet_grid(vars(Function), vars(Genre)) +
  geom_smooth(aes(y = Amplified), size=.25, col = "grey30", lty = "longdash", se = F) +
  #  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Year", y = "Percent of Amplification") #+
ggsave(file = paste(imageDirectory,"AmplifiedGenreFunction.png",sep="/"))
p1

###############################################################
# remove non-amplified instances
p2d <- pd[pd$Amplified != 0,]
p2d <- subset(p2d, Genre == "Fiction")
# start plot: all
p2 <- ggplot(p2d, aes(x = Decade, y = very)) +
  facet_grid(vars(Function)) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size = .5, se = F) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size = .5, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size = .5, se = F) +
  geom_smooth(aes(y = pretty, color = "pretty", linetype = "pretty"), size = .5, se = F) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size = .5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed","twodash", "longdash", "dotdash","solid"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very"), 
                        labels = c("other", "pretty", "really", "so", "very")) +
  scale_colour_manual(values=c("indianred4","goldenrod2", "grey50", "grey70", "grey30"),
                      name="Variant", 
                      breaks=c("other", "pretty", "really", "so", "very"), 
                      labels = c("other", "pretty", "really", "so", "very")) +
  theme_set(theme_light(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Year", y = "Percent of Amplification") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  #  guides(linetype = FALSE)+
  theme(legend.title=element_blank()) 
ggsave(file = paste(imageDirectory,"VariantFictionFunction.png",sep="/"))
p2

###############################################################
# remove non-amplified instances
p3d <- pd[pd$Amplified != 0,]

# start plot: all
p3 <- ggplot(p3d, aes(x = Decade, y = very)) +
  facet_grid(vars(Function), vars(Genre)) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size = .5, se = F) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size = .5, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size = .5, se = F) +
  geom_smooth(aes(y = pretty, color = "pretty", linetype = "pretty"), size = .5, se = F) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size = .5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed","twodash", "longdash", "dotdash","solid"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very"), 
                        labels = c("other", "pretty", "really", "so", "very")) +
  scale_colour_manual(values=c("indianred4","goldenrod2", "grey50", "grey70", "grey30"),
                      name="Variant", 
                      breaks=c("other", "pretty", "really", "so", "very"), 
                      labels = c("other", "pretty", "really", "so", "very")) +
  theme_set(theme_light(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Year", y = "Percent of Amplification") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  #  guides(linetype = FALSE)+
  theme(legend.title=element_blank()) 
ggsave(file = paste(imageDirectory,"VariantGenreFunction.png",sep="/"))
p3


###############################################################
# p4
p4d <- pd

# start plot: all
p4 <- ggplot(p4d, aes(x = Decade, y = zero)) +
  facet_grid(vars(Function), vars(Genre)) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size = .5, se = F) +
  geom_smooth(aes(y = pretty, color = "pretty", linetype = "pretty"), size = .5, se = F) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size = .5, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size = .5, se = F) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size = .5, se = F) +
  geom_smooth(aes(y = zero, color = "zero", linetype = "zero"), size = .5, se = F) +  
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed","twodash", "longdash", "dotdash","solid","solid"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very", "zero"), 
                        labels = c("other", "pretty", "really", "so", "very", "zero")) +
  scale_colour_manual(values=c("indianred4","goldenrod2", "grey50", "grey70", "grey30", "grey30"),
                      name="Variant", 
                      breaks=c("other", "pretty", "really", "so", "very", "zero"), 
                      labels = c("other", "pretty", "really", "so", "very", "zero")) +
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Year", y = "Percent of Amplification") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  #  guides(linetype = FALSE)+
  #  theme_minimal()
  theme(legend.title=element_blank()) 
ggsave(file = paste(imageDirectory,"VariantGenreFunctionWZ.png",sep="/"))
p4

###############################################################
#                 WARNING
#             DATA REDUCTION
# ampcoha
ampcoha <- subset(ampcoha, Function == "Predicative")
ampcoha <- subset(ampcoha, Genre == "Fiction")
ampcoha <- ampcoha[complete.cases(ampcoha), ]
# pd
pd <- subset(pd, Function == "Predicative")
pd <- subset(pd, Genre == "Fiction")
pd <- pd[complete.cases(pd), ]
###############################################################
# remove non-amplified instances
p5d <- pd[pd$Amplified != 0,]

# start plot: all
p5 <- ggplot(p5d, aes(x = Decade, y = very)) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size = .5, se = F) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size = .5, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size = .5, se = F) +
  geom_smooth(aes(y = pretty, color = "pretty", linetype = "pretty"), size = .5, se = F) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size = .5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed","twodash", "longdash", "dotdash","solid"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very"), 
                        labels = c("other", "pretty", "really", "so", "very")) +
  scale_colour_manual(values=c("indianred4","goldenrod2", "grey50", "grey70", "grey30"),
                      name="Variant", 
                      breaks=c("other", "pretty", "really", "so", "very"), 
                      labels = c("other", "pretty", "really", "so", "very")) +
  theme_set(theme_light(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Year", y = "Percent of Amplification") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  #  guides(linetype = FALSE)+
  theme(legend.title=element_blank()) 
ggsave(file = paste(imageDirectory,"VariantFiction.png",sep="/"))
p5

###############################################################
#                  TABULATE
# tb1
tb1d <- subset(ampcoha, Variant != "0")
tb1d <- table(tb1d$Decade, tb1d$Variant)
tb1 <- t(round(prop.table(tb1d, margin = 1)*100, 1))
tb1 <- cbind(tb1,rowSums(tb1)) 
# inspect table
head(tb1)

# save data to disc
write.table(tb1, "tb1.txt", sep = "\t", row.names = T)
###############################################################
# tb2b
tb2d <- subset(ampcoha, Variant != "0")
infreqamp <- names(table(tb2d$Variant))[which(table(tb2d$Variant) < 150)]
tb2d$Variant <- ifelse(tb2d$Variant %in% infreqamp, "other", tb2d$Variant)
tb2d <- table(tb2d$Decade, tb2d$Variant)
tb2 <- t(round(prop.table(tb2d, margin = 1)*100, 1))
# inspect table
head(tb2)

# save data to disc
write.table(tb2, "tb2.txt", sep = "\t", row.names = T)
###############################################################
# tb4
tb4d <- table(ampcoha$Variant)
tb4 <- tb4d[order(table(ampcoha$Variant), decreasing = T)]
pintnames <- as.vector(names(tb4))
pintn <- as.vector(tb4)
pintprcnt <- round(pintn/sum(pintn)*100, 2)
pintprcnt2 <-  c(0, round(pintn[2:length(pintn)]/sum(pintn[2:length(pintn)])*100, 2))
tb4 <- data.frame(pintnames, pintn, pintprcnt, pintprcnt2)
colnames(tb4) <- c("Intensifier", "TokenFrequency", "PercentageSlots", "PercentageIntensifiers")
tb4 <- rbind(tb4, c("Total", sum(as.vector(tb4$TokenFrequency)), "", ""))
rownames(tb4) <- NULL
head(tb4)

# save data to disc
write.table(tb4, "tb4.txt", sep = "\t", row.names = F)
###############################################################
tb6d <- subset(ampcoha, Variant != "0")
frqadj <- names(table(tb6d$Adjective))[which(table(tb6d$Adjective) >=150)]
tb6d$Adjective <- ifelse(tb6d$Adjective %in% frqadj, tb6d$Adjective, "other")
frqamp <- names(table(tb6d$Variant))[which(table(tb6d$Variant) >=150, useNames = F)]
tb6d$Variant <- ifelse(tb6d$Variant %in% frqamp, tb6d$Variant, "other")
tb6 <- ftable(tb6d$Adjective, tb6d$Variant, tb6d$Decade)
tb6

# save data to disc
write.table(tb6, "tb6.txt", sep = "\t", row.names = F)
###############################################################
tb7d <- subset(ampcoha, Variant != "0")
frqadj <- names(table(tb7d$Adjective))[which(table(tb7d$Adjective) >=150)]
tb7d$Adjective <- ifelse(tb7d$Adjective %in% frqadj, tb7d$Adjective, "other")
frqamp <- names(table(tb7d$Variant))[which(table(tb7d$Variant) >=150, useNames = F)]
tb7d$Variant <- ifelse(tb7d$Variant %in% frqamp, tb7d$Variant, "other")
tb7d <- ftable(tb7d$Decade, tb7d$Adjective, tb7d$Variant)
tb7d <- as.matrix(round(prop.table(tb7d, margin = 1)*100, 1))
tb7 <- data.frame(gsub("_.*", "", rownames(tb7d)), gsub(".*_", "", rownames(tb7d)), tb7d)
colnames(tb7)[1:2] <- c("Decade", "Adjective")
tb7 <- tb7[order(tb7$Adjective),]

# save data to disc
write.table(tb7, "tb7.txt", sep = "\t", row.names = T)
###############################################################
library(dplyr)
library(tidyr)
tb8d <- subset(ampcoha, Variant != "0")
frqadj <- names(table(tb8d$Adjective))[which(table(tb8d$Adjective) >=250)]
tb8d$Adjective <- ifelse(tb8d$Adjective %in% frqadj, tb8d$Adjective, "other")
frqamp <- names(table(tb8d$Variant))[which(table(tb8d$Variant) >=150, useNames = F)]
tb8d$Variant <- ifelse(tb8d$Variant %in% frqamp, tb8d$Variant, "other")
tb8 <- tb8d %>%
  group_by(Decade, Adjective, Variant) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n)*100, 1))
tb8 <- select(tb8, -n)
tb8 <- arrange(tb8, Decade)
tb8 <- spread(tb8, Decade, freq)
tb8 <- select(tb8, `Variant`, `Adjective`, `1850`, `1880`, `1910`, `1940`, `1970`, `2000`)
tb8 <- tb8 %>% 
  na.omit
tb8

# save data to disc
write.table(tb8, "tb8.txt", sep = "\t", row.names = T)
###############################################################
adjdate <- as.numeric(colnames(tb8[3:ncol(tb8)]))
adjlm <- tb8[, 3:ncol(tb8)]
#head(adjlm); str(adjlm); nrow(adjlm)
sigadj <- apply(adjlm, 1, function(x){
  x <- lm(x ~ adjdate)
  x <- summary(x)[4][[1]][[8]]})
sigadjs <- which(sigadj < .05)
sigadjs

###############################################################
#                  PLOTTING
# prepare data plotting
#create data frame with relevant variables
pd <- data.frame(ampcoha$Decade, ampcoha$Genre, ampcoha$Function, ampcoha$Amplified, ampcoha$Variant)
# clean col names
colnames(pd) <- gsub("ampcoha.", "", colnames(pd))
# multiply int * 100 to get percent for pint
pd$Amplified <- as.numeric(ifelse(pd$Amplified == 1, 100, 0))
famp <- names(table(pd$Variant))[which(table(pd$Variant) > 150)]
# reclassify amplifiers - infreq. adjs are collapsed into category other
pd$Variant <- ifelse(pd$Variant %in% famp, pd$Variant, "other")
# create vars for pint
pd$zero <- ifelse(pd$Variant == "0", 100, 0)
pd$other <- ifelse(pd$Variant == "other", 100, 0)
pd$pretty <- ifelse(pd$Variant == "pretty", 100, 0)
pd$really <- ifelse(pd$Variant == "really", 100, 0)
pd$so <- ifelse(pd$Variant == "so", 100, 0)
pd$very <- ifelse(pd$Variant == "very", 100, 0)
# inspect data
str(pd); class(pd); head(pd)

###############################################################
# remove non-amplified instances
p6d <- pd[pd$Amplified != 0,]

# start plot: all
p6 <- ggplot(p6d, aes(x = Decade, y = very)) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size = .5, se = F) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size = .5, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size = .5, se = F) +
  geom_smooth(aes(y = pretty, color = "pretty", linetype = "pretty"), size = .5, se = F) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size = .5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed","twodash", "longdash", "dotdash","solid"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very"), 
                        labels = c("other", "pretty", "really", "so", "very")) +
  scale_colour_manual(values=c("indianred4","goldenrod2", "grey50", "grey70", "grey30"),
                      name="Variant", 
                      breaks=c("other", "pretty", "really", "so", "very"), 
                      labels = c("other", "pretty", "really", "so", "very")) +
  theme_set(theme_light(base_size = 8)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Year", y = "Percent of Amplification") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  #  guides(linetype = FALSE)+
  theme(legend.title=element_blank()) 
ggsave(file = paste(imageDirectory,"fic_decade_variant.png",sep="/"))
p6

###############################################################
#              SEMANTIC VECTOR SPACE MODEL 2
# evaluation how strongly so and very correlate
# tabulate data
t1 <- tapply(ampcoha$Amplified, list(ampcoha$Adjective, ampcoha$Variant), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3coha <- t3
t3coha <- t3coha[, 2: ncol(t3coha)]
# remove Adjectives that were not Amplifiedensified
t3coha <- t3coha[rowSums(t3coha) > 0, ]
# save row and column names
colnamescoha <- colnames(t3coha)
rownamescoha <- rownames(t3coha)
# turn dataframe Amplifiedo matrix
svsmcoha <- as.matrix(t3coha)
# convert token frequency to type frequency
#svsmcoha <- apply(svsmcoha, 1, Function(x) { x <- ifelse(x > 1, 1, x) } )
svsmcoha <- t(svsmcoha)
#svsmcoha <- svsmcoha[, colSums(svsmcoha) >= 2]
#svsmcoha <- svsmcoha[rowSums(svsmcoha) >= 2, ]
#svsmcoha

# determine overall n in data
n_coha <- sum(svsmcoha)
#n_coha

# correlate amplifiers based on collocation
r_coha <- cor(t(svsmcoha))
#r_coha

# extract correlation coefficient r for so and very
r_soverycoha <- r_coha[which(attr(r_coha, "dimnames")[[1]] == "very"), 
                           which(attr(r_coha, "dimnames")[[2]] == "so")]
r_soverycoha

# extract correlation coefficient r for positively and very
r_positivelyverycoha <- r_coha[which(attr(r_coha, "dimnames")[[1]] == "very"), 
                       which(attr(r_coha, "dimnames")[[2]] == "positively")]
r_positivelyverycoha

# extract correlation coefficient r for positively and very
r_positivelysocoha <- r_coha[which(attr(r_coha, "dimnames")[[1]] == "so"), 
                               which(attr(r_coha, "dimnames")[[2]] == "positively")]
r_positivelysocoha


# load required library
library(psych)
z_soverycoha <- fisherz(r_soverycoha)
z_soverycoha

# the z value can be tested for significance using the r-test from the psych library
#r.test(n=100,r12=.5,r34=.4, n2=80) 
###############################################################
#              SEMANTIC VECTOR SPACE MODEL 3
# evaluation how strongly really and very correlate
# tabulate data
t1 <- tapply(ampcoha$Amplified, list(ampcoha$Adjective, ampcoha$Variant), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3coha <- t3
t3coha <- t3coha[, 2: ncol(t3coha)]
# remove Adjectives that were not Amplifiedensified
t3coha <- t3coha[rowSums(t3coha) > 0, ]
# save row and column names
colnamescoha <- colnames(t3coha)
rownamescoha <- rownames(t3coha)
# turn dataframe Amplifiedo matrix
svsmcoha <- as.matrix(t3coha)
# convert token frequency to type frequency
#svsmcoha <- apply(svsmcoha, 1, Function(x) { x <- ifelse(x > 1, 1, x) } )
svsmcoha <- t(svsmcoha)
#svsmcoha <- svsmcoha[, colSums(svsmcoha) >= 2]
#svsmcoha <- svsmcoha[rowSums(svsmcoha) >= 2, ]
svsmcoha

# compute expected values
svsmcoha.exp <- chisq.test(svsmcoha)$expected
# calculate PMI and PPMI
svsmcoha.PMI <- log2(svsmcoha/svsmcoha.exp)
svsmcoha.PPMI <- ifelse(svsmcoha.PMI < 0, 0, svsmcoha.PMI)
# calculate cosine similarity
svsmcoha.tmp1 <- svsmcoha.PPMI
svsmcoha.cos <- cossim(svsmcoha.tmp1)
#round(svsmcoha.cos, 2)
###############################################################
#               CLUSTER SEMANTIC VECTORS
# load library
library(cluster)
# find max value that is not 1
svsmcoha.cos.test <- apply(svsmcoha.cos, 1, function(x){
  x <- ifelse(x == 1, 0, x) } )
maxval <- max(svsmcoha.cos.test)
# create distance matrix
svsmcoha.dist <- 1 - (svsmcoha.cos/maxval)
clustd <- as.dist(svsmcoha.dist)
# create distance matrix
clustd <- dist(svsmcoha.cos, method = "manhattan") # create distance matrix (manhattan method: most popular choice)
# alternative methods
# eucledian - not good when dealing with many dimensions
# manhattan - most popular choice
# method - here the difference between points dominates
# canberra - for count data
# binary - for binary data only!
# minkowski - is not a true distance measure

# find optimal number of clusters
asw <- as.vector(unlist(sapply(2:nrow(svsmcoha)-1, function(x) pam(clustd, k = x)$silinfo$avg.width)))
# determine the optimal number of clusters (max width is optimal)
optclust <- which(asw == max(asw))+1 # optimal number of clusters

# inspect clustering with optimal number of clusters
svsmcoha.clust <- pam(clustd, optclust)
svsmcoha.clust$clustering

# create cluster object
# alternative methods: "single", "ward.D2", "average", "mcquitty", "median", "centroid"
ampcohahclust <- hclust(clustd, method="average") 
# load library
library(graphics)
# set parameters for plotting
opar <- par(mar = c(5, 4, 4, 2) + 0.1)      # make a copy of current settings
par(mar = c(3, 1, 1, 20) + 0.1)
# load libraries for nicer dendrograms
library(factoextra)
library(dendextend)
# plot with colored clusters
png("images/Clustcoha_fviz2.png",  width = 400, height = 1000) # save plot
par(mar = c(3, 1, 1, 20) + 0.1)
fviz_dend(ampcohahclust, k = optclust, cex = .8, horiz =T,  
          k_colors = c(rep("black", optclust)), 
          rect_border = c(rep("black", optclust)), 
          rect_fill = F, main = "", labels_track_height=2, rect = F)
dev.off()
# clustering of cor matrix
png("images/Clustcoha_fviz3.png",  width = 400, height = 1000) # save plot
par(mar = c(3, 1, 1, 20) + 0.1)
fviz_dend(hclust(dist(r_coha), method="average"), k = optclust, cex = .8, horiz =T,  
          k_colors = c(rep("black", optclust)), 
          rect_border = c(rep("black", optclust)), 
          rect_fill = F, main = "", labels_track_height=2, rect = F) 
dev.off()
par(opar)          # restore original settings 
###############################################################
#               PLOTTING LEXICAL DIVESRITY
# function for extracting lexdiv values
lexdiv <- function(x){
  VariantAdjectivetbcoha <- table(x$Variant, x$Adjective)
  VariantAdjectivetbcoha <- VariantAdjectivetbcoha[2:nrow(VariantAdjectivetbcoha), ]
  VariantAdjectivetbcoha <- VariantAdjectivetbcoha[rowSums(VariantAdjectivetbcoha) > 10, ]
  # extract typefrequency of Adjectiveectives
  VariantAdjectivetbcohatyp <- t(apply(VariantAdjectivetbcoha, 1, function(x) ifelse(x > 1, 1, x)  ))
  # claculate lexical diversity measure
  lexdivcoha <- rowSums(VariantAdjectivetbcohatyp)/rowSums(VariantAdjectivetbcoha)
  lexdivcoha <- lexdivcoha[order(lexdivcoha)]
  return(lexdivcoha)
}
# apply function to data
lexdivcoha <- lexdiv(ampcoha)
# ggplot2 p5
lexdivdf <- data.frame(1:length(lexdivcoha), names(lexdivcoha), round(lexdivcoha, 2))
colnames(lexdivdf) <- c("id", "amp", "lexdiv")

# start plot: int
p7 <- ggplot(lexdivdf, aes(x = jitter(id), y = lexdiv, label = lexdivdf$lexdiv), size = 8) +
  geom_line(aes(y = lexdiv), col = "indianred4", lwd = .5) + 
  geom_smooth(aes(y = lexdiv), size=.5, col = "gray30", lty = "dotted", se = F) +
  geom_text(label = lexdivdf$lexdiv, hjust = 0.1, nudge_y = -0.1, size = 1.5, angle=0) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Amplifier Type", y = "Lexical Diversity") +
  theme_light(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(name = "Amplifier Type",
                     breaks = c(1:length(lexdivdf$amp)),
                     labels=lexdivdf$amp)
ggsave(file = paste(imageDirectory,"lexdiv_coha.png", sep="/"), width = 15,  height = 7.5, units = c("cm"),  dpi = 320)
p7

###############################################################
#            PLOT LEXICAL DIVERSITY OVER TIME
ldd <- ampcoha
vld <- names(table(ldd$Variant))[which(table(ldd$Variant) > 150)]
ldd$Variant <- ifelse(ldd$Variant == "0", ldd$Variant,
                      ifelse(ldd$Variant %in% vld, ldd$Variant, "other"))

# extract Adjectivefrequency of intensifiers
a1850 <- subset(ldd, Decade == 1850)
a1860 <- subset(ldd, Decade == 1860)
a1870<- subset(ldd, Decade == 1870)
a1880<- subset(ldd, Decade == 1880)
a1890<- subset(ldd, Decade == 1890)
a1900<- subset(ldd, Decade == 1900)
a1910<- subset(ldd, Decade == 1910)
a1920<- subset(ldd, Decade == 1920)
a1930<- subset(ldd, Decade == 1930)
a1940<- subset(ldd, Decade == 1940)
a1950<- subset(ldd, Decade == 1950)
a1960<- subset(ldd, Decade == 1960)
a1970<- subset(ldd, Decade == 1970)
a1980<- subset(ldd, Decade == 1980)
a1990<- subset(ldd, Decade == 1990)
a2000<- subset(ldd, Decade == 2000)
# apply function to data sets
lexdiv1850 <- lexdiv(a1850)
lexdiv1860 <- lexdiv(a1860)
lexdiv1870 <- lexdiv(a1870)
lexdiv1880 <- lexdiv(a1880)
lexdiv1890 <- lexdiv(a1890)
lexdiv1900 <- lexdiv(a1900)
lexdiv1910 <- lexdiv(a1910)
lexdiv1920 <- lexdiv(a1920)
lexdiv1930 <- lexdiv(a1930)
lexdiv1940 <- lexdiv(a1940)
lexdiv1950 <- lexdiv(a1950)
lexdiv1960 <- lexdiv(a1960)
lexdiv1970 <- lexdiv(a1970)
lexdiv1980 <- lexdiv(a1980)
lexdiv1990 <- lexdiv(a1990)
lexdiv2000 <- lexdiv(a2000)
# find common items
cmnamps <- Reduce(intersect, list(names(lexdiv1850),names(lexdiv1860),
                                  names(lexdiv1870),names(lexdiv1880),
                                  names(lexdiv1890),names(lexdiv1900),
                                  names(lexdiv1910),names(lexdiv1920),
                                  names(lexdiv1930),names(lexdiv1940),
                                  names(lexdiv1950),names(lexdiv1960),
                                  names(lexdiv1970),names(lexdiv1980),
                                  names(lexdiv1990),names(lexdiv2000)))
# extract lex div values for amps which occur in all age groups
lexdivvls <- data.frame(lexdiv1850[which(names(lexdiv1850) %in% cmnamps)], 
                        lexdiv1860[which(names(lexdiv1860) %in% cmnamps)], 
                        lexdiv1870[which(names(lexdiv1870) %in% cmnamps)], 
                        lexdiv1880[which(names(lexdiv1880) %in% cmnamps)], 
                        lexdiv1890[which(names(lexdiv1890) %in% cmnamps)], 
                        lexdiv1900[which(names(lexdiv1900) %in% cmnamps)], 
                        lexdiv1910[which(names(lexdiv1910) %in% cmnamps)], 
                        lexdiv1920[which(names(lexdiv1920) %in% cmnamps)], 
                        lexdiv1930[which(names(lexdiv1930) %in% cmnamps)], 
                        lexdiv1940[which(names(lexdiv1940) %in% cmnamps)], 
                        lexdiv1950[which(names(lexdiv1950) %in% cmnamps)], 
                        lexdiv1960[which(names(lexdiv1960) %in% cmnamps)], 
                        lexdiv1970[which(names(lexdiv1970) %in% cmnamps)], 
                        lexdiv1980[which(names(lexdiv1980) %in% cmnamps)], 
                        lexdiv1990[which(names(lexdiv1990) %in% cmnamps)], 
                        lexdiv2000[which(names(lexdiv2000) %in% cmnamps)] 
)
# transpose data
lexdivvlst <- t(lexdivvls)
# combine lexdiv tables
p8d <- data.frame(1:length(names(table(ldd$Decade))), names(table(ldd$Decade)), lexdivvlst)
colnames(p8d)[1:2] <- c("id", "decade")
rownames(p8d) <- 1:nrow(p8d)
head(p8d)

# start plot: lexdiv across real time
p8 <- ggplot(p8d, aes(x = jitter(id), y = very, label = decade), size = 8) +
  geom_smooth(aes(y = very, color = "very", lty = "very"), size=.5, se = F) +
  geom_smooth(aes(y = so, color = "so", lty = "so"), size=.5, se = F) +
  geom_smooth(aes(y = other, color = "other", lty = "other"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("solid", "longdash", "dotted"),
                        name="",
                        breaks = c("very", "so", "other"), 
                        labels = c("very", "so", "other")) +
  scale_colour_manual(values=c("indianred4",  "goldenrod2", "gray30"),
                      name="", 
                      breaks=c("very", "so", "other"), 
                      labels = c("very", "so", "other")) +
  theme(legend.position="top") +
  theme_light(base_size = 8) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Amplifier Type", y = "Lexical Diversity") +
  scale_x_continuous(name = "Decade",
                     breaks = c(1:length(p8d$decade)),
                     labels=p8d$decade)
ggsave(file = paste(imageDirectory,"lexdivdecade.png", sep="/"), width = 15,  height = 7.5, units = c("cm"),  dpi = 320)
p8

###############################################################
#           COVARYING COLLEXEME ANALYSIS
# collex function
collex <- function(data = data, cv1 = cv1){
  # set up rslttb
  rslttb <- matrix(c("amp", "Adjective", "namp", "nAdjective", "obs", "exp", "prob", "cs", "or", "p"), ncol = 10)
  colnames(rslttb) <- c("Amp", "Adjective", "N(Amp)", "N(Adjective)", "OBS", "EXP", 
                        "Probability", "CollStrength", "OddsRatio", "p")
  rvs <- 1:nrow(t3)
  # define column values
  cv0 <- 1
  # set up table
  sapply(rvs, function(x){
    # extract values
    obs <- t3[x,cv1] # freq Adjective with amp
    fAdjective <- sum(t3[x,]) # freq Adjective
    n <- sum(t3[,cv1]) # freq amp
    fall <- sum(t3) # freq amps and Adjectives
    # calculate exp
    exp <- (fAdjective*n)/fall
    prob <- exp/n
    # create table to extract odds ratios
    m <- matrix(c(obs, (n-obs), (fAdjective-obs), (fall-fAdjective)), nrow = 2, byrow = T)
    o <- fisher.test(m)
    # perform binomial test
    rslt <- binom.test(obs, n, prob)
    # set up table with results
    rslttb <- list(c(colnames(data)[cv1], 
                     rownames(data)[x], 
                     n, 
                     fAdjective,
                     obs,
                     round(exp, 1),
                     round(prob, 2),
                     round(abs(log(as.vector(unlist(rslt$p.value, 10)), 10)), 2), 
                     round(as.vector(unlist(o$estimate)), 2),
                     round(as.vector(unlist(rslt$p.value)), 6)
    ))
    # return results
    return(rslttb)
  } )
}
###############################################################
#                 CCLA ON DATA IRRESPECTIVE OF TIME
# rename data
cclad <- ampcoha
# define and recode infrequent adjectives
ntfrqadj <- names(table(cclad$Adjective))[which(table(cclad$Adjective) <= 150)]
cclad$Adjective <- ifelse(cclad$Adjective %in% ntfrqadj, "other", cclad$Adjective)
# define and recode infrequent amplifiers
ntfrqamp <- names(table(cclad$Variant))[which(table(cclad$Variant) <= 150)]
cclad$Variant <- ifelse(cclad$Variant %in% ntfrqamp, "other", cclad$Variant)

# create table
t1 <- tapply(cclad$Amplified, list(cclad$Adjective, cclad$Variant), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex Function (Variants >= 150: 
colnames(t3)[which(colSums(t3) >= 150)]

which(colSums(t3) >= 150)

Variants <- names(which(colSums(t3) >= 150))
# extract data
other  <- collex(data = t3, cv1 = which(colnames(t3) == "other"))
pretty  <- collex(data = t3, cv1 = which(colnames(t3) == "pretty"))
really  <- collex(data = t3, cv1 = which(colnames(t3) == "really"))
so  <- collex(data = t3, cv1 = which(colnames(t3) == "so"))
very  <- collex(data = t3, cv1 = which(colnames(t3) == "very"))
# extract informaltion
other <- matrix(unlist(other),ncol=10,byrow=TRUE)
pretty <- matrix(unlist(pretty),ncol=10,byrow=TRUE)
really <- matrix(unlist(really),ncol=10,byrow=TRUE)
so <- matrix(unlist(so),ncol=10,byrow=TRUE)
very <- matrix(unlist(very),ncol=10,byrow=TRUE)
# set up table with results
collextab <- rbind(other, pretty,  really, so, very)
# write Function to process collextab (input = collextab)
collextbedit <- function(collextab){
  # convert Amplifiedo data frame
  collexdf <- as.data.frame(collextab)
  # add colnames
  colnames(collexdf) <- c("Amp", "Adjective", "N(Amp)", "N(Adjective)", "OBS", "EXP", 
                          "Probability", "CollStrength", "OddsRatio", "p")
  # add attraction column
  collexdf$attr <- ifelse(as.numeric(collexdf$OBS) > as.numeric(collexdf$EXP), "attr", "repel")
  # modify CollStrength column 
  collexdf$CollStrength <- ifelse(collexdf$attr == "repel", 
                                  paste("-", collexdf$CollStrength, sep =""), collexdf$CollStrength)
  # perform bonferroni correction
  corr05 <- 0.05/nrow(collexdf)
  collexdf$corr05 <- rep(corr05, nrow(collexdf))
  corr01 <- 0.01/nrow(collexdf)
  collexdf$corr01 <- rep(corr01, nrow(collexdf))
  corr001 <- 0.001/nrow(collexdf)
  collexdf$corr001 <- rep(corr001, nrow(collexdf))
  # calculate corrected significance status
  collexdf$sig <- as.vector(unlist(sapply(collexdf$p, function(x){
    x <- ifelse(x <= corr001, "p<.001",
                ifelse(x <= corr01, "p<.01",
                       ifelse(x <= corr001, "p<.001", "n.s."))) } )))
  return(collexdf)
}
# apply collextbedit Function
collex_all <- collextbedit(collextab)
# inspect results
subset(collex_all, sig != "n.s.")

write.table(collex_all, "collex_all.txt", sep = "\t", row.names = T)
###############################################################
#                collextime function
# write function for extracting collexemes across time
collextime <- function(data, decade){
  amps <- c("other", "pretty", "really", "so", "very")
  # covarying collexeme analysis  on data = data from time = decade
  df <- data[data$Decade == decade,]
  df <- df[complete.cases(df),]
  t1 <- tapply(df$Amplified, list(df$Adjective, df$Variant), table)
  t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
  t3 <- t(t2)
  t3 <- t3[, 2:ncol(t3)]
  t3 <- t3[rowSums(t3) > 0, ]
  # apply collex function to items in predefined vector amps
  other  <- collex(data = t3, cv1 = which(colnames(t3) == "other"))
  pretty  <- collex(data = t3, cv1 = which(colnames(t3) == "pretty"))
  really  <- collex(data = t3, cv1 = which(colnames(t3) == "really"))
  so  <- collex(data = t3, cv1 = which(colnames(t3) == "so"))
  very  <- collex(data = t3, cv1 = which(colnames(t3) == "very"))
  # extract informaltion
  other <- matrix(unlist(other),ncol=10,byrow=TRUE)
  pretty <- matrix(unlist(pretty),ncol=10,byrow=TRUE)
  really <- matrix(unlist(really),ncol=10,byrow=TRUE)
  so <- matrix(unlist(so),ncol=10,byrow=TRUE)
  very <- matrix(unlist(very),ncol=10,byrow=TRUE)
  # set up table with results
  collextab <- rbind(other, pretty, really, so, very)
  # write function to process collextab (input = collextab)
  collextbedit <- function(collextab){
    # convert ampo data frame
    collexdf <- as.data.frame(collextab)
    # add colnames
    colnames(collexdf) <- c("Amp", "Adjective", "N(Amp)", "N(Adj)", "OBS", "EXP", 
                            "Probability", "CollStrength", "OddsRatio", "p")
    # add attraction column
    collexdf$attr <- ifelse(as.numeric(collexdf$OBS) > as.numeric(collexdf$EXP), "attr", "repel")
    # modify CollStrength column 
    collexdf$CollStrength <- ifelse(collexdf$attr == "repel", 
                                    paste("-", collexdf$CollStrength, sep =""), collexdf$CollStrength)
    # perform bonferroni correction
    corr05 <- 0.05/nrow(collexdf)
    collexdf$corr05 <- rep(corr05, nrow(collexdf))
    corr01 <- 0.01/nrow(collexdf)
    collexdf$corr01 <- rep(corr01, nrow(collexdf))
    corr001 <- 0.001/nrow(collexdf)
    collexdf$corr001 <- rep(corr001, nrow(collexdf))
    # calculate corrected significance status
    collexdf$sig <- as.vector(unlist(sapply(collexdf$p, function(x){
      x <- ifelse(x <= corr001, "p<.001",
                  ifelse(x <= corr01, "p<.01",
                         ifelse(x <= corr001, "p<.001", "n.s."))) } )))
    return(collexdf)
  }
  # apply collextbedit function
  collexdecade<- collextbedit(collextab)
  collexdecade <- collexdecade[complete.cases(collexdecade),]
  return(collexdecade)
}
###############################################################
#                     CCLA OVER TIME
# start with 1870 as there are no reallys in 1900 data
collex1900 <- collextime(cclad, 1900)
collex1910 <- collextime(cclad, 1910)
collex1920 <- collextime(cclad, 1920)
collex1930 <- collextime(cclad, 1930)
collex1940 <- collextime(cclad, 1940)
collex1950 <- collextime(cclad, 1950)
collex1960 <- collextime(cclad, 1960)
collex1970 <- collextime(cclad, 1970)
collex1980 <- collextime(cclad, 1980)
collex1990 <- collextime(cclad, 1990)
collex2000 <- collextime(cclad, 2000)
###############################################################
# combine covar collex data frames
collexcoha <- rbind(collex1900[,c(1:11,15)], 
                    collex1910[,c(1:11,15)], collex1920[,c(1:11,15)], 
                    collex1930[,c(1:11,15)], collex1940[,c(1:11,15)], 
                    collex1950[,c(1:11,15)], collex1960[,c(1:11,15)], 
                    collex1970[,c(1:11,15)], collex1980[,c(1:11,15)], 
                    collex1990[,c(1:11,15)], collex2000[,c(1:11,15)])
Decade <- c(rep("1900", nrow(collex1900)),
            rep("1910", nrow(collex1910)), rep("1920", nrow(collex1920)),
            rep("1930", nrow(collex1930)), rep("1940", nrow(collex1940)),
            rep("1950", nrow(collex1950)), rep("1960", nrow(collex1960)),
            rep("1970", nrow(collex1970)), rep("1980", nrow(collex1980)),
            rep("1990", nrow(collex1990)), rep("2000", nrow(collex2000)))
# create data frame
covarcoldf <- data.frame(Decade, collexcoha)
#convert into numeric
covarcoldf$Decade <- as.numeric(covarcoldf$Decade)
covarcoldf$Probability <- as.numeric(covarcoldf$Probability)
covarcoldf$CollStrength <- as.numeric(covarcoldf$CollStrength)
covarcoldf$OddsRatio <- as.numeric(covarcoldf$OddsRatio)
# inspect data
str(covarcoldf); head(covarcoldf); summary(covarcoldf$CollStrength)

# save data to disc
write.table(covarcoldf, "collex_decade.txt", sep = "\t", row.names = T)
###########################################################################
# extract sig. collocations
sigcollex <- subset(covarcoldf, sig != "n.s.")
sosigcollex <- subset(sigcollex, Amp == "so")
verysigcollex <- subset(sigcollex, Amp == "very")
othersigcollex <- subset(sigcollex, Amp != "very" & Amp != "so")
# save data to disc
write.table(verysigcollex, "verysigcollex_decade.txt", sep = "\t", row.names = T)
write.table(sosigcollex, "sosigcollex_decade.txt", sep = "\t", row.names = T)
write.table(othersigcollex, "othersigcollex_decade.txt", sep = "\t", row.names = T)
###########################################################################
# rename data
p9d <- covarcoldf[, c(1:3, 9)] # decade, amp, adjective, collstrength
#extract adjectives present in all decades
alldecadesadj <- which(rowSums(ftable(p9d$Amp, p9d$Adjective, p9d$Decade)) == 
                         max(rowSums(ftable(p9d$Amp, p9d$Adjective, p9d$Decade))))
ampadjdecadeftb <- ftable(p9d$Amp, p9d$Adjective, p9d$Decade)
amp <- unlist(attr(ampadjdecadeftb, "row.vars")[1])
adj <- unlist(attr(ampadjdecadeftb, "row.vars")[2])
decade <- unlist(attr(ampadjdecadeftb, "col.vars")[1])
adjr <- rep(adj, length(amp))
ampr <- rep(amp, each = length(adj))
freqadjs1 <- unique(adjr[alldecadesadj])
freqadjs2 <- names(table(cclad$Adjective)[order(table(cclad$Adjective), decreasing = T)])[1:6]
freqadjs <- intersect(freqadjs1, freqadjs2)
# use only data with adjectives that are present among all amps and all decade groups
p9d <- subset(p9d, Adjective %in% freqadjs)
# create new id variable
p9d$decadeadj <- paste(p9d$Decade, "_", p9d$Adjective, sep = "")
# reorder data frame
p9tb <- reshape(p9d, idvar = "decadeadj", timevar = "Amp",direction = "wide")
# select relevant column
# decade, adjective, collstrength:pretty, collstrength:so, collstrength:really, collstrength:very
p9tb <- p9tb[, c(2:4, 7, 10, 13, 16)] 
colnames(p9tb) <- c("Decade", "Adjective", "other", "pretty", "really", "so", "very")
p9tb$Adjective <- as.factor(p9tb$Adjective)

# start plot: all
p9 <- ggplot(p9tb, aes(x = Decade, y = other)) +
  facet_wrap(vars(Adjective), ncol = 2) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size=.5, se = F) +
  geom_smooth(aes(y = pretty, color = "pretty", linetype = "pretty"), size=.5, se = F) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size=.5, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size=.5, se = F) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed","twodash", "dotdash", "longdash", "solid"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very"), 
                        labels = c("other", "pretty", "really", "so", "very")) +
  scale_colour_manual(values=c("grey90","goldenrod2", "gray60", "indianred4", "grey30"),
                      name="Variant", 
                      breaks=c("other", "pretty", "really", "so", "very"), 
                      labels = c("other", "pretty", "really", "so", "very")) +
  theme_set(theme_light(base_size = 8)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(-5, 15)) +
  labs(x = "Year", y = "Collocation Strength (LOG(p), 10)") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  #  guides(linetype = FALSE)+
  theme(legend.title=element_blank()) 
ggsave(file = paste(imageDirectory,"CovarcollAmpCOHAFreqAdjective1.png",sep="/"), width = 20, height = 20, units = c("cm"),  dpi = 320)
p9

###########################################################################
# rename data
p10d <- covarcoldf[, c(1:3, 9)] # decade, amp, adjective, collstrength
#extract adjectives present in all decades
alldecadesadj <- which(rowSums(ftable(p10d$Amp, p10d$Adjective, p10d$Decade)) == 
                         max(rowSums(ftable(p8d$Amp, p8d$Adjective, p8d$Decade))))
ampadjdecadeftb <- ftable(p10d$Amp, p10d$Adjective, p10d$Decade)
amp <- unlist(attr(ampadjdecadeftb, "row.vars")[1])
adj <- unlist(attr(ampadjdecadeftb, "row.vars")[2])
decade <- unlist(attr(ampadjdecadeftb, "col.vars")[1])
adjr <- rep(adj, length(amp))
ampr <- rep(amp, each = length(adj))
freqadjs1 <- unique(adjr[alldecadesadj])
freqadjs2 <- names(table(ampcoha$Adjective)[order(table(ampcoha$Adjective), decreasing = T)])[6:10]
freqadjs <- intersect(freqadjs1, freqadjs2)
# use only data with adjectives that are present among all amps and all decade groups
p10d <- subset(p10d, Adjective %in% freqadjs)
# create new id variable
p10d$decadeadj <- paste(p10d$Decade, "_", p10d$Adjective, sep = "")
# reorder data frame
p10tb <- reshape(p10d, idvar = "decadeadj", timevar = "Amp",direction = "wide")
# select relevant column
# decade, adjective, collstrength:pretty, collstrength:so, collstrength:really, collstrength:very
p10tb <- p10tb[, c(2:4, 7, 10, 13, 16)] 
colnames(p10tb) <- c("Decade", "Adjective", "other", "pretty", "really", "so", "very")
p10tb$Adjective <- as.factor(p10tb$Adjective)

# start plot: all
p10 <- ggplot(p10tb, aes(x = Decade, y = other)) +
  facet_grid(vars(Adjective)) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size=.5, se = F) +
  geom_smooth(aes(y = pretty, color = "pretty", linetype = "pretty"), size=.5, se = F) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size=.5, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size=.5, se = F) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed","twodash", "dotdash", "longdash", "solid"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very"), 
                        labels = c("other", "pretty", "really", "so", "very")) +
  scale_colour_manual(values=c("grey90","goldenrod2", "gray60", "indianred4", "grey30"),
                      name="Variant", 
                      breaks=c("other", "pretty", "really", "so", "very"), 
                      labels = c("other", "pretty", "really", "so", "very")) +
  theme_set(theme_light(base_size = 8)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(-15, 25)) +
  labs(x = "Year", y = "Collocation Strength (LOG(p), 10)") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  #  guides(linetype = FALSE)+
  theme(legend.title=element_blank()) 
ggsave(file = paste(imageDirectory,"CovarcollAmpCOHAFreqAdjective2.png",sep="/"), width = 7.5, height = 20, units = c("cm"),  dpi = 320)
p10

###############################################################
#                  CHANGES IN Adjective FREQ
# tabulate data
ftbadjcoha <- ftable(cclad$Adjective, cclad$Decade)
rwnms <- as.vector(unlist(attr(ftbadjcoha, "row.vars")))
ftbadjcoha <- ftbadjcoha[2:nrow(ftbadjcoha),]
rownames(ftbadjcoha) <- rwnms[2:length(rwnms)]
svrwnms <- as.vector(unlist(attr(ftbadjcoha, "dimnames")))[which(rowSums(ftbadjcoha) >= 1000)]
ftbadjcoha <- ftbadjcoha[which(rowSums(ftbadjcoha) >= 1000),]
rownames(ftbadjcoha) <- svrwnms
colnames(ftbadjcoha) <- names(table(cclad$Decade))
ptbadjcoha <- prop.table(ftbadjcoha, margin=2)*100
#ptbampcoha <- ptbampcoha[rowSums(ptbampcoha) > 1, ]
ptbadjcoha

# save data to disc
write.table(ptbadjcoha, "ptbadjcoha.txt", sep = "\t", row.names = F)
###########################################################################
p11d <- cclad
famp <- names(table(p11d$Adjective))[which(table(p11d$Adjective) > 1000)]
p11d$Adjective <- ifelse(p11d$Adjective %in% famp, p11d$Adjective, "other")
table(p11d$Adjective)[order(table(p11d$Adjective), decreasing = T)]

# create vars for Variant
p11d$other <- ifelse(p11d$Adjective == "other", 100, 0)
p11d$good <- ifelse(p11d$Adjective == "good", 100, 0)
p11d$sorry <- ifelse(p11d$Adjective == "sorry", 100, 0)
p11d$happy <- ifelse(p11d$Adjective == "happy", 100, 0)
p11d$fine <- ifelse(p11d$Adjective == "fine", 100, 0)
p11d$different <- ifelse(p11d$Adjective == "different", 100, 0)
# inspect data
head(p11d)

table(p11d$Adjective)[order(table(p11d$Adjective), decreasing = T)]

# start plot: Adjective
p11 <- ggplot(p11d, aes(x = Decade, y = other), size = 8, se = F) +
  geom_smooth(aes(y = other, color = "other", lty = "other"), size=.5, se = F) +
  geom_smooth(aes(y = good, color = "good", lty = "good"), size=.5, se = F) +
  geom_smooth(aes(y = sorry, color = "sorry", lty = "sorry"), size=.5, se = F) +
  geom_smooth(aes(y = happy, color = "happy", lty = "happy"), size=.5, se = F) +
  geom_smooth(aes(y = fine, color = "fine", lty = "fine"), size=.5, se = F) +
  geom_smooth(aes(y = different, color = "different", lty = "different"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed", "twodash","dotdash", "dashed","longdash", "solid"),
                        name="",
                        breaks = c("other", "good", "sorry", "happy", "fine", "different"), 
                        labels = c("other", "good", "sorry", "happy", "fine", "different")) +
  scale_colour_manual(values=c("grey30", "grey60", "indianred4", "grey30", "goldenrod2", "grey60"),
                      name="", 
                      breaks=c("other", "good", "sorry", "happy", "fine", "different"), 
                      labels = c("other", "good", "sorry", "happy", "fine", "different")) +
  theme(legend.position="top") +
  theme_light(base_size = 8) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Decade", y = "Percent of Adjectives") +
  ggsave(file = paste(imageDirectory,"AdjectiveFreqDecade.png", sep="/"), width = 15,  height = 7.5, units = c("cm"),  dpi = 320)
p11

###############################################################
adjdate <- as.numeric(colnames(ptbadjcoha))
adjlm <- ptbadjcoha
head(adjlm)

str(adjlm)

nrow(adjlm)
sigadj <- apply(adjlm, 1, function(x){
  x <- lm(x ~ adjdate)
  x <- summary(x)[4][[1]][[8]]})

sigadjs <- which(sigadj < .05)
sigadjs

###########################################################################
tbf1 <- table(ampcoha$Adjective)
tbf2  <- tbf1[order(tbf1, decreasing = T)]
head(tbf2, 7)

###############################################################
write.table(ampcoha, "ampcoha07_finaldat.txt", row.names= F, sep = "\t")
# extract regdat
socoha <- ampcoha[ampcoha$Amplified != 0,]
socoha$so <- ifelse(socoha$Variant == "so", 1, 0)
socoha <- subset(socoha, Decade >= 1970)
# inspect data
str(socoha); colnames(socoha)

# define vector for data inspection
cltb <- c("Decade", "Function", "Genre", "Variant", "Amplified", 
          "Priming", "Emotionality", "Gradability", "SemanticCategory", 
          "so")
# tabulate data
lapply(socoha[cltb], table)

# remove superfluous columns
socoha$Amplified <- NULL
socoha$Variant <- NULL
socoha$Genre <- NULL
socoha$Function <- NULL
socoha$id <- NULL
# recode Adjective
freadj <- names(table(socoha$Adjective))[which(table(socoha$Adjective) >= 50)]
socoha$Adjective <- ifelse(socoha$Adjective %in% freadj, socoha$Adjective, "other")
# recode SemanticCategory
infreqsc <- names(table(socoha$SemanticCategory))[which(table(socoha$SemanticCategory) >= 10)]
socoha$SemanticCategory <- ifelse(socoha$SemanticCategory %in% infreqsc, socoha$SemanticCategory, "NoSemType")
# define vector for data inspection
cltb <- c("Adjective", "Decade", "Priming", "Emotionality", "Gradability", "SemanticCategory", "so")
# tabulate data
lapply(socoha[cltb], table)

# factorize variables
clfct <- c("Adjective", "Emotionality", "Priming",  "Gradability", "SemanticCategory", "so")
socoha[clfct] <- lapply(socoha[clfct], factor)
# save data to disc
write.table(socoha, "socoha.txt", row.names= F, sep = "\t")
###############################################################
#                  END  PART 3
###############################################################
