##################################################################
# Titel:      The historical development of the AmE amplifier system - Part 4
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2018-11-16
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2018. 
#             The historical development of the AmE amplifier system, Part 4,
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

###############################################################
#                    DATA VALIDATION
# check for NAs
nrow(socoha)

test <- socoha[complete.cases(socoha),]
nrow(test)

# factorize variables
clfct <- c("Adjective", "Emotionality", "Priming",  "Gradability", "SemanticCategory")
socoha[clfct] <- lapply(socoha[clfct], factor)
# define vector for data inspection
cltb <- c("Adjective", "Decade", "Priming", "Emotionality", "Gradability", "SemanticCategory", "so")
# tabulate data
lapply(socoha[cltb], table)

###############################################################
#                     TABULARIZATION
# load library
library(dplyr)
# count adj. slots by gender and age
tb1 <- socoha %>% 
  group_by(Decade) %>%
  summarise(Freq = n())
soN <- socoha %>% 
  group_by(Decade) %>%
  count(so) %>%
  filter(so == 1)
soP <- round(soN$n/tb1$Freq*100, 1)
# create table
tb1 <- data.frame(tb1, soN$n, soP)
colnames(tb1) <- c("Decade", "Adjectives", "so (N)", "so (%)")
tb1

###########################################################################
#                     VIZUALIZATION
# prepare plot data (pd) 
pd <- socoha
pd$so <- pd$so *100
# start plotting
pdummy <- ggplot(pd, aes(Priming, so, color = Priming)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1.25) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Priming", y = "Percent (SO in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50"))
pdummy

# so : Priming
p10 <- ggplot(pd, aes(Priming, so, color = Priming)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Priming", y = "Percent (SO in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50"))
ggsave(file = paste(imageDirectory,"SoPriming.png",sep="/"))
p10

# so: Decade
p10d <- pd
p10d$Decade <- as.numeric(p10d$Decade)
p11 <- ggplot(p10d, aes(x = jitter(Decade), y = so)) +
  geom_smooth(aes(y = so), size=.5, col = "gray30", lty = "longdash", se = F) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Decade", y = "Percent (SO in Ampl. Adj. Slots)") +
  theme_light(base_size = 10) +
  ggsave(file = paste(imageDirectory,"SoDecade.png",sep="/"))
p11

# so: Freq
p12 <- ggplot(pd, aes(x = Freq, y = so)) +
  geom_smooth(aes(y = so), size=.5, col = "gray30", lty = "longdash", se = F) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Frequency of Adjective", y = "Percent (SO in Ampl. Adj. Slots)") +
  theme_light(base_size = 10) 
ggsave(file = paste(imageDirectory,"SoFreq.png",sep="/"))
p12

# so : Gradability
p13 <- ggplot(pd, aes(Gradability, so, color = Gradability)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Gradability", y = "Percent (SO in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50", "grey50"))
ggsave(file = paste(imageDirectory,"SoGradability.png",sep="/"))
p13

# so : Adjective
p14d <- pd
p14d$Adjective <- factor(p14d$Adjective, levels = names(table(p14d$Adjective))[order(tapply(p14d$so, p14d$Adjective, mean))])
p14 <- ggplot(p14d, aes(Adjective, so, color = Adjective)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Adjective", y = "Percent (SO in Ampl. Adj. Slots)") +
  scale_color_manual(values = c(rep("grey50", length(table(pd$Adjective)))))
ggsave(file = paste(imageDirectory,"SoAdjective.png",sep="/"))
p14

# so : Emotionality
p15 <- ggplot(pd, aes(Emotionality, so, color = Emotionality)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Emotionality of Adj.", y = "Percent (SO in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50"))
ggsave(file = paste(imageDirectory,"SoEmotionality.png",sep="/"))
p15


# so : SemanticCategory
p16d <- pd
p16d$SemanticCategory <- factor(p16d$SemanticCategory, 
                                levels = names(table(p16d$SemanticCategory))[order(tapply(p16d$so, 
                                                                                          p16d$SemanticCategory, mean))])
p16 <- ggplot(p16d, aes(SemanticCategory, so, color = SemanticCategory)) +
  stat_summary(fun.y = mean, geom = "point", size = 2) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Semantic class of Adj.", y = "Percent (SO in Ampl. Adj. Slots)") +
  scale_color_manual(values = c(rep("grey50", length(table(pd$SemanticCategory)))))
ggsave(file = paste(imageDirectory,"SoSem.png",sep="/"), width = 14)
p16

# interaction plots
# p100
pd100 <- data.frame(pd$Decade, pd$Priming, pd$so)
colnames(pd100) <- gsub("pd.", "", colnames(pd100))
pd100 <- na.omit(pd100)
p100 <- ggplot(pd100, aes(Decade, so, colour = Priming)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_set(theme_light(base_size = 10)) +
  #  theme(legend.position="none") +
  labs(x = "Gender", y = "Percent (REALLY in Ampl. Adj. Slots)", colour = "Priming") +
  scale_color_manual(values = c("grey40",  "grey80")) +   
  ggsave(file = paste(imageDirectory,"SoDecadePriming.png",sep="/"))
p100

# prepare data p101
p101d <- tapply(pd$so, list(pd$Decade, pd$Priming), mean)
p101d <- data.frame(rownames(p101d), p101d)
colnames(p101d) <- c("Decade", "NotPrimed", "Primed")
p101d$Decade <- as.numeric(p101d$Decade)
# p101
p101 <- ggplot(p101d, aes(Decade, jitter(NotPrimed))) +
  geom_smooth(aes(y = jitter(NotPrimed), color = "NotPrimed", linetype = "NotPrimed"), size=1, se = F) +
  geom_smooth(aes(y = jitter(Primed), color = "Primed", linetype = "Primed"), size=1, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed", "solid"),
                        name="Priming",
                        breaks = c("NotPrimed", "Primed"), 
                        labels = c("NotPrimed", "Primed")) +
  scale_colour_manual(values=c("grey40", "grey40"),
                      name="Priming", 
                      breaks=c("NotPrimed", "Primed"), 
                      labels = c("NotPrimed", "Primed")) +
  theme_set(theme_light(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Decade", y = "Percent (SO in Ampl. Adj. Slots)") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  ggsave(file = paste(imageDirectory,"ReallyDecadePrimingSmooth.png",sep="/"))
p101


###########################################################################
#           MIXED EFFECTS BIONOMIAL LOGISTIC REGRESSION
# load library
library(rms)
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
socoha.dist <- datadist(socoha)
options(datadist = "socoha.dist")
# convert Decade into numeric and scale Decade
socoha$Decade <- as.numeric(socoha$Decade)
socoha$Decade <- scale(socoha$Decade)
# convert Freq into numeric and scale Decade
socoha$Freq <- as.numeric(socoha$Freq)
socoha$Freq <- scale(socoha$Freq)
# generate initial minimal regression model 
m0.glm = glm(so ~ 1, family = binomial, data = socoha) # baseline model glm
m0.lrm = lrm(so ~ 1, data = socoha, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

m0.lrm

###########################################################################
# load library
library(lme4)
# create model with a random intercept for token
m0.lmer <- lmer(so ~ (1|Adjective), data = socoha, family = binomial)
# Baayen (2008:278-284) uses the call above but the this call is now longer
# up-to-date because the "family" parameter is deprecated
# we switch to glmer (suggested by R) instead but we will also
# create a lmer object of the final minimal adequate model as some functions
# will not (yet) work on glmer
m0.glmer = glmer(so ~ (1|Adjective), data = socoha, family = binomial)

# results of the lmer object
print(m0.lmer, corr = F)

# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# the aic of the glmer object is smaller which shows that including the random
# intercepts is justified

# test random effects
null.id = -2 * logLik(m0.glm) + 2 * logLik(m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F) # sig m0.glmer better than m0.glm

# inspect results
summary(m0.glm)

summary(m0.glmer)

###########################################################################
# model fitting
# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use a step-wise step up procedure
# we need to add "control = glmerControl(optimizer = "bobyqa")" 
# because otherwise R fails to converge
#	manual modelfitting
m0.glmer <- glmer(so ~ 1+ (1|Adjective), family = binomial, data = socoha, 
                  control=glmerControl(optimizer="bobyqa"))
# add Decade
m1.glmer <- update(m0.glmer, .~.+Decade)
m1.glm <- update(m0.glm, .~.+Decade)
vif(m1.glm) # VIFs ok
anova(m0.glmer, m1.glmer, test = "Chi") # SIG (p=0.0000578 ***)

# add Priming
ftable(socoha$Priming, socoha$so)
m2.glmer <- update(m1.glmer, .~.+Priming)
m2.glm <- update(m1.glm, .~.+Priming)
vif(m2.glm) # VIFs ok
anova(m1.glmer, m2.glmer, test = "Chi") # not sig (p=0.3351)

# add Emotionality
ftable(socoha$Emotionality, socoha$so)
m3.glmer <- update(m1.glmer, .~.+Emotionality)
m3.glm <- update(m1.glm, .~.+Emotionality)
vif(m3.glm) # VIFs ok
anova(m1.glmer, m3.glmer, test = "Chi") # not sig (p=0.9247)

# add Freq
m4.glmer <- update(m1.glmer, .~.+Freq)
m4.glm <- update(m1.glm, .~.+Freq)
vif(m4.glm) # VIFs ok (<4)
anova(m1.glmer, m4.glmer, test = "Chi") # not sig (p=0.6271)

# add SemanticCategory
ftable(socoha$SemanticCategory, socoha$so)
m5.glmer <- update(m1.glmer, .~.+SemanticCategory)
m5.glm <- update(m1.glm, .~.+SemanticCategory)
vif(m5.glm) # VIFs unacceptable

# add Gradability
ftable(socoha$Gradability, socoha$so)
m6.glmer <- update(m1.glmer, .~.+Gradability)
m6.glm <- update(m1.glm, .~.+Gradability)
vif(m6.glm) # VIFs ok (<4)
anova(m1.glmer, m6.glmer, test = "Chi") # SIG (p=0.03295 *) BUT BIC inflated! 

# AIC: AIC(m6.glmer)-AIC(m1.glmer) -> -2.825501), BIC: BIC(m6.glmer)-BIC(m1.glmer) -> 8.328743: DO NOT INCLUDE!

###########################################################################
# find all 2-way interactions
library(utils)
colnames(socoha)

vars <- c("Decade", "Priming", "Emotionality", "Gradability", "SemanticCategory",  "Freq")
intac <- t(combn(vars, 2))
intac

# add Decade*Priming
m7.glmer <- update(m1.glmer, .~.+Decade*Priming)
m7.glm <- update(m1.glm, .~.+Decade*Priming)
vif(m7.glm) # VIFs ok
anova(m1.glmer, m7.glmer, test = "Chi") # SIG (p=0.04488) BUT BIC inflated! 

# AIC: AIC(m7.glmer)-AIC(m1.glmer) -> -2.207306), BIC: BIC(m7.glmer)-BIC(m1.glmer) -> 8.946938: DO NOT INCLUDE!

# add Decade*Emotionality
m8.glmer <- update(m1.glmer, .~.+Decade*Emotionality)  
m8.glm <- update(m1.glm, .~.+Decade*Emotionality)
vif(m8.glm) # VIFs ok
anova(m8.glmer, m1.glmer, test = "Chi") # not sig (p=0.9619)

# add Decade*Gradability
m9.glmer <- update(m1.glmer, .~.+Decade*Gradability)  
m9.glm <- update(m1.glm, .~.+Decade*Gradability)
vif(m9.glm) # VIFs unacceptable

# add Decade*SemanticCategory
m10.glmer <- update(m1.glmer, .~.+Decade*SemanticCategory)  
m10.glm <- update(m1.glm, .~.+Decade*SemanticCategory)
vif(m10.glm) # VIFs unacceptable

# add Decade*Freq
m11.glmer <- update(m1.glmer, .~.+Decade*Freq)  
m11.glm <- update(m1.glm, .~.+Decade*Freq)
vif(m11.glm) # VIFs ok (<5)
anova(m11.glmer, m1.glmer, test = "Chi") # not sig (p=0.7374)

# add Priming*Emotionality
ftable(socoha$Priming, socoha$Emotionality, socoha$so)
m12.glmer <- update(m1.glmer, .~.+Priming*Emotionality)  
m12.glm <- update(m1.glm, .~.+Priming*Emotionality)
vif(m12.glm) # VIFs ok (<5)
anova(m12.glmer, m1.glmer, test = "Chi") # not sig (p=0.7792)

# add Priming*Gradability
ftable(socoha$Priming, socoha$Gradability, socoha$so) # not possible

# add Priming*SemanticCategory
ftable(socoha$Priming, socoha$SemanticCategory, socoha$so) # not possible

# add Priming*Freq
m13.glmer <- update(m1.glmer, .~.+Priming*Freq)  
m13.glm <- update(m1.glm, .~.+Priming*Freq)
vif(m13.glm) # VIFs ok (<5)
anova(m13.glmer, m1.glmer, test = "Chi") # not sig (p=0.2651)

# add Emotionality*Gradability
ftable(socoha$Emotionality, socoha$Gradability, socoha$so)
m14.glmer <- update(m1.glmer, .~.+Emotionality*Gradability)  
m14.glm <- update(m1.glm, .~.+Emotionality*Gradability)
vif(m14.glm) # VIFs unacceptable

# add Emotionality*SemanticCategory
ftable(socoha$Emotionality, socoha$SemanticCategory, socoha$so) # not possible

# add Emotionality*Freq
m15.glmer <- update(m1.glmer, .~.+Emotionality*Freq)  
m15.glm <- update(m1.glm, .~.+Emotionality*Freq)
vif(m15.glm) # VIFs ok
anova(m15.glmer, m1.glmer, test = "Chi") # not sig (p=0.1024) 

# add Gradability*SemanticCategory
ftable(socoha$Gradability, socoha$SemanticCategory, socoha$so) # not possible

# add Gradability*Freq
m16.glmer <- update(m1.glmer, .~.+Gradability*Freq)  
m16.glm <- update(m1.glm, .~.+Gradability*Freq)
vif(m16.glm) # VIFs unacceptable

# add SemanticCategory*Freq
m17.glmer <- update(m1.glmer, .~.+SemanticCategory*Freq)  
m17.glm <- update(m1.glm, .~.+SemanticCategory*Freq)
vif(m17.glm) # VIFs unacceptable

###########################################################################
# find all 3-way interactions
vars <- c("Decade", "Priming", "Emotionality", "Gradability", "SemanticCategory",  "Freq")
intac <- t(combn(vars, 3))
intac

# check if interaction is possible
PrimingEmotionalityGradability <- min(ftable(socoha$Priming, socoha$Emotionality, socoha$Gradability)) 
PrimingEmotionalitySemanticCategory <- min(ftable(socoha$Priming, socoha$Emotionality, socoha$SemanticCategory))
PrimingGradabilitySemanticCategory <- min(ftable(socoha$Priming, socoha$Gradability, socoha$SemanticCategory))
EmotionalityGradabilitySemanticCategory <- min(ftable(socoha$Emotionality, socoha$Gradability, socoha$SemanticCategory))
# test which interactions are possible
testpos3intact <- c(PrimingEmotionalityGradability, PrimingEmotionalitySemanticCategory, 
                    PrimingGradabilitySemanticCategory, EmotionalityGradabilitySemanticCategory)
names(testpos3intact) <- c("PrimingEmotionalityGradability", "PrimingEmotionalitySemanticCategory", 
                           "PrimingGradabilitySemanticCategory", "EmotionalityGradabilitySemanticCategory")
tstintact3 <- names(testpos3intact)[which(testpos3intact >= 1)]
tstintact3; length(tstintact3)

# add PrimingEmotionalityGradability
ftable(socoha$Priming, socoha$Emotionality, socoha$Gradability, socoha$so)
m18.glmer <- update(m1.glmer, .~.+Priming*Emotionality*Gradability) 
m18.glm <- update(m1.glm, .~.+Priming*Emotionality*Gradability)
vif(m18.glm) # VIFs unacceptable

# add Decade*Priming*Emotionality
ftable(socoha$Priming, socoha$Emotionality, socoha$so) 
m19.glmer <- update(m1.glmer, .~.+Decade*Priming*Emotionality) 
m19.glm <- update(m1.glm, .~.+Decade*Priming*Emotionality)
vif(m19.glm) # VIFs ok
anova(m19.glmer, m1.glmer, test = "Chi") # not sig (p=0.3638) 

# add Decade*Priming*Gradability
ftable(socoha$Priming, socoha$Gradability, socoha$so) # not possible 

# add Decade*Priming*SemanticCategory
ftable(socoha$Priming, socoha$SemanticCategory, socoha$so) # not possible

# add Decade*Priming*Freq
m20.glmer <- update(m1.glmer, .~.+Decade*Priming*Freq) 
m20.glm <- update(m1.glm, .~.+Decade*Priming*Freq)
vif(m20.glm) # VIFs ok
anova(m20.glmer, m1.glmer, test = "Chi") # not sig (p=0.1899) 

# add Decade*Emotionality*Gradability
ftable(socoha$Emotionality, socoha$Gradability, socoha$so) # not possible
m21.glmer <- update(m1.glmer, .~.+Decade*Emotionality*Gradability) 
m21.glm <- update(m1.glm, .~.+Decade*Emotionality*Gradability)
vif(m21.glm) # VIFs unacceptable

# add Decade*Emotionality*SemanticCategory
ftable(socoha$Emotionality, socoha$SemanticCategory, socoha$so) # not possible

# add Decade*Emotionality*Freq
m22.glmer <- update(m1.glmer, .~.+Decade*Emotionality*Freq) 
m22.glm <- update(m1.glm, .~.+Decade*Emotionality*Freq)
vif(m22.glm) # VIFs ok
anova(m22.glmer, m1.glmer, test = "Chi") # not sig (p=0.3598) 

# add Decade*Gradability*SemanticCategory
ftable(socoha$Gradability, socoha$SemanticCategory, socoha$so) # not possible

# add Decade*Gradability*Freq
m23.glmer <- update(m1.glmer, .~.+Decade*Gradability*Freq) 
m23.glm <- update(m1.glm, .~.+Decade*Gradability*Freq)
vif(m23.glm) # VIFs ok
anova(m23.glmer, m1.glmer, test = "Chi") # SIG (p=0.04125 *)  BUT BIC inflated! 

# AIC: AIC(m23.glmer)-AIC(m1.glmer) -> 1.077038), BIC: BIC(m23.glmer)-BIC(m1.glmer) -> 56.84826: DO NOT INCLUDE!

# add Decade*SemanticCategory*Freq
m24.glmer <- update(m1.glmer, .~.+Decade*SemanticCategory*Freq) # Model is nearly unidentifiable

# add Priming*Emotionality*Freq
ftable(socoha$Priming, socoha$Emotionality, socoha$so) 
m25.glmer <- update(m1.glmer, .~.+Priming*Emotionality*Freq) 
m25.glm <- update(m1.glm, .~.+Priming*Emotionality*Freq)
vif(m25.glm) # VIFs ok
anova(m25.glmer, m1.glmer, test = "Chi") # not sig (p=0.144 *)  

# add Priming*Gradability*Freq
ftable(socoha$Priming, socoha$Gradability, socoha$so) # not possible

# add Priming*SemanticCategory*Freq
ftable(socoha$Priming, socoha$SemanticCategory, socoha$so) # not possible

# add Emotionality*Gradability*Freq
ftable(socoha$Emotionality, socoha$Gradability, socoha$so)
m26.glmer <- update(m1.glmer, .~.+Emotionality*Gradability*Freq) 
m26.glm <- update(m1.glm, .~.+Emotionality*Gradability*Freq)
vif(m26.glm) # VIFs unacceptable

# add Emotionality*SemanticCategory*Freq
ftable(socoha$Emotionality, socoha$SemanticCategory, socoha$so)# not possible

# add Gradability*SemanticCategory*Freq
ftable(socoha$Gradability, socoha$SemanticCategory, socoha$so)# not possible

#########################################
# load function for regression table summary
source("D:\\R/meblr.summary.R")
# set up summary table
meblrm_ampcoha <- meblrm.summary(m0.glm, m1.glm, m0.glmer, m1.glmer, socoha$so) #
meblrm_ampcoha

# save results to disc
write.table(meblrm_ampcoha, "meblrm_ampcoha.txt", sep="\t")

# load function
library(car)
meblrm_ampcoha_Anova <- Anova(m1.glmer, type = "III", test = "Chi")
meblrm_ampcoha_Anova

# save results to disc
write.table(meblrm_ampcoha_Anova, "meblrm_ampcoha_Anova.txt", sep="\t")

effectdecade <- anova(m0.glmer, m1.glmer, test = "Chi")

# use customized model comparison function
# create comparisons
m0.m1 <- anova(m0.glmer, m1.glmer, test = "Chi") # SIG (p=0.0000578 ***)
m1.m2 <- anova(m1.glmer, m2.glmer, test = "Chi") # not sig (p=0.3351)
m1.m3 <- anova(m1.glmer, m3.glmer, test = "Chi") # not sig (p=0.9247)
m1.m4 <- anova(m1.glmer, m4.glmer, test = "Chi") # not sig (p=0.6271)
m1.m6 <- anova(m1.glmer, m6.glmer, test = "Chi") # SIG (p=0.03295 *) BUT BIC inflated! 
m1.m7 <- anova(m1.glmer, m7.glmer, test = "Chi") # SIG (p=0.04488) BUT BIC inflated! 
m1.m8 <- anova(m8.glmer, m1.glmer, test = "Chi") # not sig (p=0.9619)
m1.m11 <- anova(m11.glmer, m1.glmer, test = "Chi") # not sig (p=0.7374)
m1.m12 <- anova(m12.glmer, m1.glmer, test = "Chi") # not sig (p=0.7792)
m1.m13 <- anova(m13.glmer, m1.glmer, test = "Chi") # not sig (p=0.2651)
m1.m15 <- anova(m15.glmer, m1.glmer, test = "Chi") # not sig (p=0.1024) 
m1.m19 <- anova(m19.glmer, m1.glmer, test = "Chi") # not sig (p=0.3638) 
m1.m20 <- anova(m20.glmer, m1.glmer, test = "Chi") # not sig (p=0.1899) 
m1.m22 <- anova(m22.glmer, m1.glmer, test = "Chi") # not sig (p=0.3598) 
m1.m23 <- anova(m23.glmer, m1.glmer, test = "Chi") # SIG (p=0.04125 *) BUT BIC inflated! 
m1.m25 <- anova(m25.glmer, m1.glmer, test = "Chi") # not sig (p=0.144 *) 

# create a list of the model comparisons
mdlcmp <- list(m0.m1, m1.m2, m1.m3, m1.m4, m1.m6, m1.m7, m1.m8, m1.m11, m1.m12, 
               m1.m13, m1.m15, m1.m19, m1.m20, m1.m22, m1.m23, m1.m25
)
# load function
source("D:\\R/ModelFittingSummarySWSU.R") # for Mixed Effects Model fitting (step-wise step-up): Binary Logistic Mixed Effects Models
# apply function
mdl.cmp.glmersc.swsu.dm <- mdl.fttng.swsu(mdlcmp)
# inspect output
mdl.cmp.glmersc.swsu.dm

write.table(mdl.cmp.glmersc.swsu.dm, "mdl_cmp_glmersc_swsu_socoha.txt", sep="\t")
###########################################################
# Post-hoc analysis
#library (multcomp)
#summary(glht(m6.glmer, mcp(Gradability="Tukey")))

################################################################
#                 IMPORTANT OBJECTS
################################################################
# inspect very important objects
head(socoha)

# glmer
effectdecade

meblrm_ampcoha

meblrm_ampcoha_Anova

###############################################################
#                           END PART 4
###############################################################
