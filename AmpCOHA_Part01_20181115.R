##################################################################
# Titel:      The historical development of the AmE amplifier system - Part 1
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2018-11-15
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2018. 
#             The historical development of the AmE amplifier system, Part 1,
#             unpublished R script, The University of Queensland.
###############################################################
# clean current workspace
rm(list=ls(all=T))
# set wd
setwd("D:\\Uni\\Projekte\\02-Intensification\\AmpCOHA")
# load packages
library(stringr)
# set options
options(stringsAsFactors = F)
options(scipen = 999)
# define image directory
imageDirectory<-"images"
##################################################################
# concordancing
# define paths
paths <- c(
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1810s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1820s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1830s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1840s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1850s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1860s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1870s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1880s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1890s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1900s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1910s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1920s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1930s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1940s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1950s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1960s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1970s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1980s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\1990s",
  "D:\\Uni\\Korpora\\Edited\\COHApostagged\\2000s"
)
# write function to extract adjectives from pos tagged coha data
extractadjscoha <- function(paths){
# Load packages
  require(tm)
  require(stringr)
  require(gsubfn)
  require(plyr)
  require(reshape)
  require(zoo)
  source("D:\\R/ConcR_2.3.R")
  # Set options
  options(stringsAsFactors = F)
  # define path to data and define search parameters
  search.pattern <-  c("[a-z]{0,}['|-]{0,1}[a-z]{1,}\\/jj ")
  context <- 150
  # start search
  sapply(paths, function(x){
  conctb <- ConcR(x, search.pattern, context, all.pre = F)
  nm1 <- gsub("D:\\Uni\\Korpora\\Edited\\COHApostagged\\","",x, fixed = T)
  nm2 <- gsub("s","",nm1, fixed = T)
  nm3 <- paste("D:\\Uni\\Projekte\\02-Intensification\\AmpCOHA/intscoha", nm2, ".txt", sep = "", collapse = "")
  write.table(conctb, nm3, sep = "\t", row.names = F)
  rm(list=ls(all=T))
  })
}
extractadjscoha(paths)
###############################################################
#                DEFINE AMPLIFIERS
# create vector with amplifiers in data
amplifiers <- c("absolutely", "actually", 
                #"aggressively", 
                "amazingly", "appallingly", "awful", "awfully", 
                "badly", "bloody", "certainly", "clearly",
                #"complete", 
                "dead", "completely", "considerably", 
                "crazy", "decidedly", "definitely",  "distinctly", 
                "dreadfully", "enormously", "entirely", "especially", 
                #"exactly", 
                "exceedingly", "exceptionally", 
                "excruciatingly", "extraordinarily", "extremely",
                "fiercely", "firmly", "frightfully", "fucking", 
                #"fully", 
                "genuinely", "greatly",
                "grossly", "heavily", "highly", "hopelessly", 
                "horrendously", 
                #"hugely",
                "immediately", "immensely", "incredibly", 
                "infinitely", "intensely", "irrevocably",
                "mad", "mega", "mighty", 
                "most", 
                #"much", 
                "obviously", "openly", "overwhelmingly", "particularly", 
                "perfectly", "plenty", "positively", "precisely", 
                "pretty", "profoundly", "purely", 
                #"quite", 
                "real", "really", "remarkably", "seriously", 
                "shocking",   "significant", "significantly", "so", 
                "specially", "specifically", "strikingly",
                "strongly", "substantially", "super", "surely", 
                "terribly", "terrifically", 
                #"too",
                "total", "totally", "traditionally", "true", 
                "truly", "ultra", "utterly", "very",
                "viciously", 
                #"well", 
                "wholly", "wicked", "wildly")
###############################################################
# Function to clean pos tagged coha data
cleancoha <- function(path){
  x <- read.table(path, sep = "\t", skipNul = T, header = T, fill = T)
  # date
  Decade <- gsub(".*intscoha", "", path)
  Decade <- gsub(".txt", "", Decade)
  # syntactic Functionction
  x$Function <- str_trim(x$post, side = "both")
  x$Function <- tolower(x$Function)
  x$Function <- gsub(" {2,}", " ", x$Function)
  x$Function <- gsub(" .*", "", x$Function)
  x$Function <- gsub(".*/n.*", "Attributive", x$Function)
  x$Function <- gsub(".*\\./y.*", "Predicative", x$Function)
  x$Function <- ifelse(x$Function == "Attributive" | x$Function == "Predicative", x$Function, "remove")
  # remove items for which Function could not be clearly determined
  x <- x[x$Function != "remove",]
  # register
  x$Genre <- gsub("_.*", "", x$file)
  x$Genre <- gsub(".*\\/jj.*", "remove", x$Genre)
  x <- x[x$Genre != "remove", ]
  # shorten post context
  x$post <- substr(x$post, 1, ifelse((nchar(x$post)+25) <25, max(nchar(x$post)), 25))
  # pre context
  x$pre <- str_trim(x$pre, side = "both")
  x$prel <- x$pre
  x$prel <- substr(x$prel, ifelse(nchar(x$prel)-25 <=0, 1, nchar(x$prel)-25), nchar(x$prel))
  x$pre <- gsub(".* ", "", x$pre)
  # Adjective, i.e. adjective
  x$Adjective <- gsub("\\/.*", "", x$token)
  x$pre <- gsub("\\/.*", "", x$pre)
  # amplifier variant
  x$Variant <- ifelse(x$pre %in% amplifiers, x$pre, "0")
  # amplified y/n
  x$Amplified <- ifelse(x$Variant == "0", 0, 1) 
  x$Decade <- rep(Decade, nrow(x))
  return(x)
}

# apply Functionction to data
ampcoha1810 <- cleancoha("intscoha1810.txt")
ampcoha1820 <- cleancoha("intscoha1820.txt")
ampcoha1830 <- cleancoha("intscoha1830.txt")
ampcoha1840 <- cleancoha("intscoha1840.txt")
ampcoha1850 <- cleancoha("intscoha1850.txt")
ampcoha1860 <- cleancoha("intscoha1860.txt")
ampcoha1870 <- cleancoha("intscoha1870.txt")
ampcoha1880 <- cleancoha("intscoha1880.txt")
ampcoha1890 <- cleancoha("intscoha1890.txt")
ampcoha1900 <- cleancoha("intscoha1900.txt")
ampcoha1910 <- cleancoha("intscoha1910.txt")
ampcoha1920 <- cleancoha("intscoha1920.txt")
ampcoha1930 <- cleancoha("intscoha1930.txt")
ampcoha1940 <- cleancoha("intscoha1940.txt")
ampcoha1950 <- cleancoha("intscoha1950.txt")
ampcoha1960 <- cleancoha("intscoha1960.txt")
ampcoha1970 <- cleancoha("intscoha1970.txt")
ampcoha1980 <- cleancoha("intscoha1980.txt")
ampcoha1990 <- cleancoha("intscoha1990.txt")
ampcoha2000 <- cleancoha("intscoha2000.txt")

# inspect data
str(ampcoha1810)

# combine concordances
ampcoha <- rbind(ampcoha1810, ampcoha1820, ampcoha1830, ampcoha1840, ampcoha1850, 
                 ampcoha1860, ampcoha1870, ampcoha1880, ampcoha1890, ampcoha1900, 
                 ampcoha1910, ampcoha1920, ampcoha1930, ampcoha1940, ampcoha1950, 
                 ampcoha1960, ampcoha1970, ampcoha1980, ampcoha1990, ampcoha2000)
# convert into a data frame
ampcoha <- as.data.frame(ampcoha)
# inspect data
head(ampcoha); nrow(ampcoha); length(table(ampcoha$Adjective))

###############################################################
# save raw data to disc
write.table(ampcoha, "ampcoha01_raw.txt", sep = "\t", row.names = F, quote = F)
#ampcoha <- read.table("ampcoha01_raw.txt",  sep = "\t", skipNul = T, header = T, fill = T, quote = "")
###############################################################
# define forms that require removal
sups <- c(".*most.*", ".*more.*") 
negs <- c(".*not.*", ".*never.*", ".*n't.*")
downtoners <- c(".*sort/.*", ".*kind/.*", ".* bit/.*", ".*somewhat.*", ".*fairly.*", 
                ".*rather.*", ".*reasonably.*", ".*slightly.*", ".*comparatively.*", ".*semi.*", 
                ".*relatively.*", ".*little.*", ".*somehow.*", ".*almost.*", ".*partly.*", 
                ".*hardly.*", ".* less.*", ".*barely.*", ".* just/.*")
specialforms <- c(".* too.*", ".*quite.*")
postdowntoners <- c(".*enough.*")
nonpropadj <- c("only", "much", "many")
# check length of dataset
str(ampcoha); head(ampcoha); nrow(ampcoha)#; table(ampcoha$pint); head(ampcoha$prel); head(ampcoha$prel)

# find items to be removed
supsidx <- unique(grep(paste(sups,collapse="|"), ampcoha$prel, value=F))
negsidx <- unique(grep(paste(negs,collapse="|"), ampcoha$prel, value=F))
downtonersidx <- unique(grep(paste(downtoners,collapse="|"), ampcoha$prel, value=F))
specialformsidx <- unique(grep(paste(specialforms,collapse="|"), ampcoha$prel, value=F))
postdowntonersidx <- unique(grep(paste(postdowntoners,collapse="|"), ampcoha$post, value=F))
nonpropadjidx <- unique(grep(paste(nonpropadj,collapse="|"), ampcoha$Adjective, value=F))
# combine indices
idxs <- unique(c(supsidx, negsidx, downtonersidx, specialformsidx, postdowntonersidx, nonpropadjidx))
# remove forms that require removal
ampcoha <- ampcoha[-idxs,]
# remove empty values
ampcoha <- ampcoha[!ampcoha$Variant == "", ]
# inspecta data
nrow(ampcoha); length(table(ampcoha$Adjective))

# remove long pre column
ampcoha$prel <- NULL
ampcoha$post <- NULL
# inspect data
head(ampcoha)

###############################################################
# save raw data to disc
write.table(ampcoha, "ampcoha02_wo_neg.txt", sep = "\t", row.names = F)
###############################################################
# remove non-amplified adjectives
ampadjtb <- table(ampcoha$Adjective, ampcoha$Amplified)
ampadjtb2 <- apply(ampadjtb, 1, function(x){
  x <- ifelse(x > 1, 1, x)})
ampadjtb3 <- colSums(ampadjtb2)
ampadjscoha <- names(ampadjtb3)[which(ampadjtb3 > 1)]
ampcoha <- ampcoha[ampcoha$Adjective %in% ampadjscoha, ]
nrow(ampcoha)

length(table(ampcoha$Adjective))

###############################################################
write.table(ampcoha, "ampcoha03_wo_nonampadjs.txt", sep = "\t", row.names = F)
###############################################################
#                        END PART 1
###############################################################
