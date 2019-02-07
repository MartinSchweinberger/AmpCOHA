##################################################################
# Titel:      The historical development of the AmE amplifier system - Part 2
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2018-11-16
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2018. 
#             The historical development of the AmE amplifier system, Part 2,
#             unpublished R script, The University of Queensland.
###############################################################
# clean current workspace
rm(list=ls(all=T))
# set wd
setwd("D:\\Uni\\Projekte\\02-Intensification\\AmpCOHA")
# intsall neccessary packages
# load packages
library(tm)
library(stringr)
library(syuzhet)
###############################################################
# set options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.print=10000)
# define image directors
imageDirectory<-"images"
###############################################################
ampcoha <- read.table("ampcoha03_wo_nonampadjs.txt",  sep = "\t", skipNul = T, header = T, fill = T)
# inspect length of data
nrow(ampcoha); length(table(ampcoha$Adjective)); head(ampcoha)

###############################################################
# rename section levels
ampcoha$Genre <- ifelse(ampcoha$Genre == "fic", "Fiction", 
                        ifelse(ampcoha$Genre == "nf", "Non-Fiction",
                               ifelse(ampcoha$Genre == "mag", "Magazines",
                                      ifelse(ampcoha$Genre == "news", "News", ampcoha$Genre))))
###############################################################
# add id to data
ampcoha$id <- 1:nrow(ampcoha)
# add priming
# order data
ampcoha <- ampcoha[order(ampcoha$Genre, ampcoha$Decade),]
# subset data
ampcohasbsts <- split(ampcoha, with(ampcoha, interaction(ampcoha$Decade,ampcoha$Genre)), drop = TRUE)
# create priming variable
ampcohasbstsprim <- lapply(ampcohasbsts, function(x){
  prim1 <- c(rep(0, 1), x$Variant[1:length(x$Variant)-1])
  prim2 <- c(rep(0, 2), x$Variant[1:(length(x$Variant)-2)])
  prim3 <- c(rep(0, 3), x$Variant[1:(length(x$Variant)-3)])
  primtb <- cbind(x$Variant, prim1, prim2, prim3)
  # create variable
  x$priming <- as.vector(unlist(apply(primtb,1, function(x){
    x <- ifelse(x[1]== "0" , "NotPrimed",
                ifelse(x[1] == x[2] | x[1] == x[3] | x[1] == x[4], "Primed", "NotPrimed"))
  })))
})
# add priming to data
ampcoha$Priming <- unlist(ampcohasbstsprim)

# inspect priming
prmidx <- which(ampcoha$Priming == "Primed")[3]
ampcoha[(prmidx-3):prmidx,]

# reorder data
ampcoha <- ampcoha[order(ampcoha$id),]
# inspect data
head(ampcoha)

###############################################################
# remove items that were not intensified by a minimum of 2 intensifier variants
# 1. remove all non intensified items form data 
vrcntxt <- ampcoha[ampcoha$Variant != "0",]
# 2. create separate data sets for the different registers
vrcntxt_fic <- vrcntxt[vrcntxt$Genre == "Fiction",]
vrcntxt_mag <- vrcntxt[vrcntxt$Genre == "Magazines",]    
vrcntxt_news <- vrcntxt[vrcntxt$Genre == "News",]      
vrcntxt_nf <- vrcntxt[vrcntxt$Genre == "Non-Fiction",]
# 3. write function to extract adjectives that where intensified by at least two different amplifiers
extractVarCon <- function(data){
  adjpinttb <- table(data$Adjective, data$Variant)
  typs <- apply(adjpinttb, 1, function(x){
    x <- ifelse(x > 1, 1, x)})
  idxtyps <- colSums(typs)
  idx <- which(idxtyps > 1)
  ctyps <- attr(idxtyps, "names")[idx]
  return(ctyps)
}
# 4. apply function to data
varadj_fic <- extractVarCon(vrcntxt_fic)
varadj_mag <- extractVarCon(vrcntxt_mag)
varadj_news <- extractVarCon(vrcntxt_news)
varadj_nf <- extractVarCon(vrcntxt_nf)
# 5. concatenate vectors of adjectives
varadj <- c(varadj_fic, varadj_mag, varadj_news, varadj_nf)
# 6. table vector
varadjs <- names(table(varadj))
# remove rows form data that do not contain variable adjectives
ampcoha <- ampcoha[ampcoha$Adjective %in% varadjs, ]
# inspect data
nrow(ampcoha); length(table(ampcoha$Adjective))

###############################################################
# remove adjs that were intensified less than 5 percent of cases
tst11 <- as.matrix(table(ampcoha$Amplified, ampcoha$Adjective))
tst12 <- data.frame(colnames(tst11), 
                    round((tst11[2,]/(tst11[1,]+tst11[2,]))*100, 1))   
colnames(tst12) <- c("adj", "intpercent")
tst13 <- as.data.frame(tst12)
tst14 <- tst13[which(tst13[,2] >= 5),]
tst15 <- tst14[,1]
ampcoha <- ampcoha[ampcoha$Adjective %in% tst15,]
nrow(ampcoha)

# remove adjs that were intensified in 100 percent of cases
tst21 <- as.matrix(table(ampcoha$Amplified, ampcoha$Adjective))
tst22 <- data.frame(colnames(tst21), 
                    round((tst21[2,]/(tst21[1,]+tst21[2,]))*100, 1))   
colnames(tst22) <- c("adj", "intpercent")
# inspect data
head(tst22)

tst23 <- as.data.frame(tst22)
# inspect data
head(tst23)

max(tst23[,2])
## 75 -> no adjs need to be removed
###############################################################
#                   WARNING!
# remove adjectives that occurred less than 5 times
#tst31 <- table(ampcoha$Adjective)
#tst32 <- data.frame(tst31)
# add colnames
#colnames(tst32) <- c("adj", "adjfreq")
# inspect data
#head(tst32)

#nrow(tst32)

# determine which adjectives occured less than 5 times
#tst33 <- tst32[which(tst32[,2] < 6),]

# inspect data
#head(tst33)

#nrow(tst33)

# create vector of infrequent adjectives
#infreqadjs <- as.vector(unlist(tst33[,1]))
# remove adjectives that occurred less than 5 times
#ampcoha <- ampcoha[!(ampcoha$Adjective %in% infreqadjs), ]
###############################################################
# inspect data
head(ampcoha); nrow(ampcoha); length(table(ampcoha$Adjective))

###############################################################
# add emotionality
# create a vector of adjectives in the data
adjcohaemo <- names(table(ampcoha$Adjective))
# add emotionality classification of adjectives
class_emo <- get_nrc_sentiment(adjcohaemo)
emoadj <- rowSums(class_emo)
# crate a vector of emo adj
emoadjcoha <- adjcohaemo[which(emoadj > 0)]
# inspect data
head(adjcohaemo); head(emoadj); head(emoadjcoha)

# add variable emo to ampcoha data
ampcoha$Emotionality <- as.vector(unlist(sapply(ampcoha$Adjective, function(x){
  x <- ifelse(x %in% emoadjcoha, "Emotional", "NonEmptional")})))
# inspect data
head(ampcoha)

###############################################################
# save data to disc
write.table(ampcoha, "ampcoha04_varcon_emo.txt", sep = "\t", row.names = F)
#ampcoha <- read.table("ampcoha04_varcon_emo.txt",  sep = "\t", skipNul = T, header = T, fill = T)
###############################################################
# gradability - manual classification
# done by experienced grad students
# add gradability
ngrd_manual <- c("abject", "able", "abrasive", "abstract", "absurd", "abundant", "abusive",
                 "accurate", "acrimonious", "active", "advanced", "adverse", "affectionate", "afraid",
                 "aged", "aggressive", "agile", "agitated", "aimless", "airy", "alert", "alleged",
                 "allusive", "amazing", "ambitious", "amused", "amusing", "ancient", "angry",
                 "annoying", "anxious", "appalling", "apparent", "appealing", "applicable", "applied",
                 "appreciative", "apprehensive", "approachable", "appropriate", "approving",
                 "arduous", "arrogant", "ashamed", "associated", "astute", "athletic", "atrocious",
                 "attitudinal", "attractive", "authentic", "authoritarian   ", "authoritative",
                 "available", "aware", "awesome", "awful", "awkward", "awry", "bad", "bare", "base",
                 "battered", "beautiful", "beloved", "benevolent", "benign", "besetting", "bitter",
                 "bizarre", "bleak", "bleary", "bloody", "blotchy", "bold", "boppy", "bored",
                 "boring", "bossy", "brave", "brief", "bright", "brilliant", "broad", "browsing",
                 "brutal", "bubbly", "burly", "buzzy", "callous", "calm", "campy", "candid",
                 "capable", "careful", "careless", "casual", "cautious", "ceremonial", "challenging",
                 "changed", "charismatic", "charming", "cheap", "circumspect", "civic", "civil",
                 "civilised", "classy", "clever", "cocky", "cold", "collective", "colossal", "colourful",
                 "comfortable", "commandeered", "committed", "compatible", "compelling", "competent",
                 "competitive", "complex", "complicated", "conceivable", "concentrated", "concerned",
                 "confident", "confidential", "confused", "confusing", "considerable", "constructive",
                 "consultative", "contrived", "controversial", "convenient", "conventional", "converted",
                 "convinced", "cool", "corrupt", "cosy", "coy", "cramped", "crass", "crazy", "creative",
                 "criminal", "crippling", "critical", "cross", "crowded", "crucial", "cruel", "cumbersome",
                 "curious", "cushy", "cute", "cynical", "damaged", "damaging", "damp", "dangerous",
                 "daring", "darkened", "darn", "daunting", "dear", "debatable", "decent", "dedicated",
                 "deep", "defective", "defensive", "delicate", "delicious", "delighted", "delightful",
                 "dense", "dependent", "depressed", "desirable", "despairing", "desperate", "despicable",
                 "despondent", "destructive", "detailed", "detrimental", "devilish", "difficult",
                 "dirty", "disabled", "disadvantaged", "disappointed", "disappointing", "disastrous",
                 "disenchanted", "disgraceful", "disgusting", "dishonest", "disparaging", "distant",
                 "distinguished   ", "distorted", "distressed", "disturbed", "disturbing", "dizzy",
                 "dodgy", "dominant", "dotty", "double", "doubtful", "downhill", "dramatic", "dreadful",
                 "driving", "drunk", "drunken", "ductile", "dull", "dumb", "dusty", "dylan", "dynamic",
                 "dynamical", "eager", "early", "earnest", "earthy", "easterly", "eastern", "easy",
                 "eccentric", "economic", "edible", "effective", "efficient", "elderly", "elegant",
                 "eligible", "elitist", "elusive", "embarrassed", "embarrassing", "emergent", "eminent",
                 "emotional", "emotive", "encouraging", "energetic", "enlightening", "enormous",
                 "entertaining", "enthusiastic", "epic", "erudite", "estimated", "estranged", "everyday",
                 "evil", "exact", "exceptional", "excessive", "excited", "exciting", "expensive",
                 "experienced", "expert", "explicit", "express", "expressive", "extended", "extensive",
                 "extraordinary", "extravagant", "extroverted", "fabulous", "facile", "factual", "faint",
                 "familiar", "famous", "fanatic", "fancy", "fantastic", "fascinating", "fast", "fastidious",
                 "fat", "favourable", "favoured", "fearful", "feisty", "fergal", "ferocious", "fertile",
                 "fierce", "fiery", "filthy", "fine", "finished", "finite", "firm", "fitting", "fizzy",
                 "flexible", "fluffy", "fluttering", "foggy", "foolish", "forceful", "formalised",
                 "formidable", "fortunate", "frank", "frantic", "fraudulent", "fraught", "frenzied",
                 "frequent", "friendly", "frightening", "frightful", "frustrated", "frustrating",
                 "fulsome", "fun", "funny", "furious", "generous", "gentle", "giant", "gifted",
                 "gigantic", "glad", "glib", "glorious", "glossy", "good", "goodhearted", "gorgeous",
                 "gracious", "gradual", "grand", "grandiose", "grateful", "grave", "greasy", "great",
                 "grim", "groggy", "groovy", "gross", "grubby", "guilty", "gutless", "habitual",
                 "handsome", "handy", "hapless", "happy", "hard", "hardy", "harmful", "harmless",
                 "harmonic", "harsh", "hazardous", "hazy", "heavy", "hectic", "helpful", "hideous",
                 "high", "hilarious", "holy", "honest", "honorable", "honorary", "honourable",
                 "hooked", "hopeful", "hopeless", "horrendous", "horrible", "horrific", "hostile",
                 "hot", "huge", "humble", "humorous", "hungry", "hurt", "hysterical", "idealistic",
                 "igneous", "ignorant", "imaginative", "immature", "immediate", "immense", "imperative",
                 "important", "impotent", "impressive", "inane", "incompetent", "inconsistent",
                 "incorporate", "incorporated", "increased", "incredible", "incredulous", "indecent",
                 "independent", "individual", "individualistic ", "ineffective", "ineffectual",
                 "inept", "inevitable", "inexorable", "inexpensive", "inexperienced", "infamous",
                 "infertile", "informal", "infuriating", "injured", "innovative", "insatiable",
                 "insecure", "insidious", "inspirational   ", "inspired", "instructive", "insuperable",
                 "integrated", "intellectual", "intelligent", "intense", "intensive", "intimate",
                 "intolerant", "invaluable", "inventive", "ironic", "irresponsible", "irritable",
                 "irritating", "itchy", "jealous", "joyful", "justified", "justifying", "keen",
                 "labour", "ladylike", "lame", "large", "late", "layered", "lazy", "lean", "legitimate",
                 "leisurely", "less", "liberal", "liberating", "light", "likely", "limp", "little",
                 "loath", "locating", "lone", "lonely", "long", "loony", "loud", "lousy", "lovely",
                 "low", "loyal", "lucky", "lumbering", "luminous", "lumpy", "lunatic", "lush",
                 "mad", "magic", "magnificent", "major", "mandatory", "manipulated", "marginal",
                 "marvellous", "massive", "matrimonial", "mean", "meaningful", "measurable", "medical",
                 "medicinal", "mediocre", "mere", "mighty", "mild", "minatory", "minded", "minor",
                 "minted", "miraculous", "miscellaneous", "misleading", "mixed", "mock", "modal",
                 "modern", "modest", "modesty", "momentous", "monetary", "monstrous", "moral", "motivating",
                 "muddy", "muggy", "multiple", "mutual", "mystical", "mythical", "naive", "narrow", "nasty",
                 "naughty", "near", "nearby", "neat", "necessary", "neglected", "negligent", "nervous",
                 "net", "new", "nice", "noble", "noisy", "normal", "northern", "nostalgic", "notable",
                 "noted", "noteworthy", "noxious", "numerous", "objective", "obnoxious", "obscure",
                 "observant", "odd", "off", "oily", "okay", "old", "oldfashioned", "operatic", "optimistic",
                 "orderly", "ordinary", "orientated", "oriented", "other", "outdated", "outrageous",
                 "outstanding", "over", "overhanging", "overwhelming", "painful", "parky", "parlous",
                 "passionate", "pathetic", "patronising", "patterned", "peaked", "peculiar", "perforated",
                 "perishable", "pernicious", "perplexed", "perplexing", "persistent", "personal",
                 "persuasive", "perverted", "pessimistic", "petite", "petty", "phenomenal", "picturesque",
                 "pinkish", "plain", "pleasant", "pleased", "pleasing", "pleasurable", "plenty",
                 "poetic", "polite", "poor", "popular", "possessive", "potent", "potential", "powerful",
                 "practical", "pragmatic", "preachy", "precarious", "precious", "precise", "predatory",
                 "predictable", "prepared", "prescriptive", "pressing", "prestigious", "presumptuous",
                 "pretentious", "pretty", "prevalent", "primitive", "privileged", "prodigious", "productive",
                 "professional", "profitable", "profligate", "progressive", "prominent", "promotional",
                 "prone", "proper", "proportionate", "prospective", "prosperous", "protective", "proud",
                 "provocative", "prudential", "psycho", "psychotic", "public", "puerile", "purposeful",
                 "quaint", "qualitative", "queer", "quick", "quiet", "racist", "radical", "rainy",
                 "rampant", "rank", "rapid", "rapt", "rare", "rational", "rattled", "raw", "reactionary",
                 "reactive", "ready", "realistic", "reasonable", "recognisable", "recognised",
                 "recreational", "reddish", "reduced", "refreshing", "regretful", "regular", "relaxed",
                 "relaxing", "relentless", "relevant", "reliable", "reluctant", "remote", "required",
                 "resourceful", "respected", "responsible", "restless", "revealing", "rich", "ridiculous",
                 "risky", "robust", "rocky", "romantic", "rotten", "rough", "rowdy", "rude", "rumbling",
                 "rusty", "sacred", "sad", "safe", "sandy", "sarcastic", "satisfied", "satisfying",
                 "savage", "scarce", "scared", "sceptical", "scientific", "scrappy", "scratchy",
                 "scruffy", "scurrilous", "secret", "secular", "secure", "sedate", "seduced", "seedy",
                 "seismic", "selfconfessed", "selfish", "selfreliant", "senile", "sensational",
                 "sensible", "sensitive", "sentimental", "serious", "severe", "sexist", "sexual",
                 "sexy", "shadowy", "shaky", "shaped", "sharp", "shiny", "shitty", "shocking",
                 "short", "sick", "sickly", "silly", "simple", "sizeable", "skilful", "skilled",
                 "sleepy", "slight", "slim", "slippery", "sloppy", "slow", "small", "smart", "snoopy",
                 "snotty", "sociable", "social", "soft", "soggy", "solid", "sophisticated", "sore",
                 "sorry", "sour", "south", "spare", "sparkling", "spectacular", "spectral", "spiritual",
                 "spiteful", "splendid", "sporting", "starkly", "startling", "staunch", "steady",
                 "steamy", "steep", "stellar", "sticky", "stiff", "stimulating", "stoical", "stormy",
                 "strange", "strategic", "stressful", "stretched", "strict", "striking", "strong",
                 "structured", "stubborn", "stunning", "stupid", "subject", "subtle", "successful",
                 "suffering", "suitable", "sunny", "super", "superficial", "superior", "supernatural",
                 "supportive", "suppressed", "sure", "surplus", "surprised", "surprising", "susceptible",
                 "suspicious", "sustainable", "sweaty", "sweet", "swift", "sympathetic", "tacky",
                 "tactic", "talented", "tall", "tantalising", "tasteful", "tedious", "teensy", "temperate",
                 "tempting", "tended", "tense", "tentative", "terrible", "terrific", "theatrical",
                 "theoretical", "thermal", "thick", "thickened", "thin", "thirsty", "thoughtful",
                 "threatening", "thriving", "tight", "tiny", "tired", "titanic", "tony", "top", "topical",
                 "torrential", "tortious", "tortured", "torturous", "tough", "touring", "tragic",
                 "transcendental", "transferable", "traumatic", "treacherous", "tremendous", "trendy",
                 "tricky", "trim", "triumphal", "trivial", "troubled", "twee", "twisted", "typical",
                 "ugly", "ulterior", "unable", "unattractive", "unaware", "unbeknown", "unbelievable",
                 "uncaring", "uncertain", "unclear", "unctuous", "undecided", "undeniable", "undifferentiated",
                 "undignified", "uneven", "unexpected", "unfair", "unfamiliar", "unfavourable",
                 "unfit", "unflattering", "unforced", "unfortunate", "ungrateful", "unhappy", "unholy",
                 "unified", "unknown", "unlikely", "unlucky", "unorthodox", "unpleasant", "unreal",
                 "unseemly", "unsmiling", "unsocial", "unsound", "unstable", "unusual", "upset",
                 "uptight", "urban", "urbanised", "urgent", "useful", "vague", "vain", "valiant",
                 "valuable", "variable", "varied", "vast", "venerated", "vengeful", "versatile",
                 "vested", "veteran", "viable", "vigorous", "vile", "violent", "virtual", "visionary",
                 "visual", "vital", "vivid", "vocal", "volatile", "vulnerable", "wakeful", "warm",
                 "wayward", "weak", "weakly", "wealthy", "weary", "wee", "weird", "wet", "wicked",
                 "wide", "widespread", "wild", "willing", "wise", "wishful", "witty", "wobbly",
                 "wonderful", "wondrous", "worried", "worthwhile", "worthy", "wounded", "young",
                 "yukky", "yummy")
grd_manual <- c("delusive", "abdominal", "aboriginal", "absent", "absolute", "academic",
                "accented", "acceptable", "accessible", "accomplished", "accountable", "acoustical",
                "acrylic", "actual", "additional", "adequate", "adjacent", "administrative",
                "adolescent", "advantageous", "aerial", "affected", "affirmative", "affordable",
                "african", "aggregate", "agricultural", "albanian", "alive", "allergic", "alternative",
                "ambiguous", "american", "analogous", "analytical", "ancestral", "anecdotal",
                "angled", "anglican", "announced", "annual", "anonymous", "antarctic", "apocryphal",
                "aqueous", "arbitrary", "archaeological", "archaic", "arctic", "armed", "armoured",
                "artificial", "artistic", "asian", "asthmatic", "atmospheric", "atomic", "aussie",
                "australian", "austrian", "authorised", "automatic", "autonomous", "average", "awake",
                "back", "backward", "baked", "balanced", "bald", "bankrupt", "basic", "bearded",
                "beneficial", "best", "biblical", "bibliographic", "binding", "biodegradeable",
                "biographical", "biological", "black", "blank", "blatant", "blind", "blonde", "blue",
                "bodily", "booed", "botanical", "bottom", "british", "broke", "broken", "brown",
                "bucketful", "budgetary", "bureaucratic", "burnt", "businesslike", "busy", "californian",
                "canonical", "capitalistic", "captive", "cardiac", "catholic", "cellular", "central",
                "centralised", "centred", "certain", "characteristic  ", "chartered", "cheated",
                "chemical", "chilean", "chinese", "chivalrous", "christian", "chromatic", "chronological",
                "churchy", "classic", "classical", "clean", "clear", "close", "closed", "coarse",
                "coated", "coherent", "cohesive", "coincidental", "colloquial", "coloured", "coming",
                "commercial", "common", "compact", "comparable", "complete", "compound", "comprehensive",
                "compulsory", "computerised", "conceptual", "concrete", "confessional", "confirmed",
                "conscious", "conservative", "consistent", "constant", "constituent", "contemporary",
                "contestable", "continual", "contraceptive", "contrary", "cooked", "cooking",
                "corporate", "correct", "cracked", "crushed", "cubic", "cultural", "curly", "current",
                "customary", "cut", "daily", "dark", "dead", "deadly", "deaf", "decisive", "definite",
                "definitive", "deliberate", "democratic", "demographic", "determined", "diagnostic",
                "diagonal", "dietetic", "different", "digestive", "digital", "diplomatic", "direct",
                "discursive", "displaced", "disqualified", "distinct", "distinctive", "diverse", "divine",
                "domestic", "down", "downward", "dry", "dual", "dubious", "dummy", "dutch", "east",
                "educational", "effluent", "egalitarian", "electable", "electric", "electrical", "electronic",
                "elemental", "empty", "endemic", "endless", "english", "enough", "enrolled", "entailed",
                "entire", "equal", "equatorial", "equestrian", "equitable", "equivalent", "eritrean",
                "essential", "estonian", "ethic", "ethiopian", "ethnic", "european", "ewen", "exalted",
                "excellent", "executive", "exiguous", "existent", "existing", "exotic", "expected",
                "experimental", "explosive", "exponential", "external", "extinct", "extra", "extreme",
                "fair", "fake", "false", "far", "fatal", "favourite", "federal", "federated",
                "fellow", "female", "feminist", "feudal", "few", "fictional", "final", "financial",
                "first", "fixed", "flagged", "flannelled", "flat", "fleet", "flowing", "fluent",
                "fluid", "focused", "folded", "folding", "following", "foreign", "foremost", "formal",
                "forthcoming", "forward", "fossil", "foster", "founding", "fragile", "free", "french",
                "fresh", "frisian", "front", "frontal", "frosted", "fucking", "full", "fundamental",
                "funded", "further", "future", "gaelic", "gay", "general", "generational", "generic",
                "genuine", "geographical", "geological", "geotechnical", "german", "germanic", "glandular",
                "global", "gold", "golden", "governmental", "granulitic", "graphical", "gray", "greek",
                "green", "grey", "guaranteed", "half", "halved", "halving", "healthy", "hereditary",
                "heterogeneous   ", "heterogenious", "hidden", "historic", "historical", "holistic",
                "homosexual", "hooped", "horizontal", "hourly", "human", "humanitarian", "humiliating",
                "hungary", "hydroplaning", "hypocritical", "hypothetical", "iambic", "ideal", "identical",
                "ideological", "idle", "ill", "illegal", "imaginable", "immune", "imperial", "implicit",
                "implied", "impossible", "improved", "inaccessible", "inaccurate", "inadequate", "inclusive",
                "incoming", "incorrect", "incumbent", "indian", "indifferent", "indigenous", "indispensable",
                "indisputable", "industrial", "inefficient", "inescapable", "inexplicable", "infallible",
                "inflatable", "inflated", "informed", "infrequent", "inherent", "initial", "innate",
                "inner", "innocent", "innumerable", "inorganic", "inside", "insignificant", "instant",
                "instrumental", "insufficient", "intact", "integral", "intentional", "interactive",
                "intercultural", "interested", "interesting", "internal", "international", "interrupted",
                "intervening", "intriguing", "intrinsic", "inverted", "iraq", "irish", "irrelevant",
                "irrespective", "islamic", "italian", "japanese", "jewish", "joint", "journalistic",
                "judicial", "judicious", "junior", "just", "last", "latter", "leading", "learned",
                "learnt", "left", "lefthand", "legal", "legged", "legislative", "lesbian", "liable",
                "lime", "limited", "linear", "linguistic", "liquid", "literary", "liturgical", "live",
                "loaded", "local", "logarithmic", "logical", "logistic", "lost", "macrocyclic",
                "magnetic", "main", "male", "marine", "marked", "married", "masqueraded", "masterly",
                "materialistic   ", "maternal", "mathematical", "mature", "maximum", "mechanistic",
                "medieval", "mega", "melodic", "mental", "messy", "metamorphic", "meterological",
                "metrical", "metropolitan", "mexican", "micro", "microeconomic", "mid", "middle",
                "militaristic", "military", "milky", "minimal", "minimalist", "minimum", "ministerial",
                "missionary", "mobile", "moderate", "molecular", "molten", "monotonous", "mundane",
                "muscovite", "musical", "mutant", "naked", "narrative", "nasal", "natal", "national",
                "nationwide", "native", "natural", "nautical", "naval", "nazi", "needy", "negative",
                "neurotic", "next", "nitric", "north", "noticeable", "now", "nuclear", "obligatory",
                "obvious", "occasional", "octave", "official", "olympic", "ongoing", "only", "onward",
                "open", "operational", "opposed", "opposite", "optical", "optimum", "optional", "oral",
                "orange", "orchestral", "orchestrated", "organic", "original", "outside", "overlapping",
                "pacific", "painless", "pakistani", "parallel", "paramount", "parental", "parliamentary",
                "partial", "particular", "partisan", "passive", "past", "pastoral", "paternal",
                "paternalistic", "patriarchal", "patriotic", "perfect", "peripheral", "permanent",
                "permissive", "pertinent", "peruvian", "philosophical", "phonetic", "physical", "pink",
                "plastic", "pluralistic", "polar", "political", "politicised", "polynesian", "pornographic",
                "portable", "positive", "possible", "practicable", "preconceived", "preferential",
                "preferred", "pregnant", "preliminary", "presbyterian", "present", "presidential",
                "previous", "prewarned", "priceless", "primary", "prime", "principal", "prior", "pristine",
                "private", "privatised", "probable", "procedural", "programmed", "prolonged", "pronged",
                "proportional", "provincial", "psychiatric", "psychic", "pure", "purple", "quantifiable",
                "quantitative", "racial", "radioactive", "random", "readable", "real", "rear", "recent",
                "recycled", "red", "redundant", "reformed", "regional", "registered", "regulated",
                "regulatory", "reissued", "related", "relational", "relative", "remarkable", "remedial",
                "reportable", "reported", "residential", "respective", "resulting", "retrospective",
                "reusable", "reverse", "revolutionary", "ridged", "right", "rightful", "righthand",
                "rigid", "rigorous", "romanian", "rotary", "round", "royal", "ruined", "rural",
                "russian", "same", "samoan", "sane", "saturated", "scandinavian", "scholastic",
                "scottish", "scriptural", "seasonal", "secondary", "securing", "selected", "selective",
                "selfstyled", "senior", "senseless", "separate", "separated", "serial", "sheer",
                "siberian", "significant", "silver", "similar", "simultaneous", "sincere", "singaporean",
                "single", "skinned", "sleeveless", "sliced", "smokefree", "smooth", "sober", "socialist",
                "sociodemographic", "socioeconomic", "sole", "solitary", "soluble", "southern",
                "southwest", "sovereign", "soviet", "spanish", "special", "specialised", "specific",
                "spinal", "spontaneous", "spurious", "square", "stable", "stagnant", "standard",
                "stated", "stationary", "statistical", "statutory", "steely", "stereo", "stolen",
                "straight", "stratospheric", "striped", "structural", "subconscious", "subordinate",
                "subset", "substantial", "substantive", "suburban", "sudden", "sufficient", "suggestive",
                "sundry", "superheated", "supplementary", "supreme", "surgical", "sustained", "swedish",
                "swiss", "swollen", "symbolic", "synthetic", "technical", "technological", "temporary",
                "terminal", "territorial", "textual", "textural", "thematic", "thorough", "thoroughgoing",
                "timely", "total", "totalitarian", "toxic", "traditional", "transmitted", "traversable",
                "true", "twin", "ultimate", "unacceptable", "unaffected", "unallocated", "unannounced",
                "unanswered", "unbeaten", "unbiased", "unblemished", "unchanged", "uncoordinated",
                "under", "undisclosed", "undone", "unemployed", "unequal", "unexpired", "unfilled",
                "unfurnished", "unique", "universal", "unlimited", "unnatural", "unnecessary", "unoccupied",
                "unofficial", "unplayable", "unpopular", "unprecedented", "unprejudiced", "unpretentious",
                "unpromising", "unreceptive", "unregulated", "unrelated", "unresolved", "unrhymed",
                "unseeded", "unseen", "unselective", "unselfish", "unspecified", "unspoilt", "unstressed",
                "unsubsidised", "untold", "untrue", "unvarnished", "unwanted", "unwarranted", "unwilling",
                "unwrinkled", "upward", "usable", "useless", "usual", "utter", "vacant", "valid",
                "various", "veiled", "venetian", "verbal", "verbatim", "verifiable", "vertical",
                "volcanic", "voluntary", "weekly", "west", "western", "white", "whole", "wilful",
                "wooden", "woollen", "written", "wrong", "yellow", "youthful", "religious")
###############################################################
# gradability - data driven classification
# determine which adjectives are gradable
alladj <- names(table(ampcoha$Adjective)) 
grd1 <- names(table(ampcoha$Adjective[ampcoha$Variant == "very"])) # adjs with very are gradable
grd2 <- names(table(ampcoha$Adjective[ampcoha$Variant == "extremely"])) # adjs with extremely are gradable
ngrd1 <- names(table(ampcoha$Adjective[ampcoha$Variant == "completely"])) # adjs with completely are gradable
ngrd2 <- names(table(ampcoha$Adjective[ampcoha$Variant == "total"])) # adjs with total are gradable
ngrd3 <- names(table(ampcoha$Adjective[ampcoha$Variant == "totally"])) # adjs with totally are gradable
ngrd4 <- names(table(ampcoha$Adjective[ampcoha$Variant == "utterly"])) # adjs with utterly are gradable
ngrd5 <- names(table(ampcoha$Adjective[ampcoha$Variant == "absolutely"])) # adjs with absolutely are gradable
# create vector with gradable adjectives
grdadj <- intersect(grd1, grd2)
# create vector with non-gradable adjectives
ngrdadj <- c(ngrd1, ngrd2, ngrd3, ngrd4, ngrd5)
ngrdadj <- names(table(ngrdadj))
# find elements that occur in both groups
bthgrdngrd1 <- grdadj[grdadj %in% ngrdadj]
bthgrdngrd2 <- ngrdadj[ngrdadj %in% grdadj]
bthgrdngrd <- names(table(c(bthgrdngrd1, bthgrdngrd2)))
# extract adjs that are clearly gradable 
grd_datadriven <- grdadj[!grdadj %in% bthgrdngrd]
# extract adjs that are clearly not gradable 
ngrd_datadriven <- ngrdadj[!ngrdadj %in% bthgrdngrd]
# find elements that are neither in gradable nor in nongradable
gradnongrad <- names(table(c(grd_datadriven, ngrd_datadriven)))
nagrad <- alladj[!alladj %in% gradnongrad]
naadjs <- names(table(c(nagrad, bthgrdngrd)))
###############################################################
# combine data driven and manual classification
# adj unclassified by datadriven now assigned value based on manual coding
grd_add <- naadjs[naadjs %in% grd_manual]
ngrd_add <- naadjs[naadjs %in% ngrd_manual]
# combine data driven and manual coding
grdcoha <- names(table(c(grd_datadriven, grd_add)))
ngrdcoha <- names(table(c(ngrd_datadriven, ngrd_add)))
# check which adjs are still unclassified
nagradcoha1 <- alladj[!alladj %in% grdcoha]
nagradcoha <- nagradcoha1[!nagradcoha1 %in% ngrdcoha]
# inspect unclassified adj
#nagradcoha

# inspect length of vectors
length(alladj); length(grdcoha); length(ngrdcoha); length(nagradcoha)

# add gradability coding
ampcoha$Gradability <- ifelse(ampcoha$Adjective %in% grdcoha, "Gradable", ampcoha$Adjective)
ampcoha$Gradability <- ifelse(ampcoha$Gradability %in% ngrdcoha, "NotGradable", ampcoha$Gradability)
ampcoha$Gradability <- ifelse(ampcoha$Gradability  == "Gradable" | ampcoha$Gradability  == "NotGradable", 
                              ampcoha$Gradability, "GradabilityUndetermined")
# inspect data
head(ampcoha); table(ampcoha$Gradability)

# save table of gradable and non-gradable adj
gradtb <- table(ampcoha$Adjective, ampcoha$Gradability)
gradtb <- data.frame(gradtb)
colnames(gradtb) <- c("adj", "grad", "freq")
head(gradtb)

# dave table to file
write.table(gradtb, "gradtb.txt", sep = "\t", row.names = T)
###############################################################
# save data to disc
write.table(ampcoha, "ampcoha05_varcon_emograd.txt", sep = "\t", row.names = F)
#ampcoha <- read.table("ampcoha05_varcon_grad.txt",  sep = "\t", skipNul = T, header = T, fill = T)
###############################################################
# classification of semantic type
# add semantic types (tagliamonte 2008, based on dixon 1977)
# dimension = semdim (e.g. big, large, little, small, long, short, wide, narrow, thick)
# physical property = (e.g. hard, soft, heavy, light, rough, smooth, hot, sweet)
# color = semcol (e.g. black, white, red)
# human propensity: semhup (e.g. jealous, happy, kind, clever, generous, gay, rude)
# age = semage (e.g. new, young, old) 
# value (e.g. good, bad, proper, perfect, excellent, delicious, poor), 
# speed Speed (e.g. fast, quick, slow)
# position (e.g. right, left, near, far)
# other

# age
semage <- c("actual", "adolescent", "aged", "ancestral", "ancient", "annual", 
            "archaeological", "archaic", "biographical", "contemporary", 
            "elderly", "foster", "generational", "historic", "historical", 
            "immature", "junior", "late", "mature", "medieval", "modern", 
            "old", "oldfashioned", "outdated", "past", "preliminary", 
            "present", "primary", "prime", "prior", "puerile", "recent", 
            "seasonal", "senile", "senior", "temporary", "topical", 
            "veteran", "young", "youthful")
# color
semcol <- c("black", "blue", "brown", "coloured", "colourful", "colourless", 
            "dark", "darkened", "glittery", "gold", "golden", "gray", "green", 
            "grey", "lime", "marine", "orange", "pink", "pinkish", "purple", 
            "red", "reddish", "silver", "white", "yellow")
# dimension
semdim <- c("adjacent", "angled", "antique", "arctic", "back", "backward", 
            "bottom", "brief", "bright", "broad", "central", "centralised", 
            "centred", "chimerical", "circular", "circumspect", "close", 
            "compact", "deep", "diagonal", "diminutive", "direct", "distant", 
            "down", "downward", "early", "east", "easterly", "eastern", 
            "elongated", "endemic", "endless", "equatorial", "european", "ewen", 
            "far", "few", "first", "flat", "foreign", "foremost", "forthcoming", 
            "forward", "free", "front", "frontal", "further", "geographical", 
            "giant", "gigantic", "global", "grand", "half", "halved", "halving", 
            "high", "horizontal", "huge", "inner", "inside", "internal", 
            "international", "large", "last", "latter", "left", "level", 
            "limited", "linear", "little", "local", "locating", "long", 
            "long-lived", "low", "massive", "micro", "mid", "middle", "minimal", 
            "minimalist", "minimum", "minor", "misleading", "narrow", "national", 
            "nationwide", "native", "near", "nearby", "next", "north", "northern", 
            "off", "onward", "orientated", "outside", "oval", "over", 
            "overhanging", "overlapping", "pacific", "parallel", "paramount", 
            "peripheral", "petite", "polar", "prevalent", "proportional", 
            "provincial", "public", "rear", "regional", "remote", "reverse", 
            "round", "rural", "separate", "separated", "short", "shrewd", 
            "sizeable", "slight", "small", "south", "southern", "southwest", 
            "spinal", "square", "steep", "stratospheric", "suburban", "super", 
            "tall", "teensy", "terminal", "territorial", "thick", "thickened", 
            "thin", "tight", "tiny", "titanic", "top", "torrential", "touring", 
            "tremendous", "under", "universal", "unseeded", "upward", "urban", 
            "urbanised", "vast", "vertical", "warped", "wee", "west", "western", 
            "wide", "widespread")
semhup <- c("able", "abrasive", "absent-minded", "abusive", "academic", 
            "accomplished", "advanced", "advantageous", "adverse", "afraid", 
            "aggressive", "aimless", "amused", "amusing", "analytical", "angry", 
            "annoyed", "anxious", "appreciative", "apprehensive", "ashamed", 
            "astute", "attractive", "aware", "benevolent", "besetting", "bold", 
            "bossy", "brave", "brutal", "busy", "callous", "candid", "capable", 
            "challenging", "charismatic", "charming", "cheated", "chummy", 
            "clever", "cocky", "cogent", "comic", "comical", "communicative", 
            "compelling", "competent", "competitive", "concerned", "confident", 
            "considerate", "consultative", "convinced", "corny", "cross", "cruel",
            "cute", "cynical", "dear", "delighted", "depressed", "despairing", 
            "desperate", "despondent", "disappointed", "disoriented", "dodgy", 
            "dotty", "doubtful", "down-hearted", "drunk", "dubious", "dull", 
            "dumb", "eager", "emotionless", "encouraging", "enjoyable", 
            "entertaining", "enthusiastic", "erudite", "evil", "excitable", 
            "excited", "expectant", "expressive", "fanatic", "favorable", 
            "fearful", "ferocious", "fierce", "flighty", "flimsy", "foolish", 
            "forceful", "forgetful", "fortunate", "foxy", "fraudulent", "friendly",
            "frightened", "frustrated", "fun", "funny", "furious", "generous", 
            "gentlemanly", "gifted", "glad", "goodhearted", "gracious", "grateful",
            "grim", "groggy", "groovy", "gutless", "hapless", "happy", "hopeful", 
            "hopeless", "hostile", "hungry", "hysterical", "ignorant", 
            "imperative", "impressionable", "incompetent", "inexorable", 
            "inexperienced", "infallible", "informed", "insatiable", "insecure", 
            "insidious", "intellectual", "intelligent", "interested", "intriguing",
            "inventive", "jealous", "joyful", "judgmental", "jumpy", "learned", 
            "learnt", "level-headed", "literate", "lively", "loath", "lone", 
            "lonely", "lucky", "lunatic", "mad", "male", "mean", "melodramatic", 
            "militant", "minded", "motivating", "nasty", "nauseous", "nervous", 
            "obese", "optimistic", "passionate", "patient", "patronising", 
            "perceptive", "pessimistic", "picky", "pleased", "pleasing", "polite", 
            "preachy", "predictable", "prepared", "presumptuous", "primitive", 
            "procedural", "professional", "promotional", "prudential", "psycho", 
            "puzzled", "rapt", "rational", "regretful", "relentless", "reliable", 
            "resourceful", "respected", "romantic", "rowdy", "rude", "sad", "sane",
            "sarcastic", "satisfied", "satisfying", "savvy", "scared", "sceptical",
            "sea-sick", "seasick", "secretive", "selective", "selfish", "sensible",
            "sensitive", "sentimental", "serious", "sick", "silly", "skilful", 
            "skilled", "skittish", "smart", "snappy", "sneaky", "snotty", 
            "sociable", "sophisticated", "sorry", "sovereign", "spiteful", 
            "sporty", "staunch", "stern", "strategic", "strict", "stubborn", 
            "stupid", "suffering", "superior", "supportive", "surprised", "tactic",
            "talented", "talkative", "tame", "technical", "thirsty", "ticklish", 
            "tired", "treacherous", "troubled", "unable", "unacquainted", 
            "unanswered", "unaware", "uncaring", "ungallant", "ungrateful", 
            "unhappy", "unorthodox", "unsentimental", "unsmiling", "unsocial", 
            "upset", "valiant", "valid", "vengeful", "vile", "vulnerable", 
            "wealthy", "wicked", "willing", "wise", "witty", "worried")
# physical property
semphy <- c("acidic", "adaptable", "ascorbic", "audible", "automated", "aversive",
            "calm", "caloric", "clear", "cold", "combustible", "complimentary", 
            "dark", "different", "elastic", "firm", "hard", "heavy", "hollow", 
            "hot", "incompatible", "inflammable", "inoperative", "intelligible", 
            "interdependent", "intractable", "life-like", "lifelike", "light", 
            "liquid", "low-fat", "mobile", "neat", "nutritious", "objective", 
            "plain", "powerless", "prompt", "quick", "real", "same", 
            "self-supporting", "shaky", "similar", "singular", "slow", "stout", 
            "strong", "superfluous", "symmetrical", "unequal", "unhampered", 
            "uniform", "uninhibited", "unionized", "unsaturated", "versatile", 
            "virulent")
# Value
semval <- c("accurate", "agreeable", "amiable", "annoying", "appropriate", 
            "apt", "bad", "banal", "beautiful", "believable", "beneficial", 
            "biased", "blood-curdling", "boring", "chic", "classy", "cohesive", 
            "comfortable", "commendable", "competitive", "complex", 
            "complicated", "concise", "conclusive", "concrete", "confusing", 
            "contemptible", "contentious", "controversial", "convenient", 
            "convincing", "cross-eyed", "cultured", "dangerous", "deceptive", 
            "deficient", "demoniacal", "depressing", "deserving", "desirable", 
            "despicable", "difficult", "digestible", "disagreeable", 
            "discernible", "discouraging", "discreditable", "disgusting", 
            "dishonourable", "disingenuous", "displeased", "disputable", 
            "dissimilar", "distasteful", "distraught", "disturbed", "divisive", 
            "easy", "effective", "efficient", "egocentric", "egotistical", 
            "elusive", "embarrassing", "exciting", "excusable", "expendable", 
            "expensive", "facile", "fantastic", "farcical", "fat", "favorite", 
            "favourable", "favourite", "fine", "flattering", "flawed", 
            "fortuitous", "frustrating", "fussy", "genteel", "glib", "good", 
            "good-hearted", "good-looking", "gratifying", "gratuitous", "great", 
            "grown-up", "handsome", "handy", "harmless", "healthy-looking", 
            "hearty", "heinous", "helpful", "high-bred", "high-class", 
            "humorless", "icky", "ill", "ill-looking", "illegible", "illogical", 
            "imperfect", "impertinent", "implausible", "important", 
            "impracticable", "impractical", "impressive", "improbable", 
            "imprudent", "inaccurate", "inadequate", "inappropriate", 
            "inartistic", "incorrect", "indefensible", "ineffective", 
            "inefficient", "inexcusable", "infatuated", "ingenious", 
            "instructive", "interesting", "irrational", "irrelevant", 
            "kind-hearted", "lady-like", "ladylike", "laudable", "laughable", 
            "likable", "likeable", "livable", "lovable", "loveable", "lucrative", 
            "meagre", "mean-spirited", "moderate", "naive", "naughty", 
            "nauseating", "new", "nice", "nice-looking", "noteworthy", 
            "objectionable", "old", "one-sided", "overrated", "pardonable", 
            "penitent", "photogenic", "plausible", "pleasant", "pointless", 
            "popular", "precarious", "pretty", "profitable", "promising", 
            "punctual", "puzzling", "questionable", "relevant", "remarkable", 
            "reprehensible", "repugnant", "repulsive", "respectable", 
            "rewarding", "risky", "satisfactory", "scary", "seaworthy", 
            "self-centered", "self-possessed", "self-satisfied", "sexy", 
            "sheepish", "shitty", "short-sighted", "significant", "simple", 
            "skimpy", "special", "stressful", "successful", "tasty", 
            "tender-hearted", "tiresome", "tiring", "tolerable", "tolerant", 
            "touchy", "tough", "tragical", "trashy", "tricky", "trivial", 
            "troublesome", "twisted", "unacceptable", "unamiable", "unattractive",
            "unbelievable", "uncomfortable", "understandable", "undignified", 
            "unethical", "unfair", "unfavorable", "unfit", "unfounded", 
            "unimpressed", "unimpressive", "uninformed", "unintellectual", 
            "unintelligible", "uninteresting", "unjustifiable", "unkind", 
            "unlikely", "unmanageable", "unnecessary", "unoriginal", "unpleasant",
            "unpopular", "unprepared", "unprofessional", "unpromising", 
            "unrealistic", "unreasonable", "unreliable", "unromantic", "unsafe", 
            "unsatisfactory", "unscientific", "unselfish", "unspoiled", "unstable",
            "untidy", "untrue", "unwarranted", "unwise", "uplifting", "useful", 
            "valuable", "weak", "welcome", "well", "well-groomed", "well-informed",
            "womanly", "worthless", "young", "young-looking")
# clean Adjectives
ampcoha$Adjective <- gsub("-", "", ampcoha$Adjective)
# add semantic type classification
ampcoha$SemanticCategory <- ifelse(ampcoha$Adjective %in% semage, "Age", ampcoha$Adjective)
ampcoha$SemanticCategory <- ifelse(ampcoha$SemanticCategory %in% semcol, "Color", ampcoha$SemanticCategory)
ampcoha$SemanticCategory <- ifelse(ampcoha$SemanticCategory %in% semdim, "Dimension", ampcoha$SemanticCategory)
ampcoha$SemanticCategory <- ifelse(ampcoha$SemanticCategory %in% semhup, "HumanPropensity", ampcoha$SemanticCategory)
ampcoha$SemanticCategory <- ifelse(ampcoha$SemanticCategory %in% semphy, "PhysicalProperty", ampcoha$SemanticCategory)
ampcoha$SemanticCategory <- ifelse(ampcoha$SemanticCategory %in% semval, "Value", ampcoha$SemanticCategory)
ampcoha$SemanticCategory <- ifelse(ampcoha$SemanticCategory == "Age" | ampcoha$SemanticCategory == "Color" | 
                                     ampcoha$SemanticCategory == "Dimension" | ampcoha$SemanticCategory == "HumanPropensity" |
                                     ampcoha$SemanticCategory == "PhysicalProperty" | ampcoha$SemanticCategory == "Value",  
                                   ampcoha$SemanticCategory, "NoSemType")
# table sem class of tokens
table(ampcoha$SemanticCategory)

# check which adjectives are not assigned a semantic class
nosemtype <- ampcoha$Adjective[ampcoha$SemanticCategory == "NoSemType"]
#names(table(nosemtype))

# inspect data
head(ampcoha); nrow(ampcoha); length(table(ampcoha$Adjective))

###############################################################
# add freq of adj type by decade
frqadjtb <- table(ampcoha$Decade, ampcoha$Adjective)
relfreqadjtb <- round(prop.table(frqadjtb, margin = 1)*100, 5)
relfreqadjdf <- as.data.frame(relfreqadjtb)
colnames(relfreqadjdf)[1:2] <- c("Decade", "Adjective")
# add freq by date to data
ampcoha <- merge(ampcoha, relfreqadjdf, by=c("Decade", "Adjective"))
# reorder data
ampcoha <- ampcoha[order(ampcoha$id),]
# inspect data
head(ampcoha)

###############################################################
# clean data
ampcoha$file <- NULL
ampcoha$pre <- NULL
ampcoha$token <- NULL
###############################################################
# save data to disc
write.table(ampcoha, "ampcoha06_varcon_emogradsem.txt", sep = "\t", row.names = F)
###############################################################
#                         END PART 2
###############################################################
