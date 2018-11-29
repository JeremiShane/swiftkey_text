## swiftkey capstone R file
## skbt_4.R
## version for week 4
## JeremiShane
## 11/8/2018

## Explore new models and data to improve your predictive model.
## Evaluate your new predictions on both accuracy and efficiency.

## What are some alternative data sets you could consider using?
## What are ways in which the n-gram model may be inefficient?
## What are the most commonly missed n-grams? Can you think of a reason why they would be missed and fix that?
## What are some other things that other people have tried to improve their model?
## Can you estimate how uncertain you are about the words you are predicting?

# run skbt_2.R
# Get Data

library(tm)
library(tm.plugin.webmining)


## load()  save(skbt_file_summary, file="skbt_file_summary.RData")

library(ngram)
library(qdap)
library(textclean)

library(tokenizers)
library(gender)
library(dplyr)
library(tidyr)
library(tidytext)
library(stopwords)

## load clean data dfp
load("skbt_cl.RData")


## if I want to break into sentences
## issue with St. Louis for example
## library(tokenizers)
## test <- unlist(tokenize_sentences(dfp$text))
txt <- dfp$text
## replace contractions
## https://rpubs.com/williamsurles/316682

txt <- replace_contraction(txt) ## library(qdap)

## add space after commas
txt <- add_comma_space(txt) ## library(textclean)

txt <- replace_date(txt, REPLACEMENT='') ## library(textclean)

txt <- replace_abbreviation(txt, replace='')  ## qdap
txt <- replace_symbol(txt)

## stop_words = c("St.", "st.", "st", "ave.", "ave", "NA", "Miss", "Mrs", "Mrs.")
## txt <- removeWords(txt, stop_words)

txt <- replace_number(txt, replace='')

save(txt, file="skbt_txt.RData")

load("skbt_txt.RData")

## each sentance a new character string element
## this will allow for each sentance to be it's own document in the corpus
snt <- tokenize_sentences(txt) ## library(tokenizers)
snt <- unlist(snt)  ## one sentence per string

save(txt, snt,  file="skbt_txt.RData")

cs <- snt
c <- Corpus(VectorSource(cs))
corpus <- c

save(vocabFreq, "skbt_4_vocab.RData")

## For transformations on corpora use tm_map from library(tm)
profanity<-read.table("en_profanity.txt", header=FALSE, sep="\n", strip.white=TRUE)
names(profanity)<-"profanity"
profanity <- tolower(profanity[,1])

## I need a better stop words list
## maybe remove all words below a certain frequency threshold as well
mystopwords = c("st.", "st", "ave.", "ave", "na", "miss", "mrs", "mrs."
        ,"can", "can't", "change", "show", "what", "would", "so"
        , "tri", "get", "need", "at", "are", "is", "as", "just"
        ,"was", "with", "on", "to", "the", "it", "and", "in", "but", "or"
        ,"will", "want", "you", "can", "when", "i'm", "hi", "hello"
        ,"buy", "bye", "out", "go", "come", "get", "eu", "llc", "rt", "ppl")

mycorpus <- tm_map(corpus, content_transformer(tolower))
## may want to remove more stop words
## maybe will chop off words at a certain frequency or both
mycorpus<- tm_map(mycorpus, removeWords, profanity)
mycorpus <- tm_map(mycorpus, removeWords, stopwords("en")) ## tm
mycorpus <- tm_map(mycorpus, removeWords, mystopwords)
mycorpus <- tm_map(mycorpus, removeWords, stop_words$word) ## tidytext

mycorpus <- tm_map(mycorpus, removePunctuation)
## mycorpus <- tm_map(mycorpus, stemDocument)
mycorpus <- tm_map(mycorpus, stripWhitespace)

## remove proper nouns
## library(gender)
install_genderdata_package()
sets <- data(package = "genderdata")$results[,"Item"]
data(list = sets, package = "genderdata")
Names <- unique(kantrowitz$name)

start <- 1
m <- start
l <- length(Names)
n <- 100
start_time <- Sys.time()
end <- ceiling(l/n)
for (i in start:end){
        j = m + n
        if (j > l) {j = l}
        mycorpus <- tm_map(mycorpus, removeWords, Names[m:j])
        print(c(m,j))
        m = j+1
}

end_time <- Sys.time()
end_time - start_time



## we removed non-ascii characters due to previous errors here with tolower

save(mycorpus,  file="skbt_corpus.RData")
load("skbt_corpus.RData")

## remove infrequent and overused vocabulary
## remove infrequent and overused vocabulary
s <- concatenate ( lapply (mycorpus$content , "[", 1) )

start_time <- Sys.time()
vocab <- ngram(s, n=1)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
vocabFreq <- get.phrasetable(vocab)
end_time <- Sys.time()
end_time - start_time


save(vocabFreq, file="skbt_4_vocab.RData")

load("skbt_4_vocab.RData")
load("skbt_corpus.RData")su

## create a library of infrequent words
## infreq, everything below a certain frequency
infreq <- vocabFreq$ngrams[vocabFreq$freq<3]

start <- 1
m <- start
l <- length(infreq)
n <- 100
start_time <- Sys.time()
end <- ceiling(l/n)
for (i in start:end){
        j = m + n
        if (j > l) {j = l}
        newcorpus <- tm_map(mycorpus, removeWords, infreq[m:j])
        print(c(m,j))
        m = j+1
}

end_time <- Sys.time()
end_time - start_time

save(newcorpus, file="skbt_4_newcorpus.RData")

## create a library of overused words
## toofreq, everyting above a certain frequency
## mycorpus <- tm_map(mycorpus, removeWords, toofreq)

###############################################################
################################################################

dtm <- DocumentTermMatrix(newcorpus)
str <- concatenate ( lapply (newcorpus$content , "[", 1) )

fourgram <- ngram(str, n=4)
fourFreq <- get.phrasetable(fourgram)

## save(str, dtm, mycorpus, fourFreq, file="skbt_dtm_mycorpus.RData")

twogram <- ngram(str, n=2)
twoFreq <- get.phrasetable(twogram)

threegram <- ngram(str, n=3)
threeFreq <- get.phrasetable(threegram)

fivegram <- ngram(str, n=5)
fiveFreq <- get.phrasetable(fivegram)

save(str, twoFreq, threeFreq, fourFreq, fourFreq, fiveFreq, file="skbt_nFreqs.RData")

two <- separate(twoFreq, ngrams, c("one", "two"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
three <- separate(threeFreq, ngrams, c("one", "two", "three"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
four <- separate(fourFreq, ngrams, c("one", "two", "three", "four"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
five <- separate(fiveFreq, ngrams, c("one", "two", "three", "four", "five"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")

save(two, three, four, five, file="skbt_nb.RData")


