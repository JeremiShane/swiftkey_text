


## The goal of this exercise is to create a product to highlight the prediction algorithm that you have built and to provide an interface that can be accessed by others via a Shiny app..


## Create a data product to show off your prediction algorithm You should create a Shiny app that accepts an n-gram and predicts the next word.

## What are the most interesting ways you could show off your algorithm?
        ## Are there any data visualizations you think might be helpful (look at the Swiftkey data dashboard if you have it loaded on your phone)?
        ## How should you document the use of your data product (separately from how you created it) so that others can rapidly deploy your algorithm?
        ## Tips, tricks, and hints

## Consider the size of the predictive model you have developed. You may have to sacrifice some accuracy to have a fast enough/small enough model to load into Shiny.


       ##  Tasks to accomplish

       ## Create a slide deck promoting your product. Write 5 slides using RStudio Presenter explaining your product and why it is awesome!
               ##  Questions to consider

        ## How can you briefly explain how your predictive model works?
               ## How can you succinctly quantitatively summarize the performance of your prediction algorithm?
                ## How can you show the user how the product works?
               ## Tips, tricks, and hints

       ## The Rstudio presentation information is available here (https://support.rstudio.com/hc/en-us/articles/200486468-Authoring-R-Presentations).




# Get Data

## https://www.rdocumentation.org/packages/tm.plugin.webmining/versions/1.3/topics/removeNonASCII
## https://rdrr.io/rforge/tm/man/PlainTextDocument.html
## make tm PlainTextDocument then remove non-ASCII characters

library(tm)
library(tm.plugin.webmining)


con <- file("final/en_US/en_US.twitter.txt", open="rb")
twitter <- readLines(con, encoding="UTF-8")
close(con)
ptweet <- PlainTextDocument(twitter)
ptweet <- removeNonASCII(ptweet, from="UTF-8", to = "ASCII//TRANSLIT")

con <- file("final/en_US/en_US.news.txt", open="rb")
news <- readLines(con, encoding="UTF-8")
close(con)
pnews <- PlainTextDocument(news)
pnews <- removeNonASCII(pnews, from="UTF-8", to = "ASCII//TRANSLIT")

#########################################################################
#########################################################################
## Sample and Clean the data
#########################################################################

# word and line counts
neWsWordCount<- sum((nchar(news) - nchar(gsub(' ','',news))) + 1)
newsLinesCount<-NROW(news)

tweetWordCount<- sum((nchar(twitter) - nchar(gsub(' ','',twitter))) + 1)
tweetLinesCount<-NROW(twitter)

library(ngram)

#########################################################################
## sample from tm PlainTextDocuments with non-ASCII removed
##########################################################################
dfpnews <- data.frame("text"=pnews$content, "type"="news")
## dfpblogs <- data.frame("text"=pblogs$content, "type"="blogs")
dfptweet <- data.frame("text"=ptweet$content, "type"="tweet")
dfp <- rbind(dfpnews, dfptweet)
###########################################################################


#############################################################################
## clean our text data and remove profanity terms
#############################################################################
## https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# use tm package to create a corpus to remove profanity
######################################################################
## library(tm)
## corpus<- Corpus(VectorSource(clean_sample))

## use all text from all 3 files.  clean the corpus, reduce the terms by frequency.
clean_sample<- gsub('[[:digit:]]+', '', dfp$text)
clean_sample<- gsub('[[:punct:]]+', '', clean_sample)
clean_sample<- gsub("http[[:alnum:]]*", "", clean_sample)
clean_sample<- gsub("([[:alpha:]])\1+", "", clean_sample)
clean_sample <- gsub("\\s+", " ", clean_sample)
clean_sample <- gsub("\n", " ", clean_sample)
clean_sample <- gsub("\\n", "", clean_sample)
clean_sample <- gsub("*", "", clean_sample)
clean_sample <- gsub("%", "", clean_sample)

cl <- clean_sample
dfp$text <- cl

library(textclean)
start_time <- Sys.time()
print("starting replace word elongation")
cs <- replace_word_elongation(cl)
end_time <- Sys.time()
end_time - start_time

dfp$text <- cs

save(dfp, file="skbt_6.RData")


###########################################################

##############################################################

##################################################################

gc()

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
load("skbt_6.RData")


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

txt <- replace_number(txt, replace='')

## each sentance a new character string element
## this will allow for each sentance to be it's own document in the corpus
snt <- tokenize_sentences(txt) ## library(tokenizers)
snt <- unlist(snt)  ## one sentence per string

save(txt, snt,  file="skbt_66.RData")
gc()
library(tm)
library(tidytext)

load("skbt_66.RData") ###################################################

cs <- snt
c <- Corpus(VectorSource(cs))
corpus <- c

## For transformations on corpora use tm_map from library(tm)
profanity<-read.table("en_profanity.txt", header=FALSE, sep="\n", strip.white=TRUE)
names(profanity)<-"profanity"
profanity <- tolower(profanity[,1])

## I need a better stop words list
## maybe remove all words below a certain frequency threshold as well
mystopwords = c("st.", "st", "ave.", "ave", "na", "miss", "mrs", "mrs."
        ,"can", "can't", "what", "would", "so"
        , "tri", "get", "need", "at", "are", "is", "as", "just"
        ,"was", "with", "on", "to", "the", "it", "and", "in", "but", "or"
        ,"will", "you", "can", "when", "i'm", "hi", "hello"
        ,"buy", "bye", "out", "go", "come", "get", "eu", "llc", "rt", "ppl", "NA")

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

save(mycorpus,  file="skbt_mycorpus.RData")


## remove proper nouns
library(gender)
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
gc()
load("skbt_corpus.RData")
library(ngram)

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

start_time <- Sys.time()
vocab2 <- ngram(s, n=2)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
vocab2Freq <- get.phrasetable(vocab2)
end_time <- Sys.time()
end_time - start_time


save(vocabFreq, vocab2Freq, file="skbt_6_vocab.RData")

########################################################################

######################################################################

#####################################################################
gc()
load("skbt_6_vocab.RData")
load("skbt_corpus.RData")

library(tm)
library(ngram)
## create a library of infrequent words
## infreq, everything below a certain frequency
## summary(vocab2Freq$freq)
infreq <- vocab2Freq$ngrams[vocab2Freq$freq<2]

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
load("skbt_nFreqs.RData")
twoF <- head(twoFreq, 100000)
threeF <- head(threeFreq, 50000)
fourF <- head(fourFreq, 25000)
fiveF <- head(fiveFreq, 10000)

tw <- separate(twoF, ngrams, c("one", "two"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
thre <- separate(threeF, ngrams, c("one", "two", "three"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
fou <- separate(fourF, ngrams, c("one", "two", "three", "four"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
fiv <- separate(fiveF, ngrams, c("one", "two", "three", "four", "five"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")

two <- separate(twoFreq, ngrams, c("one", "two"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
three <- separate(threeFreq, ngrams, c("one", "two", "three"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
four <- separate(fourFreq, ngrams, c("one", "two", "three", "four"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
five <- separate(fiveFreq, ngrams, c("one", "two", "three", "four", "five"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")

save(two, three, four, five, file="skbt_nb.RData")

save(tw, file="skbt_two.RData")
save(thre, file="skbt_three.RData")
save(fou, file="skbt_four.RData")
save(fiv, file="skbt_fiv.RData")