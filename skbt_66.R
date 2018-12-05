


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

library(tm)
library(tm.plugin.webmining)
library(ngram)
library(qdap)
library(textclean)
library(tokenizers)
library(gender)
library(dplyr)
library(tidyr)
library(tidytext)
library(stopwords)

load("skbt_txt.RData")
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
        ,"will", "you", "can", "when", "i'm", "NA", "lol"
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
## install_genderdata_package()
## sets <- data(package = "genderdata")$results[,"Item"]
## data(list = sets, package = "genderdata")
## Names <- unique(kantrowitz$name)
## save(Names, file="skbt_names.RData")
load("skbt_names.RData")

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

save(mycorpus,  file="skbt_66_corpus.RData")
###########################################################################
############################################################################

gc()
load("skbt_66_corpus.RData")

str <- concatenate ( lapply (mycorpus$content , "[", 1) )

dtm <- DocumentTermMatrix(mycorpus)

onegram <- ngram(str, n=1)
oneFreq <- get.phrasetable(onegram)

fourgram <- ngram(str, n=4)
fourFreq <- get.phrasetable(fourgram)

twogram <- ngram(str, n=2)
twoFreq <- get.phrasetable(twogram)

threegram <- ngram(str, n=3)
threeFreq <- get.phrasetable(threegram)

fivegram <- ngram(str, n=5)
fiveFreq <- get.phrasetable(fivegram)

one <- head(oneFreq, 1000)
twoF <- head(twoFreq, 100000)
threeF <- head(threeFreq, 50000)
fourF <- head(fourFreq, 25000)
fiveF <- head(fiveFreq, 10000)

two <- separate(twoF, ngrams, c("one", "two"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
three <- separate(threeF, ngrams, c("one", "two", "three"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
four <- separate(fourF, ngrams, c("one", "two", "three", "four"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")
five <- separate(fiveF, ngrams, c("one", "two", "three", "four", "five"), sep="[ |\t]+", remove=FALSE, extra="warn", fill="warn")

save(dtm, file="skbt_66_dtm.RData")
save(one, file="skbt_one.RData")
save(two, file="skbt_two.RData")
save(three, file="skbt_three.RData")
save(four, file="skbt_four.RData")
save(five, file="skbt_five.RData")