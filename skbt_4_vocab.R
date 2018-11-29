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
n <- 1000
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

save(newcorpus, file="skbt_6_newcorpus.RData")

## create a library of overused words
## toofreq, everyting above a certain frequency
## mycorpus <- tm_map(mycorpus, removeWords, toofreq)

###############################################################
################################################################