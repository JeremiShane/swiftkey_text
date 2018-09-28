
## swiftkey capstone R file
## skbt_2.R
## version for week 2 milestone report
## JeremiShane
## 9/20/2018


# Get Data

# Zip file downloaded, unzipped in working directory
# open files and read lines
## fl <- list.files("final/en_US/.")
con <- file("final/en_US/en_US.blogs.txt", open="rb")
blogs <- readLines(con, encoding="UTF-8")
close(con)

## https://www.rdocumentation.org/packages/tm.plugin.webmining/versions/1.3/topics/removeNonASCII
## https://rdrr.io/rforge/tm/man/PlainTextDocument.html
## make tm PlainTextDocument then remove non-ASCII characters

library(tm)
library(tm.plugin.webmining)

pblogs <- PlainTextDocument(blogs)
pblogs <- removeNonASCII(pblogs, from="UTF-8", to = "ASCII//TRANSLIT")


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

##########################################################################
# File and Data Overview
############################################################################
library(stringi)
library(dplyr)
sizeMB<- function(x){round(file.info(x)$size/1024^2)} # function to generate file size in MB
char <- function(x){stri_length(x) - stri_count_fixed(x," ")} # function to count characters without spaces
filesummary<-data.frame(Source=c("blogs","twitter", "news"),
        FileSize_MB=c(sizeMB("final/en_US/en_US.blogs.txt"),sizeMB("final/en_US/en_US.twitter.txt"),sizeMB("final/en_US/en_US.news.txt")),
        Lines=c(length(blogs),length(twitter),length(news)),
        Words=c(sum(stri_count_words(blogs)),sum(stri_count_words(twitter)),sum(stri_count_words(news))),
        Characters=c(sum(char(blogs)),sum(char(twitter)),sum(char(news))))
filesummary<-mutate(filesummary,Words_Per_Line=Words/Lines,Char_Per_Line=round(Characters/Lines,1),Char_Per_Word=round(Characters/Words,2))

skbt_file_summary <- filesummary
save(skbt_file_summary, file="skbt_file_summary.RData")
## print(filesummary)
###########################################################################
## later retrieve skbt_file_summary, file="skbt_file_summary.RData"
############################################################################


#########################################################################
#########################################################################
## Sample and Clean the data
#########################################################################

# word and line counts
blogWordCount<- sum((nchar(blogs) - nchar(gsub(' ','',blogs))) + 1)
blogLinesCount<-NROW(blogs)

neWsWordCount<- sum((nchar(news) - nchar(gsub(' ','',news))) + 1)
newsLinesCount<-NROW(news)

tweetWordCount<- sum((nchar(twitter) - nchar(gsub(' ','',twitter))) + 1)
tweetLinesCount<-NROW(twitter)

docProperties<-matrix(c(blogWordCount, neWsWordCount, tweetWordCount, blogLinesCount, newsLinesCount, tweetLinesCount), nrow=3, ncol=2)
rownames(docProperties)<- c("blogs","news","twitter")
colnames(docProperties)<- c("# of words","# of lines")


##library(text2vec)  ## https://cran.r-project.org/web/packages/text2vec/text2vec.pdf
## https://www.rdocumentation.org/packages/text2vec/versions/0.5.1/topics/itoken
##library(tokenizers)
library(wordcloud)
##library(topicmodels)
##library(ldatuning)
library(ngram)

#########################################################################
## sample from tm PlainTextDocuments with non-ASCII removed
##########################################################################
dfpnews <- data.frame("text"=pnews$content, "type"="news")
dfpblogs <- data.frame("text"=pblogs$content, "type"="blogs")
dfptweet <- data.frame("text"=ptweet$content, "type"="tweet")
dfp <- rbind(dfpnews, dfpblogs, dfptweet)
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
##clean_sample<- gsub('[[:punct:]]+', '', clean_sample)
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

save(dfp, file="skbt_cl.RData")

cs <- dfp$text
c <- Corpus(VectorSource(cs))
corpus <- c

## For transformations on corpora use tm_map from library(tm)
profanity<-read.table("en_profanity.txt", header=FALSE, sep="\n", strip.white=TRUE)
names(profanity)<-"profanity"

stop_words = c("i", "me", "we", "to", "the", "it", "is", "a", "and", "in", "but", "this", "that",
        "on", "for", "of", "be", "he", "she", "him", "her", "his", "hers",
        "as", "are", "at", "am", "an", "or", "not", "the", "NA",
        "have", "was", "do", "there", "has", "with", "want", "you", "can", "if", "whi", "how",
        "go", "from", "they", "insur", "then", "see", "tri", "get", "need", "show", "when",
        "will", "im", "i'm", "hi", "one", "what", "would", "so", "say")

## may want to remove more stop words
## maybe will chop off words at a certain frequency or both
mycorpus<- tm_map(corpus, removeWords, profanity[,1])
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, stemDocument)
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, removeWords, c(stopwords("english"), stop_words))
## we removed non-ascii characters due to previous errors here with tolower


dtm <- DocumentTermMatrix(mycorpus)
save(dtm, mycorpus, file="skbt_dtm_mycorpus.RData")

## look at cummulative frequency to determine at which frequency to filter the terms
## to get the frequency occurence of each word
## freq <- colsums(as.matrix(dtm))
## this errors due to size and memory constraints
## so how do we determine a good frequency threshold for filtering the data?
inspect(dtm)  ## there are 513,752 terms
fterms <- data.frame(findFreqTerms(dtm, lowfreq=13))
str(fterms)
nrow(fterms)/dtm$ncol  ## .1538777
## using 13 as our threshold filters us down to about 15% of the total terms
## according to an expert Data Scientist 10-20% is good

## we appear to have a lot of sparsity
## dtm2 <- removeSparseTerms(dtm, 0.2)
## inspect(dtm2)
## load("skbt_dtm_mycorpus.RData")
## remove low frequency terms

## skbt_mincorpus.R
## skbt_rm_elong.R   ## optional, use str otherwise
## skbt_sorted_ngrams.R
## skbt_model_train.R (fourgram, df_four)

# a small sample for the meantime for exploring and visuals
library(tm)
library(tm.plugin.webmining)
library(textclean)
library(ngram)
library(dplyr)
str(dfp)  ## 4269678 obs. of 2 vars
str(pnews) ## content: chr [1:1010242]
dn <- data.frame("text"=pnews$content[1:100000], "type"="news")
db <- data.frame("text"=pblogs$content[1:100000], "type"="blogs")
dt <- data.frame("text"=ptweet$content[1:100000], "type"="tweet")
x <- rbind(dn, db, dt)

clean_sample<- gsub('[[:digit:]]+', '', x$text)
##clean_sample<- gsub('[[:punct:]]+', '', clean_sample)
clean_sample<- gsub("http[[:alnum:]]*", "", clean_sample)
clean_sample<- gsub("([[:alpha:]])\1+", "", clean_sample)
clean_sample <- gsub("\\s+", " ", clean_sample)
clean_sample <- gsub("\n", " ", clean_sample)
clean_sample <- gsub("\\n", "", clean_sample)
clean_sample <- gsub("*", "", clean_sample)
clean_sample <- gsub("%", "", clean_sample)

x$text <- clean_sample

start_time <- Sys.time()
print("starting replace word elongation")
cs <- replace_word_elongation(x$text)
end_time <- Sys.time()
end_time - start_time

x$text <- cs

## save(x, file="skbt_x.RData")

c <- Corpus(VectorSource(x$text))

## For transformations on corpora use tm_map from library(tm)
profanity<-read.table("en_profanity.txt", header=FALSE, sep="\n", strip.white=TRUE)
names(profanity)<-"profanity"

## may want to remove more stop words
## maybe will chop off words at a certain frequency or both
c<- tm_map(c, removeWords, profanity[,1])
c <- tm_map(c, content_transformer(tolower))
c <- tm_map(c, removePunctuation)
c <- tm_map(c, stemDocument)
c <- tm_map(c, stripWhitespace)
c <- tm_map(c, removeWords, c(stopwords("english"), stop_words))

xdtm <- DocumentTermMatrix(c)
## save(xdtm, c, file="skbt_xdtm.RData")

freqTerms <- findFreqTerms(xdtm, lowfreq=1, highfreq = 13)
df <- data.frame(freqTerms)
##str(freqTerms)
start_time <- Sys.time()
l=1
m <- ceiling(nrow(df)/1000)
n <- nrow(df)
for (i in 1:m){
        j = l + 1000
        if (j > n) {j = n}
        c <- tm_map(c,removeWords,df[l:j,1])

        l = j+1
        print(c(i,j,l,m,n))
}
end_time <- Sys.time()
end_time - start_time

## save our minimized corpus, data frame, chr string, doc term matrix
cc <- c

stop_words = c("i", "me", "we", "to", "the", "it", "is", "a", "and", "in", "but", "this", "that",
        "on", "for", "of", "be", "he", "she", "him", "her", "his", "hers",
         "as", "are", "at", "am", "an", "or", "not", "the", "NA",
        "have", "was", "do", "there", "has", "with", "want", "you", "can", "if", "whi", "how",
        "go", "from", "they", "insur", "then", "see", "tri", "get", "need", "show", "when",
        "will", "im", "i'm", "hi", "one", "what", "would", "so", "say")

c <- tm_map(cc, removeWords, c(stopwords("english"), stop_words))
mdtm <- DocumentTermMatrix(cc)
mdf <- data.frame(text=unlist(sapply(cc, `[`, "content")), stringsAsFactors=F)
st <- concatenate ( lapply ( cc$content , "[", 1) )


## create our onegram and twograms
start_time <- Sys.time()
print("starting onegram")
one <- ngram(st, n=1)
end_time <- Sys.time()
end_time - start_time
start_time <- Sys.time()
print("starting twogram")
two <- ngram(st, n=2)
print("finished twogram")
end_time <- Sys.time()
end_time - start_time

## build a data frame for wordcloud

single_words <- strsplit(st, ' ')
vocab <- get.ngrams(one)
vocab_table <- table(single_words)
single_words_df <- data.frame(vocab_table)
#Create a sorted table by decending frequency count
sorted_w <-single_words_df[order(single_words_df$Freq, decreasing = TRUE),]
dfw <- data.frame("word"=sorted_w[1:300,1], "freq"=sorted_w[1:300,2])
freqHist <- hist(log(single_words_df$Freq))

vocab <- get.ngrams(two)
vocab_table <- table(vocab)
two_df <- data.frame(vocab_table)

vocab <- get.ngrams(three)
vocab_table <- table(vocab)
three_df <- data.frame(vocab_table)

## load("skbt_exp.RData")
library(wordcloud2)
library(tm)
library(ngram)

## https://www.r-bloggers.com/the-wordcloud2-library/
library(htmlwidgets)
library(webshot)
set.seed(13)
wc <- wordcloud2(dfw, size=50, minSize=3, gridSize=1, fontFamily='Segoe UI',
        fontWeight='bold', color='random-dark', backgroundColor='white',
        minRotation = -pi/4, maxRotation=pi/4, shuffle=FALSE, rotateRatio = .4,
        shape='circle', ellipticity=.65, widgetsize=NULL, figPath=NULL,
        hoverFunction=NULL)
## saveWidget(wc, "tmp.html", selfcontained = F)
## webshot("tmp.html", "wc.png", delay = 1000, vwidth = 2000, vheight = 2000)

oneFreq <- head(get.phrasetable(one), 10)
twoFreq <- head(get.phrasetable(two), 10)
threeFreq <- head(get.phrasetable(three), 10)

save(st,cc,mdtm,mdf,oneFreq, twoFreq, threeFreq, sorted_w,dfw, wc, single_words_df, two_df, three_df, file="skbt_exp.RData")
save(oneFreq, twoFreq, threeFreq, freqHist, file="skbt_wk2.RData")
