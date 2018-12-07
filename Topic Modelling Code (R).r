#Sample Model for doing topic modelling with LDA model. Requires dataset with an ID for chats (CUST_ID) and a verbatim column (Additional.Information) with free text


foo <- read.delim('Chat4U full data.txt', header = TRUE, sep = '|')

#test <- subset(foo, AZCorp != '#N/A')
test <- foo
#summary(test)

test$Date.of.Chat.Request <- strptime(test$Date.of.Chat.Request, format = '%m/%d/%y %H:%M')
test$CUST_ID <- as.factor(test$CUST_ID)
test$Additional.Information <- as.factor(test$Additional.Information) ## used to tokenize
test$Additional.Information <- as.character((test$Additional.Information))



library(text2vec)  ## https://cran.r-project.org/web/packages/text2vec/text2vec.pdf
        ## https://www.rdocumentation.org/packages/text2vec/versions/0.5.1/topics/itoken
library(tokenizers)
library(wordcloud)
library(topicmodels)
library(ldatuning)

#set random number seed for reproducibility
set.seed(123456)

#create train/test set for model building

d <- sort(sample(nrow(test), nrow(test)*.75))
#Create training sample
train <- test[d,]
validation <- test[-d,]


#Prune out useless information from free text: numbers, puncuation marks, special symbols.

train$Additional.Information <- gsub('[[:digit:]]+', '', train$Additional.Information)
train$Additional.Information <- gsub('[[:punct:]]+', '', train$Additional.Information)
train$Additional.Information <- gsub("\\s+", " ", train$Additional.Information)

#remove extremely common words that are not useful for identifying topics (choose based on wordcloud or other metrics.

stop_words = c("i", "me", "we", "to", "the", "it", "is", "a", "and", "in", "but", "this", "that",
               "on", "for", "of", "be", "agent", "he", "she", "him", "her", "his", "hers",
               "policy", "as", "are", "at", "am", "an", "or", "policies", "polici", "not",
               "have", "was", "do", "there", "has", "with", "want", "you", "can", "if", "whi", "how",
               "go", "from", "they", "insur", "then", "see", "tri", "get", "need", "show", "when",
               "will", "im", "i'm", "hi", "one", "quot", "out", "chang", "what", "would", "so", "say")


library(tm)

#Create corprus of train and validate. convert to lower case and trim word to stems
prep_fun <- tolower
tok_fun <- tokenize_word_stems

it_train <- itoken(train$Additional.Information,
                   preprocessor = prep_fun,
                   tokenizer = tok_fun,
                   ids = train$CUST_ID,
                   progressbar = TRUE)

it_validate <- itoken(validate$Additional.Information,
                      preprocessor = prep_fun,
                      tokenizer = tok_fun,
                      ids = train$CUST_ID,
                      progressbar = TRUE)

#create vobaulary of corpus using bi-grams
vocab <- create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 2L))
#vocab <- create_vocabulary(it_train, stopwords = stop_words)

#clean out to useful words, appear in at least 300 documents, but less than 60% of all documents (might need adjusted based on volume).
pruned_vocab <- prune_vocabulary(vocab,
                                 term_count_min = 300,
                                 doc_proportion_max = 0.6,

)

vectorizer <- vocab_vectorizer(pruned_vocab)

dtm_train <- create_dtm(it_train, vectorizer, type = 'dgCMatrix')

dtm_validate <- create_dtm(it_validate, vectorizer, type = 'dgCMatrix')

library(Matrix)

rowTotals <- apply(dtm_train, 1, sum)
dtm_train2 <- dtm_train[rowTotals > 0,]

rowTotals2 <- apply(dtm_validate, 1, sum)
dtm_validate2 <- dtm_validate[rowTotals2 > 0,]

#Wordcloud, review major terms to see what topics are common, or possible additions to stop words
#Words are color coded by use amount

tdm <- data.frame(words = pruned_vocab$vocab$terms, freq = pruned_vocab$vocab$doc_counts)

wordcloud(tdm$words, tdm$freq, max.words = 250, random.order = TRUE, colors = c("#343434","#a92c28","#155f9c","#71cf8d","#8f8c8a"))

#Path Word Cloud - Helps to show correlations of words and clusters of common terms

library(huge)

#hh=huge((tmpdata), lambda = NULL, nlambda = NULL, lambda.min.ratio = NULL, method = "glasso",
#        scr = NULL, scr.num = NULL, cov.output = TRUE, sym = "or", verbose = TRUE)



hh=huge((dtm_train2), lambda = 0.15, nlambda = NULL, lambda.min.ratio = NULL, method = "glasso",
        scr = NULL, scr.num = NULL, cov.output = TRUE, sym = "or", verbose = TRUE)

vmat = as.matrix(hh$path[[1]])
colnames(vmat) = colnames(dtm_train2)
tmp1 = graph_from_adjacency_matrix(vmat,mode='undirected')
col_SUMS2 <- colSums(dtm_train2)

plot(tmp1,vertex.label=names(dtm_train2),vertex.size=log(col_SUMS2),vertex.label.cex=.5)

#Tests to help identify appropriate number of topics for LDA Model. tests 2-20 topics by 2.

#  Topic Modelling
 #    result <- FindTopicsNumber(
 #      dtm_train2,
 #      topics = seq(from = 2, to = 20, by = 2),
 #      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
 #      method = "Gibbs",
 #      control = list(seed = 77),
 #      mc.cores = 3L,
 #      verbose = TRUE
 #    )
 #
 # FindTopicsNumber_plot(result)
#Choose Best Value from Above plot

k = 10 # number of topics

#Build LDA Model using number of topics. Gibbs Random Sample, 2000 iterations to burnin, then evaluate. might need adjusted based on volume to help converge.

Gibbs = LDA(dtm_train2, k = k, method = "Gibbs", control = list(seed = 123456, burnin = 2000, thin = 100, iter = 2000))
#Gibbs = CTM(dtm_train2, k= k, method = 'VEM', control = list(seed= 123456, verbose = TRUE, estimate.beta = TRUE, keep = TRUE))


tmptopics = topics(Gibbs)
table(topics(Gibbs))

#Provides 20 most important terms to each topic.

terms(Gibbs,20)
X <- as.data.frame(terms(Gibbs,20))
write.csv(X, 'Termsoutput.csv')

#Few Measures of fit for Model

perplexity(Gibbs)

DocumentProbabilities <- as.data.frame(Gibbs@gamma)

topicProbabilities <- as.data.frame(Gibbs@beta)

#lasso_matrix <- build_graph(Gibbs, 0.25, and = TRUE)
#lasso_matrix

topics(Gibbs)

#tmptopics

Correlations_output <- data.frame(Gibbs@documents, Gibbs@gamma)

Scored <- data.frame(Gibbs@documents, topics(Gibbs))
names(Scored)[names(Scored)=="Gibbs.documents"] <- "CUST_ID"

Scored_Data <- merge(x = test, y= Scored, by.x = "CUST_ID", by.y = "CUST_ID")


write.csv(Scored_Data, "Scored_Data_values.csv")
