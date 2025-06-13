# Final project
# Matthew and Shaz

################################################################################
# Libraries 
library(smallstuff)
library(data.table)
library(readtext)
library(tm)                #Text mining
library(magrittr)          #The pipe
library(tidytext)          #For sentiments
library(wordcloud)
library(wordcloud2)
library(stringr)           #str_squish/str_trim
library(phm)               #Phrase mining and getPubMed
library(textdata)
################################################################################
# Internal Functions
################################################################################
################################################################################
# Save the environment
################################################################################
parSave=par(no.readonly = TRUE)
#par(parSave)
################################################################################

lapply(co[2],meta)
content(co[[2]])

# Creating The Corpus
co <- c("../data_final/pubmed-qualitylif-set.txt") %>% getPubMed %>% DFSource %>% VCorpus
length(co) # 100 documents
lapply(co,meta) %>% head # first 5 docs
lapply(co,content) %>% head # contents of the first 5

# A bit of information regarding the pubmed data
# This dataset compiles scholarly articles from PubMed that study or discuss quality of life 
# in various chronic diseases.



################################################################################
# Full Corpus Analysis: Word Frequency and Sentiment
################################################################################
# First lets try making a termdocumentmatrix 


# Set colors
myCol <- colors()


# Removing punctuation,stopwords,numbers, and only keeping words that appeared in atleast 2 documents 
tdmM <- TermDocumentMatrix(co, control = list(removePunctuation = TRUE,
                                              stopwords = T,
                                              removeNumbers = TRUE,
                                              bounds=list(global=c(2,Inf))))%>%as.matrix
sums=rowSums(tdmM)
# Set min.freq to 16 to get a better plot
set.seed(123L)
wordcloud(names(sums),sums,min.freq=16,colors=myCol,random.order=F, scale=c(4.5,.5))
title("All Words",cex.main=2, sub = 'min freq 16', cex.sub = 2)

# Now lets try to look for sentiments

#Data table for sums
(dt=data.table(word=names(sums),freq=sums))
#Store the nrc lexicon in a data table
nrc=as.data.table(get_sentiments("nrc"))
#Create data tables for certain sentiments
coPos=dt[nrc,on=.(word),nomatch=NULL][sentiment=='positive']
coNeg=dt[nrc,on=.(word),nomatch=NULL][sentiment=='negative']

par(mfrow=c(1,2))
#Primary word clouds
#All positive words
set.seed(123L)
wordcloud(coPos$word,coPos$freq,min.freq=3,colors=myCol,random.order=F,
          scale=c(1,.7))
title("Positive",cex.main=2)
#All negative words
set.seed(123L)
wordcloud(coNeg$word,coNeg$freq,min.freq=3,colors=myCol,random.order=F,
          scale=c(1,.7))
title("Negative",cex.main=2)
par(parSave)

wordcloud2(coPos, size=1, backgroundColor = "lightblue",shape = 'star') #wordlcoud 2 on positive
wordcloud2(coNeg,size=1,backgroundColor = "cornflowerblue", ellipticity=1) #wordcloud 2 on negative




################################################################################
# Phrase Mining on Full Corpus
################################################################################


# Checking for high frequency phrases in the whole corpus min.freq = 3 
(pd =phraseDoc(co, min.freq = 3)) # 263 principal phrases
freqPhrases(pd, 10L) # top 10 frequent phrases

# removing systematic review, and systematic review, meta-analysis and 95% CI
# since it’s not a topic, theme, or condition
(pd=removePhrases(pd,c("systematic review ","systematic review and meta-analysis", "95% ci"))) #260 principal phrases
(highfreq = freqPhrases(pd, 10L))


pdm <- as.matrix(pd)

sums=rowSums(pdm)
set.seed(123L)
#Primary Word cloud
wordcloud(names(sums),sums,min.freq=3,colors=myCol,random.order=F,scale=c(5,.3))
title("Word Frequency by phrase", cex.main = 2, sub = 'min frequency 3')


################################################################################
# Focused Analysis on Top 30 Most Informative Documents
################################################################################

(bd <- bestDocs(co, num = 30L))

lapply(bd,meta)
length(bd) # 30 Documents


# Removing punctuations, numbers, stopwords,  and only keep words that appeared in atleast 2 documents 
tdm=TermDocumentMatrix(bd, control=list(removePunctuation=T,
                                               removeNumbers=T,
                                               stopwords=T,
                                        bounds=list(global=c(2,Inf))
                                          )) %>% as.matrix
sums=rowSums(tdm)
set.seed(123L)
wordcloud(names(sums),sums,min.freq=7,scale=c(2,.5),colors=myCol,
          random.order=F)
title("words  in the top 30 documents",cex.main=2, sub = 'min frequency 7', cex.sub = 2)

################################################################################
# Sentiment Analysis Across Three Lexicons (NRC, Bing, Afinn)
################################################################################


(dt=data.table(word=names(sums),freq=sums))
#Storing the nrc lexicon in a data table
nrc=as.data.table(get_sentiments("nrc"))
#Create data tables for certain sentiments

coPos=dt[nrc,on=.(word),nomatch=NULL][sentiment=='positive']
coNeg=dt[nrc,on=.(word),nomatch=NULL][sentiment=='negative']


# Now lets check for sentiments  Using Bing

(bing=as.data.table(get_sentiments("bing")))

biPos=dt[bing,on=.(word),nomatch=NULL][sentiment=='positive']
biNeg=dt[bing,on=.(word),nomatch=NULL][sentiment=='negative']




# now for Afinn
(afin=as.data.table(get_sentiments("afinn")))
afPos = dt[afin,on=.(word),nomatch=NULL][value %in% c(1:5)]
afNeg = dt[afin,on=.(word),nomatch=NULL][value %in% c(-1:-5)]

# Comparing all the Positive for nrc,bing,afin

par(mfrow=c(1,3))
set.seed(123L)
wordcloud(coPos$word,coPos$freq,min.freq=1,colors=myCol,random.order=F,
          scale=c(3,.4))
title(sub = 'nrc', cex.sub = 2)
set.seed(123L)
wordcloud(biPos$word,biPos$freq,min.freq=1,colors=myCol,scale = c(3,.2),random.order=F)
title("Positive Words in the top 30", sub = 'bing',cex.main = 2, cex.sub = 2)
set.seed(123L)
wordcloud(afPos$word,afPos$freq,min.freq=1,colors=myCol,scale = c(4,.2),random.order=F)
title(sub = 'afin', cex.sub = 2)

par(parSave)

# For Negative words comparisons
par(mfrow=c(1,3))
set.seed(123L)
wordcloud(coNeg$word,coNeg$freq,min.freq=1,colors=myCol,random.order=F)
title(sub = 'nrc',cex.sub = 2)
set.seed(123L)
wordcloud(biNeg$word,biNeg$freq,min.freq=1,colors=myCol,random.order=F)
title("Negative Words in the top 30", sub = 'bing',cex.main = 2, cex.sub = 2)
set.seed(123L)
wordcloud(afNeg$word,afNeg$freq,min.freq=1,colors=myCol,scale = c(6,.6),random.order=F)
title(sub = 'afinn', cex.sub = 2)


par(parSave)


## Now looking at the negative words it looks like cancer is the most negative word found by all
# 3 lexicons, lets try to find the documents that has  the most cancer word

(idx=which(tdm["cancer",]!=0)) 
tdm["cancer",idx,drop=F]
# document that has the most cancer word has pmid 39879873 and 40035891
inspect(bd[["39879873"]])
inspect(bd[["40035891"]])
# Titles for the 2 documents with most cancer word
lapply(bd,meta,'title')[16] %>% unlist %>% str_squish
lapply(bd,meta,'title')[21] %>% unlist %>% str_squish


################################################################################
# Phrase Mining in Top 30 Documents
################################################################################


#Frequent phrases for document 1 (the most informative)
(PD<- phraseDoc(bd[1],min.freq=3)) # 2 principal phrases
getPhrases(PD,1L)


#Top 10 high frequency phrases in the top 30 best documents
(pd=phraseDoc(bd,min.freq=3)) # 105 principal phrases

(highFreq<- (freqPhrases(pd,10L))) 
# Quality of life was used 63 times
# Considering removing systematic review, systematic review and meta-analaysis, 95% CI
# since it’s not a topic, theme, or condition

(pd=removePhrases(pd,c("systematic review ","systematic review and meta-analysis", "95% ci"))) #260 principal phrases
(highfreq = freqPhrases(pd, 10L))

pdm <- as.matrix(pd)

sums=rowSums(pdm)
set.seed(123L)
#Primary Word cloud
wordcloud(names(sums),sums,min.freq=3,colors=myCol,random.order=F,scale=c(6,.3))
title("Word Frequency by phrase in the top 30", cex.main = 2)


################################################################################
# Deep Dive into the Most Informative Document 
################################################################################

# Most informative document
lapply(bd[1],meta) # #Old index is 45, high freq. phrases 6

# check its contents
content(bd[[1]]) %>% str_squish %>% cat

content(bd[[1]]) <- content(bd[[1]]) %>% str_squish

#Making a termdocumentmatrix for the best document
tmmb<-TermDocumentMatrix(bd[[1]],control=list(removePunctuation=T,
                                              stopwords=T,
                                              removeNumbers=T,
                                              wordLengths=c(3,Inf))) %>% as.matrix
sums=rowSums(tmmb)
set.seed(123L)
wordcloud(names(sums),sums,min.freq=9,scale=c(2,.5),colors=myCol,
          random.order=F)
title("Word Frequency", cex.main = 2,sub = 'min freq 9', cex.sub = 2)

# health-related quality of life (HRQoL)
# This document centers on pediatric quality of life and stood out for its strong 
# phrase density and clinical relevance.

















