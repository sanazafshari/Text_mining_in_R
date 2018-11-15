#Loading of the library
library(tm)
library(XML)
library(SnowballC)#stemming
library(NLP)
library(slam)
#Importation of the data
#Creation of corpus
doc1<- Corpus(DirSource(".../BigDM/Datatest"))

ovid <- system.file("texts","txt", package = "tm")
ovidCorpus<-Corpus(DirSource(ovid),readerControl = list(reader=readPlain))
inspect(ovidCorpus)

#creation of a corpus from a vector
docs<- c("This is a text.","This is another one.")
corpus1<- Corpus(VectorSource(docs))
#creation of a corpus from a repository containing web docs
reut21578<-system.file("texts","crude",package = "tm")
reuters<- Corpus(DirSource(reut21578),readerControl = list(reader=readReut21578XML))
##########
#backup of the corpus as a text in existing repository
writeCorpus(Corpus,path="repository")
####################
inspect(doc1)
class(doc1)
class(doc1[1])
summary(doc1)
#############################TEXT_MINING##################
########PREPROCESSING
#transformation of XML docs to text
reuters2<-tm_map(reuters,PlainTextDocument)
#convert to lower case
reuters3<- tm_map(reuters2, content_transformer(tolower))
writeLines(as.character(reuters3[[1]]))
#remove numbers
reuters4<-tm_map(reuters3,removeNumbers)
writeLines(as.character(reuters4[[1]]))
#remove punctuation
reuters5<-tm_map(reuters4,removePunctuation)
writeLines(as.character(reuters5[[1]]))
#elimination of stopwords
reuters6<-tm_map(reuters5,removeWords, stopwords("english"))
writeLines(as.character(reuters6[[1]]))
#list the stop words
length(stopwords("english"))
stopwords("english")
#remove own stopwords
#create a dictionary
mydic<- c("crude","oil")
#create a corpus base on mydic
Doc1RW<-tm_map(reuters5,removeWords,mydic)
Doc1RW2<-tm_map(reuters5,removeWords,c("prices"))
writeLines(as.character(Doc1RW2[[1]]))
#elimination of extra whitespace
reuters7<-tm_map(reuters6,stripWhitespace)
writeLines(as.character(reuters7[[1]]))
#stemming
reuters8<-tm_map(reuters7,stemDocument)
writeLines(as.character(reuters8[[1]]))
###########################MATRIX
dtm0<-DocumentTermMatrix(reuters2)
inspect(dtm0[5:10,740:743])
#creation of a dictionary
mydict<-c("prices","crude","oil")
#creation of a matrix from reuters8 that use the dictionary terms
dtm_mydict<- DocumentTermMatrix(reuters8,list(dictionary=c("prices","crude","oil")))
#elimination of sparse terms
dtm<-DocumentTermMatrix(reuters8)
inspect(removeSparseTerms(dtm0,0.4))
dtmeparse<-removeSparseTerms(dtm,0.05)
dtm2<-removeSparseTerms(dtm,0.7)
#write matrix to a file
write.table(as.matrix(dtm2),file = "/home/sanaz/Desktop/DataMining/BigDM/matdtm.dat")
#convert document term matrix to a simple matrix for writing to a csv
write.csv(as.matrix(dtm2),file = "/home/sanaz/Desktop/DataMining/BigDM/dtm2.csv")
###########################
##########################TEXT MINING
library(ggplot2)
library(wordcloud)
freq <- colSums(as.matrix(dtm2))
#index size
length(freq)
#list of most frequent terms by ordering the frequencies
ord<-order(freq)
#least frequent terms
freq[head(ord)]
#most frequent terms
freq[tail(ord)]
#number of terms that occurs between 1 and 15
head(table(freq),15)
#plotting word frequencies
freq<-sort(colSums(as.matrix(dtm2)), decreasing = TRUE)
head(freq,14)
wf<-data.frame(word=names(freq), freq=freq)
head(wf)
barplot(freq)
wordcloud(names(freq), freq, max.words = 14)
#indentify frequent items and associations
#search of a term that has at least x occurence
findFreqTerms(dtm2, 'oil',0.5)
