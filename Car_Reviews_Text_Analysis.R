## Amber Garner
## Last Updated 28 November 2016

## set the working directory

## import the data
cars07<-read.csv(file="2007.csv", head=TRUE, sep=",")
cars08<-read.csv(file="2008.csv", head=TRUE, sep=",")
cars09<-read.csv(file="2009.csv", head=TRUE, sep=",")

## combine the files
cars<-rbind(cars07,cars08,cars09)

## Some pre-processing
cars$num_reviews<-NULL
cars$year<-NULL

## 2007 Toyota Camry and 2007 Honda Accord 
x<-cars[cars$docid=="2007_toyota_camry",]
z<-cars[cars$docid=="2007_honda_accord",]
dist(rbind(x[-1],z[-1]))

## 2008 Toyota Camry and 2008 Honda Accord 
x<-cars[cars$docid=="2008_toyota_camry",]
z<-cars[cars$docid=="2008_honda_accord",]
dist(rbind(x[-1],z[-1]))

## 2009 Toyota Camry and 2009 Honda Accord 
x<-cars[cars$docid=="2009_toyota_camry",]
z<-cars[cars$docid=="2009_honda_accord",]
dist(rbind(x[-1],z[-1]))

## 2007 Honda Accord and 2009 Honda Accord 
x<-cars[cars$docid=="2007_honda_accord",]
z<-cars[cars$docid=="2009_honda_accord",]
dist(rbind(x[-1],z[-1]))

## 2007 Toyota Camry and 2009 Toyota Camry 
x<-cars[cars$docid=="2007_toyota_camry",]
z<-cars[cars$docid=="2009_toyota_camry",]
dist(rbind(x[-1],z[-1]))

## 2007 Toyota Corolla and 2007 Honda Civic 
x<-cars[cars$docid=="2007_toyota_corolla",]
z<-cars[cars$docid=="2007_honda_civic",]
dist(rbind(x[-1],z[-1]))

## 2008 Toyota Corolla and 2008 Honda Civic 
x<-cars[cars$docid=="2008_toyota_corolla",]
z<-cars[cars$docid=="2008_honda_civic",]
dist(rbind(x[-1],z[-1]))

## 2009 Toyota Corolla and 2009 Honda Civic 
x<-cars[cars$docid=="2009_toyota_corolla",]
z<-cars[cars$docid=="2009_honda_civic",]
dist(rbind(x[-1],z[-1]))

## 2007 Honda Civic and 2009 Honda Civic 
x<-cars[cars$docid=="2007_honda_civic",]
z<-cars[cars$docid=="2009_honda_civic",]
dist(rbind(x[-1],z[-1]))

## 2007 Toyota Corolla and 2009 Toyota Corolla 
x<-cars[cars$docid=="2007_toyota_corolla",]
z<-cars[cars$docid=="2009_toyota_corolla",]
dist(rbind(x[-1],z[-1]))

## 2007 Toyota Sienna and 2007 Honda Odyssey 
x<-cars[cars$docid=="2007_toyota_sienna",]
z<-cars[cars$docid=="2007_honda_odyssey",]
dist(rbind(x[-1],z[-1]))

## 2008 Toyota Sienna and 2008 Honda Odyssey 
x<-cars[cars$docid=="2008_toyota_sienna",]
z<-cars[cars$docid=="2008_honda_odyssey",]
dist(rbind(x[-1],z[-1]))

## 2009 Toyota Sienna and 2009 Honda Odyssey 
x<-cars[cars$docid=="2009_toyota_sienna",]
z<-cars[cars$docid=="2009_honda_odyssey",]
dist(rbind(x[-1],z[-1]))

## 2007 Honda Odyssey and 2009 Honda Odyssey 
x<-cars[cars$docid=="2007_honda_odyssey",]
z<-cars[cars$docid=="2009_honda_odyssey",]
dist(rbind(x[-1],z[-1]))

## 2007 Toyota Sienna and 2009 Toyota Sienna 
x<-cars[cars$docid=="2007_toyota_sienna",]
z<-cars[cars$docid=="2009_toyota_sienna",]
dist(rbind(x[-1],z[-1]))

## 2009 Honda Civic and 2009 Honda Pilot
x<-cars[cars$docid=="2009_honda_civic",]
z<-cars[cars$docid=="2009_honda_pilot",]
dist(rbind(x[-1],z[-1]))

## 2009 Toyota Corolla and 2009 Honda Pilot
x<-cars[cars$docid=="2009_toyota_corolla",]
z<-cars[cars$docid=="2009_honda_pilot",]
dist(rbind(x[-1],z[-1]))


#Additional Text Mining

## Install Packages
install.packages("tm")
install.packages("SnowballC")
library(SnowballC)
library(tm)
library(stats)
install.packages("wordcloud")
library(wordcloud)

## 2007 Honda Accord
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2007/honda/Accord")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaAccord07<-as.matrix(head(frequency,30))
View(hondaAccord07)

write.csv(hondaAccord07, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaAccord07.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))
png("wordcloud_honda_accord07.png")

## 2008 Honda Accord
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2008/honda/Accord")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaAccord08<-as.matrix(head(frequency,30))
View(hondaAccord08)

write.csv(hondaAccord08, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaAccord08.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))
png("wordcloud.png")

## 2009 Honda Accord
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2009/honda/Accord")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much", "one", "back"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaAccord09<-as.matrix(head(frequency,30))
View(hondaAccord09)

write.csv(hondaAccord09, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaAccord09.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))
png("wordcloud3.png")

## 2007 Honda Civic
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2007/honda/Civic")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaCivic07<-as.matrix(head(frequency,30))
View(hondaCivic07)

write.csv(hondaCivic07, file="C:/Users/Courtney/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaCivic07.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

## 2008 Honda Civic
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2008/honda/Civic")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaCivic08<-as.matrix(head(frequency,30))
View(hondaCivic08)

write.csv(hondaCivic08, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaCivic08.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

## 2009 Honda Civic
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2009/honda/Civic")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaCivic09<-as.matrix(head(frequency,30))
View(hondaCivic09)

write.csv(hondaCivic09, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaCivic09.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

## 2007 Honda CR-V
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2007/honda/CR-V")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaCRV07<-as.matrix(head(frequency,30))
View(hondaCRV07)

write.csv(hondaCRV07, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaCRV07.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

## 2008 Honda CR-V
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2008/honda/CR-V")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaCRV08<-as.matrix(head(frequency,30))
View(hondaCRV08)

write.csv(hondaCRV08, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaCRV08.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

## 2009 Honda CR-V
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2009/honda/CR-V")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaCRV09<-as.matrix(head(frequency,30))
View(hondaCRV09)

write.csv(hondaCRV09, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaCRV09.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

## 2007 Honda Element
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2007/honda/Element")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaElement07<-as.matrix(head(frequency,30))
View(hondaElement07)

write.csv(hondaElement07, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaElement07.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


## 2008 Honda Element
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2008/honda/Element")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaElement08<-as.matrix(head(frequency,30))
View(hondaElement08)

write.csv(hondaElement08, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaElement08.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


## 2007 Honda Fit
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2007/honda/Fit")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaFit07<-as.matrix(head(frequency,30))
View(hondaFit07)

write.csv(hondaFit07, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaFit07.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


## 2008 Honda Fit
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2008/honda/Fit")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaFit08<-as.matrix(head(frequency,30))
View(hondaFit08)

write.csv(hondaFit08, file="C:/Users/Courtney/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaFit08.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


## 2009 Honda Fit
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2009/honda/Fit")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaFit09<-as.matrix(head(frequency,30))
View(hondaFit09)

write.csv(hondaFit09, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaFit09.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))



## 2007 Honda Odyssey
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2007/honda/Odyssey")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaOdyssey07<-as.matrix(head(frequency,30))
View(hondaOdyssey07)

write.csv(hondaOdyssey07, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaOdyssey07.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


## 2008 Honda Odyssey
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2008/honda/Odyssey")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi", "now"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaOdyssey08<-as.matrix(head(frequency,30))
View(hondaOdyssey08)

write.csv(hondaOdyssey08, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaOdyssey08.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


## 2009 Honda Odyssey
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2009/honda/Odyssey")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi", "now"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaOdyssey09<-as.matrix(head(frequency,30))
View(hondaOdyssey09)

write.csv(hondaOdyssey09, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/hondaOdyssey09.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


## 2007 Honda Pilot
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2007/honda/Pilot")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi", "now"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaPilot07<-as.matrix(head(frequency,30))
View(hondaPilot07)

write.csv(hondaPilot07, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/Pilot07.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


## 2008 Honda Pilot
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2008/honda/Pilot")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi", "now"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaPilot08<-as.matrix(head(frequency,30))
View(hondaPilot08)

write.csv(hondaPilot08, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/Pilot08.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))



## 2009 Honda Pilot
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2009/honda/Pilot")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi", "now"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
txt<-gsub("storag", "storage", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaPilot09<-as.matrix(head(frequency,30))
View(hondaPilot09)

write.csv(hondaPilot09, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/Pilot09.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


## 2007 Honda Ridgeline
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2007/honda/Ridgeline")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi", "now"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
txt<-gsub("storag", "storage", txt)
txt<-gsub("ridgelin", "ridgeline", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaRidgeline07<-as.matrix(head(frequency,30))
View(hondaRidgeline07)

write.csv(hondaRidgeline07, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/Ridgeline07.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))



## 2008 Honda Ridgeline
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2008/honda/Ridgeline")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi", "now"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
txt<-gsub("storag", "storage", txt)
txt<-gsub("ridgelin", "ridgeline", txt)
txt<-gsub("tailgat", "tailgate", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaRidgeline08<-as.matrix(head(frequency,30))
View(hondaRidgeline08)

write.csv(hondaRidgeline08, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/Ridgeline08.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


## 2009 Honda Ridgeline
source<-DirSource("C:/Users/Courtney/Desktop/DATA 630/Module7/Dataset/cars/data/2009/honda/Ridgeline")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi", "now", "chevi", "anoth", "dont", "drove"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
txt<-gsub("storag", "storage", txt)
txt<-gsub("ridgelin", "ridgeline", txt)
txt<-gsub("tailgat", "tailgate", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondaRidgeline09<-as.matrix(head(frequency,30))
View(hondaRidgeline09)

write.csv(hondaRidgeline09, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/Ridgeline09.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))




## 2007 Honda s2000
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2007/honda/s2000")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi", "now", "chevi", "anoth", "dont", "drove",
                                "ive", "even", "mani", "sinc", "alway", "around"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
txt<-gsub("storag", "storage", txt)
txt<-gsub("ridgelin", "ridgeline", txt)
txt<-gsub("tailgat", "tailgate", txt)
txt<-gsub("beauti", "beautiful", txt)
txt<-gsub("experi","experience", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondas200007<-as.matrix(head(frequency,30))
View(hondas200007)

write.csv(hondas200007, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/s200007.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))




## 2008 Honda s2000
source<-DirSource("C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/2008/honda/s2000")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
txt<-Corpus(VectorSource(YourCorpus))
## Format properly and remove stopwords
txt<-tm_map(txt, content_transformer(tolower))
txt<-tm_map(txt, removePunctuation)
txt<-tm_map(txt, removeNumbers)
txt<-tm_map(txt, stripWhitespace)
txt<-tm_map(txt, stemDocument)
txt<-tm_map(txt, removeWords, stopwords("english"))
txt<-tm_map(txt, removeWords, c("doc", "dated", "veri", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                "got", "lot", "far", "two", "easi", "now", "chevi", "anoth", "dont", "drove",
                                "ive", "even", "mani", "sinc", "alway", "around", "character0", "although", "bit", "doe",
                                "everi", "howev"))
txt<-gsub("mileag", "mileage", txt)
txt<-gsub("handl", "handle", txt)
txt<-gsub("favorit", "favorite", txt)
txt<-gsub("reliabl", "reliable", txt)
txt<-gsub("qualiti", "quality", txt)
txt<-gsub("engin", "engine", txt)
txt<-gsub("nois", "noise", txt)
txt<-gsub("purchas", "purchase", txt)
txt<-gsub("averag", "average", txt)
txt<-gsub("featur", "feature", txt)
txt<-gsub("vehicl", "vehicle", txt)
txt<-gsub("roomi", "roomie", txt)
txt<-gsub("economi", "economic", txt)
txt<-gsub("storag", "storage", txt)
txt<-gsub("ridgelin", "ridgeline", txt)
txt<-gsub("tailgat", "tailgate", txt)
txt<-gsub("beauti", "beautiful", txt)
txt<-gsub("experi","experience", txt)
txt<-gsub("chang", "change", txt)
txt<-gsub("character0", "character", txt)
txt<-gsub("fantast", "fantastic", txt)
## Create a Document Term Matrix
txt2<-Corpus(VectorSource(txt))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
## Find the top words
frequency<-(colSums(dtm2))
frequency<-(sort(frequency, decreasing=TRUE))
hondas200008<-as.matrix(head(frequency,30))
View(hondas200008)

write.csv(hondas200008, file="C:/Users/Amber/Desktop/DATA 630/Module7/Dataset/cars/data/results/s200008.csv")
## Create a Word Cloud
wordcloud(txt, scale=c(8,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

                             ####Toyota Word Cloud Script###
## Set the Working Directory
setwd("C:/Users/Amber/Desktop/Data Mining/Group Assignment")
## Install Packages
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
library(SnowballC)
library(tm)
library(stats)
library(wordcloud)


## 2007 Toyota Avalon
source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/avalon")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
## Format properly and remove stopwords
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("roomie", "roomy", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)

# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaAvalon07 <- as.matrix(head(frequency,30))
#write a new csv
write.csv(toyotaAvalon07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_avalon.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

##  2008 Toyota Avalon

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/avalon")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaAvalon08<-as.matrix(head(frequency,30))
write.csv(toyotaAvalon08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_avalon.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

#  2009 Toyota Avalon

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Avalon")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("passeng", "passenger", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaAvalon09<-as.matrix(head(frequency,30))
write.csv(toyotaAvalon09, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_avalon.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

#  2007 Toyota Camry

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/camry")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("transmiss", "transmission", YourCorpus)
YourCorpus<-gsub("hesit", "hesitate", YourCorpus)
YourCorpus<-gsub("acceler", "accelerate", YourCorpus)
YourCorpus<-gsub("economi", "economi", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("camri", "camry", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaCamry07<-as.matrix(head(frequency,30))

write.csv(toyotaCamry07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_camry.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

#  2008 Toyota Camry
source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Camry")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("camri", "camry", YourCorpus)
YourCorpus<-gsub("transmiss", "transmission", YourCorpus)
YourCorpus<-gsub("hesit", "hesitate", YourCorpus)
YourCorpus<-gsub("acceler", "accelerate", YourCorpus)
YourCorpus<-gsub("economi", "economi", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaCamry08<-as.matrix(head(frequency,30))
write.csv(toyotaCamry08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_camry.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


# 2009 Toyota Camry

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Camry")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("transmiss", "transmission", YourCorpus)
YourCorpus<-gsub("hesit", "hesitate", YourCorpus)
YourCorpus<-gsub("acceler", "accelerate", YourCorpus)
YourCorpus<-gsub("economi", "economi", YourCorpus)
YourCorpus<-gsub("camri", "camry", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaCamry08<-as.matrix(head(frequency,30))
write.csv(toyotaCamry08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_camry.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

# 2007 Toyota Corolla



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Corolla")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("economi", "economi", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaCorolla07<-as.matrix(head(frequency,30))
write.csv(toyotaCorolla07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_corolla.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


# 2008 Toyota Corolla



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Corolla")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "the", "and","veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("economi", "economy", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaCorolla08<-as.matrix(head(frequency,30))
write.csv(toyotaCorolla08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_corolla.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(4,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2009 Toyota Corolla

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Corolla")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("economi", "economy", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("citi", "city", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaCorolla09<-as.matrix(head(frequency,30))
write.csv(toyotaCorolla09, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_corolla.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(4,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

#  2007 FJ Cruiser



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/fj cruiser")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
fjCruiser07<-as.matrix(head(frequency,30))
write.csv(fjCruiser07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_fj_cruiser.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2008 FJ Cruiser

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/fj cruiser")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("capabl", "capable", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
fjCruiser08<-as.matrix(head(frequency,30))
write.csv(fjCruiser08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_fj_cruiser.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#   2007 Toyota Four Runner

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/4runner")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaFourRunner07<-as.matrix(head(frequency,30))
write.csv(toyotaFourRunner07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_4runner.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#   2008 Toyota Four Runner

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/4runner")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota", "get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaFourRunner08<-as.matrix(head(frequency,30))
write.csv(toyotaFourRunner08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_Four_Runner.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))



#  2007 Toyota Highlander

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Highlander")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated","datedate", "character0","veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("character0", "engine", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("texti", "text", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaHighlander07<-as.matrix(head(frequency,30))
write.csv(toyotaHighlander07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_highlander.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(2,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2008 Toyota Highlander


source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Highlander")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaHighlander08<-as.matrix(head(frequency,30))
write.csv(toyotaHighlander08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_highlander.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(2,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


# 2009 Toyota Highlander


source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Highlander")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("plenti", "plenty", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaHighlander09<-as.matrix(head(frequency,30))
write.csv(toyotaHighlander09, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_highlander.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))



#   2007 Toyota Highlander Hybrid

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Highlander")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "character0","veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaHighlanderHybrid07<-as.matrix(head(frequency,30))
write.csv(toyotaHighlanderHybrid07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_highlander_hybrid.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(2,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2008 Toyota Higlander Hybrid


source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Highlander Hybrid")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaHighlanderHybrid08<-as.matrix(head(frequency,30))
write.csv(toyotaHighlanderHybrid08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_highlander_hybrid.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


##  2007 Toyota Matrix



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Matrix")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("citi", "city", YourCorpus)
YourCorpus<-gsub("economi", "economy", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaMatrix07<-as.matrix(head(frequency,30))
write.csv(toyotaMatrix07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_matrix.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))



##  2008 Toyota Matrix



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Matrix")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri","becaus", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaMatrix08<-as.matrix(head(frequency,30))
write.csv(toyotaMatrix08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_matrix.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2009 Toyota Matrix


source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Matrix")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaMatrix09<-as.matrix(head(frequency,30))
write.csv(toyotaMatrix09, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_matrix.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

#  2007 Toyota Prius



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Prius")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaPrius07<-as.matrix(head(frequency,30))
write.csv(toyotaPrius07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_prius.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))



#   2008 Toyota Prius

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Prius")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri","ive", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaPrius08<-as.matrix(head(frequency,30))
write.csv(toyotaPrius08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_prius.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2009 Toyota Prius


source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Prius")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri","ive", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("plenti", "plenty", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaPrius09<-as.matrix(head(frequency,30))
write.csv(toyotaPrius09, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_prius.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))



#  2007 Toyota Rav4



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Rav4")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("storag", "storage", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaRav407<-as.matrix(head(frequency,30))
write.csv(toyotaRav407, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_rav4.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(2,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

#  2008 Toyota Rav 4


source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Rav4")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaRav408<-as.matrix(head(frequency,30))
write.csv(toyotaRav408, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_rav4.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

#   2009 Toyota Rav 4

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Rav4")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaRav409<-as.matrix(head(frequency,30))
write.csv(toyotaRav409, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_rav4.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))





#  2007 Toyota Sequoia



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Sequoia")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated","character0", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("consol","console", YourCorpus)
YourCorpus<-gsub("warranti", "warranty", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("warranti", "warranty", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaSequoia07<-as.matrix(head(frequency,30))
write.csv(toyotaSequoia07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_sequoia.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(2,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))





#  2008 Toyota Sequoia



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Sequoia")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("famili", "family", YourCorpus)
YourCorpus<-gsub("navig", "navigation", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaSequoia08<-as.matrix(head(frequency,30))
write.csv(toyotaSequoia08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_sequoia.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))






#  2007 Toyota Sienna



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Sienna")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("plenti", "plenty", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaSienna07<-as.matrix(head(frequency,30))
write.csv(toyotaSienna07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_sienna.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(2,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2008 Toyota Sienna


source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Sienna")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaSienna08<-as.matrix(head(frequency,30))
write.csv(toyotaSienna08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_sienna.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(2,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2009 Toyota Sienna

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Sienna")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaSienna09<-as.matrix(head(frequency,30))
write.csv(toyotaSienna09, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_sienna.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2007 Toyota Tacoma



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Tacoma")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaTacoma07<-as.matrix(head(frequency,30))
write.csv(toyotaTacoma07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_tacoma.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(4,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

#  2008 Toyota Tacoma

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Tacoma")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaTacoma08<-as.matrix(head(frequency,30))
write.csv(toyotaTacoma08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_tacoma.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#   2009 Toyota Tacoma

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Tacoma")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("transmiss", "transmission", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("safeti", "safety", YourCorpus)
YourCorpus<-gsub("storag", "storage", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaTacoma09<-as.matrix(head(frequency,30))
write.csv(toyotaTacoma09, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_tacoma.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2007 Toyota Tundra



source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Tundra")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("transmiss", "transmission", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaTundra07<-as.matrix(head(frequency,30))
write.csv(toyotaTundra07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_tundra.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(4,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2008 Toyota Tundra


source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Tundra")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaTundra08<-as.matrix(head(frequency,30))
write.csv(toyotaTundra08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_tundra.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(4,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))



#   2009 Toyota Venza

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Venza")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("camri", "camry", YourCorpus)
YourCorpus<-gsub("plenti", "plenty", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaVenza09<-as.matrix(head(frequency,30))
write.csv(toyotaVenza09, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_venza.csv")

# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#   2007 Toyota Yaris

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2007/toyota/Yaris")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaYaris07<-as.matrix(head(frequency,30))
write.csv(toyotaYaris07, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2007_toyota_yaris.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(4,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2008 Toyota Yaris

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2008/toyota/Yaris")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("yari", "yaris", YourCorpus)
YourCorpus<-gsub("citi", "city", YourCorpus)
YourCorpus<-gsub("economi", "economy", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaYaris08<-as.matrix(head(frequency,30))
write.csv(toyotaYaris08, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2008_toyota_yaris.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))


#  2009 Toyota Yaris

source <- DirSource("C:/Users/Amber/Desktop/Data Mining/Group Assignment/Dataset/cars/data/2009/toyota/Yaris")
YourCorpus<-Corpus(source,readerControl=list(reader=readPlain))
YourCorpus<-tm_map(YourCorpus, content_transformer(tolower))
YourCorpus<-tm_map(YourCorpus, removePunctuation)
YourCorpus<-tm_map(YourCorpus, removeNumbers)
YourCorpus<-tm_map(YourCorpus, stripWhitespace)
YourCorpus<-tm_map(YourCorpus, stemDocument)
YourCorpus<-tm_map(YourCorpus, removeWords, stopwords("english"))
YourCorpus<-tm_map(YourCorpus, removeWords, c("doc", "dated", "veri", "toyota","get", "texti", "feel", "text", "onli", "just", "well", "much",
                                              "one", "back", "also", "littl", "can","use", "will", "nice", "realli",
                                              "got", "lot", "far", "two", "easi"))
YourCorpus<-gsub("mileag", "mileage", YourCorpus)
YourCorpus<-gsub("handl", "handle", YourCorpus)
YourCorpus<-gsub("favorit", "favorite", YourCorpus)
YourCorpus<-gsub("reliabl", "reliable", YourCorpus)
YourCorpus<-gsub("qualiti", "quality", YourCorpus)
YourCorpus<-gsub("engin", "engine", YourCorpus)
YourCorpus<-gsub("nois", "noise", YourCorpus)
YourCorpus<-gsub("purchas", "purchase", YourCorpus)
YourCorpus<-gsub("averag", "average", YourCorpus)
YourCorpus<-gsub("featur", "feature", YourCorpus)
YourCorpus<-gsub("vehicl", "vehicle", YourCorpus)
YourCorpus<-gsub("roomi", "roomie", YourCorpus)
YourCorpus<-gsub("drive.1", "drive", YourCorpus)
YourCorpus<-gsub("mpg.7", "mpg", YourCorpus)
YourCorpus<-gsub("great.1", "great", YourCorpus)
YourCorpus<-gsub("great.3", "great", YourCorpus)
YourCorpus<-gsub("seat.1", "seat", YourCorpus)
YourCorpus<-gsub("seat.2", "seat", YourCorpus)
YourCorpus<-gsub("mpg.1", "mpg", YourCorpus)
YourCorpus<-gsub("drive.3", "drive", YourCorpus)
YourCorpus<-gsub("mileage.1", "mileage", YourCorpus)
YourCorpus<-gsub("like.1", "like", YourCorpus)
YourCorpus<-gsub("gas.1", "gas", YourCorpus)
YourCorpus<-gsub("mile.1", "mile", YourCorpus)
YourCorpus<-gsub("insid", "inside", YourCorpus)
YourCorpus<-gsub("storag", "storage", YourCorpus)
YourCorpus<-gsub("yari", "yaris", YourCorpus)
# Create a Document Term Matrix
txt2<-Corpus(VectorSource(YourCorpus))
txt2<-tm_map(txt2, removePunctuation)
dtm<-DocumentTermMatrix(txt2)
dtm<-removeSparseTerms(dtm, 0.94)
dtm2<-as.matrix(dtm)
toyotaYaris09<-as.matrix(head(frequency,30))
write.csv(toyotaYaris09, file="C:/Users/Amber/Desktop/Data Mining/Group Assignment/Output/2009_toyota_yaris.csv")
# create word cloud
wordcloud(YourCorpus, scale=c(3,0.5), max.words=30, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8,"Paired"))

                 ###Additional Word Cloud Script#####

#Install required packages
library(stringr) 
library(rvest)

# Create data file direct source
source<-DirSource("/Users/elizabethmatthews/Desktop/UMUC/Data Mining/AssignmentGroup")

# Create corpus
source <- Corpus(VectorSource(Toyota2))

for(j in seq(Toyota2))
{
  source[[j]] <- gsub(":", " ", source[[j]])
  source[[j]] <- gsub("\n", " ", source[[j]])
  source[[j]] <- gsub("-", " ", source[[j]])
}

source <- tm_map(source, removePunctuation)
source <- tm_map(source, removeNumbers)
source <- tm_map(source, tolower)
source <- tm_map(source, removeWords, stopwords("english"))

source <- tm_map(source, stemDocument)
source <- tm_map(source, stripWhitespace)
source <- tm_map(source, PlainTextDocument)

dtm <- DocumentTermMatrix(source)

termFreq <- colSums(as.matrix(dtm))
head(termFreq)

tf <- data.frame(term = names(termFreq), freq = termFreq)
tf <- tf[order(-tf[,2]),]
head(tf)

source <- tm_map(source, stripWhitespace)
source <- tm_map(source, PlainTextDocument)
dtm1 <- DocumentTermMatrix(source)
termFreq1 <- colSums(as.matrix(dtm1))
tf1 <- data.frame(term = names(termFreq1), freq = termFreq1)
tf1 <- tf1[order(-tf1[,2]),]
head(tf1, 100)

m = as.matrix(dtm1)
wf <- sort(rowSums(m),decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)
#Plot and save wordcloud image
png('Toyota.png', width=1280,height=800,res=300)
wordcloud(source, scale=c(5,0.5), max.words=Inf, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))
dev.off()

# Created by Amber Garner
# 11/29/2016
# Create bar plot of makes with highest and lowest overall rating in each year

install.packages("ggplot2")
library(ggplot2)

# set working directory
setwd("C:/Users/Amber/Desktop/Data Mining/Group Assignment")

#******2007**********

# load 2007 data
a <- read.csv("2007.csv", header = T, sep = ",")

# subset to only include toyota and camry models relevant to analysis
a <-subset(a, subset= docid %in% c("2007_toyota_4runner", "2007_toyota_camry","2007_toyota_corolla","2007_toyota_prius", "2007_toyota_rav4", "2007_toyota_sienna", 
                                   "2007_toyota_sienna", "2007_toyota_tacoma", "2007_toyota_yaris","2007_honda_accord","2007_honda_civic", "2007_honda_fit","2007_honda_cr-v", "2007_honda_odyssey", "2007_honda_pilot", "2007_honda_ridgeline"))
# Look at data
View(a)

# Create new columns for model and make
a$model <- c("Accord", "Civic", "Cr-v", "Fit", "Odyssey", "Pilot", "Ridgeline", "4Runner", "Camry","Corolla", "Prius", "Rav4", "Sienna", "Tacoma", "Yaris" )
a$make <- c("honda", "honda","honda","honda","honda","honda","honda", "toyota","toyota","toyota","toyota","toyota","toyota","toyota","toyota")

# Verify new columns
View(a[,c(1,13, 14)])

# Create graph for 2007 with y axis model and x axis overall rating and color as make
ggplot(a, aes(x=reorder(model, overall_rating), y=overall_rating, fill=make)) + geom_bar(,stat="identity") + coord_flip()+ theme(text = element_text(size=8), legend.position="none") + ggtitle("2007 Overall Rating by Make") + xlab("Model") + ylab("Overall Rating")

#******2008*********

# load 2008 data
b <- read.csv("2008.csv", header = T, sep = ",")

# subset to only include toyota and camry models relevant to analysis
b <- subset(b, subset= docid %in% c("2008_honda_accord","2008_honda_civic","2008_honda_cr-v","2008_honda_fit","2008_honda_odyssey","2008_honda_pilot","2008_honda_ridgeline","2008_toyota_4runner","2008_toyota_camry", "2008_toyota_corolla", "2008_toyota_prius", "2008_toyota_rav4", "2008_toyota_sienna", 
                                    "2008_toyota_sienna", "2008_toyota_tacoma", "2008_toyota_yaris"))
# Look at data
View(b)

# Create new columns for model and make
b$model <- c("Accord", "Civic", "Cr-v", "Fit", "Odyssey", "Pilot", "Ridgeline", "4Runner", "Camry","Corolla", "Prius", "Rav4", "Sienna", "Tacoma", "Yaris" )

b$make <- c("honda", "honda","honda","honda","honda","honda","honda", "toyota","toyota","toyota","toyota","toyota","toyota","toyota","toyota")

# verify new columns
View(b[,c(1,13, 14)])

# Create graph for 2008 with y axis model and x axis overall rating and color as make
ggplot(b, aes(x=reorder(model, overall_rating), y=overall_rating, fill=make)) + geom_bar(,stat="identity") + coord_flip()+ theme(text = element_text(size=8), legend.position="none") + ggtitle("2008 Overall Rating by Model") + xlab("Model") + ylab("Overall Rating")


#*******2009********

# load 2007 data
c <- read.csv("2009.csv", header = T, sep = ",")

# subset to only include toyota and camry models relevant to analysis
c <- subset(c, subset= docid %in% c("2009_honda_accord","2009_honda_civic","2009_honda_cr-v","2009_honda_fit","2009_honda_odyssey","2009_honda_pilot","2009_honda_ridgeline","2009_toyota_4runner","2009_toyota_camry", "2009_toyota_corolla", "2009_toyota_prius", "2009_toyota_rav4", "2009_toyota_sienna", 
                                    "2009_toyota_sienna", "2009_toyota_tacoma", "2009_toyota_yaris"))

# Create new columns for model and make
c$model <- c("Accord", "Civic", "Cr-v", "Fit", "Odyssey", "Pilot", "Ridgeline","Camry","Corolla", "Prius", "Rav4", "Sienna", "Tacoma", "Yaris" )

c$make <- c("honda", "honda","honda","honda","honda","honda","honda", "toyota","toyota","toyota","toyota","toyota","toyota","toyota")

# verify new columns
View(c[,c(1,13, 14)])

# Create graph for 2008 with y axis model and x axis overall rating and color as make
ggplot(c, aes(x=reorder(model, overall_rating), y=overall_rating, fill=make)) + geom_bar(,stat="identity") + coord_flip()+ theme(text = element_text(size=8), legend.position="none") + ggtitle("2009 Overall Rating by Model") + xlab("Model") + ylab("Overall Rating")

# find correlation between variables

# install needed packages
install.packages("corrplot")
library(corrplot)

# combine all years into one variable 
d <- rbind(a,b,c)

# subset to only include numeric variables
d.cor <- d[,c(3:12)]

# create correlation matrix
M<-cor(d.cor)

#plot correlation matrix with circles
corrplot(M, method = "circle", diag=F, cl.cex = .7, tl.col = "black")

# subset to only include only honda accords
d.accord <-subset(d, subset= docid %in% c("2007_honda_accord", "2008_honda_accord", "2009_honda_accord"))
# print results needed
d.accord[,2:12]
# save as csv file for later use
write.csv(d.accord[,2:12], "accord ratings")

# find median for all vehicles
median(d$RELIABILITY)
median(d$FUN)
median(d$BUILD)


 ###Plotting & Charts of Complaints Data ###

install.packages("fpc")
library(fpc)
install.packages("igraph")
library(igraph)
library(cluster)
library(ggplot2)
install.packages("calibrate")
library(calibrate)
library (discretization)
library (arules)

#Create Table
table(ToyotaHondaComplaints$NHTSAComplaints)


#plot NHTSA and Carcomplaints.com complaints
plot(ToyotaHondaComplaints$NHTSAComplaints, ToyotaHondaComplaints$CarComplaints, cex=2.5, pch=20, main="NHTSA and CarComplaints.com Complaints", ylab="CarComplaints.com Complaints", xlab="NHTSA Complaints", col="red")
#create plot of type of complaint count by car model
plot(table(ToyotaHondaComplaints$Model), main="Car Complaint Categories by Model", xlab="Car Model",ylim=c(0,100), ylab="Number of Complaint Categories Reported", type="h", col="dark green")
#Barplot of carcomplaints.com complaints by year
barplot(ToyotaHondaComplaints$CarComplaints, ToyotaHondaComplaints$year)
#Create histogram
hist(ToyotaHondaComplaints$CarComplaints)
#Create Pie of Complaints by year
pie(table(ToyotaHondaComplaints$Year))


### KMeans Text Analysis ##

#Assign libraries
library("RSiteCatalyst")
library("RTextTools") #Loads many packages useful for text mining

#Open Source

source<-DirSource("/Users/elizabethmatthews/Desktop/UMUC/Data Mining/AssignmentGroup")

#Create Corpus
source <- Corpus(VectorSource(Toyota2))


#pre-processing

#Create document-term matrix
dtm <- create_matrix(Toyota2, stemWords=TRUE, removeStopwords=TRUE, minWordLength=1,removePunctuation= TRUE)
#Omit NA values
dtm <- na.omit(dtm)
#Find frequent terms
findFreqTerms(dtm, lowfreq=10)
#create KMeans
kmeansToyota<- kmeans(dtm,10)
kmeansToyota 
kmeansToyota$tot.withinss<-kmeans$tot.withinss
kmeansToyota$tot.withinss

                    ###Overall Mean Data ####


                                           ##### Data References ####

# CarComplaints. (n.d.). 2007 Toyota Camry: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Camry/2007/ 
# CarComplaints. (n.d.). 2008 Toyota Camry: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Camry/2008/ 
# CarComplaints. (n.d.). 2009 Toyota Camry: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Camry/2009/  
# CarComplaints. (n.d.). 2007 Toyota Corolla: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Corolla/2008/ 
# CarComplaints. (n.d.). 2009 Toyota Corolla: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Corolla/2008/ 
# CarComplaints. (n.d.). 2007 Toyota Rav4: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Camry/2007/ 
#  CarComplaints. (n.d.). 2008 Toyota Rav4: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Rav4y/2008/ 
#  CarComplaints. (n.d.). 2009 Toyota Rav4: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Rav4/2009/ 
#  CarComplaints. (n.d.). 2007 Toyota 4Runner: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/4Runner/2007/ 
#  CarComplaints. (n.d.). 2008 Toyota 4Runner: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/4Runner/2008/ 
#  CarComplaints. (n.d.). 2009 Toyota 4Runner: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/4Runner/2009/ 
#  CarComplaints. (n.d.). 2007 Toyota Sienna: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Sienna/2007/ 
#  CarComplaints. (n.d.). 2008 Toyota Sienna: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Sienna/2008/ 
#  CarComplaints. (n.d.). 2009 Toyota Sienna: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Sienna/2009/ 
#  CarComplaints. (n.d.). 2007 Toyota Tacoma: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Tacoma/2007/ 
#  CarComplaints. (n.d.). 2008 Toyota Tacoma: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Tacoma/2008/ 
#  CarComplaints. (n.d.). 2009 Toyota Tacoma: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Tacoma/2009/ 
#  CarComplaints. (n.d.). 2007 Toyota Yaris: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Yaris/2007/
#  CarComplaints. (n.d.). 2008 Toyota Yaris: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Yaris/2008/
#  CarComplaints. (n.d.). 2009 Toyota Yaris: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Yaris/2009/
#  CarComplaints. (n.d.). 2007 Toyota Prius: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Prius/2007/
#  CarComplaints. (n.d.). 2008 Toyota Prius: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Prius/2008/
#  CarComplaints. (n.d.). 2009 Toyota Prius: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Toyota/Prius/2009/
#  CarComplaints. (n.d.). 2007 Honda Accord: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Accord/2007/
#  CarComplaints. (n.d.). 2008 Honda Accord: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Accord/2008/
#  CarComplaints. (n.d.). 2009 Honda Accord: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Accord/2009/
#  CarComplaints. (n.d.). 2007 Honda Civic: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Civic/2007/
#  CarComplaints. (n.d.). 2008 Honda Civic: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Civic/2008/
#  CarComplaints. (n.d.). 2009 Honda Civic: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Civic/2009/
#  CarComplaints. (n.d.). 2007 Honda Pilot: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Pilot/2007/
#  CarComplaints. (n.d.). 2008 Honda Pilot: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Pilot/2008/
#  CarComplaints. (n.d.). 2009 Honda Pilot: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Pilot/2009/
#  CarComplaints. (n.d.). 2007 Honda CR-V: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/CRV/2007/
#  CarComplaints. (n.d.). 2008 Honda CR-V: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/CRV/2008/
#  CarComplaints. (n.d.). 2009 Honda CR-V: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/CRV/2009/
#  CarComplaints. (n.d.). 2007 Honda Odyssey: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Odyssey/2007/
#  CarComplaints. (n.d.). 2008 Honda Odyssey: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Odyssey/2008/
#  CarComplaints. (n.d.). 2009 Honda Odyssey: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Odyssey/2009/
#  CarComplaints. (n.d.). 2007 Honda Ridgeline: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Ridgeline/2007/
#  CarComplaints. (n.d.). 2008 Honda Ridgeline: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Ridgeline/2008/
#  CarComplaints. (n.d.). 2009 Honda Ridgeline: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Ridgeline/2009/
# CarComplaints. (n.d.). 2007 Honda Fit: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Fit/2007/
#  CarComplaints. (n.d.). 2007 Honda Fit: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Fit/2007/
# CarComplaints. (n.d.). 2008 Honda Fit: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Fit/2008/
#  CarComplaints. (n.d.). 2009 Honda Fit: Problems & Complaints. Retrieved November 12, 2016, from http://www.carcomplaints.com/Honda/Fit/2009/
#  Cargurus. (2007). 2007 Toyota Sienna Overview. Retrieved November 30, 2016, from https://www.cargurus.com/Cars/2007-Toyota-Sienna-Overview-c5264
# Cargurus. (n.d.). 2007 Toyota Camry. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Toyota-Camry-Reviews-c5224 
# Cargurus. (n.d.). 2008 Toyota Camry. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Toyota-Camry-Reviews-c8354
# Cargurus. (n.d.). 2009 Toyota Camry. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Toyota-Camry-Reviews-c15445
# Cargurus. (n.d.). 2007 Toyota Corolla. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Toyota-Corolla-Reviews-c6750
# Cargurus. (n.d.). 2008 Toyota Corolla. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Toyota-Corolla-Reviews-c8300 
# Cargurus. (n.d.). 2009 Toyota Corolla. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Toyota-Corolla-Reviews-c9224
# Cargurus. (n.d.). 2007 Toyota Rav4. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Toyota-RAV4-Reviews-c5253
# Cargurus. (n.d.). 2008 Toyota Rav4. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Toyota-RAV4-Reviews-c9103
# Cargurus. (n.d.). 2009 Toyota Rav4. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Toyota-RAV4-Reviews-c21294
# Cargurus. (n.d.). 2007 Toyota 4Runner. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Toyota-4Runner-Reviews-c6254
# Cargurus. (n.d.). 2008 Toyota 4Runner. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Toyota-4Runner-Reviews-c9151
# Cargurus. (n.d.). 2009 Toyota 4Runner. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Toyota-4Runner-Overview-c21308 
# Cargurus. (n.d.). 2007 Toyota Sienna. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Toyota-Sienna-Overview-c5264
# Cargurus. (n.d.). 2008 Toyota Sienna. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Toyota-Sienna-Overview-c9152
# Cargurus. (n.d.). 2009 Toyota Sienna. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Toyota-Sienna-Overview-c21285
# Cargurus. (n.d.). 2007 Toyota Tacoma. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Toyota-Tacoma-Overview-c8003
# Cargurus. (n.d.). 2008 Toyota Tacoma. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Toyota-Tacoma-Overview-c9153
# Cargurus. (n.d.). 2009 Toyota Tacoma. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Toyota-Tacoma-Overview-c21326
# Cargurus. (n.d.). 2007 Toyota Yaris. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Toyota-Yaris-Overview-c5266
# Cargurus. (n.d.). 2008 Toyota Yaris. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Toyota-Yaris-Overview-c8712
# Cargurus. (n.d.). 2009 Toyota Yaris. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Toyota-Yaris-Overview-c21321
# Cargurus. (n.d.). 2007 Toyota Prius. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Toyota-Prius-Overview-c5251
# Cargurus. (n.d.). 2008 Toyota Prius. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Toyota-Prius-Overview-c9102
# Cargurus. (n.d.). 2009 Toyota Prius. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Toyota-Prius-Overview-c21279
# Cargurus. (n.d.). 2007 Honda Accord. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Honda-Accord-Overview-c3835 
# Cargurus. (n.d.). 2008 Honda Accord. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Honda-Accord-Overview-c8378
# Cargurus. (n.d.). 2009 Honda Accord. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Honda-Accord-Overview-c21310 
# Cargurus. (n.d.). 2007 Honda Civic. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Honda-Civic-Overview-c3834 
# Cargurus. (n.d.). 2008 Honda Civic. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Honda-Civic-Overview-c10418 
# Cargurus. (n.d.). 2009 Honda Civic. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Honda-Civic-Overview-c21312
# Cargurus. (n.d.). 2007 Honda Pilot. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Honda-Pilot-Overview-c3844
# Cargurus. (n.d.). 2008 Honda Pilot. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Honda-Pilot-Overview-c8316
# Cargurus. (n.d.). 2009 Honda Pilot. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Honda-Pilot-Overview-c21061
# Cargurus. (n.d.). 2007 Honda CR-V. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Honda-CR-V-Overview-c3838
# Cargurus. (n.d.). 2008 Honda CR-V. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Honda-CR-V-Overview-c9123
# Cargurus. (n.d.). 2009 Honda CR-V. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Honda-CR-V-Overview-c21311
# Cargurus. (n.d.). 2007 Honda Odyssey. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Honda-Odyssey-Overview-c3864
# Cargurus. (n.d.). 2008 Honda Odyssey. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Honda-Odyssey-Overview-c9342
# Cargurus. (n.d.). 2009 Honda Odyssey. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Honda-Odyssey-Overview-c21300
# Cargurus. (n.d.). 2007 Honda Ridgeline. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2007-Honda-Ridgeline-Overview-c3845
# Cargurus. (n.d.). 2008 Honda Ridgeline. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Honda-Ridgeline-Overview-c8275
# Cargurus. (n.d.). 2009 Honda Ridgeline. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Honda-Ridgeline-Overview-c21315
# Cargurus. (n.d.). 2007 Honda Fit. Retrieved November 10, 2016, from  https://www.cargurus.com/Cars/2007-Honda-Fit-Overview-c3846
# Cargurus. (n.d.). 2008 Honda Fit. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2008-Honda-Fit-Overview-c9122
# Cargurus. (n.d.). 2009 Honda Fit. Retrieved November 10, 2016, from https://www.cargurus.com/Cars/2009-Honda-Fit-Overview-c21314

