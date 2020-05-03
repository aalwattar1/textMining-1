# Final Term Project
#Name: Ahmed Al Wattar
#Date: 04/26/2020
#___________________________

rm(list = ls())
cat("\014")

#--Call Librairies ----
library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(ggplot2) # Plot word frequencies.
library(scales) # Common data analysis activities.
library(pdftools)
library(RColorBrewer)
library(class)
library(wordcloud)
library(tibble)
#library(vcd) didn't use it for the mosiac plot


# Path and Data Setup----
###################################################

# Where are the tm example files stored----
system.file("texts", package = "tm")

#Set your working directory----
setwd("C:/Users/alwat/OneDrive/Documents/R/win-library/3.6/tm")

#Topics are 'rec.sport.baseball' as positive and 'talk.politics.mideast' as negative


#A and B) Creat five Corpus----
# rec.sport.baseball and talk.politics.mideast
# load the rec.sport.baseball training files
rec.train.path <-
  system.file("texts",
              "20Newsgroups",
              "20news-bydate-train",
              "rec.sport.baseball",
              package = "tm")
rec.train.files <- DirSource(rec.train.path)

#check the lenght for the rec train files

length(rec.train.files)

# Build a corpus of 300 Documents/files for training 
rec.train.corpus <-
  VCorpus(URISource(rec.train.files$filelist[1:300]), #Use Vcorpus to prevent droping docs
          readerControl = list(reader = readPlain))
# Load the rec.sport.baseball test files
rec.test.path <-
  system.file("texts",
              "20Newsgroups",
              "20news-bydate-test",
              "rec.sport.baseball",
              package = "tm")
rec.test.files <- DirSource(rec.test.path)
# Build a corpus of 300 Documents/files for testing
rec.test.corpus <-
  VCorpus(URISource(rec.test.files$filelist[1:300]),
          readerControl = list(reader = readPlain))

# inspect the meta data for thrid item in the rec train corpus
rec.test.corpus[[3]]$meta

#Now we do the same above but for the other topic talk.politics.mideast

# load the talk.politics.mideast training files
talk.train.path <-
  system.file("texts",
              "20Newsgroups",
              "20news-bydate-train",
              "talk.politics.mideast",
              package = "tm")
talk.train.files <- DirSource(talk.train.path)
# Build a corpus of 300 Documents/files for training
talk.train.corpus <-
  VCorpus(URISource(talk.train.files$filelist[1:300]),
          readerControl = list(reader = readPlain))
#load the talk.politics.mideast test files
talk.test.path <-
  system.file("texts",
              "20Newsgroups",
              "20news-bydate-test",
              "talk.politics.mideast",
              package = "tm")
talk.test.files <- DirSource(talk.test.path)
length(talk.test.files)
# Build a corpus of 300 Documents/files for testing
talk.test.corpus <-
  VCorpus(URISource(talk.test.files$filelist[1:300]),
          readerControl = list(reader = readPlain))

# Merge all four collections of documents into a single corpus total 1200 docs
# Becareful with the sequence for the corpora -train-train-test-test
Mega.corpus <-
  c(rec.train.corpus,
    talk.train.corpus,
    rec.test.corpus,
    talk.test.corpus)

inspect(head(Mega.corpus, n = 2))

#length for the full corpus, should be 1200, first 600 docs for training and second 600 for testing

length(Mega.corpus)


#---End of creating the four corpora sports and politics corpora---####################



#--C) Will extraxt the Subject lines from each documents
# and do the preprocessing to end with build a DocumentTermMatrix DTM file

# Build three functions to extract the Subject line, From Line, and Organization

# Subject Line Function 
Subject.Line<- function(my.corpus){
  Subj.List <-list()
  for (i in 1:length(my.corpus)){#Loop through full corpus
    temp <- unlist(my.corpus[[i]][])# Flatten the list 
    for (j in 1:length(temp)){# Internal loop to add the subjects to a list
      Textline <- temp[j]
      if (grepl("Subject: ", Textline)){
        Subj.List[i] <-gsub("Subject: ", "", Textline)
        my.corpus[[i]]$meta$Subject <- gsub("Subject: ", "", Textline)
      }
    }
  }
  return(Subj.List)
}

#Subject.Line()


# From Line Function

From.Line<- function(my.corpus){
  From.List <-list()
  for (i in 1:length(my.corpus)){#Loop through full corpus
    temp <- unlist(my.corpus[[i]][])# Flatten the corpus 
    for (j in 1:length(temp)){# Loop throught the flatten corpus 
      Textline <- temp[j]
      if (grepl("From: ", Textline)){# if we see From means this is the from line
        From.List[i] <-gsub("From: ", "", Textline) #grab the text after word from
      }
    }
  }
  return(From.List)
}

#From.Line()

# Org. Line Function 

Org.Line<- function(my.corpus){
  Org.List <-list()
  for (i in 1:length(my.corpus)){#Loop through full corpus
    temp <- unlist(my.corpus[[i]][])# Flatten the corpus 
    for (j in 1:length(temp)){# Loop throught the flatten corpus 
      Textline <- temp[j]
      if (grepl("Organization: ", Textline)){# if we see From means this is the from line
        Org.List[i] <-gsub("Organization: ", "", Textline) #grab the text after word from
      }
    }
  }
  return(Org.List)
}

#Org.Line()


# Calling the Function by passing a corpus 

Subject1.Train <- Subject.Line(rec.train.corpus)
Subject1.Test <- Subject.Line(rec.test.corpus)
Subject2.Train <- Subject.Line(talk.train.corpus)
Subject2.Test <- Subject.Line(talk.test.corpus)

# Pass each copus to the user function

From1.Train <- From.Line(rec.train.corpus) # BaseBall Rec Train
From1.Test <- From.Line(rec.test.corpus) #BaseBall Rec Test
From2.Train <- From.Line(talk.train.corpus) #Politics Mideast Train
From2.Test <- From.Line(talk.test.corpus) # Politics Mideast Test

# Pass each corpus to the user function

Org1.Train <- Org.Line(rec.train.corpus)
Org1.Test <- Org.Line(rec.test.corpus)
Org2.Train <- Org.Line(talk.train.corpus)
Org2.Test <- Org.Line(talk.test.corpus)

#--Count Unique Email Addresses in 1200 Document

Count.df <- data.frame('Corpus-Type'=c('Baseball News Corpus-Train', 'Baseball News Corpus-Test',
                                       'Mideast Politics-Train', 'Mideast Politics-Test'),
                       'Total Unique Email' = c(length(unique(From1.Train)), length(unique(From1.Test)), 
                                                length(unique(From2.Train)), length(unique(From2.Test))))
Count.df
#or present a tibble forthe unique emails
Count.tibble <- as_tibble(Count.df)
Count.tibble

#---End Of C---#####
#rm(Mega.Subject.Corpus)

#--D) Combine the subject Corpora using VectorSource function
# Becareful with the sequence of the corpora 
Mega.Subject.Corpus<- VCorpus(VectorSource(c(Subject1.Train,Subject1.Test,Subject2.Train,Subject2.Test))) 

inspect(head(Mega.Subject.Corpus, n =3))

#--E) Mega Subject Corpus pre-processing, and creating DTM

# Remove punctuations
Mega.Subject.Corpus.temp <- tm_map(Mega.Subject.Corpus, removePunctuation)

# Transform to lowercase 
Mega.Subject.Corpus.temp <- tm_map(Mega.Subject.Corpus.temp, content_transformer(tolower))

# Remove numbers 
Mega.Subject.Corpus.temp <- tm_map(Mega.Subject.Corpus.temp, removeNumbers)

# Remove some words  because I didn't convert
blank.chr <-
  content_transformer(function(x, pattern)
    gsub(pattern, " ", x))
Mega.Subject.Corpus.temp <- tm_map(Mega.Subject.Corpus.temp, blank.chr, "An|Re: ")
Mega.Subject.Corpus.temp <- tm_map(Mega.Subject.Corpus.temp, removeWords, c("th", "re", "al ", "apr", "wasgo","ii",
                                                                            "bb", "hdp", "mlb"))
Mega.Subject.Corpus.temp <- tm_map(Mega.Subject.Corpus.temp, removePunctuation)

# Remove english stopwords
Mega.Subject.Corpus.temp <- tm_map(Mega.Subject.Corpus.temp, removeWords, stopwords("english"))

# Stemming the Words to the root
#Mega.Subject.Corpus.temp <- tm_map(Mega.Subject.Corpus.temp, stemDocument)

# Test and Compare Before and After Preprocessing

Mega.Subject.Corpus[[721]]$content[1] #500
Mega.Subject.Corpus.temp[[721]]$content[1] #500

Mega.Subject.Corpus[[500]]$content[1] #500
Mega.Subject.Corpus.temp[[500]]$content[1] #500


# Creating the Subject  DTM ----------

DTM.Subject <-DocumentTermMatrix(Mega.Subject.Corpus.temp,
                                 control=list(wordLengths =c(2,Inf),bounds=list(global = c(5,Inf))))



Freq.Subject <-colSums(as.matrix(DTM.Subject))
head(Freq.Subject)
length(Freq.Subject)

Subject.Ord <- order(Freq.Subject, decreasing = TRUE)

#length(words.subject$name)


words.subject <-
  data.frame(row.names = c(1:10),
             name = names(Freq.Subject[head(Subject.Ord, n=10)]),
             value = Freq.Subject[head(Subject.Ord, n=10)])
words.subject.plot <-   data.frame(row.names = c(1:216),
                                   name = names(Freq.Subject[head(Subject.Ord, n=216)]),
                                   value = Freq.Subject[head(Subject.Ord, n=216)])

#-- Word cloud for the frequent words
wordcloud(words = words.subject.plot$name, freq =words.subject.plot$value, min.freq = 3,
          max.words=75, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, 'Dark2'))

#---End of Part E----#######################################

#--F) Classify and Display results using knn()

Subject.train.doc <- DTM.Subject[c(c(1:300), c(601:900)),]
Subject.test.doc <- DTM.Subject[c(301:600, 901:1200),]
# Verify if the copora dim make sense
dim(DTM.Subject)
dim(Subject.train.doc)
dim(Subject.test.doc)

# Create tags that we will use in knn

tags <- factor(c(rep("Sports",300), rep("Politics",300)))



set.seed(100)
# kNN function is applied over the first test and train set
prob.test <- knn(Subject.train.doc, Subject.test.doc, tags, k = 3, prob = TRUE)
head(prob.test)


# Classification result
# which document number
a <- 1:length(prob.test)
# Predicted Classification
b <- levels(prob.test)[prob.test]
# Probility of right classification
c <- attributes(prob.test)$prob
# Predictions vs. Correct Tags (Whether correct prediction or not)
d <- prob.test == tags
# make a data frame of the a, b, c, d variables built above
# display the data frame
result <- data.frame(Doc=a, Predict=b, Prob=c, Correct=d)
head(result,200)


#True Classification result
true.classifications <- sum(c)/ length(tags)
sprintf("The true classification is %.4f", true.classifications)


# Confusion Matrix
confusion.matrix <- table(matrix(tags,ncol=1), matrix(prob.test,ncol=1))

colnames(confusion.matrix) <- c("Sports(1)", "Politics(0)")
rownames(confusion.matrix) <- c("Sports(1)", "Politics(0)")

confusion.matrix

## Rec.Sport as positive and Politics-Mideast as negative
TP <- confusion.matrix[[1,1]]
TN <- confusion.matrix[[2,2]]
FP <- confusion.matrix[[1,2]]
FN <- confusion.matrix[[2,1]]
sprintf("TP = %d, TN = %d, FP = %d, FN = %d",TP, TN, FP, FN)

# Calculating the precision, recall and f-score
precision <- TP /sum(confusion.matrix[1,]) 
recall <- TP /sum(confusion.matrix[,1])
f.score <- 2*precision*recall/ (precision+recall)
sprintf("The precision is %.4f", precision)
sprintf("The recall is %.4f", recall)
sprintf("The f-score is %.4f", f.score)

# Plot the confusion matrix





mosaicplot(confusion.matrix, main = "Confusion Matrix-Sports Documents vs Politics",
           sub = "",
           xlab = "Actuals",
           ylab = "Classified As",
           las = 2,
           dir = c("h", "v"),
           color = c(("#d8b365"),("#5ab4ac")),
           border = "#1b9e77")

