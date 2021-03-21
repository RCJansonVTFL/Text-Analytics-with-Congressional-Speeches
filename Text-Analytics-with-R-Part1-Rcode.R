
library(knitr)
install.packages("text2vec")
install.packages("sentimentr")
install.packages("tidytext")
install.packages("textdata")
library(readtext)
library(quanteda)
library(text2vec)
library(sentimentr)
library(tidytext)
library(textdata)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Read in the speaker map and text
speaker_list <- c('109_SpeakerMap.txt','110_SpeakerMap.txt','111_SpeakerMap.txt',
                  '112_SpeakerMap.txt','113_SpeakerMap.txt')
speeches_list <- c('speeches_109.txt','speeches_110.txt','speeches_111.txt',
                   'speeches_112.txt','speeches_113.txt')
description_list <- c('descr_109.txt','descr_110.txt','descr_111.txt','descr_112.txt',
                      'descr_113.txt')

#reset tables when necessary
cr_speaker <- data.frame()
cr_speeches <- data.frame()
cr_descr <- data.frame()
cr_merged_pre <- data.frame()
cr_merged <- data.frame()

cr_speaker <- read.table("~/R/Legal Analytics/Project Files/108_SpeakerMap.txt", header = TRUE, sep = "|")
cr_speeches <- read.table("~/R/Legal Analytics/Project Files/speeches_108.txt", header = TRUE, sep = "|",
                          na.strings=".", quote="", fill = TRUE)
cr_descr <- read.table("~/R/Legal Analytics/Project Files/descr_108.txt", header = TRUE, sep = "|")

#Create tables from Stanford files

for (i in speaker_list)
{
  cr_speaker <- bind_rows(cr_speaker,read.table(paste('~/R/Legal Analytics/Project Files/',i,sep = ""), header = TRUE, sep = "|"))
}

for (i in speeches_list)
{
  cr_speeches <- rbind(cr_speeches,read.table(paste('~/R/Legal Analytics/Project Files/',i,sep = ""),header = TRUE, sep = "|",
                                              na.strings=".", quote="", fill = TRUE))
}

for (i in description_list)
{
  cr_descr <- merge(cr_descr,read.table(paste('~/R/Legal Analytics/Project Files/',i,sep = ""), header = TRUE, sep = "|"),all = TRUE)
}

# Merge on speech_id
cr_merged_pre <- merge(cr_speaker,cr_speeches, by = "speech_id")
cr_merged_fin <- merge(cr_merged_pre,cr_descr, by = "speech_id")

#filter out party = P - this is a small party that isn't necessary for our analysis
main_df <- filter(cr_merged_fin, party != 'P')

#for this analysis we only need the file_id and text elements
cr_txt <- main_df[, c("file", "speech")]



View(head(cr_txt))


#creating a corpus
cr_corpus <- corpus(cr_txt,text = 'speech')


cr_toks <- tokens(cr_corpus)


cr_dfm <- dfm(cr_corpus,
              remove = stopwords('en'),
              remove_punct = TRUE,
              remove_numbers = TRUE,
              remove_symbols = TRUE)


View(head(cr_dfm,10,20))


#Generates a frequency table showing the appearances per document of our keyword
immigrant_kws <- c('immigrant','migrant','immig*','dreamer', 'visa','aliens')
dict <- dictionary(list(immigrant = immigrant_kws))
# Specify glob because one dictionary item is in glob format
dfm_keywords <- dfm_lookup(cr_dfm, dict, valuetype = "glob",case_insensitive = TRUE) 

View(head(dfm_keywords,10,20))



#Kwic & Sentiment Analysis


#create kwic window for immig* - this is a globular text object which will match for any word that starts with 'immig'. Capturing words like immigrant or immigration. The window is the # of words surrounding our keyword.
immigrant <- kwic(cr_corpus, pattern = "immig*", 
                  window = 50, valuetype = "glob")
#Turn kwic window into dataframe
immigrant <- as.data.frame(immigrant)
#create pre-post column
immigrant$pre_post <- paste(immigrant$pre, immigrant$post)

View(head(immigrant))

#create senitment measure
kwic_sentimentr <- sentiment_by(immigrant$pre_post)
#view the average sentiment score
summary(kwic_sentimentr$ave_sentiment)
#Check out the table
View(head(kwic_sentimentr))



kwic_sentimentr <- cbind(immigrant$docname, kwic_sentimentr)
View(head(kwic_sentimentr))

qplot(kwic_sentimentr$ave_sentiment,geom="histogram",binwidth=0.1,main="Immigrant Sentiment Histogram")

