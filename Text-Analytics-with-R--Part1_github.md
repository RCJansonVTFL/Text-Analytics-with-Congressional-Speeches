Congressional Text Analytics - Part 1
================
Ryan Janson
3/19/2021

In this document I will show how to perform the following: - Tokenize
your text data - Create a document frequency matrix - Create
keywords-in-context window using kwic function - Use kwic window to
perform sentiment analysis

``` r
library(knitr)
```

Installing and loading packages

``` r
library(readtext)
library(quanteda)
```

    ## Package version: 2.1.2

    ## Parallel computing: 2 of 40 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following object is masked from 'package:utils':
    ## 
    ##     View

``` r
library(text2vec)
library(sentimentr)
library(tidytext)
library(textdata)
library(readxl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.5     ✓ stringr 1.4.0
    ## ✓ tidyr   1.1.2     ✓ forcats 0.5.1
    ## ✓ readr   1.4.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
```

After downloading the text files from the Stanford site, we will create
some text documents for our analysis. These include dataframes, document
frequency matrices(DFM), and corpus objects.

``` r
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
```

View the table we have built thus far

``` r
kable(head(cr_txt))
```

| file         | speech                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|:-------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 01072003.txt | Mr. Clerk. for 4 years we have been blessed to have an individual of fairness. honesty. and common sense lead us without regard to rank or party. During even the most difficult of times. this common man with an uncommon conviction to do what is right has risen to the task and served as the Speaker for the whole House of Representatives. Therefore. Mr. Clerk. as chairman of the House Republican Conference. I am directed by the unanimous vote of that conference. and am very honored to present for election to the Office of the Speaker of the House of Representatives of the 108th Congress of the United States of America. the name of the Honorable J. DENNIS HASTERT. a representativeelect from the State of Illinois.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| 01072003.txt | Mr. Clerk. as chairman of the Democratic Caucus. I am directed by the unanimous vote of that caucus to present for election to the Office of the Speaker of the House of Representatives for the 108th Congress an incredibly talented Member of the Democratic Caucus and. for the first time in history. the name of a woman. the name of the Honorable NANCY PELOSI. a representativeelect from the State of California.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| 01072003.txt | First. congratulations to each and every Member of this House on your swearingin to the 108th Congress which is about to occur. A special congratulations and welcome to the freshmen to the Capitol and certainly to their families and friends. Let us all welcome our freshmen Members. Let me also thank my Democratic colleagues. I am humbled by the honor they have bestowed upon me to become the House Democratic leader. I know that I speak for all of us when I express profound gratitude to our esteemed colleague. the gentleman from Missouri . We thank the gentleman for his unwavering service to this institution and to our country. It is a great honor to follow in his footsteps. And to my family. my dear husband. Paul. our five children. and our five grandchildren. and to my DAlesandro family. I thank them very much for the love. support. encouragement. and joy that they have given me. Because of you. and the people of San Francisco. whom I am honored to serve. I had the unprecedented privilege today to have my name placed in nomination as the first woman ever to do so in the history of the House of Representatives. I am grateful to my colleagues for the confidence and proud of my party for breaking down another barrier and leading America closer to the ideal of equality that is both our heritage and our hope. We serve in the peoples House. and today. I want to pay tribute to the American people. It is their greatness. their fairmindedness. their commitment to family. their willingness to hope and dream that sustain our country. I especially wish to acknowledge the men and women in uniform whose courage keeps our country free and safe and makes it possible for us to strive for peace on Earth and goodwill toward mankind. For more than 214 years. the American people have issued a most awesome challenge to those of us in Congress. Debate. the American people tell us when they send us here. debate the great issues of our Nation. Decide matters of war and peace. Fashion laws and policies that will make our economy sound. our institutions fair. our society just. our environment protected. our people educated and healthy. our religions and beliefs free from constraint. and our homeland secure from terror. Debate policies. the American people tell us. which will ensure peace and justice throughout the world. comfort the afflicted. give voice to the oppressed. and make the future brighter for our children. Today I speak as the leader of the minority in a closely divided House of Representatives. We are on different sides of the aisle. but we have shared oath and a greater obligation to serve our country together. both to find common ground wherever we can and to stand our ground wherever we must to be true to the people we represent. My colleagues. I commit to all of you and to the American people that our party will always stand for the principles in which we believe. for I believe those principles represent the mainstream beliefs of our Nation: fairness. opportunity. patriotism. community. equal rights and a strong America. safe and prosperous at home. and committed abroad to a more secure and just world. free from the fear of terrorism. So in that spirit. I ask the majority in this House and the administration to join us in a new spirit to get our economy moving again in a way that helps working families. I ask that you join us in creating jobs and providing access to quality health care for Americas families. including a prescription drug coverage for our seniors. I ask that. after having passed the Leave No Child Behind Act. we act now to pledge to put our children first and fully fund their education. Finally and fundamentally. on the great and fateful issues we have all faced as Americans. especially since September 11. let me pledge for my party our absolute commitment to our national security. to winning the battle against terrorism and countering the threat of weapons of mass destruction. At times. we will have to debate on how best to provide for the common defense. That debate is not only right and necessary. it is at the heart of our democracy. But let there be no doubt. in our commitment to the strength and safety of America. there are no Democrats. there are no Republicans. Together. as Americans. we must and will prevail. We have great and grave issues to decide. as fateful as any faced by any of the 107 Congresses before us. So let us reach across party lines as we stand for principle. and let this be our own test. to advance and defend what is best for America. Now it is my privilege to present the Speaker of the House with my hardiest congratulations. Mr. Speaker. I hope in the next Congress our roles will be reversed. and you will have this wonderful privilege of presenting the gavel. In introducing our Speaker. let me first pay tribute to his skill. his decency and his integrity. We all hold the title of “honorable” by virtue of the office we hold. DENNIS HASTERT holds the title of “honorable” by virtue of his character. He is a man of honor. It is my privilege. colleagues. to present the Speaker of the House for the 108th Congress. the gentleman from Illinois . |
| 01072003.txt | then administered the oath of office to Mr. HASTERT of Illinois. as follows: Do you solemnly swear that you will support and defend the Constitution of the United States against all enemies. foreign and domestic. that you will bear true faith and allegiance to the same: that you take this obligation freely. without any mental reservation or purpose of evasion. and that you will well and faithfully discharge the duties of the office upon which you are about to enter. So help you God.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| 01072003.txt | Congratulations.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| 01072003.txt | Mr. Speaker. as chairman of the Republican Conference. I am directed by that conference to notify the House officially that the Republican Members have selected as their majority leader the gentleman from Texas. the Honorable TOM DELAY.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |

Next we create a corpus using the speech text as our corpus element.
Keep in mind these next few steps may take a long time to process. The
code is working through a lot of text data.

``` r
#creating a corpus
cr_corpus <- corpus(cr_txt,text = 'speech')
```

This step tokenizes the speeches in the corpus.

``` r
cr_toks <- tokens(cr_corpus)
```

Now we create a document frequency matrix using the dfm command. This
will display how often words are used throughout the corpus. We also
remove punctuation, numbers, symbols, and stopwords.

``` r
cr_dfm <- dfm(cr_corpus,
              remove = stopwords('en'),
              remove_punct = TRUE,
              remove_numbers = TRUE,
              remove_symbols = TRUE)
```

View the DFM

``` r
kable(head(cr_dfm,10,10))
```

    ## Warning: 'as.data.frame.dfm' is deprecated.
    ## Use 'convert(x, to = "data.frame")' instead.
    ## See help("Deprecated")

| doc\_id |  mr | clerk | years | blessed | individual | fairness | honesty | common | sense | lead |
|:--------|----:|------:|------:|--------:|-----------:|---------:|--------:|-------:|------:|-----:|
| text1   |   2 |     2 |     1 |       1 |          1 |        1 |       1 |      2 |     1 |    1 |
| text2   |   1 |     1 |     0 |       0 |          0 |        0 |       0 |      0 |     0 |    0 |
| text3   |   1 |     0 |     1 |       0 |          0 |        1 |       0 |      2 |     0 |    0 |
| text4   |   1 |     0 |     0 |       0 |          0 |        0 |       0 |      0 |     0 |    0 |
| text5   |   0 |     0 |     0 |       0 |          0 |        0 |       0 |      0 |     0 |    0 |
| text6   |   1 |     0 |     0 |       0 |          0 |        0 |       0 |      0 |     0 |    0 |
| text7   |   1 |     0 |     0 |       0 |          0 |        0 |       0 |      0 |     0 |    0 |
| text8   |   1 |     0 |     0 |       0 |          0 |        0 |       0 |      0 |     0 |    0 |
| text9   |   2 |     0 |     0 |       0 |          0 |        0 |       0 |      0 |     0 |    0 |
| text10  |   1 |     0 |     0 |       0 |          0 |        0 |       0 |      0 |     0 |    0 |

Now we will build a dictionary using immigrant and the related keywords
to display the frequency of these keywords occurring per document using
dfm\_lookup.

``` r
#Generates a frequency table showing the appearances per document of our keyword
immigrant_kws <- c('immigrant','migrant','immig*','dreamer', 'visa','aliens')
dict <- dictionary(list(immigrant = immigrant_kws))
# Specify glob because one dictionary item is in glob format
dfm_keywords <- dfm_lookup(cr_dfm, dict, valuetype = "glob",case_insensitive = TRUE) 
```

Lets view the results! These are the speeches with the most occurances
of our keywords along with the frequency.

``` r
kable(head(dfm_sort(dfm_keywords,decreasing = TRUE, margin = 'documents'),20))
```

    ## Warning: 'as.data.frame.dfm' is deprecated.
    ## Use 'convert(x, to = "data.frame")' instead.
    ## See help("Deprecated")

| doc\_id    | immigrant |
|:-----------|----------:|
| text591430 |       131 |
| text74371  |       125 |
| text158659 |       108 |
| text211164 |       100 |
| text93751  |        90 |
| text280352 |        79 |
| text15775  |        78 |
| text71123  |        75 |
| text92964  |        74 |
| text189803 |        72 |
| text21341  |        70 |
| text286796 |        70 |
| text277206 |        69 |
| text277889 |        69 |
| text643502 |        67 |
| text197183 |        64 |
| text636249 |        64 |
| text39993  |        61 |
| text201509 |        61 |
| text599848 |        61 |

Kwic & Sentiment Analysis

Okay! Now we are going to utilize the power of the kwic window to
extract a text window around one of our keywords. Then we combine the
pre- and post- windows into one column.

``` r
#create kwic window for immig* - this is a globular text object which will match for any word that starts with 'immig'. Capturing words like immigrant or immigration. The window is the # of words surrounding our keyword.
immigrant <- kwic(cr_corpus, pattern = "immig*", 
                  window = 50, valuetype = "glob")
#Turn kwic window into dataframe
immigrant <- as.data.frame(immigrant)
#create pre-post column
immigrant$pre_post <- paste(immigrant$pre, immigrant$post)
```

View the kwic dataframe

``` r
kable(head(immigrant))
```

| docname | from |   to | pre                                                                                                                                                                                                                                                                                                                                 | keyword     | post                                                                                                                                                                                                                                                                                                                      | pattern | pre\_post                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|:--------|-----:|-----:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| text67  |  250 |  250 | The SAkNCHEZ sisters from California and the DIAZBALART brothers from Florida will serve simultaneously as teams in this Congress . representing diverse districts on opposite coasts of this great country . My esteemed colleagues . with their formidable backgrounds . are all accomplished in their own rights . coming from   | immigrant   | families and immigrant backgrounds who have truly lived the American dream with hard work . as productive members of society . giving back and serving the people of their communities . They have now been elected to the U.S . Congress . The senior sister . the gentlewoman from California                           | immig\* | The SAkNCHEZ sisters from California and the DIAZBALART brothers from Florida will serve simultaneously as teams in this Congress . representing diverse districts on opposite coasts of this great country . My esteemed colleagues . with their formidable backgrounds . are all accomplished in their own rights . coming from families and immigrant backgrounds who have truly lived the American dream with hard work . as productive members of society . giving back and serving the people of their communities . They have now been elected to the U.S . Congress . The senior sister . the gentlewoman from California         |
| text67  |  253 |  253 | from California and the DIAZBALART brothers from Florida will serve simultaneously as teams in this Congress . representing diverse districts on opposite coasts of this great country . My esteemed colleagues . with their formidable backgrounds . are all accomplished in their own rights . coming from immigrant families and | immigrant   | backgrounds who have truly lived the American dream with hard work . as productive members of society . giving back and serving the people of their communities . They have now been elected to the U.S . Congress . The senior sister . the gentlewoman from California . has an                                         | immig\* | from California and the DIAZBALART brothers from Florida will serve simultaneously as teams in this Congress . representing diverse districts on opposite coasts of this great country . My esteemed colleagues . with their formidable backgrounds . are all accomplished in their own rights . coming from immigrant families and backgrounds who have truly lived the American dream with hard work . as productive members of society . giving back and serving the people of their communities . They have now been elected to the U.S . Congress . The senior sister . the gentlewoman from California . has an                     |
| text281 |  180 |  180 | allowing injured and disabled individuals to survive conditions that would have proven fatal in past years . An inadequate number of physical therapists has led to an increased reliance on foreigneducated . nonimmigrant temporary workers who enter the U.S . as H1B visa holders . The U.S . Commission on                     | Immigration | Reform has identified physical therapy and occupational therapy as having the highest number of H1B visa holders in the United States . second only to computer specialists . In addition to the shortage of practitioners . a shortage of faculty impedes the expansion of established education programs . The critical | immig\* | allowing injured and disabled individuals to survive conditions that would have proven fatal in past years . An inadequate number of physical therapists has led to an increased reliance on foreigneducated . nonimmigrant temporary workers who enter the U.S . as H1B visa holders . The U.S . Commission on Reform has identified physical therapy and occupational therapy as having the highest number of H1B visa holders in the United States . second only to computer specialists . In addition to the shortage of practitioners . a shortage of faculty impedes the expansion of established education programs . The critical |
| text288 |  574 |  574 | The law expired at the end of 1946 . but not before the United States had withdrawn its sole naturalization examiner from the Philippines for a ninemonth period . This effectively denied Filipino veterans the opportunity to become citizens during this ninemonth window . Fortyfive years later . under the                    | Immigration | Act of 1990 . certain Filipino veterans who had served during World War II became eligible for United States citizenship . Between November . 1990 . and February . 1995 . approximately 24.000 veterans took advantage of this opportunity and became United States citizens . Although progress has been made           | immig\* | The law expired at the end of 1946 . but not before the United States had withdrawn its sole naturalization examiner from the Philippines for a ninemonth period . This effectively denied Filipino veterans the opportunity to become citizens during this ninemonth window . Fortyfive years later . under the Act of 1990 . certain Filipino veterans who had served during World War II became eligible for United States citizenship . Between November . 1990 . and February . 1995 . approximately 24.000 veterans took advantage of this opportunity and became United States citizens . Although progress has been made          |
| text308 | 1523 | 1523 | provided through this program to help eligible working families pay their share of an employerbased health insurance plan . In short . the legislation will help ensure that the entire family receives the health care they need . The legislation will also allow States to expand coverage to eligible legal                     | immigrants  | through Medicaid and SCHIP . Maine is one of a number of states that is currently covering eligible legal immigrant pregnant women and children under Medicaid using 100 percent state dollars . Giving States the option of covering these children and families under Medicaid will enable them to receive matching     | immig\* | provided through this program to help eligible working families pay their share of an employerbased health insurance plan . In short . the legislation will help ensure that the entire family receives the health care they need . The legislation will also allow States to expand coverage to eligible legal through Medicaid and SCHIP . Maine is one of a number of states that is currently covering eligible legal immigrant pregnant women and children under Medicaid using 100 percent state dollars . Giving States the option of covering these children and families under Medicaid will enable them to receive matching     |
| text308 | 1543 | 1543 | short . the legislation will help ensure that the entire family receives the health care they need . The legislation will also allow States to expand coverage to eligible legal immigrants through Medicaid and SCHIP . Maine is one of a number of states that is currently covering eligible legal                               | immigrant   | pregnant women and children under Medicaid using 100 percent state dollars . Giving States the option of covering these children and families under Medicaid will enable them to receive matching federal funds . and will help relieve the pressure that most a State budgets are currently experiencing due to the      | immig\* | short . the legislation will help ensure that the entire family receives the health care they need . The legislation will also allow States to expand coverage to eligible legal immigrants through Medicaid and SCHIP . Maine is one of a number of states that is currently covering eligible legal pregnant women and children under Medicaid using 100 percent state dollars . Giving States the option of covering these children and families under Medicaid will enable them to receive matching federal funds . and will help relieve the pressure that most a State budgets are currently experiencing due to the                |

Now we can use the kwic dataframe to perform some sentiment analysis

``` r
#create senitment measure
kwic_sentimentr <- sentiment_by(immigrant$pre_post)
```

    ## Warning: Each time `sentiment_by` is run it has to do sentence boundary disambiguation when a
    ## raw `character` vector is passed to `text.var`. This may be costly of time and
    ## memory.  It is highly recommended that the user first runs the raw `character`
    ## vector through the `get_sentences` function.

``` r
#view the average sentiment score
summary(kwic_sentimentr$ave_sentiment)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -0.65566 -0.04696  0.05771  0.05225  0.15548  0.64713

Look at sentiment table

``` r
#Check out the table
kable(head(kwic_sentimentr))
```

| element\_id | word\_count |        sd | ave\_sentiment |
|------------:|------------:|----------:|---------------:|
|           1 |          90 | 0.1885582 |      0.1226719 |
|           2 |          89 | 0.1820722 |      0.1218478 |
|           3 |          95 | 0.2067029 |     -0.1418227 |
|           4 |          84 | 0.3064519 |      0.1211720 |
|           5 |          94 | 0.1589375 |      0.2848073 |
|           6 |          94 | 0.1813768 |      0.2267275 |

If we want to look at sentiment by Speech, we can combine columns from
the immigrant dataframe and the sentiment dataframe. Let’s take a look!

``` r
kwic_sentimentr <- cbind(immigrant$docname, kwic_sentimentr)
kable(head(kwic_sentimentr))
```

| V1      | element\_id | word\_count |        sd | ave\_sentiment |
|:--------|------------:|------------:|----------:|---------------:|
| text67  |           1 |          90 | 0.1885582 |      0.1226719 |
| text67  |           2 |          89 | 0.1820722 |      0.1218478 |
| text281 |           3 |          95 | 0.2067029 |     -0.1418227 |
| text288 |           4 |          84 | 0.3064519 |      0.1211720 |
| text308 |           5 |          94 | 0.1589375 |      0.2848073 |
| text308 |           6 |          94 | 0.1813768 |      0.2267275 |

We can look at the corresponding text \# from the above immigrant table
to see how the estimated sentiment lines up with our expectations. As
you can see, text281 has a negative sentiment while text308 has a much
more positive sentiment.

Last, let’s look at this visually! We will create a histogram showing
the distribution of average sentiment throughout all of the speeches.
![](Text-Analytics-with-R--Part1_github_files/figure-gfm/histogram-1.png)<!-- -->

This plot shows that the average sentiment of speech around the word
immigrant is close to 0, but looks to be slightly more positive, with
the center of the distribution around 0.1.
