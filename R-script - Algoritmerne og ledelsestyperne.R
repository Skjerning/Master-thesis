#=======================================================================================#
# Dato    : 31.05.2021                                                                  #
# Forfattere : Frederik Skjerning & Sune Stöckel                                           #
#=======================================================================================#
#### SETUP ####
Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')

# renser hukommelse
rm(list=ls())

# indlæser software-pakker
library(caret)                   # for Machine Learning (Naive Bayes) 
library(quanteda)                # for text processing
library(e1071)                   # for SVM
library(dplyr)                   # for data manipulation
library(plyr)                    # for Splitting, Applying and Combining Data
library(readxl)                  # for getting data out of Excel and into R
library(data.table)              # for fast aggregation of large data
library(tidyr)                   # for creating tidy data
library(tibble)                  # for stricter checking and better formatting than the traditional data frame
library(pdftools)                # for reading pdf-files into text
library(xlsx)                    # for reading/writing/formating Excel file formats
library(naivebayes)              # for Naive Bayes classifier
library(ggplot2)                 # for graphics and graphs
library(gsubfn)                  # for replacing signs etc. in strings
library(tidyverse)               # for creating tidy data
library(klaR)

# sætter sti
setwd("/Users/FrederikSkjerning/Desktop/Spezzz/Data")

#=======================================================================================#

#### 1. INDLÆSNING OG PREPROCESSING AF LABELED DATA (TEORETISKE SNIPPETS) ####
Labelled_data <- read_excel("Labelled_data.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text",
                                          "text", "text", "text",
                                          "text", "text"))

# gemmer data som R-data fil
save(Labelled_data, file = "Labelled_data.Rdata")

#### 2. INDLÆSNING OG PREPROCESSING AF UNLABELLED DATA (LEDELSESDOKUMENTER) ####

# sætter sti
setwd("~/Desktop/Spezzz/Data/Unlabelled data")

#=======================================================================================#

# pdftools pakke loades - funktion muliggør udtrækning af tekst fra pdf'er

# Herefter genereres en vektor med pdf-filnavnene ved hjælp af list.files-funktionen
# Pattern-funktionen sikrer, at kun filer, der slutter med .pdf, medtages
files <- list.files(pattern = "pdf$")

# Denne vektor/liste af pdf-filnavne anvendes til automatisk at indlæse text fra pdf-filerne. 

# med pdftools funktionen pdf_text udtrækkes text fra pdf-filerne. Ved hjælp af lapply funktionen kan vi anvende denne funktion på hver af filerne i ovenstående liste.
ledelsesdokumenter <- lapply(files, pdf_text)

# kontrollerer antallet af indlæste ledelsesdokumenter
length(ledelsesdokumenter)

# Hvert element i "ledelsesdokumenter" er en vektor der indeholder teksten fra det givne ledelsesdokument. Længden på denne vektor svarer til antallet af sider i pdf-filen.
# Vi anvender dette til at finde ud af hvilken pdf-fil der er længst
lapply(ledelsesdokumenter, length)

# pdf-filerne, dvs. ledelsesdokumenterne, er nu klar til at blive renset og preprocessed
# vi starter med at inspicere det indlæste.
# der er her tydeligt at der er en række indlæsnings-"fejl" og udfordringer, såsom fejlindlæste bogstaver osv. ( \r and \n)
ledelsesdokumenter

# vi laver transformerer ledelsesdokumenterne fra et list-objekt til en dataframe
ledelsesdokumenter_df <- data.frame(t(sapply(ledelsesdokumenter, function(x) x[1:max(lengths(ledelsesdokumenter))])))

# vi slår alle tekstkolonner (repræsenterende hver side i ledelsesdokumentet) sammen til én kolonne indeholdende al tekst fra ledelsesdokumentet
ledelsesdokumenter_df <- unite(ledelsesdokumenter_df, "text", X1:X44, na.rm = T)

# vi kobler navnet på ledelsesdokumentet, på dataframen
ledelsesdokumenter_df <- data.frame(cbind(files, ledelsesdokumenter_df))

# indlæser oversigt over split og holdout-data i ledelsesdokumenter
setwd("/Users/FrederikSkjerning/Desktop/Spezzz/Data")
overview <- read_excel("Labelled_data.xlsx", sheet = 2,
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text"))

# fletter dataframes med overblik over split og holudout data og dropper irrelevante variable
ledelsesdokumenter_df <- merge(ledelsesdokumenter_df, overview, by.x = "files", by.y = "Dokument_id")
drop <- c("Ansvarlig", "Kodet")
ledelsesdokumenter_df <- ledelsesdokumenter_df[,!(names(ledelsesdokumenter_df) %in% drop)]

# gemmer data som Rdata-fil
save(ledelsesdokumenter_df, file = "Unlabelled_data.Rdata")

#### 3. MERGER DATAFRAMES (LABELED OG UNLABELED) TIL SIMPLIFICEREDE DATAFRAMES ####
# navngiver kolonner
names(Labelled_data)[1] <- "Dokument_id"
names(Labelled_data)[9] <- "Tekst"
names(Labelled_data)[6] <- "Label"
names(Labelled_data)[7] <- "Label_num"
names(Labelled_data)[3] <- "Subtype"
names(Labelled_data)[4] <- "Subtype_num"
names(ledelsesdokumenter_df)[1] <- "Dokument_id"
names(ledelsesdokumenter_df)[2] <- "Tekst"
ledelsesdokumenter_df$Dokument_id <- as.character(ledelsesdokumenter_df$Dokument_id)
ledelsesdokumenter_df$Tekst <- as.character(ledelsesdokumenter_df$Tekst)

ledelsesdokumenter_df <- ledelsesdokumenter_df %>%
  add_column(Testdata = 1)
Labelled_data <- Labelled_data %>%
  add_column(Testdata = 2)

data_endelig <- rbind.fill(ledelsesdokumenter_df, Labelled_data)

# Genererer en simplificeret dataframe og dropper variable
data_simplificeret = data_endelig[-c(8:12)]

data_simplificeret$Tekst <- as.character(data_simplificeret$Tekst)
data_simplificeret$Label <- as.character(data_simplificeret$Label)
data_simplificeret$Label_num <- as.numeric(data_simplificeret$Label_num)
data_simplificeret$Subtype_num <- as.numeric(data_simplificeret$Subtype_num)

# fjerner bindestrege, underscores og skråstreger (new line)
data_simplificeret$Tekst <- gsub('_', ' ', data_simplificeret$Tekst)
data_simplificeret$Tekst <- gsub('-', '', data_simplificeret$Tekst)
data_simplificeret$Tekst <- gsub('\\\n', ' ', data_simplificeret$Tekst)
data_simplificeret[which(data_simplificeret$Dokument_id=="Erhvervsministeriet_ledelsesprincipper.pdf"),]$Tekst <- "Kunne ikke finde tekst"
data_simplificeret[which(data_simplificeret$Dokument_id=="Holstebro Kommune_Ledelsesværdier.pdf"),]$Tekst <- "Kunne ikke finde tekst"
data_simplificeret[which(data_simplificeret$Dokument_id=="Udlændinge- og Integrationsministeriet_Fælles koncern ledelsesgrundlag - stjerne.pdf"),]$Tekst <- "Kunne ikke finde tekst"

# sætter sti
setwd("/Users/FrederikSkjerning/Desktop/Spezzz/Data")

# gemmer data som Rdata-fil
save(data_simplificeret, file = "data_endelig.Rdata")

#### 4. DATARENSNING (TOKENIZATION) ####
# renser hukommelse
rm(list=ls())

# indlæsning af data som Rdata-fil
load('data_endelig.Rdata')

# generering af corpus
corpus <- corpus(data_simplificeret, text_field = "Tekst")

# tokenizing af corpus - inkl. preprocessing (fjerner tegnsætning, tal, specialtegn/symboler og potentielle url's)
tokens <- tokens(corpus, 
                 remove_numbers = T,
                 remove_punct = T,
                 remove_symbols = T,
                 remove_separators = T,
                 split_hyphens = T, 
                 remove_url = T,
                 include_docvars = T)
                 #ngrams = 1L)

# generering af document feature matrix
# vi fjerner her danske stopord, konverterer alt til lower case (små bogstaver) også stemmer ord til deres rod
dfm <- dfm(tokens,
           remove = stopwords('danish'),
           tolower = T,
           stem = T)

#### 5. DUPLIKERING AF DATAFRAMES TIL FORSKELLIGE ANALYSER ####
# duplikerer dfm for dfm uden frekvente og infrekvente ord/features
dfm_freq <- dfm

# duplikerer dfm for binær dfm
dfm_bi <- dfm

# duplikerer dfm for dfm uden frekvente og infrekvente ord/features samt binær
dfm_freq_bi <- dfm

# fjerner frekvente og infrekvente ord/features fra dataframe dfm_freq
dfm_freq <- dfm_trim(dfm_freq, termfreq_type = "quantile", min_termfreq = 0.1)
dfm_freq <- dfm_trim(dfm_freq, termfreq_type = "quantile", max_termfreq = 0.9)

# fjerner frekvente og infrekvente ord/features fra dataframe dfm_freq_bi
dfm_freq_bi <- dfm_trim(dfm_freq_bi, termfreq_type = "quantile", min_termfreq = 0.1)
dfm_freq_bi <- dfm_trim(dfm_freq_bi, termfreq_type = "quantile", max_termfreq = 0.9)

# konverterer tilbage til en dataframe
dfm_df <- quanteda::convert(dfm, to = 'data.frame')
dfm_df_freq <- quanteda::convert(dfm_freq, to = 'data.frame')
dfm_df_bi <- quanteda::convert(dfm_bi, to = 'data.frame')
dfm_df_freq_bi <- quanteda::convert(dfm_freq_bi, to = 'data.frame')

# vi tilføjer vores label dvs. indikatoren for ledelsestypen
dfm_df <- cbind(data_simplificeret[, c(1, 3:5, 7)], dfm_df)
dfm_df_freq <- cbind(data_simplificeret[, c(1, 3:5, 7)], dfm_df_freq)
dfm_df_bi <- cbind(data_simplificeret[, c(1, 3:5, 7)], dfm_df_bi)
dfm_df_freq_bi <- cbind(data_simplificeret[, c(1, 3:5, 7)], dfm_df_freq_bi)

# Gøre alle variable til dikotome dvs. forekomstbaseret og ikke frekvensbaseret for de to bi-dataframes
dfm_df_bi[ ,8:10490][ dfm_df_bi[ ,8:10490] > 0 ] <- 1
dfm_df_freq_bi[ ,8:9466][ dfm_df_freq_bi[ ,8:9466] > 0 ] <- 1

# vi transformerer alle variable i datasættet til faktor-variable - dfm_df
dfm_df[sapply(dfm_df, is.numeric)] <- lapply(dfm_df[sapply(dfm_df, is.numeric)], as.factor)
dfm_df[sapply(dfm_df, is.character)] <- lapply(dfm_df[sapply(dfm_df, is.character)], as.factor)

# vi transformerer alle variable i datasættet til faktor-variable - dfm_df_freq
dfm_df_freq[sapply(dfm_df_freq, is.numeric)] <- lapply(dfm_df_freq[sapply(dfm_df_freq, is.numeric)], as.factor)
dfm_df_freq[sapply(dfm_df_freq, is.character)] <- lapply(dfm_df_freq[sapply(dfm_df_freq, is.character)], as.factor)

# vi transformerer alle variable i datasættet til faktor-variable - dfm_df_bi
dfm_df_bi[sapply(dfm_df_bi, is.numeric)] <- lapply(dfm_df_bi[sapply(dfm_df_bi, is.numeric)], as.factor)
dfm_df_bi[sapply(dfm_df_bi, is.character)] <- lapply(dfm_df_bi[sapply(dfm_df_bi, is.character)], as.factor)

# vi transformerer alle variable i datasættet til faktor-variable - dfm_df_freq_bi
dfm_df_freq_bi[sapply(dfm_df_freq_bi, is.numeric)] <- lapply(dfm_df_freq_bi[sapply(dfm_df_freq_bi, is.numeric)], as.factor)
dfm_df_freq_bi[sapply(dfm_df_freq_bi, is.character)] <- lapply(dfm_df_freq_bi[sapply(dfm_df_freq_bi, is.character)], as.factor)

# dropper kolonner (features) som ikke er relevant for senere analyse
drop <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p",
          "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "æ", "ø", "å","0", "1", "2", 
          "3", "4", "5", "6", "7", "8", "9", "bl.a", "ii", "iii", "afd.chef", "3leder", "4leder",
          "m.v", "m.fl", "f.ek", "4årige", "4årigt", "www.domstol.dk", "www.favrskov.dk", "5leder", 
          "o.l", "www.fko.dk", "fptbstpf4055", "virksomhedsledelse3", "chefvirke5", "føring4", "hrn010001",
          "h.j", "xx", "00.01.00g01818", "t'et", "vi'fornemmels", "m.m", "tu19", "o.a", "www.lolland.dk",
          "www.middelfart.dk", "3i1", "co2belastend", "www.naturstyrelsen.dk", "www.lederweb.dk", "m.h.p",
          "odsherred.dk","1dialogen", "h:s", "www.regionh.dk", "www.godledelse.intra.rm.dk", "www.politikker.rm.dk",
          "www.medarbmiljo.rm.dk", "km2" , "bilag5", "4årig", "3.0sporet", "vii", "www.loneboenielsen.dk",
          "skive.dk", "www.solrod.dk", "sonderborgkommune.dk", "2018niveau", "2019niveau", "2020niveau", "f2",
          "ac'er", "el.lign", "pc'en" , "www.skat.dk", "1dag", "2dage", "27f", "www.eboks.dk", "www.feri" ,
          "konto.dk", "www.feriekonto.dk" , "www.trm.dk",  "r2")
          
dfm_df <- dfm_df[,!(names(dfm_df) %in% drop)]
dfm_df_freq <- dfm_df_freq[,!(names(dfm_df_freq) %in% drop)]
dfm_df_bi <- dfm_df_bi[,!(names(dfm_df_bi) %in% drop)]
dfm_df_freq_bi <- dfm_df_freq_bi[,!(names(dfm_df_freq_bi) %in% drop)]

# sætter sti
setwd("/Users/FrederikSkjerning/Desktop/Spezzz/Data")

# gemmer data som Rdata-fil
save(dfm_df, file = "dfm_df.Rdata")
save(dfm_df_freq, file = "dfm_df_freq.Rdata")
save(dfm_df_bi, file = "dfm_df_bi.Rdata")
save(dfm_df_freq_bi, file = "dfm_df_freq_bi.Rdata")

#### 6. GENERERER TEST OG TRÆNINGSDATASÆT ####
# ryder hukommelse
rm(list=ls())

# indlæsning af labeled data som Rdata-fil
setwd("/Users/FrederikSkjerning/Desktop/Spezzz/Data")
load('dfm_df.Rdata')
load('dfm_df_freq.Rdata')
load('dfm_df_bi.Rdata')
load('dfm_df_freq_bi.Rdata')

# genererer test og træningssæt - koden herunder er blevet kørt og metadata meta-data er indlæst med de datasæt der indlæses i toppen af scriptet
# set.seed(666)
# n <- nrow(dfm_df)  # Number of observations
# training_data <- dfm_df[which(dfm_df$Testdata=='2'),]   # Create training set (theoretical snippets)
# testholdout_data <- dfm_df[which(dfm_df$Testdata=='1'),]   # Create test set (leadership documents)

# genererer holdout data af testsæt
# n <- nrow(testholdout_data)  # Number of observations
# nholdout <- round(n*0.1667)  # 16.67 % af testdata for hold out set (1/6 of testdata)
# tindex <- sample(n, nholdout)   # Create a random index
# holdout_data <- testholdout_data[tindex,]   # Create holdout set
# test_data <- testholdout_data[-tindex,]   # Create test set

# genererer to testdatasæt
# n <- nrow(test_data)  # Number of observations
# ntest1 <- round(n*0.5)  # 
# tindex <- sample(n, ntest1)   # Create a random index
# test_data1 <- test_data[tindex,]   # Create test set 1
# test_data2 <- test_data[-tindex,]   # Create test set 2

# kontrollerer for overlap i trænings-, test og holdput data
# training_data$Dokument_id
# holdout_data$Dokument_id
# test_data1$Dokument_id
# test_data2$Dokument_id

#### 7. GENERERER DATASÆT TIL MODELLER ####
# anvendes til at droppe variable og kolonner med ubetydelig metainformation
drop_columns <- c("Subtype", "Subtype_num", "Testdata")

# Original - genererer datasæt og dropper irrelevante variable
model1_data <- subset(dfm_df, Subtype_num == 1)
model1_data <- model1_data[,!(names(model1_data) %in% drop_columns)]
model2_data <- subset(dfm_df, Subtype_num == 1 | Subtype_num == 2)
model2_data <- model2_data[,!(names(model2_data) %in% drop_columns)]
model3_data <- subset(dfm_df, Subtype_num == 1 | Subtype_num == 2| Subtype_num==3)
model3_data <- model3_data[,!(names(model3_data) %in% drop_columns)]
prediction_model1_data <- subset(dfm_df, Subtype_num == 4)
prediction_model1_data <- prediction_model1_data[,!(names(prediction_model1_data) %in% drop_columns)]
prediction_model2_data <- subset(dfm_df, Subtype_num == 5)
prediction_model2_data <- prediction_model2_data[,!(names(prediction_model2_data) %in% drop_columns)]
prediction_model3_data <- subset(dfm_df, Subtype_num == 6)
prediction_model3_data <- prediction_model3_data[,!(names(prediction_model3_data) %in% drop_columns)]
prediction_model4_data <- subset(dfm_df, Subtype_num==4 | Subtype_num==5 |Subtype_num == 6)
prediction_model4_data <- prediction_model4_data[,!(names(prediction_model4_data) %in% drop_columns)]

# Frequency - genererer datasæt og dropper irrelevante variable
model1_data_freq <- subset(dfm_df_freq, Subtype_num == 1)
model1_data_freq <- model1_data_freq[,!(names(model1_data_freq) %in% drop_columns)]
model2_data_freq <- subset(dfm_df_freq, Subtype_num == 1 | Subtype_num == 2)
model2_data_freq <- model2_data_freq[,!(names(model2_data_freq) %in% drop_columns)]
model3_data_freq <- subset(dfm_df_freq, Subtype_num == 1 | Subtype_num == 2| Subtype_num==3)
model3_data_freq <- model3_data_freq[,!(names(model3_data_freq) %in% drop_columns)]
prediction_model1_data_freq <- subset(dfm_df_freq, Subtype_num == 4)
prediction_model1_data_freq <- prediction_model1_data_freq[,!(names(prediction_model1_data_freq) %in% drop_columns)]
prediction_model2_data_freq <- subset(dfm_df_freq, Subtype_num == 5)
prediction_model2_data_freq <- prediction_model2_data_freq[,!(names(prediction_model2_data_freq) %in% drop_columns)]
prediction_model3_data_freq <- subset(dfm_df_freq, Subtype_num == 6)
prediction_model3_data_freq <- prediction_model3_data_freq[,!(names(prediction_model3_data_freq) %in% drop_columns)]
prediction_model4_data_freq <- subset(dfm_df_freq, Subtype_num==4 | Subtype_num==5 |Subtype_num == 6)
prediction_model4_data_freq <- prediction_model4_data_freq[,!(names(prediction_model4_data_freq) %in% drop_columns)]

# Binary - genererer datasæt og dropper irrelevante variable
model1_data_bi <- subset(dfm_df_bi, Subtype_num == 1)
model1_data_bi <- model1_data_bi[,!(names(model1_data_bi) %in% drop_columns)]
model2_data_bi <- subset(dfm_df_bi, Subtype_num == 1 | Subtype_num == 2)
model2_data_bi <- model2_data_bi[,!(names(model2_data_bi) %in% drop_columns)]
model3_data_bi <- subset(dfm_df_bi, Subtype_num == 1 | Subtype_num == 2| Subtype_num==3)
model3_data_bi <- model3_data_bi[,!(names(model3_data_bi) %in% drop_columns)]
prediction_model1_data_bi <- subset(dfm_df_bi, Subtype_num == 4)
prediction_model1_data_bi <- prediction_model1_data_bi[,!(names(prediction_model1_data_bi) %in% drop_columns)]
prediction_model2_data_bi <- subset(dfm_df_bi, Subtype_num == 5)
prediction_model2_data_bi <- prediction_model2_data_bi[,!(names(prediction_model2_data_bi) %in% drop_columns)]
prediction_model3_data_bi <- subset(dfm_df_bi, Subtype_num == 6)
prediction_model3_data_bi <- prediction_model3_data_bi[,!(names(prediction_model3_data_bi) %in% drop_columns)]
prediction_model4_data_bi <- subset(dfm_df_bi, Subtype_num==4 | Subtype_num==5 |Subtype_num == 6)
prediction_model4_data_bi <- prediction_model4_data_bi[,!(names(prediction_model4_data_bi) %in% drop_columns)]

# Frequency and Binary - genererer datasæt og dropper irrelevante variable
model1_data_freq_bi <- subset(dfm_df_freq_bi, Subtype_num == 1)
model1_data_freq_bi <- model1_data_freq_bi[,!(names(model1_data_freq_bi) %in% drop_columns)]
model2_data_freq_bi <- subset(dfm_df_freq_bi, Subtype_num == 1 | Subtype_num == 2)
model2_data_freq_bi <- model2_data_freq_bi[,!(names(model2_data_freq_bi) %in% drop_columns)]
model3_data_freq_bi <- subset(dfm_df_freq_bi, Subtype_num == 1 | Subtype_num == 2| Subtype_num==3)
model3_data_freq_bi <- model3_data_freq_bi[,!(names(model3_data_freq_bi) %in% drop_columns)]
prediction_model1_data_freq_bi <- subset(dfm_df_freq_bi, Subtype_num == 4)
prediction_model1_data_freq_bi <- prediction_model1_data_freq_bi[,!(names(prediction_model1_data_freq_bi) %in% drop_columns)]
prediction_model2_data_freq_bi <- subset(dfm_df_freq_bi, Subtype_num == 5)
prediction_model2_data_freq_bi <- prediction_model2_data_freq_bi[,!(names(prediction_model2_data_freq_bi) %in% drop_columns)]
prediction_model3_data_freq_bi <- subset(dfm_df_freq_bi, Subtype_num == 6)
prediction_model3_data_freq_bi <- prediction_model3_data_freq_bi[,!(names(prediction_model3_data_freq_bi) %in% drop_columns)]
prediction_model4_data_freq_bi <- subset(dfm_df_freq_bi, Subtype_num==4 | Subtype_num==5 |Subtype_num == 6)
prediction_model4_data_freq_bi <- prediction_model4_data_freq_bi[,!(names(prediction_model4_data_freq_bi) %in% drop_columns)]

#### 8. SUPPORT VECTOR MACHINE (SVM) MODELLER ####
# Parameter selection
# Kernel - linear, polynomial, radial, and sigmoid. Vi bruger kernel=”radial” (som er default)
# Gamma - argument der bruges til kernel funktionen
# Cost - tillader os at specificere cost ved en "a violation to the margin". Når Cost er lille vil margins være brede, resulterende i mange support vectors.

#### 8.1 SVM - Original ####
# Træner model på træningsdata == 1 (teoretiske snippets)
# Tuner model for bedre performance
# svm1_tune <- tune(svm, Label~.,
                  #data=model1_data,
                  #kernel = "radial",
                  #ranges = list(
                    #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                    #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm1_tune)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.02924242
svm1 <- svm(Label~., data=model1_data, 
            type="C-classification",
            kernel="radial", 
            probability = TRUE,
            gamma=0.1,
            cost=10)

# Træner model på træningsdata == 1 or 2 (teoretiske snippets + empirisk træningsdata 1)
# Tuner model for bedre performance
# svm2_tune <- tune(svm, Label~.,
                  #data=model2_data,
                  #kernel = "radial",
                  #ranges = list(
                    #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                    #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm2_tune)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.02156537
svm2 <- svm(Label~., data=model2_data, 
            type="C-classification",
            kernel="radial", 
            probability = TRUE,
            gamma=0.1,
            cost=10)

# Træner model på træningsdata == 1, 2 or 3 (teoretiske snippets + empirisk træningsdata 1 + empiriske træningsdata 2)
# Tuner model for bedre performance
# svm3_tune <- tune(svm, Label~.,
                  #data=model3_data,
                  #kernel = "radial",
                  #ranges = list(
                    #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                    #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm3_tune)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.01850968
svm3 <- svm(Label~., data=model3_data, 
            type="C-classification",
            kernel="radial", 
            probability = TRUE,
            gamma=0.1,
            cost=10)

# prædiktering og afrapportering
prediction_SVM1 <- predict(svm1, prediction_model1_data,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM1 <- table(prediction_model1_data$Label, prediction_SVM1)
xtab_SVM1

prediction_SVM2 <- predict(svm2, prediction_model2_data,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM2 <- table(prediction_model2_data$Label, prediction_SVM2)
xtab_SVM2

prediction_SVM3 <- predict(svm3, prediction_model3_data,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM3 <- table(prediction_model3_data$Label, prediction_SVM3)
xtab_SVM3

prediction_SVM4 <- predict(svm3, prediction_model4_data,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM4 <- table(prediction_model4_data$Label, prediction_SVM4)
xtab_SVM4

#### 8.2 SVM - Uden frequent og infrequent words ####
# Træner model på træningsdata == 1 (teoretiske snippets)
# Tuner model for bedre performance
# svm1_tune_freq <- tune(svm, Label~.,
                       #data=model1_data_freq,
                       #kernel = "radial",
                       #ranges = list(
                         #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                         #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm1_tune_freq)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.1731313
svm1_freq <- svm(Label~., data=model1_data_freq, 
            type="C-classification",
            kernel="radial", 
            probability = TRUE,
            gamma=0.1,
            cost=10)

# Træner model på træningsdata == 1 or 2 (teoretiske snippets + empiriske træningsdata 1)
# Tuner model for bedre performance
# svm2_tune_freq <- tune(svm, Label~.,
                       #data=model2_data_freq,
                       #kernel = "radial",
                       #ranges = list(
                         #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                         #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm2_tune_freq)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.3053626
svm2_freq <- svm(Label~., data=model2_data_freq, 
            type="C-classification",
            kernel="radial", 
            probability = TRUE,
            gamma=0.1,
            cost=10)

# Træner model på træningsdata == 1, 2 or 3 (teoretiske snippets + empirisk træningsdata 1 + empirisk træningsdata 2)
# Tuner model for bedre performance
# svm3_tune_freq <- tune(svm, Label~.,
                  #data=model3_data_freq,
                  #kernel = "radial",
                  #ranges = list(
                    #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                    #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm3_tune_freq)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.3636839
svm3_freq <- svm(Label~., data=model3_data_freq, 
            type="C-classification",
            kernel="radial", 
            probability = TRUE,
            gamma=0.1,
            cost=10)

# prædiktering og afrapportering
prediction_SVM1_freq <- predict(svm1_freq, prediction_model1_data_freq,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM1_freq <- table(prediction_model1_data_freq$Label, prediction_SVM1_freq)
# xtab_SVM1_freq

prediction_SVM2_freq <- predict(svm2_freq, prediction_model2_data_freq,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM2_freq <- table(prediction_model2_data_freq$Label, prediction_SVM2_freq)
# xtab_SVM2_freq

prediction_SVM3_freq <- predict(svm3_freq, prediction_model3_data_freq,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM3_freq <- table(prediction_model3_data_freq$Label, prediction_SVM3_freq)
# xtab_SVM3_freq

prediction_SVM4_freq <- predict(svm3_freq, prediction_model4_data_freq,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM4_freq <- table(prediction_model4_data_freq$Label, prediction_SVM4_freq)
# xtab_SVM4_freq

#### 8.3 SVM - Binær/ordforekomst ####
# Træner model på træningsdata == 1 (teoretiske snippets)
# Tuner model for bedre performance
# svm1_tune_bi <- tune(svm, Label~.,
                  #data=model1_data_bi,
                  #kernel = "radial",
                  #ranges = list(
                    #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                    #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm1_tune_bi)
# Tuner model for bedre performance
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.02242424
svm1_bi <- svm(Label~., data=model1_data_bi, 
            type="C-classification",
            kernel="radial", 
            probability = TRUE,
            gamma=0.1,
            cost=10)

# Træner model på træningsdata == 1 or 2 (teoretiske snippets + empiriske træningsdata 1)
# Tuner model for bedre performance
# svm2_tune_bi <- tune(svm, Label~.,
                  #data=model2_data_bi,
                  #kernel = "radial",
                  #ranges = list(
                    #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                    #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm2_tune_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.01930541
svm2_bi <- svm(Label~., data=model2_data_bi, 
            type="C-classification",
            kernel="radial", 
            probability = TRUE,
            gamma=0.1,
            cost=10)

# Træner model på træningsdata == 1, 2 or 3 (teoretiske snippets + empiriske træningsdata 1 + empiriske træningsdata 2)
# Tuner model for bedre performance
# svm3_tune_bi <- tune(svm, Label~.,
                  #data=model3_data_bi,
                  #kernel = "radial",
                  #ranges = list(
                    #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                    #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm3_tune_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.01608387
svm3_bi <- svm(Label~., data=model3_data_bi, 
            type="C-classification",
            kernel="radial", 
            probability = TRUE,
            gamma=0.1,
            cost=10)

# prædiktering og afrapportering
prediction_SVM1_bi <- predict(svm1_bi, prediction_model1_data_bi,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM1_bi <- table(prediction_model1_data_bi$Label, prediction_SVM1_bi)
# xtab_SVM1_bi

prediction_SVM2_bi <- predict(svm2_bi, prediction_model2_data_bi,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM2_bi <- table(prediction_model2_data_bi$Label, prediction_SVM2_bi)
# xtab_SVM2_bi

prediction_SVM3_bi <- predict(svm3_bi, prediction_model3_data_bi,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM3_bi <- table(prediction_model3_data_bi$Label, prediction_SVM3_bi)
# xtab_SVM3_bi

prediction_SVM4_bi <- predict(svm3_bi, prediction_model4_data_bi,
                           decision.values = TRUE,
                           #na.action = na.omit,
                           probability = TRUE)
xtab_SVM4_bi <- table(prediction_model4_data_bi$Label, prediction_SVM4_bi)
# xtab_SVM4_bi

#### 8.4 SVM - Without frequent/infrequent words & Binary/Appearence ####
# Træner model på træningsdata == 1 (teoretiske snippets)
# Tuner model for bedre performance
# svm1_tune_freq_bi <- tune(svm, Label~.,
                       #data=model1_data_freq_bi,
                       #kernel = "radial",
                       #ranges = list(
                         #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                         #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm1_tune_freq_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.1641414
svm1_freq_bi <- svm(Label~., data=model1_data_freq_bi, 
                 type="C-classification",
                 kernel="radial", 
                 probability = TRUE,
                 gamma=0.1,
                 cost=10)

# Træner model på træningsdata == 1 or 2 (teoretiske snippets + empiriske træningsdata 1)
# Tuner model for bedre performance
# svm2_tune_freq_bi <- tune(svm, Label~.,
                       #data=model2_data_freq_bi,
                       #kernel = "radial",
                       #ranges = list(
                         #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                         #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm2_tune_freq_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.3166624
svm2_freq_bi <- svm(Label~., data=model2_data_freq_bi, 
                 type="C-classification",
                 kernel="radial", 
                 probability = TRUE,
                 gamma=0.1,
                 cost=10)

# Træner model på træningsdata == 1, 2 or 3 (teoretiske snippets + empiriske træningsdata 1 + empiriske træningsdata 2)
# Tuner model for bedre performance
# svm3_tune_freq_bi <- tune(svm, Label~.,
                  #data=model3_data_freq_bi,
                  #kernel = "radial",
                  #ranges = list(
                    #cost=c(0.001, 0.01, 0.1, 1, 10, 100),
                    #gamma=c(0.1, 0.5, 1, 5)))
# summary(svm3_tune_freq_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:cost gamma
#                   10   0.1
# - bedste performance: 0.3555935
svm3_freq_bi <- svm(Label~., data=model3_data_freq_bi, 
                 type="C-classification",
                 kernel="radial", 
                 probability = TRUE,
                 gamma=0.1,
                 cost=10)

# prædiktering og afrapportering
prediction_SVM1_freq_bi <- predict(svm1_freq_bi, prediction_model1_data_freq_bi,
                                decision.values = TRUE,
                                #na.action = na.omit,
                                probability = TRUE)
xtab_SVM1_freq_bi <- table(prediction_model1_data_freq_bi$Label, prediction_SVM1_freq_bi)
# xtab_SVM1_freq_bi

prediction_SVM2_freq_bi <- predict(svm2_freq_bi, prediction_model2_data_freq_bi,
                                decision.values = TRUE,
                                #na.action = na.omit,
                                probability = TRUE)
xtab_SVM2_freq_bi <- table(prediction_model2_data_freq_bi$Label, prediction_SVM2_freq_bi)
# xtab_SVM2_freq_bi

prediction_SVM3_freq_bi <- predict(svm3_freq_bi, prediction_model3_data_freq_bi,
                                decision.values = TRUE,
                                #na.action = na.omit,
                                probability = TRUE)
xtab_SVM3_freq_bi <- table(prediction_model3_data_freq_bi$Label, prediction_SVM3_freq_bi)
# xtab_SVM3_freq_bi

prediction_SVM4_freq_bi <- predict(svm3_freq_bi, prediction_model4_data_freq_bi,
                                decision.values = TRUE,
                                #na.action = na.omit,
                                probability = TRUE)
xtab_SVM4_freq_bi <- table(prediction_model4_data_freq_bi$Label, prediction_SVM4_freq_bi)
# xtab_SVM4_freq_bi

#### 9. NAIVE BAYES (NB) MODELS ####
#### 9.1 NB - Original ####
# Information om parametre i Naive Bayes prædiktering
# Threshold - værdi der vil erstatte sandsynligheder mindre end hvadend denne er sat til for eps
# eps - maksimale værdi man vil beholde "as is". Hvadend mindre end dette vil blive erstattet af threshold-værdien
# laplace - tilføjer et 1 for enhver kombination af faktorer der aldrig forekommer

# Træner Niave Bayes models
# Tuner model for bedre performance
# nb1_tune <- tune(naiveBayes, Label~.,
                 #data=model1_data,
                 #ranges = list(
                   #laplace=c(0.01, 0.1, 1, 10)))
# summary(nb1_tune)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.01
# - bedste performance: 0.02919192
nb1 <- naive_bayes(Label ~ ., model1_data,
                   prior = NULL,
                   laplace = 0.01,
                   usekernel = FALSE,
                   usepoisson = TRUE)

# Tuner model for bedre performance
# nb2_tune <- tune(naiveBayes, Label~.,
                 #data=model2_data,
                 #ranges = list(
                   #laplace=c(0.01, 0.1, 1, 10)))
# summary(nb2_tune)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.01
# - bedste performance: 0.04765066
nb2 <- naive_bayes(Label ~ ., model2_data,
                   prior = NULL,
                   laplace = 0.01,
                   usekernel = FALSE,
                   usepoisson = TRUE)

# Tuner model for bedre performance
# nb3_tune <- tune(naiveBayes, Label~.,
                 #data=model3_data,
                 #ranges = list(
                   #laplace=c(0.01, 0.1, 1, 10)))
# summary(nb3_tune)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.01
# - bedste performance: 0.04746452
nb3 <- naive_bayes(Label ~ ., model3_data,
                   prior = NULL,
                   laplace = 0.01,
                   usekernel = FALSE,
                   usepoisson = TRUE)

nb4 <- naive_bayes(Label ~ ., model3_data,
                   prior = NULL,
                   laplace = 0.01,
                   usekernel = FALSE,
                   usepoisson = TRUE)

options(scipen = 999)

prediction_NB1 <- predict(nb1, prediction_model1_data,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB1 <- as.data.frame(prediction_NB1)
prediction_NB1$Label <- colnames(prediction_NB1)[max.col(prediction_NB1,ties.method="first")]
xtab_NB1 <- table(prediction_model1_data$Label, prediction_NB1$Label)
xtab_NB1

prediction_NB2 <- predict(nb2, prediction_model2_data,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB2 <- as.data.frame(prediction_NB2)
prediction_NB2$Label <- colnames(prediction_NB2)[max.col(prediction_NB2,ties.method="first")]
xtab_NB2 <- table(prediction_model2_data$Label, prediction_NB2$Label)
xtab_NB2

prediction_model3_data <- prediction_model3_data
prediction_NB3 <- predict(nb3, prediction_model3_data,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB3 <- as.data.frame(prediction_NB3)
prediction_NB3$Label <- colnames(prediction_NB3)[max.col(prediction_NB3,ties.method="first")]
xtab_NB3 <- table(prediction_model3_data$Label, prediction_NB3$Label)
xtab_NB3

prediction_NB4 <- predict(nb4, prediction_model4_data,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB4 <- as.data.frame(prediction_NB4)
prediction_NB4$Label <- colnames(prediction_NB4)[max.col(prediction_NB4,ties.method="first")]
xtab_NB4 <- table(prediction_model4_data$Label, prediction_NB4$Label)
xtab_NB4

#### 9.2 NB - Uden frequent/infrequent ord/features ####
# Træner Niave Bayes models
# Tuner model for bedre performance
#nb1_tune_freq <- tune(naiveBayes, Label~.,
                 #data=model1_data_freq,
                 #ranges = list(
                   #laplace=c(0.01, 0.1, 1, 10)))
# summary(nb1_tune_freq)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.01
# - bedste performance: 0.1434343
nb1_freq <- naive_bayes(Label ~ ., model1_data_freq,
                   prior = NULL,
                   laplace = 0.01,
                   usekernel = FALSE,
                   usepoisson = TRUE)

# Tuner model for bedre performance
# nb2_tune_freq <- tune(naiveBayes, Label~.,
                      #data=model2_data_freq,
                      #ranges = list(laplace=c(0.01, 0.1, 1, 10)))
# summary(nb2_tune_freq)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.1
# - bedste performance: 0.341573
nb2_freq <- naive_bayes(Label ~ ., model2_data_freq,
                   prior = NULL,
                   laplace = 0.01,
                   usekernel = FALSE,
                   usepoisson = TRUE)

# Tuner model for bedre performance
# nb3_tune_freq <- tune(naiveBayes, Label~.,
                      #data=model3_data_freq,
                      #ranges = list(laplace=c(0.01, 0.1, 1, 10)))
# summary(nb3_tune_freq)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.1
# - bedste performance: 0.3813161
nb3_freq <- naive_bayes(Label ~ ., model3_data_freq,
                   prior = NULL,
                   laplace = 0.01,
                   usekernel = FALSE,
                   usepoisson = TRUE)

nb4_freq <- naive_bayes(Label ~ ., model3_data_freq,
                   prior = NULL,
                   laplace = 0.01,
                   usekernel = FALSE,
                   usepoisson = TRUE)

options(scipen = 999)

# prædikteringer og afrapportering
prediction_NB1_freq <- predict(nb1_freq, prediction_model1_data_freq,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB1_freq <- as.data.frame(prediction_NB1_freq)
prediction_NB1_freq$Label <- colnames(prediction_NB1_freq)[max.col(prediction_NB1_freq,ties.method="first")]
xtab_NB1_freq <- table(prediction_model1_data_freq$Label, prediction_NB1_freq$Label)
# xtab_NB1_freq

prediction_NB2_freq <- predict(nb2_freq, prediction_model2_data_freq,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB2_freq <- as.data.frame(prediction_NB2_freq)
prediction_NB2_freq$Label <- colnames(prediction_NB2_freq)[max.col(prediction_NB2_freq,ties.method="first")]
xtab_NB2_freq <- table(prediction_model2_data_freq$Label, prediction_NB2_freq$Label)
# xtab_NB2_freq

prediction_NB3_freq <- predict(nb3_freq, prediction_model3_data_freq,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB3_freq <- as.data.frame(prediction_NB3_freq)
prediction_NB3_freq$Label <- colnames(prediction_NB3_freq)[max.col(prediction_NB3_freq,ties.method="first")]
xtab_NB3_freq <- table(prediction_model3_data_freq$Label, prediction_NB3_freq$Label)
# xtab_NB3_freq

prediction_NB4_freq <- predict(nb4_freq, prediction_model4_data_freq,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB4_freq <- as.data.frame(prediction_NB4_freq)
prediction_NB4_freq$Label <- colnames(prediction_NB4_freq)[max.col(prediction_NB4_freq,ties.method="first")]
xtab_NB4_freq <- table(prediction_model4_data_freq$Label, prediction_NB4_freq$Label)
# xtab_NB4_freq

#### 9.3 NB - Binær/Ordforekomst ####
# Træner Niave Bayes models
# Tuner model for bedre performance
# nb1_tune_bi <- tune(naiveBayes, Label~.,
                 #data=model1_data_bi,
                 #ranges = list(
                   #laplace=c(0.01, 0.1, 1, 10)))
# summary(nb1_tune_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.01
# - bedste performance: 0.0179798
nb1_bi <- naive_bayes(Label ~ ., model1_data_bi,
                   prior = NULL,
                   laplace = 0.2,
                   usekernel = FALSE,
                   usepoisson = TRUE)

# Tuner model for bedre performance
# nb2_tune_bi <- tune(naiveBayes, Label~.,
                 #data=model2_data_bi,
                 #ranges = list(
                   #laplace=c(0.01, 0.1, 1, 10)))
# summary(nb2_tune_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.01
# - bedste performance: 0.04768897
nb2_bi <- naive_bayes(Label ~ ., model2_data_bi,
                   prior = NULL,
                   laplace = 0.2,
                   usekernel = FALSE,
                   usepoisson = TRUE)

# Tuner model for bedre performance
# nb3_tune_bi <- tune(naiveBayes, Label~.,
                 #data=model3_data_bi,
                 #ranges = list(
                   #laplace=c(0.01, 0.1, 1, 10)))
# summary(nb3_tune_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.01
# - bedste performance: 0.04747742 
nb3_bi <- naive_bayes(Label ~ ., model3_data_bi,
                   prior = NULL,
                   laplace = 0.2,
                   usekernel = FALSE,
                   usepoisson = TRUE)

nb4_bi <- naive_bayes(Label ~ ., model3_data_bi,
                   prior = NULL,
                   laplace = 0.2,
                   usekernel = FALSE,
                   usepoisson = TRUE)

options(scipen = 999)

# prædiktering og afrapportering
prediction_NB1_bi <- predict(nb1_bi, prediction_model1_data_bi,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB1_bi <- as.data.frame(prediction_NB1_bi)
prediction_NB1_bi$Label <- colnames(prediction_NB1_bi)[max.col(prediction_NB1_bi,ties.method="first")]
xtab_NB1_bi <- table(prediction_model1_data_bi$Label, prediction_NB1_bi$Label)
xtab_NB1_bi

prediction_NB2_bi <- predict(nb2_bi, prediction_model2_data_bi,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB2_bi <- as.data.frame(prediction_NB2_bi)
prediction_NB2_bi$Label <- colnames(prediction_NB2_bi)[max.col(prediction_NB2_bi,ties.method="first")]
xtab_NB2_bi <- table(prediction_model2_data_bi$Label, prediction_NB2_bi$Label)
xtab_NB2_bi

prediction_NB3_bi <- predict(nb3_bi, prediction_model3_data_bi,
                          decision.values = TRUE,
                          type = "prob",
                          threshold = 0.001,
                          eps = 0)
prediction_NB3_bi <- as.data.frame(prediction_NB3_bi)
prediction_NB3_bi$Label <- colnames(prediction_NB3_bi)[max.col(prediction_NB3_bi,ties.method="first")]
xtab_NB3_bi <- table(prediction_model3_data_bi$Label, prediction_NB3_bi$Label)
xtab_NB3_bi

prediction_NB4_bi <- predict(nb4_bi, prediction_model4_data_bi,
                             decision.values = TRUE,
                             type = "prob",
                             threshold = 0.001,
                             eps = 0)
prediction_NB4_bi <- as.data.frame(prediction_NB4_bi)
prediction_NB4_bi$Label <- colnames(prediction_NB4_bi)[max.col(prediction_NB4_bi,ties.method="first")]
xtab_NB4_bi <- table(prediction_model4_data_bi$Label, prediction_NB4_bi$Label)
xtab_NB4_bi

#### 9.4 NB - Uden frequent/infrequent ord/features & Binær/Ordforekomst ####
# Træner Niave Bayes models
# Tuner model for bedre performance
# nb1_tune_freq_bi <- tune(naiveBayes, Label~.,
                      #data=model1_data_freq_bi,
                      #ranges = list(laplace=c(0.01, 0.1, 1, 10)))
# summary(nb1_tune_freq_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.01
# - bedste performance: 0.1456566
nb1_freq_bi <- naive_bayes(Label ~ ., model1_data_freq_bi,
                        prior = NULL,
                        laplace = 0.01,
                        usekernel = FALSE,
                        usepoisson = TRUE)

# Tuner model for bedre performance
# nb2_tune_freq_bi <- tune(naiveBayes, Label~.,
                      #data=model2_data_freq_bi,
                      #ranges = list(laplace=c(0.01, 0.1, 1, 10)))
# summary(nb2_tune_freq_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.1
# - bedste performance: 0.339428
nb2_freq_bi <- naive_bayes(Label ~ ., model2_data_freq_bi,
                        prior = NULL,
                        laplace = 0.01,
                        usekernel = FALSE,
                        usepoisson = TRUE)

# Tuner model for bedre performance
# nb3_tune_freq_bi <- tune(naiveBayes, Label~.,
                      #data=model3_data_freq_bi,
                      #ranges = list(laplace=c(0.01, 0.1, 1, 10)))
# summary(nb3_tune_freq_bi)
# resultater fra tuning
# - sampling-metode: 10-fold cross validation 
# - bedste parametre:laplace
#                    0.1
# - bedste performance: 0.3716968
nb3_freq_bi <- naive_bayes(Label ~ ., model3_data_freq_bi,
                        prior = NULL,
                        laplace = 0.01,
                        usekernel = FALSE,
                        usepoisson = TRUE)

nb4_freq_bi <- naive_bayes(Label ~ ., model3_data_freq_bi,
                        prior = NULL,
                        laplace = 0.01,
                        usekernel = FALSE,
                        usepoisson = TRUE)

options(scipen = 999)

# prædiktering og afrapportering
prediction_NB1_freq_bi <- predict(nb1_freq_bi, prediction_model1_data_freq_bi,
                               decision.values = TRUE,
                               type = "prob",
                               threshold = 0.001,
                               eps = 0)
prediction_NB1_freq_bi <- as.data.frame(prediction_NB1_freq_bi)
prediction_NB1_freq_bi$Label <- colnames(prediction_NB1_freq_bi)[max.col(prediction_NB1_freq_bi,ties.method="first")]
xtab_NB1_freq_bi <- table(prediction_model1_data_freq_bi$Label, prediction_NB1_freq_bi$Label)
# xtab_NB1_freq_bi

prediction_NB2_freq_bi <- predict(nb2_freq_bi, prediction_model2_data_freq_bi,
                               decision.values = TRUE,
                               type = "prob",
                               threshold = 0.001,
                               eps = 0)
prediction_NB2_freq_bi <- as.data.frame(prediction_NB2_freq_bi)
prediction_NB2_freq_bi$Label <- colnames(prediction_NB2_freq_bi)[max.col(prediction_NB2_freq_bi,ties.method="first")]
xtab_NB2_freq_bi <- table(prediction_model2_data_freq_bi$Label, prediction_NB2_freq_bi$Label)
# xtab_NB2_freq_bi

prediction_NB3_freq_bi <- predict(nb3_freq_bi, prediction_model3_data_freq_bi,
                               decision.values = TRUE,
                               type = "prob",
                               threshold = 0.001,
                               eps = 0)
prediction_NB3_freq_bi <- as.data.frame(prediction_NB3_freq_bi)
prediction_NB3_freq_bi$Label <- colnames(prediction_NB3_freq_bi)[max.col(prediction_NB3_freq_bi,ties.method="first")]
xtab_NB3_freq_bi <- table(prediction_model3_data_freq_bi$Label, prediction_NB3_freq_bi$Label)
# xtab_NB3_freq_bi

prediction_NB4_freq_bi <- predict(nb4_freq_bi, prediction_model4_data_freq_bi,
                               decision.values = TRUE,
                               type = "prob",
                               threshold = 0.001,
                               eps = 0)
prediction_NB4_freq_bi <- as.data.frame(prediction_NB4_freq_bi)
prediction_NB4_freq_bi$Label <- colnames(prediction_NB4_freq_bi)[max.col(prediction_NB4_freq_bi,ties.method="first")]
xtab_NB4_freq_bi <- table(prediction_model4_data_freq_bi$Label, prediction_NB4_freq_bi$Label)
# xtab_NB4_freq_bi

#### 10. Optimerede og tilpassede modeller ####
#### 6.3.1 Udviklet modeller der alene anvender empiriske ledelsesdokumenter og IKKE TEORETISKE SNIPPETS ####
#### NB_opt_5_1
xxx_traning <- subset(dfm_df, Subtype_num == 2)
xxx_prediction <- subset(dfm_df, Subtype_num == 5)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_nb <- naive_bayes(Label ~ ., xxx_traning,
                      prior = NULL,
                      laplace = 0.01,
                      usekernel = FALSE,
                      usepoisson = TRUE)
xxx_prediction_NB <- predict(xxx_nb, xxx_prediction,
                             decision.values = TRUE,
                             type = "prob",
                             threshold = 0.001,
                             eps = 0)
xxx_prediction_NB <- as.data.frame(xxx_prediction_NB)
xxx_prediction_NB$Label <- colnames(xxx_prediction_NB)[max.col(xxx_prediction_NB,ties.method="first")]
NB_opt_5_1_xtab <- table(xxx_prediction$Label, xxx_prediction_NB$Label)
NB_opt_5_1_xtab

#### SVM_opt_5_1
xxx_traning <- subset(dfm_df, Subtype_num == 2)
xxx_prediction <- subset(dfm_df, Subtype_num==5)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_svm <- svm(Label~., data=xxx_traning, 
               type="C-classification",
               kernel="radial", 
               probability = TRUE,
               gamma=0.1,
               cost=10)
xxx_prediction_SVM <- predict(xxx_svm, xxx_prediction,
                              decision.values = TRUE,
                              #na.action = na.omit,
                              probability = TRUE)
SVM_opt_5_1_xtab <- table(xxx_prediction$Label, xxx_prediction_SVM)
SVM_opt_5_1_xtab

#### NB_opt_5_2
xxx_traning <- subset(dfm_df, Subtype_num == 2 | Subtype_num == 3)
xxx_prediction <- subset(dfm_df, Subtype_num == 6)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_nb <- naive_bayes(Label ~ ., xxx_traning,
                      prior = NULL,
                      laplace = 0.01,
                      usekernel = FALSE,
                      usepoisson = TRUE)
xxx_prediction_NB <- predict(xxx_nb, xxx_prediction,
                             decision.values = TRUE,
                             type = "prob",
                             threshold = 0.001,
                             eps = 0)
xxx_prediction_NB <- as.data.frame(xxx_prediction_NB)
xxx_prediction_NB$Label <- colnames(xxx_prediction_NB)[max.col(xxx_prediction_NB,ties.method="first")]
NB_opt_5_2_xtab <- table(xxx_prediction$Label, xxx_prediction_NB$Label)
NB_opt_5_2_xtab

#### SVM_opt_5_2
xxx_traning <- subset(dfm_df, Subtype_num == 2 | Subtype_num == 3)
xxx_prediction <- subset(dfm_df, Subtype_num == 6)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_svm <- svm(Label~., data=xxx_traning, 
               type="C-classification",
               kernel="radial", 
               probability = TRUE,
               gamma=0.1,
               cost=10)
xxx_prediction_SVM <- predict(xxx_svm, xxx_prediction,
                              decision.values = TRUE,
                              #na.action = na.omit,
                              probability = TRUE)
SVM_opt_5_2_xtab <- table(xxx_prediction$Label, xxx_prediction_SVM)
SVM_opt_5_2_xtab

#### NB_opt_5_3
xxx_traning <- subset(dfm_df, Subtype_num == 2 | Subtype_num == 3)
xxx_prediction <- subset(dfm_df, Subtype_num == 4 | Subtype_num == 5| Subtype_num == 6)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_nb <- naive_bayes(Label ~ ., xxx_traning,
                      prior = NULL,
                      laplace = 0.01,
                      usekernel = FALSE,
                      usepoisson = TRUE)
xxx_prediction_NB <- predict(xxx_nb, xxx_prediction,
                             decision.values = TRUE,
                             type = "prob",
                             threshold = 0.001,
                             eps = 0)
xxx_prediction_NB <- as.data.frame(xxx_prediction_NB)
xxx_prediction_NB$Label <- colnames(xxx_prediction_NB)[max.col(xxx_prediction_NB,ties.method="first")]
NB_opt_5_3_xtab <- table(xxx_prediction$Label, xxx_prediction_NB$Label)
NB_opt_5_3_xtab

#### SVM_opt_5_3
xxx_traning <- subset(dfm_df, Subtype_num == 2 | Subtype_num == 3)
xxx_prediction <- subset(dfm_df, Subtype_num==4 | Subtype_num==5 |Subtype_num == 6)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_svm <- svm(Label~., data=xxx_traning, 
               type="C-classification",
               kernel="radial", 
               probability = TRUE,
               gamma=0.1,
               cost=10)
xxx_prediction_SVM <- predict(xxx_svm, xxx_prediction,
                              decision.values = TRUE,
                              #na.action = na.omit,
                              probability = TRUE)
SVM_opt_5_3_xtab <- table(xxx_prediction$Label, xxx_prediction_SVM)
SVM_opt_5_3_xtab

#### 6.3.2 Udviklet modeller der anvender samme analyseniveau - DOKUMENT-DOKUMENT ####
#### NB_opt_6_1
xxx_traning <- subset(dfm_df, Subtype_num == 4)
xxx_prediction <- subset(dfm_df, Subtype_num == 5)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_nb <- naive_bayes(Label ~ ., xxx_traning,
                      prior = NULL,
                      laplace = 0.01,
                      usekernel = FALSE,
                      usepoisson = TRUE)
xxx_prediction_NB <- predict(xxx_nb, xxx_prediction,
                             decision.values = TRUE,
                             type = "prob",
                             threshold = 0.001,
                             eps = 0)
xxx_prediction_NB <- as.data.frame(xxx_prediction_NB)
xxx_prediction_NB$Label <- colnames(xxx_prediction_NB)[max.col(xxx_prediction_NB,ties.method="first")]
NB_opt_6_1_xtab <- table(xxx_prediction$Label, xxx_prediction_NB$Label)
NB_opt_6_1_xtab

#### SVM_opt_6_1
xxx_traning <- subset(dfm_df, Subtype_num == 4)
xxx_prediction <- subset(dfm_df, Subtype_num == 5)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_svm <- svm(Label~., data=xxx_traning, 
               type="C-classification",
               kernel="radial", 
               probability = TRUE,
               gamma=0.1,
               cost=10)
xxx_prediction_SVM <- predict(xxx_svm, xxx_prediction,
                              decision.values = TRUE,
                              #na.action = na.omit,
                              probability = TRUE)
SVM_opt_6_1_xtab <- table(xxx_prediction$Label, xxx_prediction_SVM)
SVM_opt_6_1_xtab

#### NB_opt_6_2
xxx_traning <- subset(dfm_df, Subtype_num == 4 | Subtype_num == 5)
xxx_prediction <- subset(dfm_df, Subtype_num == 6)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_nb <- naive_bayes(Label ~ ., xxx_traning,
                      prior = NULL,
                      laplace = 0.01,
                      usekernel = FALSE,
                      usepoisson = TRUE)
xxx_prediction_NB <- predict(xxx_nb, xxx_prediction,
                             decision.values = TRUE,
                             type = "prob",
                             threshold = 0.001,
                             eps = 0)
xxx_prediction_NB <- as.data.frame(xxx_prediction_NB)
xxx_prediction_NB$Label <- colnames(xxx_prediction_NB)[max.col(xxx_prediction_NB,ties.method="first")]
NB_opt_6_2_xtab <- table(xxx_prediction$Label, xxx_prediction_NB$Label)
NB_opt_6_2_xtab

#### SVM_opt_6_2
xxx_traning <- subset(dfm_df, Subtype_num == 4 | Subtype_num == 5)
xxx_prediction <- subset(Subtype_num == 6)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_svm <- svm(Label~., data=xxx_traning, 
               type="C-classification",
               kernel="radial", 
               probability = TRUE,
               gamma=0.1,
               cost=10)
xxx_prediction_SVM <- predict(xxx_svm, xxx_prediction,
                              decision.values = TRUE,
                              #na.action = na.omit,
                              probability = TRUE)
SVM_opt_6_2_xtab <- table(xxx_prediction$Label, xxx_prediction_SVM)
SVM_opt_6_2_xtab

#### 6.3.3 Udviklet modeller der anvender samme analyseniveau - SÆTNING-SÆTNING ####
#### NB_opt_7_1
xxx_traning <- subset(dfm_df, Subtype_num == 1)
xxx_prediction <- subset(dfm_df, Subtype_num == 2)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_nb <- naive_bayes(Label ~ ., xxx_traning,
                      prior = NULL,
                      laplace = 0.01,
                      usekernel = FALSE,
                      usepoisson = TRUE)
xxx_prediction_NB <- predict(xxx_nb, xxx_prediction,
                             decision.values = TRUE,
                             type = "prob",
                             threshold = 0.001,
                             eps = 0)
xxx_prediction_NB <- as.data.frame(xxx_prediction_NB)
xxx_prediction_NB$Label <- colnames(xxx_prediction_NB)[max.col(xxx_prediction_NB,ties.method="first")]
NB_opt_7_1_xtab <- table(xxx_prediction$Label, xxx_prediction_NB$Label)
NB_opt_7_1_xtab

#### SVM_opt_7_1
xxx_traning <- subset(dfm_df, Subtype_num == 1)
xxx_prediction <- subset(dfm_df, Subtype_num == 2)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_svm <- svm(Label~., data=xxx_traning, 
               type="C-classification",
               kernel="radial", 
               probability = TRUE,
               gamma=0.1,
               cost=10)
xxx_prediction_SVM <- predict(xxx_svm, xxx_prediction,
                              decision.values = TRUE,
                              #na.action = na.omit,
                              probability = TRUE)
SVM_opt_7_1_xtab <- table(xxx_prediction$Label, xxx_prediction_SVM)
SVM_opt_7_1_xtab

#### NB_opt_7_2
xxx_traning <- subset(dfm_df, Subtype_num == 1 | Subtype_num == 2)
xxx_prediction <- subset(dfm_df, Subtype_num == 3)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_nb <- naive_bayes(Label ~ ., xxx_traning,
                      prior = NULL,
                      laplace = 0.01,
                      usekernel = FALSE,
                      usepoisson = TRUE)
xxx_prediction_NB <- predict(xxx_nb, xxx_prediction,
                             decision.values = TRUE,
                             type = "prob",
                             threshold = 0.001,
                             eps = 0)
xxx_prediction_NB <- as.data.frame(xxx_prediction_NB)
xxx_prediction_NB$Label <- colnames(xxx_prediction_NB)[max.col(xxx_prediction_NB,ties.method="first")]
NB_opt_7_2_xtab <- table(xxx_prediction$Label, xxx_prediction_NB$Label)
NB_opt_7_2_xtab

#### SVM_opt_7_2
xxx_traning <- subset(dfm_df, Subtype_num == 1 | Subtype_num == 2)
xxx_prediction <- subset(dfm_df, Subtype_num == 3)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_svm <- svm(Label~., data=xxx_traning, 
               type="C-classification",
               kernel="radial", 
               probability = TRUE,
               gamma=0.1,
               cost=10)
xxx_prediction_SVM <- predict(xxx_svm, xxx_prediction,
                              decision.values = TRUE,
                              #na.action = na.omit,
                              probability = TRUE)
SVM_opt_7_2_xtab <- table(xxx_prediction$Label, xxx_prediction_SVM)
SVM_opt_7_2_xtab

#### NB_opt_7_3
xxx_traning <- subset(dfm_df, Subtype_num == 1 | Subtype_num == 2| Subtype_num == 3)
xxx_prediction <- subset(dfm_df, Subtype_num == 7)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_nb <- naive_bayes(Label ~ ., xxx_traning,
                      prior = NULL,
                      laplace = 0.01,
                      usekernel = FALSE,
                      usepoisson = TRUE)
xxx_prediction_NB <- predict(xxx_nb, xxx_prediction,
                             decision.values = TRUE,
                             type = "prob",
                             threshold = 0.001,
                             eps = 0)
xxx_prediction_NB <- as.data.frame(xxx_prediction_NB)
xxx_prediction_NB$Label <- colnames(xxx_prediction_NB)[max.col(xxx_prediction_NB,ties.method="first")]
NB_opt_7_3_xtab <- table(xxx_prediction$Label, xxx_prediction_NB$Label)
NB_opt_7_3_xtab

#### SVM_opt_7_3
xxx_traning <- subset(dfm_df, Subtype_num == 1 | Subtype_num == 2| Subtype_num == 3)
xxx_prediction <- subset(dfm_df, Subtype_num == 7)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_svm <- svm(Label~., data=xxx_traning, 
               type="C-classification",
               kernel="radial", 
               probability = TRUE,
               gamma=0.1,
               cost=10)
xxx_prediction_SVM <- predict(xxx_svm, xxx_prediction,
                              decision.values = TRUE,
                              #na.action = na.omit,
                              probability = TRUE)
SVM_opt_7_3_xtab <- table(xxx_prediction$Label, xxx_prediction_SVM)
SVM_opt_7_3_xtab

#### NB_opt_7_4
xxx_traning <- subset(dfm_df, Subtype_num == 2)
xxx_prediction <- subset(dfm_df, Subtype_num == 3)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_nb <- naive_bayes(Label ~ ., xxx_traning,
                      prior = NULL,
                      laplace = 0.01,
                      usekernel = FALSE,
                      usepoisson = TRUE)
xxx_prediction_NB <- predict(xxx_nb, xxx_prediction,
                             decision.values = TRUE,
                             type = "prob",
                             threshold = 0.001,
                             eps = 0)
xxx_prediction_NB <- as.data.frame(xxx_prediction_NB)
xxx_prediction_NB$Label <- colnames(xxx_prediction_NB)[max.col(xxx_prediction_NB,ties.method="first")]
NB_opt_7_4_xtab <- table(xxx_prediction$Label, xxx_prediction_NB$Label)
NB_opt_7_4_xtab

#### SVM_opt_7_4
xxx_traning <- subset(dfm_df, Subtype_num == 2)
xxx_prediction <- subset(dfm_df, Subtype_num == 3)
xxx_traning <- xxx_traning[,!(names(xxx_traning) %in% drop_columns)]
xxx_prediction <- xxx_prediction[,!(names(xxx_prediction) %in% drop_columns)]
xxx_svm <- svm(Label~., data=xxx_traning, 
               type="C-classification",
               kernel="radial", 
               probability = TRUE,
               gamma=0.1,
               cost=10)
xxx_prediction_SVM <- predict(xxx_svm, xxx_prediction,
                              decision.values = TRUE,
                              #na.action = na.omit,
                              probability = TRUE)
SVM_opt_7_4_xtab <- table(xxx_prediction$Label, xxx_prediction_SVM)
SVM_opt_7_4_xtab