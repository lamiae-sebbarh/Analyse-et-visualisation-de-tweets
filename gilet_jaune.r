library(NLP)
library(tm)
library(twitteR)
library(stringi)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)



##################################
# Connection to Twitter
##################################
setup_twitter_oauth(consumer_key = "***************",
                    consumer_secret = "***************",
                    access_token = "***************",
                    access_secret = "***************")

#############################################################
# Recherche de Tweets sur Twitter avec le(s) mot(s) clé(s)
#############################################################
tweets <- searchTwitter("#giletsjaunes", n = 500, lang = "fr", 
                        resultType = "mixed", since = "2018-12-01")

############################################################
# Transformer la liste de tweets sous forme de dataFrame (tableau)
###########################################################

tweets_df <- twListToDF(tweets) # cette fonction va permettre de transformer la liste 
#de tweets extraits en un "dataframe" (tableau)
tweets_df
############################################################
# Écrire les tweets dans un fichier CSV 
###########################################################
write.csv2(tweets_df, 
           file = "D:/R/tweets-gilletjaune.csv", row.names = FALSE)

############################################################
# Lire les tweets depuis un fichier CSV 
###########################################################
tweets_table <- read.table(file = "D:/R/tweets-gilletjaune.txt", 
                           header=TRUE, 
                           sep='\t')

####################################################################
# Mise en minuscules de la colonne Text du fichier Tweets.CSV 
##########################################################################
tweets_text=tweets_df$text

#tweets_text <- stri_trans_general(tweets_df$text, "lower") 

#Construire le corpus de tweets
tweets_corpus <- VCorpus(VectorSource(tweets_text))

# Pré-trairtement (nettoyage du corpus)
tweets_corpus <- tm_map(tweets_corpus, removePunctuation) # ici cela va supprimer automatiquement tous les caractères de ponctuation
tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("fr")) # ici cela va supprimer automatiquement une bonne partie des mots français "basiques", tels que "le", "la", etc. mais il en manque ! Il manque par exemple "les"...
tweets_corpus <- tm_map(tweets_corpus, stripWhitespace)

# Construire la matrice de fréquence : termes/Documents
dtm <- TermDocumentMatrix(tweets_corpus)
dtm
m <- as.matrix(dtm)
m

# Visualisation 
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v),freq = v)
head(d, 10) 
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Mots les plus fréquents",
        ylab = "Fréquences")

##############################################################
###################### # Visualisation  sous forme de tag cloud
#############################################################

set.seed(100)
wordcloud(words=d$word, freq = d$freq, min.freq = 1, max.word = 100, 
          random.order=FALSE, colors=brewer.pal(8,"Dark2"))


