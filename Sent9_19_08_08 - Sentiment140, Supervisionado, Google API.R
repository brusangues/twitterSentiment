### Analise de Sentimento no Twitter nas Eleicoes de 2018
### Codigo escrito por Bruno Sanches Rodrigues
### UFABC 2018 Projeto financiado pelo CNPq
### Documentacao do rtweet https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

### Realizar análise de sentimento por meio de léxicos em pequenos conjuntos de tweets
### Fonte: https://analyzecore.com/2017/02/08/twitter-sentiment-analysis-doc2vec/
# https://console.cloud.google.com/apis/credentials?project=our-chess-249400
# https://console.developers.google.com/apis/api/language.googleapis.com/overview?project=our-chess-249400
# https://console.developers.google.com/apis/api/translate.googleapis.com/overview?project=our-chess-249400

#setwd("C:/Users/Bruno/Desktop/TwitterData/")
#setwd("C:/Users/Bruno-NB/Desktop/TwitterData")

if(F){
  library(tidyverse) # pq nao da pra viver sem
  library(ggExtra)
  library(magrittr) # <3
  library(lubridate)
  library(stringr) # essencial para trabalhar com textos
  library(tidytext) # um dos melhores pacotes para text mining
  library(lexiconPT)
  library(tm)
  library(qdapRegex)
  library(RColorBrewer)
  library(ggplot2)
  library(wordcloud)
  library(tidytext)
  library(quanteda)
  library(ggplot2)
  library(scales)
  library(tidyr)
  library(reshape2)
  library(wordcloud)
  library(SentimentAnalysis)
} #Bibliotecas antigas

# Pacotes
     {
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(purrrlyr)))
suppressWarnings(suppressMessages(library(text2vec)))
suppressWarnings(suppressMessages(library(caret)))
suppressWarnings(suppressMessages(library(glmnet)))
suppressWarnings(suppressMessages(library(ggrepel)))
}

#############################################################################################
# Treinando Classificador
if(F){
### loading and preprocessing a training set of tweets
# function for converting some symbols
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

##### loading classified tweets ######
# source: http://help.sentiment140.com/for-students/
# 0 - the polarity of the tweet (0 = negative, 4 = positive)
# 1 - the id of the tweet
# 2 - the date of the tweet
# 3 - the query. If there is no query, then this value is NO_QUERY.
# 4 - the user that tweceted
# 5 - the text of the tweet

tweets_classified <- read_csv('training.1600000.processed.noemoticon.csv', col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text')) %>%
  # Small Subsample
  head(1000)
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))
# there are some tweets with NA ids that we replace with dummies
tweets_classified_na <- tweets_classified %>%
  filter(is.na(id) == TRUE) %>%
  mutate(id = list(1:n()))

tweets_classified <- tweets_classified %>%
  filter(!is.na(id)) %>%
  rbind(., tweets_classified_na)

# data splitting on train and test
set.seed(2340)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]

##### Vectorization #####
# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   ids = tweets_train$id,
                   progressbar = TRUE)
it_test <- itoken(tweets_test$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = tweets_test$id,
                  progressbar = TRUE)

# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
# define tf-idf model
tfidf <- TfIdf$new()
# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
# apply pre-trained tf-idf transformation to test data
dtm_test_tfidf  <- create_dtm(it_test, vectorizer) %>% 
  transform(tfidf)

# train the model
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf,
                               y = tweets_train[['sentiment']], 
                               family = 'binomial', 
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 5-fold cross-validation
                               nfolds = 5,
                               # high value is less accurate, but has faster training
                               thresh = 1e-3,
                               # again lower number of iterations for faster training
                               maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
auc(as.numeric(tweets_test$sentiment), preds)

# save the objects for future using
rm(list = setdiff(ls(), c('glmnet_classifier', 'conv_fun', 'prep_fun', 'tok_fun', 'vectorizer', 'tfidf')))
save.image('image.RData')
rm(list = ls())
}

#############################################################################################
# Conjunto de teste próprio

    load('image.RData')
    # Abrindo tweets coletados
    df_tweets2 <- readRDS("rt_segunda_10_25-10_27.rds")
    
    # Selecionando Tweets contendo bolsonaro
    df_tweets2 <- df_tweets2[grep('bolsonaro', df_tweets2$text, ignore.case = T),]
    
    # Selecionando apenas 1000 tweets
    df_tweets2 <- df_tweets2[1:100,]

    #Renomeando colunas para ficar compatível com twitteR
    colnames(df_tweets2)[colnames(df_tweets2) == "user_id"] <- "id"
    colnames(df_tweets2)[colnames(df_tweets2) == "created_at"] <- "created"
    colnames(df_tweets2)[colnames(df_tweets2) == "text"] <- "raw"
    
    #Retirando o x da coluna ID
    df_tweets2$id <- gsub("x", "", df_tweets2$id)
    
    #Adicionando coluna de índices
    df_tweets2$i <- 1:nrow(df_tweets2)

# Tradução usando Google API
    
    #Função para preprocessamento do texto
    f_prep <- function(tweetxtUtf){
      #tweetxtUtf<- tolower(tweetxtUtf)                 #Caixa baixa
      tweetxtUtf <- rm_url(tweetxtUtf)                  #URLs e mais alguns símbolos inúteis
      tweetxtUtf <- gsub("?<[u|U].*f>", "", tweetxtUtf) #<U+009F>
      tweetxtUtf <- gsub("?<f0>", "", tweetxtUtf)       #<f0>
      #tweetxtUtf <- rm_hash(tweetxtUtf)                 #Twitter Hash Tags
      #tweetxtUtf <- rm_tag(tweetxtUtf)                  #Twitter Tags ou arrobas
      tweetxtUtf <- gsub("[[:punct:]]", "", tweetxtUtf) #Pontuação e caracteres especiais
      tweetxtUtf <- gsub("[ |\t]{2,}", " ", tweetxtUtf) #Tabs ou espaços duplos
      tweetxtUtf <- gsub("^ ", "", tweetxtUtf)          #Espaço no início da string
      tweetxtUtf <- gsub(" $", "", tweetxtUtf)          #Espaço no fim da string
      tweetxtUtf <- gsub("[^[:alnum:][:blank:]]", " ", tweetxtUtf)
      tweetxtUtf <- gsub("[[:digit:]]", "", tweetxtUtf)
      tweetxtUtf <- gsub("k{3,}", "", tweetxtUtf)       #Strings de apenas 'k', com mais de 2 letras
      
      tweetxtUtfUnique <- tweetxtUtf %>% unique() #Removendo tweets repetidos
      
      tibble(tweetxtUtfUnique) #Visualizando
      return(tweetxtUtfUnique)
}

    #install.packages("googleLanguageR")
    library(googleLanguageR)
    
    #Autorização usando arquivo json
    gl_auth("My First Project-62a1927a1a87.json")
    
    #Tradução em coluna separada
    translation <- gl_translate(df_tweets2$raw, source = "pt", target = "en")
    df_tweets2$text <- translation$translatedText
    rm(translation)
    
# Sentimento usando Google API
    
    #Criando df separado com text e ID
    nlptext <- df_tweets2[,c("i", "text")]
    
    #Selecionando linhas com mais de 20 palavras
    nlptext <- nlptext[lengths(gregexpr("\\s+", nlptext$text))>20,]
    
    #Análise de sentimento, resulta em uma lista
    t1 <- Sys.time()
    nlp <- gl_nlp(nlptext$text, language = "en")
    print(difftime(Sys.time(), t1, units = 'mins'))
    
#Agregando nos dataframes
    nlptext <- cbind(nlptext,
        gsentiment = nlp[["documentSentiment"]][["score"]])
    
    #Merging
    df_tweets2 <- merge(df_tweets2, nlptext, all.x = T, sort = TRUE)
    
    #Ordenando pelo I depois do merge
    df_tweets2 <- df_tweets2[order(df_tweets2$i),]

#Finalização
    df_tweets <- df_tweets2

#############################################################################################
# Conjunto de teste padrão
# fetching some tweets
if(F){
library(twitteR)
library(ROAuth)
download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              destfile = "cacert.pem")
setup_twitter_oauth("mk64zKzYZX80ETCz5AeYxepqW", # api key
                    "a993kwnC15CizKmDx1ymJEZExWdGqoILBoHYedrbwqTuiVMnVE", # api secret
                    "417475374-ApAzScCMc8AfUO0rgrwzy0eu3pjayhrzAnLP1qZ2", # access token
                    "LVvsp3hDksH6Gy5eGT6h4FLSwTjgW7x8RAdJzgFkCQSD5" # access token secret
)

df_tweets <- twListToDF(searchTwitter('setapp OR #setapp', n = 1000, lang = 'en')) %>%
  # converting some symbols
  dmap_at('text', conv_fun)
}

##########################################################################################
# Análise de sentimento supervisionada

    # preprocessing and tokenization
    it_tweets <- itoken(df_tweets$text,
                        preprocessor = prep_fun,
                        tokenizer = tok_fun,
                        ids = df_tweets$id,
                        progressbar = TRUE)
    
    # creating vocabulary and document-term matrix
    dtm_tweets <- create_dtm(it_tweets, vectorizer)
    
    # transforming data with tf-idf
    dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)
    
    # predict probabilities of positiveness
    preds_tweets <- predict(glmnet_classifier, dtm_tweets_tfidf, type = 'response')[ ,1]
    
    # adding rates to initial dataset
    df_tweets$tsentiment <- preds_tweets

    

##########################################################################################
# Resultados
    # Selecionando análise pelo google ou pelo classificador local
    df_tweets$sentiment <- df_tweets$tsentiment
    #df_tweets$sentiment <- (df_tweets$gsentiment+1)/2
    
    # color palette
    cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")
    
    set.seed(932)
    samp_ind <- sample(c(1:nrow(df_tweets)), nrow(df_tweets) * 0.01) # 10% for labeling
    
    # plotting
    ggplot(df_tweets, aes(x = created, y = sentiment, color = sentiment)) +
      theme_minimal() +
      scale_color_gradientn(colors = cols, limits = c(0, 1),
                            breaks = seq(0, 1, by = 1/4),
                            labels = c("0", round(1/4*1, 1), round(1/4*2, 1), round(1/4*3, 1), round(1/4*4, 1)),
                            guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
      geom_point(aes(color = sentiment), alpha = 0.8) +
      geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
      geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
      geom_smooth(size = 1.2, alpha = 0.2) +
      geom_label_repel(data = df_tweets[samp_ind, ],
                       aes(label = round(sentiment, 2)),
                       fontface = 'bold',
                       size = 2.5,
                       max.iter = 100) +
      theme(legend.position = 'bottom',
            legend.direction = "horizontal",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
            axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
      ggtitle("Sentimento Sentiment140 - Bolsonaro(probabilidade de positividade)")
