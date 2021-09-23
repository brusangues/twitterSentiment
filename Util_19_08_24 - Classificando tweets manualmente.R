### Analise de Sentimento no Twitter nas Eleicoes de 2018
### Codigo escrito por Bruno Sanches Rodrigues
### UFABC 2018 Projeto financiado pelo CNPq
### Documentacao do rtweet https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

### Plota dados de pesquisas de opinião
### Dados Datafolha
### file:///C:/Users/Bruno/Google%20Drive/IC/Surveys/10_02_Datafolha_10_03b.pdf
### file:///C:/Users/Bruno/Google%20Drive/IC/Surveys/10_2425_Datafolha_10_26.pdf
### https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html

#setwd("C:/Users/Bruno/Desktop/TwitterData/")
#setwd("C:/Users/Bruno-NB/Desktop/TwitterData")

#  Bibliotecas
{ 
  library(tidyverse)
  library(utils)
  library(reticulate)
  library(RJSONIO)
  library(rjson)
  library(jsonlite)
}

#Testando funções em A
  #Perguntando se está no pc ou note
  machine <- winDialog("yesno", "Desktop?")
  if (machine == "YES") {machine <- 1} else machine <- 2
  
  #Carregando funções e definindo o work enviroment
  path <- c("C:/Users/Bruno/", "C:/Users/Bruno-NB/")
  setwd(paste(path[machine], "Desktop/TwitterData", sep = ""))
  source(paste(path[machine], "Google Drive/IC/RCode/SentB_19_08_16-Function,LexicalAnalysis,LDA.R", sep = ""))
  
# Arquivos gerados
  files <- c(
    "rt_from2018-09-08_to2018-09-20.rds",
    "rt_from2018-09-21_to2018-10-04.rds",    
    "rt_from2018-10-05_to2018-10-11.rds",
    "rt_from2018-10-12_to2018-10-18.rds",
    "rt_from2018-10-19_to2018-10-26.rds",
    "rt_from2018-10-27_to2018-10-31.rds",   
    "rt_from2018-11-01_to2018-11-05.rds")


# Iniciando contador
  ntweets_total <- 0
  
#Retirando sample por fração
if (F){
  t9 <- Sys.time()
  print(t9)
  rm("tweets_sample")
  for (j in 1:length(files)){
    #Carregando tweets
    file <- files[j]
    tweets <- readRDS(file)
    #Mostrando números
    print(file)
    print(nrow(tweets))
    #print(nrow(unique(tweets)))
    
    #Somando
    ntweets_total <- ntweets_total + nrow(tweets)
    print(ntweets_total)
    #Tirando Sample
    if (!any(grepl("tweets_sample",ls()))) {
      tweets_sample <- tweets[sample(nrow(tweets), nrow(tweets)/5000),]
      print("tweets_sample created")
      print(nrow(tweets_sample))
    }else{
      tweets_sample <- rbind(tweets_sample, 
                             tweets[sample(nrow(tweets), nrow(tweets)/5000),])
      print("tweets_sample binded")
      print(nrow(tweets_sample))
    }

  }
  # Mostrando tempo final
  print(difftime(Sys.time(), t9, units = 'mins'))
  saveRDS(tweets_sample, file = "tweets_sample_1000.rds")
  #tweets_sample_1000 <- readRDS("tweets_sample_1000.rds")
}
  
#Retirando sample por candidato
if (F){
    # Parametros
    input_duplicate <- 2
    
    # Iniciando Loop Principal
    t9 <- Sys.time()
    print(t9)
    for (j in 1:length(files)){
      #Carregando tweets
      file <- readRDS(files[j])
      #Selecionando datas
      from <- substring(files[j], 8,17)
      to   <- substring(files[j], 21,30)
      dates_seq <- seq(as.Date(from), as.Date(to), "days")
      
      #Chamando Funções
      t0 <- Sys.time()
      print(t0)
      for (i in 1:length(dates_seq)){
        # Selecionando dia
        day <- dates_seq[i]
        print(paste("day",day))    
        # Salvando tempo inicial
        t1 <- Sys.time()
        print(t1)
        # Chamando análise léxica
        output <- f_subsample(file, day, input_duplicate)
        output <- cbind(output, created_at = rep(day, nrow(output)))
        
        # Número de palavras total
        print(paste("nrow(output)",nrow(output)))
        
        # Salvando no dataframe adequado
        if (!any(grepl("tweets_sample_bydate",ls()))) {
          tweets_sample_bydate <- as.data.frame(output)
          print("tweets_sample_bydate created")
        }else{
          tweets_sample_bydate <- rbind.data.frame(tweets_sample_bydate, output)
          print("tweets_sample_bydate binded")
        }
        
        # Mostrando tempo final
        print(difftime(Sys.time(), t1, units = 'mins'))
      }
      # Mostrando tempo final
      print(difftime(Sys.time(), t0, units = 'mins'))
      print(paste("nrow", nrow(tweets_sample_bydate)))
    }
    # Mostrando tempo final
    print(difftime(Sys.time(), t9, units = 'mins'))
    
    # Salvando
    saveRDS(tweets_sample_bydate, file=paste(Sys.Date(),"_tweets_sample_bydate.rds", sep=""))
    print("tweets_sample_bydate saved")
    
    tweets_sample_bydate <- readRDS("2019-08-26_tweets_sample_bydate.rds")
    l <- nrow(tweets_sample_bydate)
    tweets_sample  <- tweets_sample_bydate[sample(l, 200),]
    l <- nrow(tweets_sample)
}
    
# Classificação manual
{
    tweets_sample <- readRDS("2019-08-26_tweets_sample_manual_lexicon.rds")
    l <- nrow(tweets_sample)
    
    # Ordenando
    tweets_sample <- tweets_sample[order(tweets_sample[,89], tweets_sample[,90]), ]
    
    # Criando coluna de sentimento
    #tweets_sample[,91] <- rep(NA,l)
    #tweets_sample[,92] <- rep(NA,l)
    
    cat("1=Positivo; 2=Negativo; 0=Neutro/ambíguo")
    i=84
    for (i in i:l){
      i_name <- as.character(tweets_sample[i,89])
      i_date <- as.character(tweets_sample[i,90])
      i_text <- tweets_sample[i,02]
      
      cat(i, i_name, i_date, i_text, sep="|")
      i_sentiment <- readline(prompt="Sentiment: ")
      
      if (i_sentiment==""){
        cat("Review please:")
        View(tweets_sample[i,])
        i_sentiment <- readline(prompt="Sentiment: ")
      }
      
      tweets_sample[i,91] <- i_sentiment
      if      (i_sentiment==1){
        tweets_sample[i,92] <- "Positivo"
      }else if(i_sentiment==2){
        tweets_sample[i,92] <- "Negativo"
      }else{
        tweets_sample[i,92] <- "Neutro"
      }
      
      saveRDS(tweets_sample, file=paste(Sys.Date(),"_tweets_sample_manual_lexicon.rds", sep=""))
      print("tweets_sample_manual_lexicon saved")
      
    }
    
    # Salvando
    saveRDS(tweets_sample, file=paste(Sys.Date(),"_tweets_sample_manual_lexicon.rds", sep=""))
    print("tweets_sample_manual_lexicon saved")
}

# Classificação algoritmo
{
    tweets_sample <- tweets_test
    l <- nrow(tweets_sample)
    
    #tweets_sample[,94] <- rep(NA,l)
    #tweets_sample[,95] <- rep(NA,l)
    #rm(sample_sentiments)
    i=1
    for (i in i:196){
      cat("i=", i, "\n")
      tweets_sample[i,95] <- f_prep(tweets_sample[i,2])
      #output <- f_lexicalbysample(tweets_sample[i,2])
      output <- f_lexicalbysample(paste(tweets_sample[i,2], "bom", "ruim", sep=" "))
      
      
      # Salvando no dataframe adequado
      if (!any(grepl("sample_sentiments",ls()))) {
        sample_sentiments <- as.data.frame(output)
        cat("sentiments created\n")
      }else{
        sample_sentiments <- rbind.data.frame(sample_sentiments, output)
        cat("sentiments binded\n")
      }
      
      if(output$score>0){
        tweets_sample[i,94] <- "Positivo"
      }else if(output$score<0){
        tweets_sample[i,94] <- "Negativo"
      }else{
        tweets_sample[i,94] <- "Neutro"
      }
      
    }
    # Salvando
    tweets_sample_manual_lexicon <- tweets_sample[1:196,]
    sample_sentiments <- sample_sentiments[1:196,]
    
    saveRDS(tweets_sample_manual_lexicon, file=paste(Sys.Date(),"_tweets_sample_manual_lexicon.rds", sep=""))
    saveRDS(sample_sentiments, file=paste(Sys.Date(),"_sample_sentiments_2.rds", sep=""))
    print("All saved")
}

View(tweets_sample[c(1:12,183:196),c(1,2,89:95)])

tweets_sample_subset <- tweets_sample[tweets_sample[,92]!="Neutro"&tweets_sample[,94]!="Neutro",]

tweets_test <- readRDS("2019-08-27_tweets_sample_manual_lexicon_yoko.rds")
colnames(tweets_test)[91:95] <- c("manual_class", "manual_sentiment", "sentiment_normal","sentiment_unilex", "text_cleaned")
results <- confusionMatrix(as.factor(tweets_test$manual_sentiment), as.factor(tweets_test$sentiment_normal))
results

results <- confusionMatrix(as.factor(tweets_test$manual_sentiment), as.factor(tweets_test$sentiment_unilex))