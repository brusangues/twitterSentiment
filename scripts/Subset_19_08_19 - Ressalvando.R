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

# Arquivos gerados
readRDS("rt_from2018-11-01_to2018-11-05.rds")
readRDS("rt_from2018-10-27_to2018-10-31.rds")
readRDS("rt_from2018-10-19_to2018-10-26.rds")
readRDS("rt_from2018-10-12_to2018-10-18.rds")
readRDS("rt_from2018-10-05_to2018-10-11.rds")
readRDS("rt_from2018-09-21_to2018-10-04.rds")
readRDS("rt_from2018-09-08_to2018-09-20.rds")

# Resalvando tweets
{
  #Selecionando datas
  from <- "2018-09-08"
  to   <- "2018-09-20"
  dates_seq <- seq(as.Date(from), as.Date(to), "days")
  dates_logical <- paste(dates, collapse="|")
  
  #Carregando tweets
  tweets <- readRDS("rt_01.rds")
  tweets <- rbind(tweets, readRDS("rt_02.rds"))
  tweets <- rbind(tweets, readRDS("rt_03.rds"))
  tweets <- rbind(tweets, readRDS("rt_04.rds"))
  
  #Ordenando
  tweets <- tweets[order(tweets$created_at),]
  #Selecionando
  tweets_subset <- tweets[grep(dates_logical, tweets$created_at, ignore.case = T),]
  #View((unique(substring(tweets2$created_at,1, 10))))

  #Salvando
  saveRDS(tweets_subset, file = "rt_from2018-09-08_to2018-09-20.rds")
}
{
    #Selecionando datas
    from <- "2018-09-21"
    to   <- "2018-10-04"
    dates_seq <- seq(as.Date(from), as.Date(to), "days")
    dates_logical <- paste(dates_seq, collapse="|")
    
    #Carregando tweets
    tweets <- readRDS("rt_04.rds")
    tweets <- rbind(tweets, readRDS("rt_05.rds"))
    tweets <- rbind(tweets, readRDS("rt_06.rds"))
    tweets <- rbind(tweets, readRDS("rt_03.rds"))
    
    #Ordenando
    tweets <- tweets[order(tweets$created_at),]
    #Selecionando
    tweets_subset <- tweets[grep(dates_logical, tweets$created_at, ignore.case = T),]
    View((unique(substring(tweets_subset$created_at,1, 10))))
    
    #Salvando
    saveRDS(tweets_subset, file = "rt_from2018-09-21_to2018-10-04.rds")
    rm(tweets_subset, tweets)
}
{
    #Selecionando datas
    from <- "2018-10-05"
    to   <- "2018-10-11"
    dates_seq <- seq(as.Date(from), as.Date(to), "days")
    dates_logical <- paste(dates_seq, collapse="|")
    
    #Carregando tweets
    tweets <- readRDS("rt_06.rds")
    tweets <- rbind(tweets, readRDS("rt_07.rds"))
    tweets <- rbind(tweets, readRDS("rt_08.rds"))
    
    #Ordenando
    tweets <- tweets[order(tweets$created_at),]
    #Selecionando
    tweets <- tweets[grep(dates_logical, tweets$created_at, ignore.case = T),]
    View((unique(substring(tweets$created_at,1, 10))))
    
    #Salvando
    saveRDS(tweets, file = "rt_from2018-10-05_to2018-10-11.rds")
    rm(tweets)
}
{
    #Selecionando datas
    from <- "2018-10-12"
    to   <- "2018-10-18"
    dates_seq <- seq(as.Date(from), as.Date(to), "days")
    dates_logical <- paste(dates_seq, collapse="|")
    
    #Carregando tweets
    tweets <- readRDS("rt_08.rds")
    tweets <- tweets[grep("2018-10-12|2018-10-13", tweets$created_at, ignore.case = T),]
    tweets <- rbind(tweets, readRDS("rt_09.rds"))
    tweets <- rbind(tweets, readRDS("rt_010.rds"))
    
    #Ordenando
    tweets <- tweets[order(tweets$created_at),]
    #Selecionando
    tweets_subset <- tweets[grep(dates_logical, tweets$created_at, ignore.case = T),]
    View((unique(substring(tweets_subset$created_at,1, 10))))
    
    #Salvando
    saveRDS(tweets_subset, file = "rt_from2018-10-12_to2018-10-18.rds")
    rm(tweets_subset, tweets)
}
{
    #Selecionando datas
    from <- "2018-10-19"
    to   <- "2018-10-26"
    dates_seq <- seq(as.Date(from), as.Date(to), "days")
    dates_logical <- paste(dates_seq, collapse="|")
    
    #Carregando tweets
    tweets <- readRDS("rt_010.rds")
    tweets <- tweets[grep("2018-10-19", tweets$created_at, ignore.case = T),]
    tweets <- rbind(tweets, readRDS("rt_011.rds"))
    tweets <- rbind(tweets, readRDS("rt_012.rds"))
    
    #Ordenando
    tweets <- tweets[order(tweets$created_at),]
    #Selecionando
    tweets_subset <- tweets[grep(dates_logical, tweets$created_at, ignore.case = T),]
    View((unique(substring(tweets_subset$created_at,1, 10))))
    
    #Salvando
    saveRDS(tweets_subset, file = "rt_from2018-10-19_to2018-10-26.rds")
    rm(tweets_subset, tweets)
}
{
    #Selecionando datas
    from <- "2018-10-27"
    to   <- "2018-10-31"
    dates_seq <- seq(as.Date(from), as.Date(to), "days")
    dates_logical <- paste(dates_seq, collapse="|")
    
    #Carregando tweets
    tweets <- readRDS("rt_012.rds")
    tweets <- tweets[grep("2018-10-27", tweets$created_at, ignore.case = T),]
    tweets <- rbind(tweets, readRDS("rt_013.rds"))
    tweets <- rbind(tweets, readRDS("rt_014.rds"))
    
    #Ordenando
    tweets <- tweets[order(tweets$created_at),]
    #Selecionando
    tweets_subset <- tweets[grep(dates_logical, tweets$created_at, ignore.case = T),]
    View((unique(substring(tweets_subset$created_at,1, 10))))
    
    #Salvando
    saveRDS(tweets_subset, file = "rt_from2018-10-27_to2018-10-31.rds")
    rm(tweets_subset, tweets)
}
{
    #Selecionando datas
    from <- "2018-11-01"
    to   <- "2018-11-05"
    dates_seq <- seq(as.Date(from), as.Date(to), "days")
    dates_logical <- paste(dates_seq, collapse="|")
    
    #Carregando tweets
    tweets <- readRDS("rt_014.rds")
    tweets <- rbind(tweets, readRDS("rt_015.rds"))
    
    #Ordenando
    tweets <- tweets[order(tweets$created_at),]
    #Selecionando
    tweets_subset <- tweets[grep(dates_logical, tweets$created_at, ignore.case = T),]
    View((unique(substring(tweets_subset$created_at,1, 10))))
    
    #Salvando
    saveRDS(tweets_subset, file = "rt_from2018-11-01_to2018-11-05.rds")
    rm(tweets_subset, tweets)
}
