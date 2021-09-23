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
  
# Parametros
  input_duplicate <- 5000
  input_ressample <- F

# Iniciando Loop Principal
  t9 <- Sys.time()
  print(t9)
  for (j in 1:length(files)){
    #Carregando tweets
    file <- files[j]
    #Selecionando datas
    from <- substring(file, 8,17)
    to   <- substring(file, 21,30)
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
      output <- f_lexicalbydate(file, day, input_duplicate, input_ressample)
      output <- cbind(output, created_at = rep(day, nrow(output)))
      
      # Número de palavras total
      print(paste("sum(output$negativo)+sum(output$positivo)",sum(output$negativo)+sum(output$positivo)))
      
      # Salvando no dataframe adequado
      if (!any(grepl("sentiments",ls()))) {
        sentiments <- as.data.frame(output)
        print("sentiments created")
        saveRDS(sentiments, file=paste(Sys.Date(),"_sentiments.rds", sep=""))
        print("sentiments saved0")
      }else{
        sentiments <- rbind.data.frame(sentiments, output)
        print("sentiments binded")
        saveRDS(sentiments, file=paste(Sys.Date(),"_sentiments.rds", sep=""))
        print("sentiments saved1")
      }
      
      # Mostrando tempo final
      print(difftime(Sys.time(), t1, units = 'mins'))
    }
    # Mostrando tempo final
    print(difftime(Sys.time(), t0, units = 'mins'))
    saveRDS(sentiments, file=paste(Sys.Date(),"_sentiments.rds", sep=""))
    print("sentiments saved2")
  }
  # Mostrando tempo final
  print(difftime(Sys.time(), t9, units = 'mins'))

  
  
  # Arrumando nomes
  #names <- c("created_at", "alckmin", "bolsonaro", "ciro", "haddad", "marina", "multiplos", "nenhum")
  
  save.image("C:/Users/Bruno/Desktop/TwitterData/image_important8.RData")
  saveRDS(sentiments, file=paste(Sys.Date(),"_sentiments.rds", sep=""))