### Analise de Sentimento no Twitter nas Eleicoes de 2018
### Codigo escrito por Bruno Sanches Rodrigues
### UFABC 2018 Projeto financiado pelo CNPq
### Documentacao do rtweet https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

{
library(stringr)
library(rtweet)
library(utils)
library(plyr)

#Perguntando se est√° no pc ou note
machine <- winDialog("yesno", "Desktop?")
if (machine == "YES") {machine <- 1} else machine <- 2

#Carregando fun√ß√µes e definindo o work enviroment
path <- c("C:/Users/Bruno/", "C:/Users/Bruno-NB/")
setwd(paste(path[machine], "Desktop/TwitterData", sep = ""))
source(paste(path[machine], "Google Drive/IC/RCode/Rtweet_Functions.R", sep = ""))
}

#Parametros de busca ________________________________________________________________________
#query <- "bolsonaro OR bolsomito OR alckmin OR haddad OR ciro OR marina"
query <- c("bolsonaro OR jair bolsonaro OR bolsomito",
           "haddad OR fernando haddad")
           #,"alckmin OR geraldo alckmin",
           #"ciro OR ciro gomes",
           #"marina OR marina silva")
n =  100              #n˙mero de tweets solicitados por candidato 27000+ funciona bem
since = c("") #data limite inferior no formato AAAA-MM-DD
until = c("") #data limite superior
type  = "recent"        #"recent", "mixed" ou "popular"
include_rts = TRUE      #Incluir retweets ou n„o na pesquisa
retryonratelimit = TRUE #Continuar ou n„o depois do limite de 18000 tweets por 15 minutos
#Declarando vetor para guardar informaÁıes (N„o implementado completamente)
if (is.data.frame(df)==FALSE) {
  df <- data.frame(Data = character(), i=integer(), Palavras=character(), Solicitados=integer(), Retornados=integer(),
                   Unicos=integer(), Primeiro=character(), Ultimo=character(), stringsAsFactors=FALSE)}



#FunÁ„o Principal ___________________________________________________________________________
{
#Calculando estimativa do tempo para concluir todas as pesquisas
total_time_expected <- floor(length(until)*length(query)*n/18000)*15/60
continue <- winDialog("yesno", paste("Tempo demandado:", total_time_expected, "horas\nContinuar?"
                                     , Sys.time(), "\n", since, until,"\n", n, "\n",query[1]))
if(continue == "NO") {break}
  
#Criando o token
twitter_token <- token(machine)

  for (h in 1:length(until)){
    for(i in 1:length(query)){
      
      #Informa√ß√£o
      print(paste("Pesquisa", h, i, Sys.time(),":"))
      print(paste("   Palavras          :", query[i]))
      print(paste("   Tweets solicitados:", n))
      TA <- as.numeric(substring(Sys.time(), 15, 16))
      
      #Buscando
      rt <- search_tweets(
        query[i], n, lan = "pt"
        ,retryonratelimit = retryonratelimit
        ,since = since[h]
        ,until = until[h]
        ,type  = type
        ,include_rts = include_rts
      )
      
      #Informa√ß√£o
      TB <- as.numeric(substring(Sys.time(), 15, 16))
      if(TA>TB) TB = TB+60
      print(paste("   Tweets retornados :", nrow(rt))) 
      print(paste("   Primeiro tweet    :", head(sort(rt$created_at),1)))
      print(paste("   Ultimo tweet      :", tail(sort(rt$created_at),1)))
      print(paste("   Fim da pesquisa   :", Sys.time()))
      print(paste("   Tempo de pesquisa :", TB-TA, "minutos")) #Calculo pode apresentar erros
      
      #Criando o nome do arquivo CSV
      t0 <- substring(head(sort(rt$created_at),1) , 9, 16)
      t0 <- gsub(":", "_", t0)
      t0 <- gsub(" ", "_", t0)
      t1 <- substring(tail(sort(rt$created_at),1) , 9, 16)
      t1 <- gsub(":", "_", t1)
      t1 <- gsub(" ", "_", t1)
      period <- paste(t0, t1, sep = "-")
      rows <- paste("(", nrow(rt), ")", sep = "")
      csv_name <- paste("rt", h, i, "_", period, rows, sep = "")
      
      #Salvando o data frame como CSV na codifica√ß√£o do portugues
      write_as_csv(rt, csv_name, fileEncoding = "latin1//TRANSLIT")
      
      #Salvando os parametros em um arquivo separado
      txt <- list(query[i], n, nrow(rt), since[h], until[h], type, 
                  include_rts, retryonratelimit, TB-TA, 
                  "query[i], n, nrow(rt), since[h], until[h], type, 
                  include_rts, retryonratelimit, TB-TA")
      capture.output(txt, file = paste(csv_name, ".txt", sep=""))
      
      #Salvando os dados no dataframe
      df[nrow(df)+1,] <- list(as.character(Sys.time()), i, substring(query[i], 1, 3), n, nrow(rt), nrow(unique(rt)), 
                              as.character(sort(rt$created_at)[1]), as.character(tail(sort(rt$created_at),1)))
      
    }
  }

winDialog("ok", paste("Pesquisa terminada", Sys.time()))
}

#Processamento Final

print(tapply(df$Retornados, df$Palavras, mean, na.rm=TRUE))
plot(tapply(df$Retornados, df$Palavras, mean, na.rm=TRUE))
#write.csv(df, file = "meta.csv", append = TRUE)
#system('shutdown -s')
