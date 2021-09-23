### Analise de Sentimento no Twitter nas Eleicoes de 2018
### Codigo escrito por Bruno Sanches Rodrigues
### UFABC 2018 Projeto financiado pelo CNPq
### Documentacao do rtweet https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

#install.packages("httpuv") 
#install.packages("stringr")
#install.packages("rtweet")
#install.packages("utils")
#install.packages("plyr")

token <- function(x) {
  
  #Parametros iniciais
  appname         <- c("Mining Demo First", "Mining Demo Second")
  consumer_key    <- c("-------------------------", "-------------------------")
  consumer_secret <- c("--------------------------------------------------", "--------------------------------------------------")
  access_token    <- c("--------------------------------------------------", "--------------------------------------------------")
  access_secret   <- c("---------------------------------------------", "---------------------------------------------")
  
  return(
    create_token(
      app = appname[x],
      consumer_key = consumer_key[x],
      consumer_secret = consumer_secret[x],
      access_token = access_token[x],
      access_secret = access_secret[x]
    )
  )
}

