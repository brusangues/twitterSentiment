### Analise de Sentimento no Twitter nas Eleicoes de 2018
### Codigo escrito por Bruno Sanches Rodrigues
### UFABC 2018 Projeto financiado pelo CNPq
### Documentacao do rtweet https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

{
library(stringr)
library(rtweet)
library(utils)
library(plyr)

#Perguntando se está no pc ou note
machine <- winDialog("yesno", "Desktop?")
if (machine == "YES") {machine <- 1} else machine <- 2

#Carregando funções e definindo o work enviroment
path <- c("C:/Users/Bruno/", "C:/Users/Bruno-NB/")
setwd(paste(path[machine], "Desktop/TwitterData", sep = ""))
source(paste(path[machine], "Google Drive/IC/RCode/Rtweet_Functions.R", sep = ""))

#Criando o token
twitter_token <- token(machine)
}

#Fun��o Principal ___________________________________________________________________________
time = 30 # Minutos
reps = 18
sleep = 1
#query <- "elen�o, elenao, bolsonaro, bolsomito, haddad, lula, alckmin, ciro, marina"
query1 <- "bolsonaro, bolsomito, elesim"
query2 <- "haddad, elenao, elen�o"
query <- query1
type  = "recent"        #"recent", "mixed" ou "popular"
include_rts = TRUE      #Incluir retweets ou n�o na pesquisa

#Calculando estimativa do tempo para concluir todas as pesquisas
continue <- winDialog("yesno", paste("Tempo demandado:", (time+sleep)*reps, "minutos ou", (time+sleep)*reps/60,
                                     "horas \nContinuar?", Sys.time(),query[1]))
if(continue == "NO") break

start <- paste("Inicio:", Sys.time())
print(start)
for (i in 1:reps){
  print(paste(i, "iniciado:", Sys.time()))
  json_name = paste(format(Sys.time(), "st_%d_%H_%M-"), time, sep="")
  stream_tweets(
    q = query,
    language = "pt",
    timeout = 60*time,
    type  = type,
    file_name = json_name,
    parse = FALSE,
    include_rts = include_rts
  )
  print(paste(i, "terminado:", Sys.time()))
  Sys.sleep(60*sleep)
  if (i%%2==0) {query <- query1
  } else {query <- query2}
}
print(start)
print(paste("Fim:", Sys.time()))

#df <- parse_stream("st_04_20_12-30.json")

