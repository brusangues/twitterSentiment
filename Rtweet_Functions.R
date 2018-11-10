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
  appname <- c("Mining Demo First", "Mining Demo Second")
  consumer_key <- c("mk64zKzYZX80ETCz5AeYxepqW", "14B13oi1kRzQxD1CSuP147DaT")
  consumer_secret <- c("a993kwnC15CizKmDx1ymJEZExWdGqoILBoHYedrbwqTuiVMnVE", "LE4cLIsOTF8BTSLGxowCnA0X90HhJNp5DdPA0n4tNhlZvkIsjQ")
  access_token <- c("417475374-ApAzScCMc8AfUO0rgrwzy0eu3pjayhrzAnLP1qZ2", "1041079280770310144-y8KSsot5v4uIZmPkplFQu1mdZtgTSB")
  access_secret <- c("LVvsp3hDksH6Gy5eGT6h4FLSwTjgW7x8RAdJzgFkCQSD5", "DDL6J0V5iF6w1qmSP5VvvMhV46NX28zmlm5u63H1xrfJ6")
  
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

