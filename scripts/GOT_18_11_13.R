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
}

library(reticulate)
library(RJSONIO)
library(rjson)
library(jsonlite)

use_condaenv("tweets")

source_python("E:/Documents/GitHub/twitterSentiment/GetOldTweets_18_11_13.py")

test <- qsearch("bolsonaro", "2018-11-10","2018-11-12", 200)

print(test)


# Import data from json file
setwd("E:/Documents/GitHub/twitterSentiment/")
jsonData <- RJSONIO::fromJSON('st_04_19_41-30.json')
print(toJSON(head(jsonData), pretty = TRUE))

x <- '{"items":[{"name":"Item 1","group":1},{"name":"Item 2","group":1},{"name":"Item 3","group":2}]}'

# to go back and create your example list of 1 data frame
dfl <- fromJSON(x)
# prettify your json output
toJSON(x)

json_file2 = RJSONIO::fromJSON(json_file)
head(json_file2)



