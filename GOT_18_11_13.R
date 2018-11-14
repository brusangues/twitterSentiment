### Analise de Sentimento no Twitter nas Eleicoes de 2018
### Codigo escrito por Bruno Sanches Rodrigues
### UFABC 2018 Projeto financiado pelo CNPq
### Documentacao do rtweet https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

library(reticulate)
use_condaenv("tweets")

source_python("E:/Documents/GitHub/twitterSentiment/GetOldTweets_18_11_13.py")

test <- qsearch("bolsonaro", "2018-11-10","2018-11-12", 2)

print(test)