### Analise de Sentimento no Twitter nas Eleicoes de 2018
### Codigo escrito por Bruno Sanches Rodrigues
### UFABC 2018 Projeto financiado pelo CNPq
### Documentacao do rtweet https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

### Realizar análise de sentimento por meio de léxicos em pequenos conjuntos de tweets

{
  library(Rfacebook) # usado para extrair dados do facebook
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
} #Bibliotecas

#setwd("C:/Users/Bruno/Desktop/TwitterData/")
#setwd("C:/Users/Bruno-NB/Desktop/TwitterData")

# Salvando samples de pesquisas do bolsonaro no início da coleta e na véspera da 2a fase
if(F){

rt = ""
nome = "bolsonaro"
for (i in 1:15)
{
  filei <- paste("C:/Users/Bruno/Desktop/TwitterData/rt_0", i, ".rds", sep="")
  rti <- readRDS(file = filei)
  rti <- rti[,"text"]
  
  lines <- grep(nome, ignore.case = TRUE, rti)
  subset(lines, lines%%200==0)
  rti <- rti[lines]
  
  #rti$created_at %>% head()
  #rti$created_at %>% tail()
  
  if (!is.vector(rt)){
    rt <-rti
    print("overwrite")
  }
  else{
    rt <- c(rt,rti)
  }
  print(i)
  print(length(rt))
  
  rm(rti)
}
filei <- paste("rt_", nome, "_200em200_",length(rt), ".rds", sep="")
saveRDS(rt, file = filei)
rm(rt)

}
##########################################################
# Exemplo rpubs https://rstudio-pubs-static.s3.amazonaws.com/342346_fcc3e8b926c7498285d49099140bd247.html
      text <- readRDS("rt_bolsonaro_200em200_172750.rds")
      df <- as.data.frame(text)
      #Limpando texto
      #df$text<- tolower(df$text)                 #Caixa baixa
      df$text<- rm_url(df$text)                  #URLs e mais alguns símbolos inúteis
      df$text<- gsub("?<[u|U].*f>", "", df$text) #<U+009F>
      df$text<- gsub("?<f0>", "", df$text)       #<f0>
      df$text<- rm_hash(df$text)                 #Twitter Hash Tags
      df$text<- rm_tag(df$text)                  #Twitter Tags ou arrobas
      df$text<- gsub("[[:punct:]]", "", df$text) #Pontuação e caracteres especiais
      df$text<- gsub("[ |\t]{2,}", " ", df$text) #Tabs ou espaços duplos
      df$text<- gsub("^ ", "", df$text)          #Espaço no início da string
      df$text<- gsub(" $", "", df$text)          #Espaço no fim da string
      df$text %>% head()
      
      #In tm package, the documents are managed by a structure called Corpus
      #Criação do Corpus
      df$doc_id <- row.names(df)
      df <- df[,c("doc_id", "text")]
      df <- Corpus(VectorSource(df$text))
      
      #Create a term-document matrix from a corpus
      #Criando o dtm usando palavras que tem mais de 3 letras e uma frequencia >19
      dtm <- DocumentTermMatrix(df, control=list(
        wordLengths=c(4, 20),
        language=locale('pt'), 
        stopwords=c("banana", stopwords('portuguese')),
        removeNumbers = TRUE,
        tolower = TRUE,
        bounds = list(global = c(150,500))
        ))
      dtm
      head(findFreqTerms(dtm))
      
      #Convert as matrix
      m = as.matrix(dtm)
      
      #Get word counts in decreasing order
      word_freqs = sort(base::colSums(m), decreasing=TRUE) 
      
      #Create data frame with words and their frequencies
      dm = data.frame(word=names(word_freqs), freq=word_freqs)
      
      
      require(devtools)
      #install_github("lchiffon/wordcloud2")
      library(wordcloud2)
      
      #Setando palheta do R com cores escuras
      #install.packages("RColorBrewer")
      library(RColorBrewer)
      #display.brewer.pal(n = 8, name = 'Dark2')
      palette(brewer.pal(name = "Dark2",n = 8))
      colorCut <- 1.85*mean(dm$freq)
      
      #Construindo Wordcloud
      wordcloud2(dm[1:1000,],
                 color = ifelse(dm$freq > colorCut, 'darkgreen', palette()),
                 size = .17,
                 shuffle = F,
                 rotateRatio = 0.3,
                 widgetsize = c(1000, 500)
      )
      
      
      
      
      
      
      
      
      
      
      
      