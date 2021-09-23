### Analise de Sentimento no Twitter nas Eleicoes de 2018
### Codigo escrito por Bruno Sanches Rodrigues
### UFABC 2018 Projeto financiado pelo CNPq
### Documentacao do rtweet https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

### Realizar análise de sentimento por meio de léxicos em pequenos conjuntos de tweets
### Exemplo rpubs: https://rstudio-pubs-static.s3.amazonaws.com/342346_fcc3e8b926c7498285d49099140bd247.html

#setwd("C:/Users/Bruno/Desktop/TwitterData/")
#setwd("C:/Users/Bruno-NB/Desktop/TwitterData")


#  Bibliotecas
{
  library(tidyverse) 
  library(ggExtra)
  library(magrittr) 
  library(lubridate)
  library(tidytext)
  library(lexiconPT)
  library(tm)
  library(RColorBrewer)
  library(tidytext)
  library(quanteda)
  library(scales)
  library(reshape2)
  library(wordcloud)
  library(qdapRegex)
  library(ptstem)
}

###INICIO f_prep
f_prep <- function(tweetxtUtf){
  tweetxtUtf<- tolower(enc2native(tweetxtUtf))      #Caixa baixa
  tweetxtUtf <- rm_url(tweetxtUtf)                  #URLs e mais alguns símbolos inúteis
  tweetxtUtf <- gsub("?<[u|U|f][^<]*>", "", tweetxtUtf) #<U+009F>
  #tweetxtUtf <- rm_hash(tweetxtUtf)                 #Twitter Hash Tags
  #tweetxtUtf <- rm_tag(tweetxtUtf)                  #Twitter Tags ou arrobas
  tweetxtUtf <- gsub("[[:punct:]]", "", tweetxtUtf) #Pontuação e caracteres especiais
  tweetxtUtf <- gsub("[ |\t]{1,}", " ", tweetxtUtf) #Tabs ou espaços duplos
  tweetxtUtf <- gsub("^ ", "", tweetxtUtf)          #Espaço no início da string
  tweetxtUtf <- gsub(" $", "", tweetxtUtf)          #Espaço no fim da string
  tweetxtUtf <- gsub("[^[:alnum:][:blank:]]", " ", tweetxtUtf)
  tweetxtUtf <- gsub("[[:digit:]]", "", tweetxtUtf)
  tweetxtUtf <- gsub("k{3,}", "", tweetxtUtf)       #Strings de apenas 'k', com mais de 2 letras
  
  tweetxtUtfUnique <- tweetxtUtf %>% unique() #Removendo tweets repetidos
  
  tibble(tweetxtUtfUnique) #Visualizando
  return(tweetxtUtfUnique)
}
###FIM f_prep

#testdata
#input_df <- "rt_01.rds"
#input_date <- "2018-09-08"
#input_duplicate <- 1000
#input_ressample <- F

###INICIO f_lexicalbydate
f_lexicalbydate <- function(input_df, input_date, input_duplicate, input_ressample){

  #Carregando dados
      df <- readRDS(input_df)
      df <- df[grep(input_date, df$created_at, ignore.case = T),]
      tweets_txt <- readr::parse_character(df$text, locale = readr::locale('pt')) #Vetor de tweets
      rm(df)
      
    #Limpando texto com função definida previamente
      tweets_txt <- f_prep(tweets_txt)
      twdf <- tibble(tweet = tweets_txt)
      rm(tweets_txt)
    
  #Separando tweets por nome de candidato
    # Opção com múltiplos e nenhum
    if(!input_duplicate){
      # Vamos 
      candidatos <- c('alckmin', 'bolsonaro', 'ciro', 'haddad', 'marina')
      # Criando DF auxiliar
      logical <- data.frame(id = 1:nrow(twdf))
      # Iterando sobre os nomes dos candidatos para gerar valores lógicos
      for (i in 1:length(candidatos)){
        logical[,i+1] <- grepl(candidatos[i], twdf$tweet, ignore.case = T)
      }
      # Cria coluna com número de ocorrências de cada candidato
      logical[,7] <- rowSums(logical[,2:6])
      
      # Crio coluna no df principal
      twdf_single <- twdf
      twdf_single$whois <- NA
      # Preencho quando ocorre 0 ou mais de um candidatos
      twdf_single$whois[logical[,7]>=2] <- 'multiplos'
      twdf_single$whois[logical[,7]==0] <- 'nenhum'
      
      # Preencho quando ocorre exatamente um candidato
      for (i in 1:length(candidatos)){
        twdf_single$whois[is.na(twdf_single$whois) & 
                     grepl(candidatos[i], twdf_single$tweet, ignore.case = T)] <- candidatos[i]
      }
      #View(twdf_single[seq(1, 13638, 1000),])
      twdf <- twdf_single
      rm(twdf_single)
    }else{
    # Opção com duplicatas
      candidatos <- c('alckmin', 'bolsonaro', 'ciro', 'haddad', 'marina')
      
      # Criando DF auxiliar
      twdf_duplicate <- data.frame()
      # Preencho quando ocorre algum candidato
      for (i in 1:length(candidatos)){
          twdf_instance  <- twdf[grepl(candidatos[i], twdf$tweet, ignore.case = T),]
          l <- nrow(twdf_instance)
          if (l>=input_duplicate){
            twdf_instance <- twdf_instance[sample(l, input_duplicate),]
          }else if (input_ressample){
            twdf_instance <- rbind(twdf_instance, 
                                   twdf_instance[sample(l, input_duplicate-l, replace = T),])
          }
          twdf_instance  <- cbind(twdf_instance, rep(candidatos[i],nrow(twdf_instance)) )
          twdf_duplicate <- rbind(twdf_duplicate, twdf_instance) 
      }
      #View(twdf_duplicate[seq(1, 25000, 1000),])
      rm(twdf_instance)
      colnames(twdf_duplicate) <- c("tweet", "whois")
      twdf <- twdf_duplicate
      rm(twdf_duplicate)
    }
      
    #Frequência
      #freq <- twdf %>% count(whois, sort = T) %>% select( whois,freq = n) 
      #freq
      #pie(table(twdf$whois))
      #barplot(table(twdf$whois))
      
  
  #Criando o document term matrix
      myCorpus <- corpus(twdf,  text_field = 'tweet', 
                         metacorpus = list(source = "tweets contento Bolsonaro, Haddad, Marina, Ciro e Alckmin")) 
      myCorpus # Corpus consisting of 1,865 documents and 1 docvar.

  # LDAAAAAAAA
    if(F){
      library(tm)
      corpus <- Corpus(VectorSource(tweets_txt[1:5000]))
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, as.character(stopwords('portuguese')))
      corpus <- tm_map(corpus, removeWords, c("ciro","alckmin","marina","haddad","bolsonaro"))
      dtm = DocumentTermMatrix(corpus)
      
      doc.length = apply(dtm, 1, sum)
      dtm = dtm[doc.length > 0,]
      
      library(topicmodels)
      #LDA model with 5 topics selected
      lda_5 = LDA(dtm, k = 5, method = 'Gibbs', 
                  control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                                 thin = 500, burnin = 4000, iter = 2000))
      top10terms_5 = as.matrix(terms(lda_5,10))
      top10terms_5
      
      lda.topics_2 = as.matrix(topics(lda_2))
      summary(as.factor(lda.topics_2[,1]))
      
      topicprob_2 = as.matrix(lda_2@gamma)
      head(topicprob_2,1)
}

  #Analise de Sentimentos
      #O tidy é uma ferramenta poderosa para text mining, junto com o dplyer pode-se 
      #fazer tudo que vimos antes apenas usando a intuição é claro um bacground em 
      #T-SQL ajuda muito, mas o tidy é bem intuitivo
      
    #Criando coluna id
      twdf$id <- rownames(twdf)
      
    #tw é um df criado a partir de twdf
      tw <- twdf %>% 
        mutate(document = id, word = tweet) %>% 
        select(document, word, whois)
      
    #tdm é
      tdm <- tw %>% unnest_tokens(word,word)
      head(tdm, 24)
      
    # Removendo Stopwords
      #https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt
      #http://miningtext.blogspot.com/2008/11/listas-de-stopwords-stoplist-portugues.html
      #http://snowball.tartarus.org/algorithms/portuguese/stop.txt
      stopwords <- as.data.frame(stopwords('portuguese'))
      colnames(stopwords) <- "V1"
      stopwords <- rbind(stopwords, 
                         read.csv("stopwords_github_19_08_19.txt", stringsAsFactors=F, header = FALSE))
      stopwords <- rbind(stopwords, 
                         read.csv("stopwords_blog_19_08_19.txt", stringsAsFactors=F, header = FALSE))
      stopwords <- rbind(stopwords, 
                         read.csv("stopwords_snowball_19_08_19.txt", 
                                  comment.char = "|", stringsAsFactors=F, header = FALSE))
      stopwords <- unique(stopwords)
      
      stopwords <- as.character(stopwords$V1)
      candidatos <- c('alckmin', 'bolsonaro', 'ciro', 'haddad', 'marina')
      stopwords <- c(stopwords, candidatos)
      
    # Aplicando stopwords
      tdm <- tdm %>% 
        anti_join(tibble(word = stopwords))
      head(tdm, 24)
      tdm <- tdm  %>% group_by(document) %>% mutate(word_per_doc = n()) 
      tdm <- tdm  %>% group_by(whois) %>% mutate(word_per_whois = n()) 
      head(tdm, 24)
      
    # Stemming
      # https://cran.r-project.org/web/packages/ptstem/vignettes/ptstem.html
      #library(ptstem)
      #library(SnowballC)
      #tdm$word<- wordStem(tdm$word, language = "pt")
      tdm$word<- ptstem_words(tdm$word, algorithm = "porter", complete = F)
    
  #Plotando
      # Carregando léxico
      polaridades_pt <- readRDS('polaridades_pt.rds')
      polaridades_pt$word <- gsub("[[:punct:]]", "", readRDS('polaridades_pt.rds')$word)
      
      # Carregando léxico alternativo
      #https://dicionariounilex.wixsite.com/unilex
      polaridades_unilex <- read.csv2("TB_SYM_04.txt", sep=",")
      polaridades_unilex[,3] <- ifelse(polaridades_unilex[,2]>0 , "positivo", "negativo")
      #polaridades_unilex[,3] <- ifelse(polaridades_unilex[,2]==0 , "neutro")
      colnames(polaridades_unilex) <- c("word", "polaridade", "sentimento")
      
      # Selecionando léxico e aplicando análise
        polaridades <- polaridades_pt
        #polaridades <- polaridades_unilex
        
      # Stemming nas polaridades?
        polaridades_s <- polaridades
        polaridades_s$word <- ptstem_words(polaridades$word, 
                                       algorithm = "porter", complete = F)
        polaridades <- rbind(polaridades_s, polaridades)
        polaridades <- unique(polaridades)
      
        sentJoin <- tdm %>%
          inner_join(polaridades, by='word')
    
      # Resultado final
      scored <- sentJoin %>%
        count(whois, sentimento)%>%
        spread(sentimento, n, fill = 0) %>%
        mutate(score = positivo -negativo) %>%
        mutate(positivoperc = (positivo / (positivo + negativo)) * 100) %>%
        mutate(negativoperc = (negativo / (positivo + negativo)) * 100)
      
      # Resultado final novo
      #scored <- sentJoin %>%
      #  count(document, whois, sentimento)%>%
      #  spread(sentimento, n, fill = 0) %>%
      #  mutate(score = positivo -negativo) %>%
      #  mutate(positivoperc = (positivo / (positivo + negativo)) * 100) %>%
      #  mutate(negativoperc = (negativo / (positivo + negativo)) * 100) %>%
      #  group_by(whois) %>%
      #  summarise(negativo = mean(negativo), positivo = mean(positivo), n = n(), 
      #            score = mean(score), positivoperc =mean(positivoperc), negativoperc =mean(negativoperc))
      
      # Plot para teste
      #ggplot(scored, aes(whois, positivoperc, fill = whois)) +
      #  geom_bar(stat = "identity", show.legend = T)
    
  #Retornando
      #rm("input_date","input_df","myCorpus",
      #   "sentJoin","tdm","tw","twdf","candidatos")
      return(scored)
} 
#FIM f_lexicalbydate


###INICIO f_subsample
f_subsample <- function(input_df, input_date, input_duplicate){
  
  #Carregando dados
  twdf <- input_df
  twdf <- twdf[grep(input_date, twdf$created_at, ignore.case = T),]
  
  #Separando tweets por nome de candidato
  {
    # Opção com duplicatas
    candidatos <- c('alckmin', 'bolsonaro', 'ciro', 'haddad', 'marina')
    
    # Criando DF auxiliar
    twdf_duplicate <- data.frame()
    # Preencho quando ocorre algum candidato
    for (i in 1:length(candidatos)){
      twdf_instance  <- twdf[grepl(candidatos[i], twdf$text, ignore.case = T),]
      
      l <- nrow(twdf_instance)
      twdf_instance <- twdf_instance[sample(l, input_duplicate),]
      
      twdf_instance  <- cbind(twdf_instance, rep(candidatos[i],nrow(twdf_instance)) )
      twdf_duplicate <- rbind(twdf_duplicate, twdf_instance) 
    }
    #View(twdf_duplicate[seq(1, 25000, 1000),])
    colnames(twdf_duplicate) <- c(colnames(twdf_duplicate)[1:88], "candidato")
    rm(twdf_instance, l)
  }

  return(twdf_duplicate)
} 
#FIM f_lexicalbydate

###INICIO f_lexicalbysample
f_lexicalbysample <- function(input){
  
  #Carregando dados
  tweets_txt <- readr::parse_character(input, locale = readr::locale('pt')) #Vetor de tweets
  
  #Limpando texto com função definida previamente
  tweets_txt <- f_prep(tweets_txt)
  twdf <- tibble(tweet = tweets_txt)
  rm(tweets_txt)
  
  #Criando o document term matrix
  myCorpus <- corpus(twdf,  text_field = 'tweet', 
                     metacorpus = list(source = "tweets contento Bolsonaro, Haddad, Marina, Ciro e Alckmin")) 
  myCorpus # Corpus consisting of 1,865 documents and 1 docvar.
  
  #Analise de Sentimentos
  #O tidy é uma ferramenta poderosa para text mining, junto com o dplyer pode-se 
  #fazer tudo que vimos antes apenas usando a intuição é claro um bacground em 
  #T-SQL ajuda muito, mas o tidy é bem intuitivo
  
  #Criando coluna id
  twdf$id <- rownames(twdf)
  
  #tw é um df criado a partir de twdf
  tw <- twdf %>% 
    mutate(document = id, word = tweet) %>% 
    select(document, word)#, whois)
  
  #tdm é
  tdm <- tw %>% unnest_tokens(word,word)
  head(tdm, 24)
  
  # Removendo Stopwords
      #https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt
      #http://miningtext.blogspot.com/2008/11/listas-de-stopwords-stoplist-portugues.html
      #http://snowball.tartarus.org/algorithms/portuguese/stop.txt
      stopwords <- as.data.frame(stopwords('portuguese'))
      colnames(stopwords) <- "V1"
      stopwords <- rbind(stopwords, 
                         read.csv("stopwords_github_19_08_19.txt", stringsAsFactors=F, header = FALSE))
      stopwords <- rbind(stopwords, 
                         read.csv("stopwords_blog_19_08_19.txt", stringsAsFactors=F, header = FALSE))
      stopwords <- rbind(stopwords, 
                         read.csv("stopwords_snowball_19_08_19.txt", 
                                  comment.char = "|", stringsAsFactors=F, header = FALSE))
      stopwords <- unique(stopwords)
      
      stopwords <- as.character(stopwords$V1)
      candidatos <- c('alckmin', 'bolsonaro', 'ciro', 'haddad', 'marina')
      stopwords <- c(stopwords, candidatos)
  
    # Aplicando
    tdm <- tdm %>% 
      anti_join(tibble(word = stopwords))
    head(tdm, 24)
    
  # Stemming
    # https://cran.r-project.org/web/packages/ptstem/vignettes/ptstem.html
    #library(ptstem)
    #library(SnowballC)
    tdm$word<- ptstem_words(tdm$word, algorithm = "porter", complete = F)
    #tdm$stem_porter_SB<- wordStem(tdm$word, language = "pt")
    
  # Agregando novamente
    tdm <- tdm  %>% group_by(document) %>% mutate(word_per_doc = n()) 
    #tdm <- tdm  %>% group_by(whois) %>% mutate(word_per_whois = n()) 
    head(tdm, 24)
  
  #Plotando
    # Carregando léxico
    polaridades_pt <- readRDS('polaridades_pt.rds')
    polaridades_pt$word <- gsub("[[:punct:]]", "", readRDS('polaridades_pt.rds')$word)
    
    # Carregando léxico alternativo
    #https://dicionariounilex.wixsite.com/unilex
    polaridades_unilex <- read.csv2("TB_SYM_04.txt", sep=",")
    polaridades_unilex[,3] <- ifelse(polaridades_unilex[,2]>0 , "positivo", "negativo")
    #polaridades_unilex[,3] <- ifelse(polaridades_unilex[,2]==0 , "neutro")
    colnames(polaridades_unilex) <- c("word", "polaridade", "sentimento")
    
    # Selecionando léxico e aplicando análise
    #polaridades <- polaridades_pt
    polaridades <- polaridades_unilex
    
    # Stemming nas polaridades?
    polaridades_s <- polaridades
    polaridades_s$word <- ptstem_words(polaridades$word, 
                                       algorithm = "porter", complete = F)
    polaridades <- rbind(polaridades_s, polaridades)
    polaridades <- unique(polaridades)
    
  sentJoin <- tdm %>%
    inner_join(polaridades, by='word')
  
  # Resultado final
  scored <- sentJoin %>%
    count(whois, sentimento)%>%
    spread(sentimento, n, fill = 0) %>%
    mutate(score = positivo -negativo) %>%
    mutate(positivoperc = (positivo / (positivo + negativo)) * 100) %>%
    mutate(negativoperc = (negativo / (positivo + negativo)) * 100)
  
  #Retornando
  #rm("input_date","input_df","myCorpus",
  #   "sentJoin","tdm","tw","twdf","candidatos")
  return(scored)
} 
#FIM f_lexicalbysample
