### Analise de Sentimento no Twitter nas Eleicoes de 2018
### Codigo escrito por Bruno Sanches Rodrigues
### UFABC 2018 Projeto financiado pelo CNPq
### Documentacao do rtweet https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

### Junta os tweets de cada pasta (1-15) e os salva em um json e um rds iguais

{ library(stringr)
  library(rtweet)
  library(utils)
  library(plyr)
  library(dplyr)
  suppressMessages(library(readr))
  
  library(reticulate)
  library(RJSONIO)
  library(rjson)
  library(jsonlite)
  
  ##Perguntando se esta no pc ou note
  #machine <- winDialog("yesno", "Desktop?")
  #if (machine == "YES") {machine <- 1} else machine <- 2
  ##Definindo o work enviroment
  #path <- c("C:/Users/Bruno/", "C:/Users/Bruno-NB/")
  #setwd(paste(path[machine], "Desktop/TwitterData", sep = ""))
} #Carrega libraries

setwd("C:/Users/Bruno/Desktop/TwitterData/")

# keep <- c("created_at", "text", "source", "is_retweet", "retweet_created_at", "retweet_text", "verified")
keep <- c(3, 5, 6, 12, 48, 47, 1, 2, 4, 7:11, 13:46, 49:88)

timej <- as.character(Sys.time())

for (j in 7:7){
  
  timei <- as.character(Sys.time())
  
  path <- paste("./", j, sep="")
  
  ##### Carregando os arquivos no R ________________________________________________________________
  
  csvNames  <- list.files(path, pattern = ".csv")
  print(paste("length(csvNames):", length(csvNames)))
  jsonNames <- list.files(path, pattern = ".json")
  print(paste("length(jsonNames):", length(jsonNames)))
  
  ##### CSV                        _________________________________________________________________
  
  files <- csvNames
  
  if(length(files) > 0) for(i in 1:length(files)){
    print("csv")
    
    #ERROR HANDLING
    rti <- tryCatch(
      suppressMessages(suppressWarnings(read_csv(paste(path, files[i], sep="/"), locale = locale(encoding = "WINDOWS-1252"))))
      ,error=function(e) e
    )
    
    if(inherits(rti, "error")) next
    
    #rti <- read.csv2(paste(path, files[i], sep="/"), sep=",", fileEncoding = "latin1//TRANSLIT", encoding = "latin1//TRANSLIT") # Carrega arquivo i
    
    #rti <- subset(rti, nchar(as.character(rti$status_id))==20) # Remove linhas não formatadas improve
    print(paste("nrow(rti):",nrow(rti)))
    print("remove duplicate text")
    rti <- rti[!duplicated(rti[,c("text", "retweet_text")]) ,] # Remove linhas duplicadas  
    print(paste("nrow(rti):",nrow(rti)))
    rti <- rti[keep] # Retira colunas desnecessárias do dataframe
    
    # Lê datas como formato e zona certos
    rti$created_at <- as.POSIXct(strptime(rti$created_at, "%Y-%m-%d %H:%M:%S"), tz="GMT")
    attributes(rti$created_at)$tzone <- Sys.timezone() # Faz a conversão das zonas
    
    rti$retweet_created_at <- as.POSIXct(strptime(rti$retweet_created_at, "%Y-%m-%d %H:%M:%S"), tz="GMT")
    attributes(rti$retweet_created_at)$tzone <- Sys.timezone() # Faz a conversão das zonas
    
    rti$account_created_at <- as.POSIXct(strptime(rti$account_created_at, "%Y-%m-%d %H:%M:%S"), tz="GMT")
    attributes(rti$account_created_at)$tzone <- Sys.timezone() # Faz a conversão das zonas
    
    # Tentativa de formar arquivo geral contendo todos os dados
    if(is.data.frame(rt)){
      rt <- rbind(rt, rti)
    }else rt <- rti
    
    print(paste("nrow(rt):",nrow(rt)))
    print("remove duplicate tweets")
    rt <- unique(rt) # Remove linhas duplicadas
    print(paste("nrow(rt):",nrow(rt)))
    #
    
    table <- as.data.frame(table(format(as.Date(rti$created_at), "%m_%d")))
    
    
    print(paste("i:",i))
    print(paste("j:",j))
    
  } 
  
  ##### JSON                         _________________________________________________________________
  
  files <- jsonNames
  
  if(length(files)!= 0) for(i in 1:length(files)){
    print("json")
    #ERROR HANDLING
    rti <- tryCatch(
      parse_stream(paste(path, files[i], sep="/")),
      error=function(e) e
    )
    
    if(inherits(rti, "error")) next
    
    #rti <- parse_stream(paste(path, files[i], sep="/"))
    
    print(paste("nrow(rti):",nrow(rti)))
    print("remove duplicate text")
    rti <- rti[!duplicated(rti[,c("text", "retweet_text")]) ,] # Remove linhas duplicadas  
    print(paste("nrow(rti):",nrow(rti)))
    rti <- rti[keep] # Retira colunas desnecessárias do dataframe
    
    # Lê datas como formato e zona certos
    rti$created_at <- as.POSIXct(strptime(rti$created_at, "%Y-%m-%d %H:%M:%S"), tz="GMT")
    attributes(rti$created_at)$tzone <- Sys.timezone() # Faz a conversão das zonas
    
    rti$retweet_created_at <- as.POSIXct(strptime(rti$retweet_created_at, "%Y-%m-%d %H:%M:%S"), tz="GMT")
    attributes(rti$retweet_created_at)$tzone <- Sys.timezone() # Faz a conversão das zonas
    
    rti$account_created_at <- as.POSIXct(strptime(rti$account_created_at, "%Y-%m-%d %H:%M:%S"), tz="GMT")
    attributes(rti$account_created_at)$tzone <- Sys.timezone() # Faz a conversão das zonas
    
    # Tentativa de formar arquivo geral contendo todos os dados
    if(is.data.frame(rt)){
      rt <- rbind(rt, rti)
    }else rt <- rti
    
    print(paste("nrow(rt):",nrow(rt)))
    print("remove duplicate tweets")
    rt <- unique(rt) # Remove linhas duplicadas
    print(paste("nrow(rt):",nrow(rt)))
    #
    
    table <- as.data.frame(table(format(as.Date(rti$created_at), "%m_%d")))
    
    print(paste("i:",i))
    print(paste("j:",j))
    
  }
  
  print(paste("folder start:",timei))
  print(as.character(Sys.time()))
  print(paste("j:", j))
  print("Mudança de pasta____________________________")
  
  saveRDS(rt, file = paste("C:/Users/Bruno/Desktop/TwitterData/rt_0", j, ".rds", sep=""))
  # rt <- readRDS(file = paste("C:/Users/Bruno/Desktop/TwitterData/rt_0", j, ".rds", sep=""))

  # Salvando dataset master
  rt %>% 
    toJSON() %>%
    write_lines(paste("C:/Users/Bruno/Desktop/TwitterData/rt_0", j, ".json", sep=""))
  # rt <- fromJSON("C:/Users/Bruno/Desktop/TwitterData/rt_master.json")
  

  
  # Exploratório
  #rt_master<- fromJSON("C:/Users/Bruno/Desktop/TwitterData/rt_master.json")
  plot(tail(sort(table(rt$source)), 10))
  print(nrow(rt))
  
  remove(rt)
  
}
comment <- '
print("FIM:")
print(timej)
print(as.character(Sys.time()))

# Salvando dataset master
rt %>% 
  toJSON() %>%
  write_lines(paste("C:/Users/Bruno/Desktop/TwitterData/rt_0", j, ".json", sep=""))
# rt <- fromJSON("C:/Users/Bruno/Desktop/TwitterData/rt_master.json")

# Exploratório
#rt_master<- fromJSON("C:/Users/Bruno/Desktop/TwitterData/rt_master.json")
plot(tail(sort(table(rt$source)), 10))
print(sum(count2$Freq))
print(nrow(rt))

ERROR 19_07_20

[1] "i: 29"
[1] "j: 7"
[1] "folder start: 2019-07-20 10:53:44"
[1] "2019-07-20 15:21:24"
[1] "j: 7"
[1] "Mudança de pasta____________________________"
Error: cannot allocate vector of size 414.5 Mb
'
