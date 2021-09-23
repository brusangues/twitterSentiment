{library(stringr)
library(rtweet)
library(utils)
library(plyr)
##Perguntando se esta no pc ou note
#machine <- winDialog("yesno", "Desktop?")
#if (machine == "YES") {machine <- 1} else machine <- 2
##Definindo o work enviroment
#path <- c("C:/Users/Bruno/", "C:/Users/Bruno-NB/")
#setwd(paste(path[machine], "Desktop/TwitterData", sep = ""))
} #Carrega libraries
load("2018_11_03_02 freq112.RData") # Carrega dados passados
setwd("C:/Users/Bruno/Desktop/TwitterData")

##### Carregando os arquivos no R _________________________________________________________________
{
txtFiles <- list.files(pattern = ".txt") #procura no wd  arquivos .txt
for (txt in txtFiles){ #txt recebe o nome de cada elemento do vetor txtFiles
  q <- readChar(txt, file.info(txt)$size) #q recebe conteúdo do arquivo txt
  q <- substring(q, 13,73) #q eh reduzido para conter a informação de busca apenas
  if (q != "bolsonaro OR bolsomito OR alckmin OR haddad OR ciro OR marina"){
    txtFiles <- txtFiles[-match(txt, txtFiles)] #remove elemento txt
  }
  else{
    q <- substring(txt, 1,nchar(txt)-4) #retira o .txt do nome do arquivo
    q <- paste(q, ".csv", sep="") #adiciona .csv no nome do arquivo
    txtFiles[match(txt, txtFiles)] <- q #substitui elemento
  }
}
csvFiles <- txtFiles
remove(q,txt,txtFiles) #Limpando

##Tecnica velha:
#folder <- list.files(pattern = ".csv") 
#folder <- subset(folder, file.info(folder)$size>100000000)
# files <- c( "rt11_02_21_10-02_23_36(141690).csv", "rt11_02_01_50-02_04_06(136109).csv"
#             ,"rt11_01_01_41-01_03_40(133100).csv", "rt11_30_01_11-30_03_37(137157).csv"
#             ,"rt11_22_23_15-22_23_59(89060).csv",  "rt11_28_00_39-28_03_29(130477).csv"
#             ,"rt11_27_00_53-27_02_26(151926).csv", "rt11_26_01_07-26_03_38(124817).csv" 
#             ,"rt11_23_23_19-23_23_59(84871).csv",  "rt11_24_23_23-24_23_59(113780).csv"
#             ,"rt11_28_22_41-28_23_59(182597).csv", "rt11_03_23_17-03_23_59(250092).csv"
#           )
##files <- c("rt11_28_22_41-28_23_59(182597).csv", "rt11_03_23_17-03_23_59(250092).csv")

} #Le wd e retorna csvFiles com nomes dos .csv de interesse

{
filesMonth <- format(file.info(csvFiles)$mtime, "%m") #Vetor com o mes de criacao do arquivo
filesDay <- format(file.info(csvFiles)$mtime, "%d") #Vetor com o dia de criação do arquivo
filesLower <- substring(csvFiles, 6,10) #Vetor com o dia e hora do primeiro tweet, de acordo com o nome
filesUpper <- substring(csvFiles, 15,19) #Dia e hora do ultimo tweet
df <- data.frame(names=csvFiles,month=filesMonth, day=filesDay, lower=filesLower, upper=filesUpper)
df <- df[order(df$month,df$lower),] #Ordena dataframe df a partir do mes e depois pelo dia e hora
df <- cbind(df, c(1:48)) #Une com uma última coluna de índices para melhor visualização
# Separação dos csvFiles em intervalos
csv1<- as.vector(df$names[01:13]) # 09_18 - 10_01
csv2<- as.vector(df$names[12:25]) # 10_02 - 10_12
csv3<- as.vector(df$names[24:37]) # 10_13 - 10_23
csv4<- as.vector(df$names[36:48]) # 10_24 - 11_02

#print(sort(format(file.info(csv1)$mtime, "%m_%d"))) #Codigo velho

remove(filesMonth, filesDay, filesLower, filesUpper) #Limpando
} #Separa csvFiles em csv1(09_18 - 10_01) csv2(10_02 - 10_12) csv3(10_13 - 10_23) csv4(10_24 - 11_02)

files <- csv5Aux

##### Manipulação dos dados pesados _________________________________________________________________

#f <- winDialog("yesno", paste("Update?", files[1],"...")) #Pergunta se ha um dataframe carregado
f = "NO" #Vai criar dataframe novo

for(i in 1:length(files)){
  rti <- read.csv2(files[i], sep=",", fileEncoding = "latin1//TRANSLIT") # Carrega arquivo i
      print(nrow(rti))
  rti <- subset(rti,rti$lang=="pt") # Remove linhas não formatadas
      print(nrow(rti))
  rti <- unique(rti) # Remove linhas duplicadas
      print(nrow(rti))
  #rti <- rti[,1:88] #Para casos especiais
  # Lê datas como formato e zona certos
  rti$created_at <- as.POSIXct(strptime(rti$created_at, "%Y-%m-%d %H:%M:%S"), tz="GMT")
  attributes(rti$created_at)$tzone <- Sys.timezone() # Faz a conversão das zonas
  # Cria um dataframe para conter todas as linhas ou adiciona linhas nele
  
  if(i==1 & f == "NO") rt <- rti #Cria dataframe novo dependendo da resposta
  else rt <- rbind(rt, rti) #Une dataframe grande com os menores
      print(nrow(rt))
  rt <- unique(rt) # Remove linhas duplicadas
      print(nrow(rt))
} #Carrega todos os arquivos de files no dataframe rt
remove(rti)
table(format(rt$created_at, "%m_%d"))
rt <- subset(rt, format(rt$created_at, "%m_%d")>="10_28" &
             format(rt$created_at, "%m_%d")<="11_3")
table(format(rt$created_at, "%m_%d"))

##### Fazendo a leitura de dados _________________________________________________________________
{

format = "%Y-%m-%d" # Definimos como será feita a divisão
words <- c("Bolsonaro", "Bolsomito", "Alckmin","Haddad","Ciro","Marina") # Vetor com as palavras
# Vetor contendo todas as datas presentes no dataframe
subset <- names(table(format(rt$created_at, format=format))) 
freq <- data.frame() # Cria dataframe com as informações

for(i in 1:length(subset)){
  
  # Cria um dataframe secundário contendo as linhas que pertençam à data em questão
  rti <- subset(rt, format(rt$created_at, format=format) == subset[i])
  freq[i,1] <- subset[i] # Primeira coluna é a data
  freq[i,2] <- nrow(rti) # Segunda coluna é o número de linhas
  # Para cada palavra chave, soma o número de linhas que ela aparece
  for(j in 1:length(words)){
    freq[i,j+2] <- length(grep(words[j], ignore.case = TRUE, rti$text))
  }
  freq[i,9] <- sum(freq[i,3:8]) # Coluna adicional com a soma das 6 últimas
  
  freq[,10:15] <- round(freq[,3:8]/freq[,2], 2) # Colunas de porcentagens
  
}
# Nomeando as colunas do dataframe
names(freq) <- c("Data (y-m-d)", "Tweets", words, "Soma", substring(words, 1, 3))
#freq <- arrange(freq, freq[,1], decreasing =T) # Reordenando as linhas
print(freq) # Mostrando os resultados
csv_name <- format(Sys.time(), "freq_%m_%d %H_%M") 
write_as_csv(freq, csv_name) # Salvando num CSV

} #A partir de rt contendo os tweets, cria freq, um dataframe contendo a data, número de
      #tweets de cada candidato e frequencias relativas e ainda salva essa tabela com a data

#freqAux <- freq
freqAux <- rbind(freqAux,freq)
freq <- freqAux
###### Plotando _________________________________________________________________
{

matplot(y=freq[,c(10,12:15)], type = "o", pch=20,col = 1:6
        ,ylab = "%", xlab = "Dias", main = "N de Tweets", xaxt="n")
labels <- format(as.Date(freq[,1]), format = "%d")
axis(1, 1:length(labels), labels = labels)
box()
legend("left", names(freq[,c(10,12:15)]), col=1:6, pch=1) # Legenda

}

###### Finalizando _________________________________________________________________
save <- winDialog("yesno", paste("Processo terminado. Salvar imagem?", Sys.time())) # Salvar ou não ws
if(save=="YES") save.image(file=paste(format(Sys.time(), "%Y_%m_%d_%H "), "freq112.RData" , sep=""))

{
# matplot(x=as.POSIXct(freq[,1]), y=freq[,c(10,12:15)], type = "o", pch=20,col = 1:6
#         ,ylab = "%", xlab = "Hours", main = "N de Tweets", axes = F)
# w <- seq(as.POSIXct("2018-09-22 00:00:00"), as.POSIXct("2018-10-02 00:00:00"), by = "day")
# axis(side=1, at=w)
# box()
} #Velho

