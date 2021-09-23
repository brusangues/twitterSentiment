#install.packages("rjson")
#install.packages("foreign")
#install.packages("stringr")
library(foreign) #Necessario para criar o arquivo ARFF
library(stringr) #Necessario para editar os strings
library(rjson)   #Necessario para os arquivos JSON

#Parametros

names<-c("text","XXXCLASS") #Nomes dos atributos

index<-8 #Numero da coluna do texto

#Diretorio contendo not?cias da classe fake
fakedir<-"C:/Users/Bruno/Google Drive/Weka Datasets Final/FakeNetData/FakeNewsContent"

#Diretorio contendo not?cias da classe real
realdir<-"C:/Users/Bruno/Google Drive/Weka Datasets Final/FakeNetData/RealNewsContent"

#Diretorio para salvar o aqruivo final
finaldir<-"C:/Users/Bruno/Google Drive/Weka Datasets Final"

################################################################################

setwd(fakedir)
#Criando uma lista com os nomes dos arquivos
filenames <- list.files(full.names=TRUE)
#Criando lista com os arquivos
filelist <- lapply(filenames, function(x) fromJSON(file=x))
#Criando um dataframe com tudo
jsondatafake<-as.data.frame(do.call(rbind, filelist))
#Retirando linhas vazias
jsondatafake<-subset(jsondatafake, top_img != "")
#Selecionando a coluna de texto
jsondatafake<-data.frame(as.character(jsondatafake[,index]))
#Acrescentando uma coluna chamada class com a string "fake"
jsondatafake$XXXCLASS<-"fake"
#Nomeando as colunas
colnames(jsondatafake)<-names

##############################################################################

setwd(realdir)
#Criando uma lista com os nomes dos arquivos
filenames <- list.files(full.names=TRUE)
#Criando lista com os arquivos
filelist <- lapply(filenames, function(x) fromJSON(file=x))
#Criando um dataframe com tudo
jsondatareal<-as.data.frame(do.call(rbind, filelist))
#Retirando linhas vazias
jsondatareal<-subset(jsondatareal, top_img != "")
#Selecionando a coluna de texto
jsondatareal<-data.frame(as.character(jsondatareal[,index]))
#Acrescentando uma coluna chamada class com a string "real"
jsondatareal$XXXCLASS<-"real"
#Nomeando as colunas
colnames(jsondatareal)<-names

##############################################################################

#Juntando os dois dataframes
jsondata<-rbind(jsondatafake,jsondatareal)

#Retirando mais erros
jsondata<-jsondata[grep("Page not found",jsondata$text,invert = TRUE), ]

#Retirando caracteres especiais dos textos
badchars<-c(",","\n","@","$","\n","\r\n","\"","\'")
for(char in badchars){
  jsondata$text<- sapply(jsondata$text, function(y) {gsub(char, " ", y)})
}

#Restringindo os possiveis valores da classe para fake ou real
jsondata$XXXCLASS <- factor(jsondata$XXXCLASS)

#Eliminando exemplos repetidos
jsondata<-unique(jsondata)

#Salvando o arquivo ARFF
setwd(finaldir)
write.arff(jsondata, file="news_net_data.arff")
