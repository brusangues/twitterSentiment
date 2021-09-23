#install.packages("foreign")
#install.packages("stringr")
library(foreign) #Necessario para criar o arquivo ARFF
library(stringr) #Necessario para editar os strings

#Par?metros
names<-c("text","XXXCLASS") #Nomes dos atributos
indexes<-c(5,2) #Numero das colunas dos atributos no arquivo original
csvfilename<-"news_titles_data" #Nome do arquivo CSV sem a extensao no final
setwd("C:/Users/Bruno/Google Drive/Weka Datasets Final") #Pasta

#Lendo o arquivo CSV
csvdata<-data.frame(read.csv(file = paste(csvfilename,".csv", sep="")))

#Selecionando e nomeando as colunas certas
csvdata<-data.frame(as.character(csvdata[,indexes[1]]),
                    csvdata[,indexes[2]]
                    )
colnames(csvdata)<-names

#Retirando caracteres especiais dos textos
badchars<-c(",","\n","@","$","\n","\r\n","\"","\'")
for(char in badchars){
  csvdata$text<- sapply(csvdata$text, function(y) {gsub(char, " ", y)})
}

#Retirando exemplos cujas classes nao sao fake nem real
csvdata<-subset(csvdata, XXXCLASS == "real" | XXXCLASS == "fake")

#Restringindo os possiveis valores da classe para fake ou real
csvdata$XXXCLASS <- factor(csvdata$XXXCLASS)

#Eliminando exemplos repetidos
csvdata<-unique(csvdata)

#Salvando o arquivo ARFF
write.arff(csvdata, file = paste(csvfilename,".arff", sep=""))