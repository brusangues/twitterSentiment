### Analise de Sentimento no Twitter nas Eleicoes de 2018
### Codigo escrito por Bruno Sanches Rodrigues
### UFABC 2018 Projeto financiado pelo CNPq
### Documentacao do rtweet https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

### Plota dados de pesquisas de opinião
### Dados Datafolha
### 10_02_Datafolha_10_03b PRIMEIRO TURNO.pdf
### 10_0506_Ibope_10_06 PRIMEIRO TURNO.pdf
### 10_2425_Datafolha_10_26 SEGUNDO TURNO.pdf
### 10_2627_Ibope_10_27 SEGUNDO TURNO.pdf
### https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html

#setwd("C:/Users/Bruno/Desktop/TwitterData/")
#setwd("C:/Users/Bruno-NB/Desktop/TwitterData")

#  Bibliotecas
{ 
  library(tidyverse)
  library(utils)
  library(reticulate)
  library(RJSONIO)
  library(rjson)
  library(jsonlite)
}

# Definindo uma seleção de cores, linha e tamanhos para os gráficos
{
paleta <- c("#009E73", "blue", "#CC79A7", "red", "orange", "cornflowerblue","dark grey")
linhas <- c("longdash", "solid", "longdash", "solid", "longdash","longdash","longdash")
tamanhos <- c(.5,1,.5,1,.5,.5,.5,.5,.5,.5)
x_turnos <- as.Date(c("2018-09-08", "2018-10-07","2018-10-28","2018-11-05"))
}

#Plotando resultados
# Carregando análise realizada 19_08_11
#sentiments <- readRDS(paste(Sys.Date(),"_sentiments.rds", sep=""))
#sentiments_old <- readRDS("2019-08-11_sentiments.rds")
#sentiments_2019_08_27 <- readRDS("2019-08-27_sentiments.rds")
#sentiments_2019_08_28 <- readRDS("2019-08-28_sentiments3.rds")
sentiments <- readRDS("2019-08-30_sentiments.rds")

#Dados Datafolha
if(T){
  #Dados Datafolha
  # Fonte: Se a eleição para presidente fosse hoje e os candidatos fossem estes, em quem você votaria?
  Bolsonaro         <-c(22,24,26,28,28,32,35,36, -1, 49,50,48,-1)
  Haddad            <-c(04,09,13,16,22,21,22,22, -1, 36,35,38,-1)
  Ciro              <-c(10,13,13,13,11,11,11,13, -1, -1,-1,-1,-1)
  Alckmin           <-c(09,10,09,09,10,09,08,07, -1, -1,-1,-1,-1)
  Marina            <-c(16,11,08,07,05,04,04,06, -1, -1,-1,-1,-1)
  Nulo_Branco_Nenhum<-c(22,15,13,12,10,08,06,06, -1, 08,10,08,-1)
  Nao_sabe          <-c(06,07,06,05,05,05,05,04, -1, 06,05,06,-1)
  Datas <-c("2018-08-20", "2018-09-10", "2018-09-13", "2018-09-18", "2018-09-26", "2018-10-02", "2018-10-03", "2018-10-05","2018-10-07",
            "2018-10-10", "2018-10-18", "2018-10-25", "2018-10-28")
    datafolha <- data.frame(Datas, Bolsonaro, Haddad, Ciro, 
                            Alckmin, Marina, 
                            Nulo_Branco_Nenhum, Nao_sabe)
    datafolha <- datafolha %>%
                  gather(Candidatos, "Percentual", c("Bolsonaro", "Haddad", "Ciro", "Alckmin", 
                                                    "Marina", "Nulo_Branco_Nenhum","Nao_sabe"))
    #View(datafolha)
    
    # Intenção de voto estimulada para presidente 2018 - Datafolha
    datafolha_plot <- ggplot(datafolha, aes(x=as.Date(Datas), y=Percentual, group=Candidatos)) +
      # Tipo de gráfico - linhas
      geom_line(aes(linetype=Candidatos, color=Candidatos, size=Candidatos))+
      ylim(0, 50)+
      # Tipo de pontos
      geom_point(aes(color=Candidatos))+
      # Usando a paleta de cores definida anteriormente
      scale_colour_manual(values=paleta)+
      scale_linetype_manual(values=linhas)+
      scale_size_manual(values=tamanhos)+
      # Legendas das linhas
      theme(legend.position="top", axis.text.x = element_text(angle = 90, vjust = 0.5))+
      # Rótulos
      labs(title = "Intenção de voto estimulada para presidente 2018 - Datafolha",
           x = "Dias",
           y = "Porcentual")+
      # Anotações, linhas de demarcação
      ggplot2::annotate("segment", x = x_turnos, xend = x_turnos, y = 0, yend = 50, colour = "dark grey")+
      ggplot2::annotate("text", x = as.Date("2018-10-07")-2, y = 2, label = "1° Turno")+
      ggplot2::annotate("text", x = as.Date("2018-10-28")-2, y = 2, label = "2° Turno")+
      # Formatando as datas
      scale_x_date(date_labels = "%Y-%m-%d", 
                   date_breaks = "1 day", 
                   date_minor_breaks = "1 day",
                   expand = expand_scale(.01),
                   limits = c(x_turnos[1], x_turnos[4]))
    print(datafolha_plot)
    
    #pdf("datafolha_plot.pdf")
    #print(datafolha_plot)
    #dev.off()

}

#Dados Ibope
if(T){
  #Dados Ibope
  Bolsonaro         <-c(20,22,26,28,28,27,31,32,36, -1, 52, 50, 47,-1)
  Haddad            <-c(04,06,08,19,22,21,21,23,22, -1, 37, 37, 41,-1)
  Ciro              <-c(09,12,11,11,11,12,11,10,11, -1, -1,-1,-1,-1)
  Alckmin           <-c(07,09,09,07,08,08,08,07,07, -1, -1,-1,-1,-1)
  Marina            <-c(12,12,09,06,05,06,04,04,03, -1, -1,-1,-1,-1)
  Nulo_Branco_Nenhum<-c(29,21,19,14,12,11,12,11,07, -1, 09, 10, 10,-1)
  Nao_sabe          <-c(09,07,07,07,06,07,05,06,05, -1, 02, 03, 02,-1)
  
  Datas <-c("2018-08-20", "2018-09-04", "2018-09-11", "2018-09-18", "2018-09-24", 
            "2018-09-26", "2018-10-01", "2018-10-03", "2018-10-06","2018-10-07",
            "2018-10-15", "2018-10-23", "2018-10-27", "2018-10-28")
  ibope <- data.frame(Datas, Bolsonaro, Haddad, Ciro, 
                          Alckmin, Marina, 
                          Nulo_Branco_Nenhum, Nao_sabe)
  ibope <- ibope %>%
    gather(Candidatos, "Percentual", c("Bolsonaro", "Haddad", "Ciro", "Alckmin", 
                                       "Marina", "Nulo_Branco_Nenhum","Nao_sabe"))
  #View(ibope)
  
  # Intenção de voto estimulada para presidente 2018 - Ibope
  ibope_plot <- ggplot(ibope, aes(x=as.Date(Datas), y=Percentual, group=Candidatos)) +
    # Tipo de gráfico - linhas
    geom_line(aes(linetype=Candidatos, color=Candidatos, size=Candidatos))+
    ylim(0, 52)+
    # Tipo de pontos
    geom_point(aes(color=Candidatos))+
    # Usando a paleta de cores definida anteriormente
    scale_colour_manual(values=paleta)+
    scale_linetype_manual(values=linhas)+
    scale_size_manual(values=tamanhos)+
    # Legendas das linhas
    theme(legend.position="top", axis.text.x = element_text(angle = 90, vjust = 0.5))+
    # Rótulos
    labs(title = "Intenção de voto estimulada para presidente 2018 - Ibope",
         x = "Dias",
         y = "Porcentual")+
    # Anotações, linhas de demarcação
    ggplot2::annotate("segment", x = x_turnos, xend = x_turnos, y = 0, yend = 50, colour = "dark grey")+
    ggplot2::annotate("text", x = as.Date("2018-10-07")-2, y = 2, label = "1° Turno")+
    ggplot2::annotate("text", x = as.Date("2018-10-28")-2, y = 2, label = "2° Turno")+
    # Formatando as datas
    scale_x_date(date_labels = "%Y-%m-%d", 
                 date_breaks = "1 day", 
                 date_minor_breaks = "1 day",
                 expand = expand_scale(.01),
                 limits = c(x_turnos[1], x_turnos[4]))
  
  print(ibope_plot)
  
  #pdf("ibope_plot.pdf")
  #print(ibope_plot)
  #dev.off()
  
}

#Dados XPIpespe
if(T){
  #Dados XPIpespe
  # Fonte: Se a eleição para presidente fosse hoje e os candidatos fossem estes, em quem você votaria?
  Bolsonaro         <-c(23,26,28,28,36, -1, 51,51,-1)
  Haddad            <-c(08,10,16,21,22, -1, 36,37,-1)
  Ciro              <-c(11,12,11,11,11, -1, -1,-1,-1)
  Alckmin           <-c(09,09,07,08,07, -1, -1,-1,-1)
  Marina            <-c(-1,-1,-1,-1,-1, -1, -1,-1,-1)
  Nulo_Branco_Nenhum<-c(21,15,16,12,07, -1, 10,10,-1)
  Nao_sabe          <-c(06,08,06,05,03, -1, 04,03,-1)
  Datas <-c("2018-09-03", "2018-09-10", "2018-09-17", "2018-09-24", "2018-10-03",   "2018-10-07",
            "2018-10-08", "2018-10-15",   "2018-10-28")
  xpipespe <- data.frame(Datas, Bolsonaro, Haddad, Ciro, 
                          Alckmin, Marina, 
                          Nulo_Branco_Nenhum, Nao_sabe)
  xpipespe <- xpipespe %>%
    gather(Candidatos, "Percentual", c("Bolsonaro", "Haddad", "Ciro", "Alckmin", 
                                       "Marina", "Nulo_Branco_Nenhum","Nao_sabe"))
  #View(xpipespe)
  
  # Intenção de voto estimulada para presidente 2018 - xpipespe
  xpipespe_plot <- ggplot(xpipespe, aes(x=as.Date(Datas), y=Percentual, group=Candidatos)) +
    # Tipo de gráfico - linhas
    geom_line(aes(linetype=Candidatos, color=Candidatos, size=Candidatos))+
    ylim(0, 52)+
    # Tipo de pontos
    geom_point(aes(color=Candidatos))+
    # Usando a paleta de cores definida anteriormente
    scale_colour_manual(values=paleta)+
    scale_linetype_manual(values=linhas)+
    scale_size_manual(values=tamanhos)+
    # Legendas das linhas
    theme(legend.position="top", axis.text.x = element_text(angle = 90, vjust = 0.5))+
    # Rótulos
    labs(title = "Intenção de voto estimulada para presidente 2018 - xpipespe",
         x = "Dias",
         y = "Porcentual")+
    # Anotações, linhas de demarcação
    ggplot2::annotate("segment", x = x_turnos, xend = x_turnos, y = 0, yend = 50, colour = "dark grey")+
    ggplot2::annotate("text", x = as.Date("2018-10-07")-2, y = 2, label = "1° Turno")+
    ggplot2::annotate("text", x = as.Date("2018-10-28")-2, y = 2, label = "2° Turno")+
    # Formatando as datas
    scale_x_date(date_labels = "%Y-%m-%d", 
                 date_breaks = "1 day", 
                 date_minor_breaks = "1 day",
                 expand = expand_scale(.01),
                 limits = c(x_turnos[1], x_turnos[4]))
  print(xpipespe_plot)
  
  #pdf("xpipespe_plot.pdf")
  #print(xpipespe_plot)
  #dev.off()
  
}

#Plotando resultados
  
# Porcentagem Sentimentos Positivos
if(T){
  positivo_plot <- ggplot(sentiments, aes(x=as.Date(created_at), y=positivoperc, group=whois))+
    # Tipo de gráfico - linhas
    geom_line(aes(linetype=whois, color=whois, size=whois))+
    # Tipo de pontos
    geom_point(aes(color=whois))+
    # Usando a paleta de cores definida anteriormente
    scale_colour_manual(values=paleta)+
    scale_linetype_manual(values=linhas)+
    scale_size_manual(values=tamanhos)+
    # Legendas das linhas
    theme(legend.position="top", axis.text.x = element_text(angle = 90, vjust = 0.5))+
    # Rótulos
    ylim(25,75)+
    labs(title = "Porcentagem de Sentimentos Positivos por Dia",
        x = "Dias",
        y = "Percentual")+
    # Anotações, linhas de demarcação
    ggplot2::annotate("segment", x = x_turnos, xend = x_turnos, y = 25, yend = 75, colour = "dark grey")+
    ggplot2::annotate("text", x = as.Date("2018-10-07")-2, y = 27, label = "1° Turno")+
    ggplot2::annotate("text", x = as.Date("2018-10-28")-2, y = 27, label = "2° Turno")+
    # Formatando as datas
    scale_x_date(date_labels = "%Y-%m-%d", 
                 date_breaks = "1 day", 
                 date_minor_breaks = "1 day",
                 expand = expand_scale(.01))
  
  print(positivo_plot)
}

# Porcentagem Sentimentos Negativos
if(T){ 
  negativo_plot <- ggplot(sentiments, aes(x=as.Date(created_at), y=negativoperc, group=whois)) +
    # Tipo de gráfico - linhas
    geom_line(aes(linetype=whois, color=whois, size=whois))+
    # Tipo de pontos
    geom_point(aes(color=whois))+
    # Usando a paleta de cores definida anteriormente
    scale_colour_manual(values=paleta)+
    scale_linetype_manual(values=linhas)+
    scale_size_manual(values=tamanhos)+
    # Legendas das linhas
    theme(legend.position="top", axis.text.x = element_text(angle = 90, vjust = 0.5))+
    # Rótulos
    ylim(25, 75)+
    labs(title = "Porcentagem de Sentimentos Negativos por Dia",
         x = "Dias",
         y = "Percentual")+
    # Anotações, linhas de demarcação
    ggplot2::annotate("segment", x = x_turnos, xend = x_turnos, y = 25, yend = 75, colour = "dark grey")+
    ggplot2::annotate("text", x = as.Date("2018-10-07")-2, y = 27, label = "1° Turno")+
    ggplot2::annotate("text", x = as.Date("2018-10-28")-2, y = 27, label = "2° Turno")+
    # Formatando as datas
    scale_x_date(date_labels = "%Y-%m-%d", 
                 date_breaks = "1 day", 
                 date_minor_breaks = "1 day",
                 expand = expand_scale(.01))
  print(negativo_plot)
}

# Número de Tweets Positivos
if(T){
  positivo_hist <- ggplot(sentiments, aes(x=as.Date(created_at), y=positivo/1000, group=whois)) +
    # Tipo de gráfico - linhas
    geom_line(aes(linetype=whois, color=whois, size=whois))+
    # Tipo de pontos
    geom_point(aes(color=whois))+
    # Usando a paleta de cores definida anteriormente
    scale_colour_manual(values=paleta)+
    scale_linetype_manual(values=linhas)+
    scale_size_manual(values=tamanhos)+
    # Legendas das linhas
    theme(legend.position="top", axis.text.x = element_text(angle = 90, vjust = 0.5))+
    # Rótulos
    ylim(-1, 20)+
    labs(title = "Número de Sentimentos Positivos por Dia",
         x = "Dias",
         y = "Número de Sentimentos [x10³]")+
    # Anotações, linhas de demarcação
    ggplot2::annotate("segment", x = x_turnos, xend = x_turnos, y = 0, yend = 20, colour = "dark grey")+
    ggplot2::annotate("text", x = as.Date("2018-10-07")-2, y = -1, label = "1° Turno")+
    ggplot2::annotate("text", x = as.Date("2018-10-28")-2, y = -1, label = "2° Turno")+
    # Formatando as datas
    scale_x_date(date_labels = "%Y-%m-%d", 
                 date_breaks = "1 day", 
                 date_minor_breaks = "1 day",
                 expand = expand_scale(.01))
  
  print(positivo_hist)
}
  
# Número de Tweets Negativos
if(T){
  negativo_hist <- ggplot(sentiments, aes(x=as.Date(created_at), y=negativo/1000, group=whois)) +
    # Tipo de gráfico - linhas
    geom_line(aes(linetype=whois, color=whois, size=whois))+
    # Tipo de pontos
    geom_point(aes(color=whois))+
    # Usando a paleta de cores definida anteriormente
    scale_colour_manual(values=paleta)+
    scale_linetype_manual(values=linhas)+
    scale_size_manual(values=tamanhos)+
    # Legendas das linhas
    theme(legend.position="top", axis.text.x = element_text(angle = 90, vjust = 0.5))+
    # Rótulos
    ylim(-1, 20)+
    labs(title = "Número de Sentimentos Negativos por Dia",
         x = "Dias",
         y = "Número de Sentimentos [x10³]")+
    # Anotações, linhas de demarcação
    ggplot2::annotate("segment", x = x_turnos, xend = x_turnos, y = 0, yend = 20, colour = "dark grey")+
    ggplot2::annotate("text", x = as.Date("2018-10-07")-2, y = -1, label = "1° Turno")+
    ggplot2::annotate("text", x = as.Date("2018-10-28")-2, y = -1, label = "2° Turno")+
    # Formatando as datas
    scale_x_date(date_labels = "%Y-%m-%d", 
                 date_breaks = "1 day", 
                 date_minor_breaks = "1 day",
                 expand = expand_scale(.01))
  
  print(negativo_hist)
}
  
# Score
if(T){
  score_plot <- ggplot(sentiments, aes(x=as.Date(created_at), y=score/1000, group=whois)) +
    # Tipo de gráfico - linhas
    geom_line(aes(linetype=whois, color=whois, size=whois))+
    # Tipo de pontos
    geom_point(aes(color=whois))+
    # Usando a paleta de cores definida anteriormente
    scale_colour_manual(values=paleta)+
    scale_linetype_manual(values=linhas)+
    scale_size_manual(values=tamanhos)+
    # Legendas das linhas
    theme(legend.position="top", legend.box = "horizontal", axis.text.x = element_text(angle = 90, vjust = 0.5))+
    # Rótulos
    ylim(-6, 6)+
    labs(title = "Pontuação por Dia",
         x = "Dias",
         y = "Pontuação (Sentimentos Positivos-Negativos) [x10³]")+
    # Anotações, linhas de demarcação
    ggplot2::annotate("segment", x = x_turnos, xend = x_turnos, y = -6, yend = 6, colour = "dark grey")+
    ggplot2::annotate("text", x = as.Date("2018-10-07")-2, y = -5.5, label = "1° Turno")+
    ggplot2::annotate("text", x = as.Date("2018-10-28")-2, y = -5.5, label = "2° Turno")+
    # Formatando as datas
    scale_x_date(date_labels = "%Y-%m-%d", 
                 date_breaks = "1 day", 
                 date_minor_breaks = "1 day",
                 expand = expand_scale(.01))
  
  print(score_plot)
}

# Salvando Plots em PDF
{
  pdf("all_plot6.pdf", width=12,height=6.5)
  print(datafolha_plot)
  print(ibope_plot)
  print(xpipespe_plot)
  
  print(positivo_plot)
  print(negativo_plot)
  
  print(positivo_hist)
  print(negativo_hist)
  
  print(score_plot)
  
  dev.off()
}
  