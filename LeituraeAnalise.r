## Leitura e análise de dados do SNIS

# Criação: 26/12/2021


#---------- Pacotes -------------

library(tidyverse)


#---------- Função de leitura e limpeza--------------

#Definir o diretório de trabalho onde estão os arquivos
setwd("D:/DEV/SNIS")

# Definir o nome do arquivo
Arquivo <- "Agregado Sudeste.csv"


# Função para leitura e limpeza do arquivo - o segundo argumento define se o data frame solicitado deve
# vir em formato "Tidy" (Adicionar o TRUE)
LeituraSnis = function(Arquivo, Tidy ){

  db<-read.csv2(Arquivo,header = TRUE, skipNul = TRUE, row.names = NULL)

  nomes<-db %>%
    colnames()

  iniciais <-c("CODIGO","Municipio","Estado","Ano","COD_PREST", "Prestador","SIGLA","Abrangencia","Servico","Natureza")
  indicadores <- str_sub(nomes[11:length(nomes)], start = 1, end = 6)
  indicadores <-str_remove_all(indicadores,"[_.]")

  colnames(db)<-c(iniciais,indicadores)

  if( Tidy == "TRUE"){

  td<-gather(db,colnames(db[11]):colnames(db[length(db)]), key = indicador, value = valores)

  return(td)

  } else{

  return(db)

  }
}


# Exemplo
dados <- LeituraSnis(Arquivo, FALSE)

glimpse(dados)
