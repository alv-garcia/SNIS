## Leitura e análise de dados do SNIS

# Criação: 26/12/2021


#---------- Pacotes -------------

library(tidyverse)
library(geobr)
library(viridis)


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



#---------- Mapas -----------------


MG <- read_municipality(code_muni = "MG" , year = 2018)
SP <- read_municipality(code_muni = "SP" , year = 2018)
RJ <- read_municipality(code_muni = "RJ" , year = 2018)


sudeste <- rbind(MG,SP,RJ)


sudeste <-sudeste %>%
  mutate(code_muni,"CODIGO" = str_sub(code_muni, start = 1, end = 6))

snis_sudeste <- dados %>%
  filter(Ano == 2020)

mapa <- sudeste %>%
  inner_join(snis_sudeste, by = "CODIGO")


# Mapa


ggplot()+
  geom_sf(data = mapa, aes(fill = IN049), color ="#f5f5f2" ,size = 0, alpha = 0.9)+
  theme_void()+
  scale_fill_viridis(trans = "log", name = "ìndice de perdas de faturamento")

