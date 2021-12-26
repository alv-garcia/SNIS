## Leitura e análise de dados do SNIS

# Criação: 26/12/2021


#---------- Pacotes -------------

library(tidyverse)
library(geobr)
library(viridis)
library(ggpubr)

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

# Mapas dos municípios
MG <- read_municipality(code_muni = "MG" , year = 2018)
SP <- read_municipality(code_muni = "SP" , year = 2018)
RJ <- read_municipality(code_muni = "RJ" , year = 2018)

sudeste <- rbind(MG,SP,RJ)

# Mapas dos Estados
MG1 <- read_state(code_state = "MG")
SP1 <- read_state(code_state = "SP")
RJ1 <- read_state(code_state = "RJ")

estados <- rbind(MG1,SP1,RJ1)


sudeste <-sudeste %>%
  mutate(code_muni,"CODIGO" = str_sub(code_muni, start = 1, end = 6))

snis_sudeste <- dados %>%
  filter(Ano == 2019)

mapa <- sudeste %>%
  inner_join(snis_sudeste, by = "CODIGO")


# Mapa

mapa_sudeste <-ggplot()+
  geom_sf(data  = estados)+
  geom_sf(data = sudeste)+
  geom_sf(data = mapa, aes(fill = IN058),size = 0, alpha = 0.9)+
  theme_void()+
  scale_fill_viridis(trans = "log", name = "Índice de Perdas \n de faturamento")


# ------------- Tabela -------------------------


stable<- snis_sudeste %>% desc_statby(measure.var = "IN058" , grps = "Estado")

stable <- stable[, c("Estado", "length", "mean","min" , "max")]
stable.p <- ggtexttable(stable, rows = NULL,
                        theme = ttheme("light"))


# Unindo a tabela e o gráfico

figure <- ggarrange(mapa_sudeste, stable.p,
          ncol = 1, nrow = 2,
          heights = c(1, 0.5))

annotate_figure(figure,
                top = text_grob("Tarifa média de água \n 2019 - Sudeste -Brasil",
                                color = "black", face = "bold", size = 14),
                bottom = text_grob("Data source: \n SNIS,2019", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                fig.lab = "Figure 1", fig.lab.face = "bold"
)


mapa_sudeste



