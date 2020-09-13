##### Cargar Paquetes #######

library(readxl)
library(dplyr)
library(tidyverse) 
library(lubridate)
library (ggplot2)

##### Importar dataset #######

dcpp <- read_excel("data/dcpp_para_analisis.xls")

# Filtrar los incluidos

dcpp_incluidos <- dcpp %>% filter(incluido == 1)

##### Generar Variable de Resultado: DCPP #####################################

dcpp_incluidos$natremia_basal <- as.numeric(dcpp_incluidos$natremia_basal)
dcpp_incluidos$natremia_post <- as.numeric(dcpp_incluidos$natremia_post)

dcpp_incluidos$creatinina_basal <- as.numeric(dcpp_incluidos$creatinina_basal)
dcpp_incluidos$creatinina_post <- as.numeric(dcpp_incluidos$creatinina_post)

dcpp_incluidos <- dcpp_incluidos %>% 
  mutate (dif_na = natremia_basal - natremia_post)

## Construir variable DCPP en relación a natremia:
# Diferencia en natremia >4 con natremia_posterior < 130

dcpp_incluidos <- dcpp_incluidos %>%
        mutate (dcpp = ifelse (dif_na > 4 & natremia_post < 130, "1", "0"))

## Construir variable DCPP en relación a creatinina:
# Aumento igual o mayor al 50% con respecto al basal

dcpp_incluidos <- dcpp_incluidos %>%
   mutate (dcpp = ifelse (creatinina_post >= (creatinina_basal*1.5), "1", dcpp))

#Aumento igual o mayor al 30% con respecto al basal cuando éste es >1.5

dcpp_incluidos <- dcpp_incluidos %>%
  mutate (dcpp = ifelse (creatinina_basal > 1.5 &
                           creatinina_post >= (creatinina_basal*1.3), "1", dcpp))

##### Generar variable de Resultado: adherencia a guías ####################

dcpp_incluidos <- dcpp_incluidos %>% 
  mutate(gramosxlitro = albumina_repuesta_gr/liquido_removido_lts)

dcpp_incluidos <- dcpp_incluidos %>% 
  mutate(adherencia = ifelse(gramosxlitro>0.6 & gramosxlitro<1, "1", "o"))

dcpp_incluidos <- dcpp_incluidos %>% 
  mutate(adherencia = ifelse(liquido_removido_lts>5, "1", "o"))

dcpp_incluidos$adherencia <- as.numeric(dcpp_incluidos$adherencia)

##### Construir otras variables predictoras ####################################
# IMC

dcpp_incluidos$peso_kg <- as.numeric(dcpp_incluidos$peso_kg)
dcpp_incluidos$talla_cm <- as.numeric(dcpp_incluidos$talla_cm)

dcpp_incluidos <- dcpp_incluidos %>% mutate (imc = peso_kg/((talla_cm/100)^2))

# EDAD 

dcpp_incluidos <- dcpp_incluidos %>% 
  mutate (edad = year(fecha_paracentesis) - 
            year(fecha_nacimiento))

dcpp_incluidos$imc <- format(round(dcpp_incluidos$imc, 2), nsmall=2)
dcpp_incluidos$imc <- as.numeric(dcpp_incluidos$imc)

# TAM

dcpp_incluidos <- dcpp_incluidos %>% 
  mutate (tam_basal = ((tas_basal + 2*tad_basal)/3))

dcpp_incluidos$tam_basal <- format(round(dcpp_incluidos$tam_basal, 2), nsmall=2)
dcpp_incluidos$tam_basal <- as.numeric(dcpp_incluidos$tam_basal)

##### Modelar otras variables predictoras #####################################

dcpp_incluidos <- subset(dcpp_incluidos, select = -dccp)
dcpp_incluidos$dcpp <- as.numeric(dcpp_incluidos$dcpp)

dcpp_incluidos$liquido_removido_lts <- as.numeric(dcpp_incluidos$liquido_removido_lts)
dcpp_incluidos$liquido_removido_lts <- format(round(dcpp_incluidos$liquido_removido_lts, 1), nsmall=1)
dcpp_incluidos$liquido_removido_lts <- as.numeric(dcpp_incluidos$liquido_removido_lts)


dcpp_incluidos <- mutate (dcpp_incluidos, otras_cual = 
                            ifelse(otras_cual %in% "hepatitis b", "hepatitis_b", otras_cual))

dcpp_incluidos <- mutate (dcpp_incluidos, otras_cual = 
                            ifelse(otras_cual %in% "hbv", "hepatitis_b", otras_cual))

dcpp_incluidos <- mutate (dcpp_incluidos, otras_cual = 
                            ifelse(otras_cual %in% "hepatitis B", "hepatitis_b", otras_cual))

dcpp_incluidos <- dcpp_incluidos %>% 
  mutate(gramosxlitro = albumina_repuesta_gr/liquido_removido_lts)

##### Escribir base en xls #####################################################

writexl:: write_xlsx(dcpp_incluidos,"base_final.xlsx") 

##### Explorar y describir la información #####################################

library(GGally)

ggpairs(dcpp_incluidos[, c("tam_basal", "edad", "natremia_basal", "creatinina_basal",
                           "peso_kg", "liquido_removido_lts", "imc")], 
        cardinality_threshold = 45)

ggsave("predictoras.png")

dcpp_incluidos %>%
  ggplot (aes(edad))+
  geom_bar()

dcpp_incluidos %>% filter (dcpp !="NA") %>%
  ggplot (aes(x = dcpp, y = natremia_basal, fill =dcpp)) +
  geom_boxplot()

dcpp_incluidos %>% filter (dcpp !="NA") %>%
  ggplot (aes(x = dcpp, y = creatinina_basal, fill =dcpp)) +
  geom_boxplot()

dcpp_incluidos %>% filter (dcpp !="NA") %>%
  ggplot (aes(x = dcpp, y = tam_basal, fill =dcpp)) +
  geom_boxplot()

##### Generar base con pacientes agrupando episodios ###########################

pacientes_unico <- dcpp_incluidos %>% distinct(id_paciente, .keep_all = TRUE)

view(pacientes_unico)
sum(pacientes_unico$coles)
sum(pacientes_unico$oh)
sum(pacientes_unico$ai)
sum(pacientes_unico$cripto)
sum(pacientes_unico$nash)
sum(pacientes_unico$otras)
sum(pacientes_unico$hcv)
table(pacientes_unico$otras_cual)

sum(pacientes_unico$eps)
sum(pacientes_unico$varices)
sum(pacientes_unico$pbe)
sum(pacientes_unico$chc)
sum(pacientes_unico$icc)
sum(pacientes_unico$irc)

##### Estadística Descriptiva (Características basales: Tabla 1) ########################################

mean(dcpp_incluidos$edad)
sd(dcpp_incluidos$edad)
median(dcpp_incluidos$edad)
quantile(dcpp_incluidos$edad)

table(dcpp_incluidos$sexo)

median(dcpp_incluidos$meld)
quantile(dcpp_incluidos$meld)
table(dcpp_incluidos$child)

median(dcpp_incluidos$imc, na.rm = TRUE)
quantile(dcpp_incluidos$imc , na.rm = TRUE)
sum(is.na(dcpp_incluidos$imc))

sum(dcpp_incluidos$bbloq)
sum(dcpp_incluidos$diur)
sum(dcpp_incluidos$ieca)

mean(dcpp_incluidos$creatinina_basal)
sd(dcpp_incluidos$creatinina_basal)
mean(dcpp_incluidos$natremia_basal)
sd(dcpp_incluidos$natremia_basal)

mean(dcpp_incluidos$tam_basal, na.rm = TRUE)
sd(dcpp_incluidos$tam_basal, na.rm = TRUE)
sum(is.na(dcpp_incluidos$tam_basal))

mean(dcpp_incluidos$tas_basal, na.rm = TRUE)
sd(dcpp_incluidos$tas_basal, na.rm = TRUE)
sum(is.na(dcpp_incluidos$tas_basal))

mean(dcpp_incluidos$tad_basal, na.rm = TRUE)
sd(dcpp_incluidos$tad_basal, na.rm = TRUE)
sum(is.na(dcpp_incluidos$tad_basal))

##### Estadística Descriptiva (Características de las paracentesis: Tabla 2)####

table(dcpp_incluidos$lugar_paracentesis)
mean(dcpp_incluidos$liquido_removido_lts, na.rm = TRUE)
median(dcpp_incluidos$liquido_removido_lts, na.rm = TRUE)
quantile(dcpp_incluidos$liquido_removido_lts, na.rm = TRUE)
sum(is.na(dcpp_incluidos$liquido_removido_lts))
mean(dcpp_incluidos$albumina_repuesta_gr, na.rm = TRUE)
median(dcpp_incluidos$albumina_repuesta_gr, na.rm = TRUE)
quantile(dcpp_incluidos$albumina_repuesta_gr, na.rm = TRUE)
sum(is.na(dcpp_incluidos$albumina_repuesta_gr))

mean(dcpp_incluidos$gramosxlitro, na.rm = TRUE)
median(dcpp_incluidos$gramosxlitro, na.rm = TRUE)
quantile(dcpp_incluidos$gramosxlitro, na.rm = TRUE)
sum(is.na(dcpp_incluidos$gramosxlitro))

sum(dcpp_incluidos$c_eps, na.rm = TRUE)
sum(dcpp_incluidos$c_fistu, na.rm = TRUE)
sum(dcpp_incluidos$c_hemo, na.rm = TRUE)
sum(dcpp_incluidos$c_perfo, na.rm = TRUE)

   

