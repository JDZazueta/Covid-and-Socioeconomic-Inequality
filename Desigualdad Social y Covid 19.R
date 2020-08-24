# Titulo:   Publicación revista Adminsitración Publica de Mexico
# Datos:    Datos coronavirus Mexico
# Fecha:    22-04-2020
# Autores:  Daniel y Max

# --- Directory and packages
rm(list = ls())
setwd("~/Desktop/Covid 19 Mex")

library(survival)
library(survminer)
library(data.table)
library(tidyverse)
library(reshape2)
library(gghighlight)
library(RColorBrewer)
library(ggridges)
library(wesanderson)
library(ggpubr)
require(gridExtra)
library(lubridate)

# ---  Importar los datos
# Datos del 22 de abril
#data <- read.csv("Data/200422COVID19MEXICO.csv", header = T)

# Datos del 01 de agosto
data <- read.csv("Data/200801COVID19MEXICO.csv", header = T)

data <- data.table(data)
table(data$RESULTADO)

data <- data[data$RESULTADO==1]
table(data$RESULTADO)


########################################################################################################
#
#                       Descriptivo de las muertes
#
########################################################################################################


# Crear variable de mortalidad para aquellos que falleciron
# 1 Fallecieron, 0 no Fallecieron
data$mortalidad[data$FECHA_DEF!="9999-99-99"] <-1
data$mortalidad[data$FECHA_DEF=="9999-99-99"] <-0
data$mortalidad <- as.factor(data$mortalidad)

table(data$mortalidad)

plot(table(data$FECHA_DEF[data$mortalidad==1]))


# Grafica de Fecha de inicio de sintomas
Grafica_1 <- ggplot(data, mapping = aes(x=FECHA_SINTOMAS, fill=mortalidad))+
  geom_bar(color="black") + 
  theme_classic() +
  scale_fill_manual(values = c("red2","lightblue"), 
                    name = "Tipo de caso", labels = c("Caso nuevo", "Defuncion")) +
  #scale_fill_discrete() +
  theme(axis.text.x = element_text(color="black", 
                                   size=9, angle=90),
        legend.position = c(0.2, 0.8)) +
  #labs(fill = "Tipo de caso") +

  scale_x_discrete(breaks=c("2020-03-01", "2020-03-15","2020-04-01","2020-04-15",
                            "2020-05-01","2020-05-15","2020-06-01","2020-06-15",
                            "2020-07-01","2020-07-15", "2020-08-01")) +
  labs(x="Fecha",
       y="Numero de casos nuevos por dia",
       caption = "")
Grafica_1
ggsave(filename = "Fecha_inicio_sintomas.pdf", path= "R Codes/Publicacion Mex/Graficas publicacion/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# Grafica de fecha de ingreso al hosptial
Grafica_2 <- ggplot(data[data$mortalidad==0], mapping = aes(x=FECHA_INGRESO))+
  geom_bar() + 
  theme_classic()
Grafica_2

# Grafica de fecha de defuncion
Grafica_3 <- ggplot(data[data$FECHA_DEF!="9999-99-99"], mapping = aes(x=FECHA_DEF))+
  geom_bar(fill="lightblue", color="black") + 
  theme_classic()  +
  theme(axis.text.x = element_text(color="black",  size=9, angle=90),
                         legend.position = "") +
  scale_x_discrete(breaks=c("2020-03-01", "2020-03-15","2020-04-01","2020-04-15",
                            "2020-05-01","2020-05-15","2020-06-01","2020-06-15",
                            "2020-07-01","2020-07-15", "2020-08-01")) +
  labs(x="Fecha de defunción",
       y="Numero de defunciones por dia")
Grafica_3
ggsave(filename = "Fecha_defunciones.pdf", path= "R Codes/Publicacion Mex/Graficas publicacion/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# Combinar sintomas y defunciones

Plot_1 <- ggarrange(Grafica_1, Grafica_3,
                    labels = c("   Sintomas", " Defunciones"),
                    ncol = 2, nrow = 1)
Plot_1
ggsave(filename = "Figura_1.pdf", path= "R Codes/Publicacion Mex/Graficas publicacion/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# Grafica de Fecha de inicio de sintomas
# No es buena idea esta grafica y quita tiempo
#Grafica_Estados <- ggplot(data, mapping = aes(x=FECHA_SINTOMAS, y=factor(ENTIDAD_RES)))+
#  geom_density_ridges() +
#  theme_classic()
#Grafica_Estados



# -------------------------------------------------------------------------------------- #
#       ----        1. Preparar bases por mes con variables
# -------------------------------------------------------------------------------------- #

# Declarar las variables como Tiempo
data$FECHA_INGRESO  <- as.Date(data$FECHA_INGRESO,"%Y-%m-%d")
data$FECHA_SINTOMAS  <- as.Date(data$FECHA_SINTOMAS,"%Y-%m-%d")
data$FECHA_ACTUALIZACION  <- as.Date(data$FECHA_ACTUALIZACION,"%Y-%m-%d")
data$FECHA_DEF <- as.Date(data$FECHA_DEF,"%Y-%m-%d")


# Empezar a seleccionar las muestras por tiempo, con base en la fecha de ingreso

Febrero <- data %>% 
  filter(FECHA_INGRESO<="2020-02-28") %>% # n = 10
  select(ID_REGISTRO, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, 
         SEXO, EDAD, mortalidad, EMBARAZO, HABLA_LENGUA_INDIG,
         NEUMONIA, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION,
         CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, OBESIDAD, TABAQUISMO)


########################################################
# ----- Marzo
########################################################

Marzo <- data %>% 
  filter(FECHA_INGRESO>="2020-03-01" & FECHA_INGRESO<="2020-03-31") %>% # n = 2,514
  select(ID_REGISTRO, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, ENTIDAD_RES, MUNICIPIO_RES,
         SEXO, EDAD, mortalidad, EMBARAZO, HABLA_LENGUA_INDIG,
         NEUMONIA, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION,
         CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, OBESIDAD, TABAQUISMO, ORIGEN, SECTOR)

# Fechas con datos recientes
Marzo$Fecha_inicio <- as.Date("2020-03-01")
Marzo$Fecha_final <- as.Date("2020-03-31")

Marzo$No_Lifespan <- Marzo$Fecha_final - Marzo$FECHA_SINTOMAS
Marzo$Lifespan <- Marzo$FECHA_DEF - Marzo$FECHA_SINTOMAS


# Ahora necesito saber la contribution de tiempo de cada uno
Marzo <- as.data.table(Marzo)

Marzo$Contribucion_dias[Marzo$mortalidad==0] <- Marzo$Fecha_final[Marzo$mortalidad==0] - Marzo$FECHA_SINTOMAS[Marzo$mortalidad==0]
Marzo$Contribucion_dias[Marzo$mortalidad==1] <- Marzo$FECHA_DEF[Marzo$mortalidad==1] - Marzo$FECHA_SINTOMAS[Marzo$mortalidad==1]

Marzo$Contribucion_dias <- as.numeric(Marzo$Contribucion_dias)

# --- Creamos variable Evento
Marzo$Evento[Marzo$mortalidad==0] <- 0
Marzo$Evento[Marzo$mortalidad==1] <- 1

table(Marzo$Evento,Marzo$mortalidad)
table(Marzo$Evento)

#Marzo$Evento <- factor(Marzo$Evento, levels = c(0,1),
#                               labels = c("Sobreviviente","Falleció"))


# --- Sex
Marzo$sex[Marzo$SEXO==1] <-0
Marzo$sex[Marzo$SEXO==2] <-1

Marzo$sex <- factor(Marzo$sex, levels = c(0,1),
                    labels = c("Mujeres","Hombres"))


# --- Edad
Marzo$Edad_agru[Marzo$EDAD>=0 & Marzo$EDAD<=39] <- 39
Marzo$Edad_agru[Marzo$EDAD>=40 & Marzo$EDAD<=59] <- 59
Marzo$Edad_agru[Marzo$EDAD>=60 & Marzo$EDAD<=69] <- 69
Marzo$Edad_agru[Marzo$EDAD>=70 & Marzo$EDAD<=79] <- 79
Marzo$Edad_agru[Marzo$EDAD>=80 & Marzo$EDAD<=120] <- 80

Marzo$Edad_agru <- as.factor(Marzo$Edad_agru)

# ----- EMBARAZO 
Marzo$Embarazo[Marzo$EMBARAZO==2] <- 0
Marzo$Embarazo[Marzo$EMBARAZO==1] <- 1

# ---- Lengua Indigena
Marzo$Lengua_Ind[Marzo$HABLA_LENGUA_INDIG==2] <- 0
Marzo$Lengua_Ind[Marzo$HABLA_LENGUA_INDIG==1] <- 1

# -- Neumonia
Marzo$Neumonia[Marzo$NEUMONIA==2] <- 0
Marzo$Neumonia[Marzo$NEUMONIA==1] <- 1

# -- EPOC
Marzo$Epoc[Marzo$EPOC==2] <- 0
Marzo$Epoc[Marzo$EPOC==1] <- 1

# -- ASMA
Marzo$Asma[Marzo$ASMA==2] <- 0
Marzo$Asma[Marzo$ASMA==1] <- 1

# -- Respiratorias
# Numero de complicaciones respirtaorias
Marzo$Respiratorias <- Marzo$Neumonia + Marzo$Epoc + Marzo$Asma
table(Marzo$Respiratorias)

# Problemas respiratorios
Marzo$Prob_respiratorios[Marzo$Respiratorias==0] <-0
Marzo$Prob_respiratorios[Marzo$Respiratorias>=1 & Marzo$Respiratorias<=3] <-1

table(Marzo$Prob_respiratorios)

####
# --- Enfermedades crónicas
####

# Diabetes
Marzo$Diabetes[Marzo$DIABETES==2] <-0 
Marzo$Diabetes[Marzo$DIABETES==1] <-1

# Hipertension
Marzo$Hipertension[Marzo$HIPERTENSION==2] <-0 
Marzo$Hipertension[Marzo$HIPERTENSION==1] <-1 

# Cardiovascular
Marzo$Cardiovascular[Marzo$CARDIOVASCULAR==2] <-0 
Marzo$Cardiovascular[Marzo$CARDIOVASCULAR==1] <-1 

# Problemas renal cronicos
Marzo$Renal_cro[Marzo$RENAL_CRONICA==2] <-0 
Marzo$Renal_cro[Marzo$RENAL_CRONICA==1] <-1 


# Comorbilidades cronicas
Marzo$Sum_enf <- Marzo$Diabetes + Marzo$Hipertension + Marzo$Cardiovascular + Marzo$Renal_cro
table(Marzo$Sum_enf)

Marzo$Enf_cronica[Marzo$Sum_enf==0] <- 0
Marzo$Enf_cronica[Marzo$Sum_enf>=1 & Marzo$Sum_enf<=4] <- 1

# Inmunosuprecion
Marzo$Inmunosuprecion[Marzo$INMUSUPR==2] <-0 
Marzo$Inmunosuprecion[Marzo$INMUSUPR==1] <-1 

####
# Conducta salud
####

# - Obesidad
Marzo$Obesidad[Marzo$OBESIDAD==2] <-0 
Marzo$Obesidad[Marzo$OBESIDAD==1] <-1

# - Tabaquismo
Marzo$Fumar[Marzo$TABAQUISMO==2] <-0 
Marzo$Fumar[Marzo$TABAQUISMO==1] <-1

#####
# Variables de servicios de salud
#####

# USMER
Marzo$USMER[Marzo$ORIGEN==2] <-0 
Marzo$USMER[Marzo$ORIGEN==1] <-1 

# Institución que brindo atención
# 0	IMSS 	IMSS-BIENESTAR	ISSSTE
# 1	Cruz roja	DIF	ESTATAL	MUNICIPA	UNIVERSITAORIO
# 2	SSA				
# 3	PEMEX	SEDENA	SEMAR		
# 4	Privado				
#
Marzo$Institucion[Marzo$SECTOR>=4 & Marzo$SECTOR<=6] <- 0
Marzo$Institucion[Marzo$SECTOR==1 | Marzo$SECTOR==2 | Marzo$SECTOR==3 |
                    Marzo$SECTOR==7 | Marzo$SECTOR==13] <- 1
Marzo$Institucion[Marzo$SECTOR==12] <- 2
Marzo$Institucion[Marzo$SECTOR==8 | Marzo$SECTOR==10 | Marzo$SECTOR==11] <- 3
Marzo$Institucion[Marzo$SECTOR==9] <- 4

# Checamos 
table(Marzo$SECTOR, Marzo$Institucion)


Marzo_Pre_survival <- Marzo %>% 
  select(ID_REGISTRO, FECHA_SINTOMAS, FECHA_DEF, Fecha_inicio, Fecha_final, Contribucion_dias,
         sex, Edad_agru, Evento, Embarazo,Lengua_Ind, ENTIDAD_RES, MUNICIPIO_RES,
         Neumonia, Epoc,Asma, Respiratorias, Prob_respiratorios,
         Diabetes, Hipertension, Cardiovascular, Renal_cro, Sum_enf, Enf_cronica,
         Inmunosuprecion, USMER, Institucion,
         Obesidad,Fumar) %>% 
  filter(Contribucion_dias>=1 & Contribucion_dias<=31)

hist(Marzo_Pre_survival$Contribucion_dias)

########################################################
# ----- Abril
########################################################

Abril <- data %>% 
  filter(FECHA_INGRESO>="2020-04-01" & FECHA_INGRESO<="2020-04-30") %>% # n = 26,134
  select(ID_REGISTRO, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, ENTIDAD_RES, MUNICIPIO_RES,
         SEXO, EDAD, mortalidad, EMBARAZO, HABLA_LENGUA_INDIG,
         NEUMONIA, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION,
         CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, OBESIDAD, TABAQUISMO, ORIGEN, SECTOR)


# Fechas con datos recientes
Abril$Fecha_inicio <- as.Date("2020-04-01")
Abril$Fecha_final <- as.Date("2020-04-30")

Abril$No_Lifespan <- Abril$Fecha_final - Abril$FECHA_SINTOMAS
Abril$Lifespan <- Abril$FECHA_DEF - Abril$FECHA_SINTOMAS

# Ahora necesito saber la contribution de tiempo de cada uno
Abril <- as.data.table(Abril)

Abril$Contribucion_dias[Abril$mortalidad==0] <- Abril$Fecha_final[Abril$mortalidad==0] - Abril$FECHA_SINTOMAS[Abril$mortalidad==0]
Abril$Contribucion_dias[Abril$mortalidad==1] <- Abril$FECHA_DEF[Abril$mortalidad==1] - Abril$FECHA_SINTOMAS[Abril$mortalidad==1]

Abril$Contribucion_dias <- as.numeric(Abril$Contribucion_dias)

# --- Creamos variable Evento
Abril$Evento[Abril$mortalidad==0] <- 0
Abril$Evento[Abril$mortalidad==1] <- 1

table(Abril$Evento,Abril$mortalidad)

table(Abril$Evento)

#Abril$Evento <- factor(Abril$Evento, levels = c(0,1),
#                               labels = c("Sobreviviente","Falleció"))


# --- Sex
Abril$sex[Abril$SEXO==1] <-0
Abril$sex[Abril$SEXO==2] <-1

Abril$sex <- factor(Abril$sex, levels = c(0,1),
                    labels = c("Mujeres","Hombres"))


# --- Edad
Abril$Edad_agru[Abril$EDAD>=0 & Abril$EDAD<=39] <- 39
Abril$Edad_agru[Abril$EDAD>=40 & Abril$EDAD<=59] <- 59
Abril$Edad_agru[Abril$EDAD>=60 & Abril$EDAD<=69] <- 69
Abril$Edad_agru[Abril$EDAD>=70 & Abril$EDAD<=79] <- 79
Abril$Edad_agru[Abril$EDAD>=80 & Abril$EDAD<=120] <- 80

Abril$Edad_agru <- as.factor(Abril$Edad_agru)

# ----- EMBARAZO 
Abril$Embarazo[Abril$EMBARAZO==2] <- 0
Abril$Embarazo[Abril$EMBARAZO==1] <- 1

# ---- Lengua Indigena
Abril$Lengua_Ind[Abril$HABLA_LENGUA_INDIG==2] <- 0
Abril$Lengua_Ind[Abril$HABLA_LENGUA_INDIG==1] <- 1

# -- Neumonia
Abril$Neumonia[Abril$NEUMONIA==2] <- 0
Abril$Neumonia[Abril$NEUMONIA==1] <- 1

# -- EPOC
Abril$Epoc[Abril$EPOC==2] <- 0
Abril$Epoc[Abril$EPOC==1] <- 1

# -- ASMA
Abril$Asma[Abril$ASMA==2] <- 0
Abril$Asma[Abril$ASMA==1] <- 1

# -- Respiratorias
# Numero de complicaciones respirtaorias
Abril$Respiratorias <- Abril$Neumonia + Abril$Epoc + Abril$Asma
table(Abril$Respiratorias)

# Problemas respiratorios
Abril$Prob_respiratorios[Abril$Respiratorias==0] <-0
Abril$Prob_respiratorios[Abril$Respiratorias>=1 & Abril$Respiratorias<=3] <-1

table(Abril$Prob_respiratorios)

####
# --- Enfermedades crónicas
####

# Diabetes
Abril$Diabetes[Abril$DIABETES==2] <-0 
Abril$Diabetes[Abril$DIABETES==1] <-1

# Hipertension
Abril$Hipertension[Abril$HIPERTENSION==2] <-0 
Abril$Hipertension[Abril$HIPERTENSION==1] <-1 

# Cardiovascular
Abril$Cardiovascular[Abril$CARDIOVASCULAR==2] <-0 
Abril$Cardiovascular[Abril$CARDIOVASCULAR==1] <-1 

# Problemas renal cronicos
Abril$Renal_cro[Abril$RENAL_CRONICA==2] <-0 
Abril$Renal_cro[Abril$RENAL_CRONICA==1] <-1 


# Comorbilidades cronicas
Abril$Sum_enf <- Abril$Diabetes + Abril$Hipertension + Abril$Cardiovascular + Abril$Renal_cro
table(Abril$Sum_enf)

Abril$Enf_cronica[Abril$Sum_enf==0] <- 0
Abril$Enf_cronica[Abril$Sum_enf>=1 & Abril$Sum_enf<=4] <- 1

# Inmunosuprecion
Abril$Inmunosuprecion[Abril$INMUSUPR==2] <-0 
Abril$Inmunosuprecion[Abril$INMUSUPR==1] <-1 

####
# Conducta salud
####

# - Obesidad
Abril$Obesidad[Abril$OBESIDAD==2] <-0 
Abril$Obesidad[Abril$OBESIDAD==1] <-1

# - Tabaquismo
Abril$Fumar[Abril$TABAQUISMO==2] <-0 
Abril$Fumar[Abril$TABAQUISMO==1] <-1

#####
# Variables de servicios de salud
#####

# USMER
Abril$USMER[Abril$ORIGEN==2] <-0 
Abril$USMER[Abril$ORIGEN==1] <-1 

# Institución que brindo atención
# 0	IMSS 	IMSS-BIENESTAR	ISSSTE
# 1	Cruz roja	DIF	ESTATAL	MUNICIPA	UNIVERSITAORIO
# 2	SSA				
# 3	PEMEX	SEDENA	SEMAR		
# 4	Privado				
#
Abril$Institucion[Abril$SECTOR>=4 & Abril$SECTOR<=6] <- 0
Abril$Institucion[Abril$SECTOR==1 | Abril$SECTOR==2 | Abril$SECTOR==3 |
                    Abril$SECTOR==7 | Abril$SECTOR==13] <- 1
Abril$Institucion[Abril$SECTOR==12] <- 2
Abril$Institucion[Abril$SECTOR==8 | Abril$SECTOR==10 | Abril$SECTOR==11] <- 3
Abril$Institucion[Abril$SECTOR==9] <- 4

# Checamos 
table(Abril$SECTOR, Abril$Institucion)

Abril_Pre_survival <- Abril %>% 
  select(ID_REGISTRO, FECHA_SINTOMAS, FECHA_DEF, Fecha_inicio, Fecha_final, Contribucion_dias,
         sex, Edad_agru, Evento, Embarazo,Lengua_Ind, ENTIDAD_RES, MUNICIPIO_RES,
         Neumonia, Epoc,Asma, Respiratorias, Prob_respiratorios,
         Diabetes, Hipertension, Cardiovascular, Renal_cro, Sum_enf, Enf_cronica,
         Inmunosuprecion, USMER, Institucion,
         Obesidad,Fumar) %>% 
  filter(Contribucion_dias>=1 & Contribucion_dias<=30)


########################################################
# ----- Mayo
########################################################

Mayo <- data %>% 
  filter(FECHA_INGRESO>="2020-05-01" & FECHA_INGRESO<="2020-05-31") %>% # n = 84,623
  select(ID_REGISTRO, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, ENTIDAD_RES, MUNICIPIO_RES,
         SEXO, EDAD, mortalidad, EMBARAZO, HABLA_LENGUA_INDIG,
         NEUMONIA, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION,
         CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, OBESIDAD, TABAQUISMO, ORIGEN, SECTOR)


# Fechas con datos recientes
Mayo$Fecha_inicio <- as.Date("2020-05-01")
Mayo$Fecha_final <- as.Date("2020-05-31")

Mayo$No_Lifespan <- Mayo$Fecha_final - Mayo$FECHA_SINTOMAS
Mayo$Lifespan <- Mayo$FECHA_DEF - Mayo$FECHA_SINTOMAS

# Ahora necesito saber la contribution de tiempo de cada uno
Mayo <- as.data.table(Mayo)

Mayo$Contribucion_dias[Mayo$mortalidad==0] <- Mayo$Fecha_final[Mayo$mortalidad==0] - Mayo$FECHA_SINTOMAS[Mayo$mortalidad==0]
Mayo$Contribucion_dias[Mayo$mortalidad==1] <- Mayo$FECHA_DEF[Mayo$mortalidad==1] - Mayo$FECHA_SINTOMAS[Mayo$mortalidad==1]

Mayo$Contribucion_dias <- as.numeric(Mayo$Contribucion_dias)

# --- Creamos variable Evento
Mayo$Evento[Mayo$mortalidad==0] <- 0
Mayo$Evento[Mayo$mortalidad==1] <- 1

table(Mayo$Evento,Mayo$mortalidad)

table(Mayo$Evento)

#Mayo$Evento <- factor(Mayo$Evento, levels = c(0,1),
#                               labels = c("Sobreviviente","Falleció"))


# --- Sex
Mayo$sex[Mayo$SEXO==1] <-0
Mayo$sex[Mayo$SEXO==2] <-1

Mayo$sex <- factor(Mayo$sex, levels = c(0,1),
                   labels = c("Mujeres","Hombres"))


# --- Edad
Mayo$Edad_agru[Mayo$EDAD>=0 & Mayo$EDAD<=39] <- 39
Mayo$Edad_agru[Mayo$EDAD>=40 & Mayo$EDAD<=59] <- 59
Mayo$Edad_agru[Mayo$EDAD>=60 & Mayo$EDAD<=69] <- 69
Mayo$Edad_agru[Mayo$EDAD>=70 & Mayo$EDAD<=79] <- 79
Mayo$Edad_agru[Mayo$EDAD>=80 & Mayo$EDAD<=120] <- 80

Mayo$Edad_agru <- as.factor(Mayo$Edad_agru)

# ----- EMBARAZO 
Mayo$Embarazo[Mayo$EMBARAZO==2] <- 0
Mayo$Embarazo[Mayo$EMBARAZO==1] <- 1

# ---- Lengua Indigena
Mayo$Lengua_Ind[Mayo$HABLA_LENGUA_INDIG==2] <- 0
Mayo$Lengua_Ind[Mayo$HABLA_LENGUA_INDIG==1] <- 1

# -- Neumonia
Mayo$Neumonia[Mayo$NEUMONIA==2] <- 0
Mayo$Neumonia[Mayo$NEUMONIA==1] <- 1

# -- EPOC
Mayo$Epoc[Mayo$EPOC==2] <- 0
Mayo$Epoc[Mayo$EPOC==1] <- 1

# -- ASMA
Mayo$Asma[Mayo$ASMA==2] <- 0
Mayo$Asma[Mayo$ASMA==1] <- 1

# -- Respiratorias
# Numero de complicaciones respirtaorias
Mayo$Respiratorias <- Mayo$Neumonia + Mayo$Epoc + Mayo$Asma
table(Mayo$Respiratorias)

# Problemas respiratorios
Mayo$Prob_respiratorios[Mayo$Respiratorias==0] <-0
Mayo$Prob_respiratorios[Mayo$Respiratorias>=1 & Mayo$Respiratorias<=3] <-1

table(Mayo$Prob_respiratorios)

####
# --- Enfermedades crónicas
####

# Diabetes
Mayo$Diabetes[Mayo$DIABETES==2] <-0 
Mayo$Diabetes[Mayo$DIABETES==1] <-1

# Hipertension
Mayo$Hipertension[Mayo$HIPERTENSION==2] <-0 
Mayo$Hipertension[Mayo$HIPERTENSION==1] <-1 

# Cardiovascular
Mayo$Cardiovascular[Mayo$CARDIOVASCULAR==2] <-0 
Mayo$Cardiovascular[Mayo$CARDIOVASCULAR==1] <-1 

# Problemas renal cronicos
Mayo$Renal_cro[Mayo$RENAL_CRONICA==2] <-0 
Mayo$Renal_cro[Mayo$RENAL_CRONICA==1] <-1 


# Comorbilidades cronicas
Mayo$Sum_enf <- Mayo$Diabetes + Mayo$Hipertension + Mayo$Cardiovascular + Mayo$Renal_cro
table(Mayo$Sum_enf)

Mayo$Enf_cronica[Mayo$Sum_enf==0] <- 0
Mayo$Enf_cronica[Mayo$Sum_enf>=1 & Mayo$Sum_enf<=4] <- 1

# Inmunosuprecion
Mayo$Inmunosuprecion[Mayo$INMUSUPR==2] <-0 
Mayo$Inmunosuprecion[Mayo$INMUSUPR==1] <-1 

####
# Conducta salud
####

# - Obesidad
Mayo$Obesidad[Mayo$OBESIDAD==2] <-0 
Mayo$Obesidad[Mayo$OBESIDAD==1] <-1

# - Tabaquismo
Mayo$Fumar[Mayo$TABAQUISMO==2] <-0 
Mayo$Fumar[Mayo$TABAQUISMO==1] <-1

#####
# Variables de servicios de salud
#####

# USMER
Mayo$USMER[Mayo$ORIGEN==2] <-0 
Mayo$USMER[Mayo$ORIGEN==1] <-1 

# Institución que brindo atención
# 0	IMSS 	IMSS-BIENESTAR	ISSSTE
# 1	Cruz roja	DIF	ESTATAL	MUNICIPA	UNIVERSITAORIO
# 2	SSA				
# 3	PEMEX	SEDENA	SEMAR		
# 4	Privado				
#
Mayo$Institucion[Mayo$SECTOR>=4 & Mayo$SECTOR<=6] <- 0
Mayo$Institucion[Mayo$SECTOR==1 | Mayo$SECTOR==2 | Mayo$SECTOR==3 |
                   Mayo$SECTOR==7 | Mayo$SECTOR==13] <- 1
Mayo$Institucion[Mayo$SECTOR==12] <- 2
Mayo$Institucion[Mayo$SECTOR==8 | Mayo$SECTOR==10 | Mayo$SECTOR==11] <- 3
Mayo$Institucion[Mayo$SECTOR==9] <- 4

# Checamos 
table(Mayo$SECTOR, Mayo$Institucion)


Mayo_Pre_survival <- Mayo %>% 
  select(ID_REGISTRO, FECHA_SINTOMAS, FECHA_DEF, Fecha_inicio, Fecha_final, Contribucion_dias,
         sex, Edad_agru, Evento, Embarazo,Lengua_Ind, ENTIDAD_RES, MUNICIPIO_RES,
         Neumonia, Epoc,Asma, Respiratorias, Prob_respiratorios,
         Diabetes, Hipertension, Cardiovascular, Renal_cro, Sum_enf, Enf_cronica,
         Inmunosuprecion, USMER, Institucion,
         Obesidad,Fumar) %>% 
  filter(Contribucion_dias>=1 & Contribucion_dias<=31)


########################################################
# ----- Junio
########################################################

Junio <- data %>% 
  filter(FECHA_INGRESO>="2020-06-01" & FECHA_INGRESO<="2020-06-30") %>% # n = 147,367
  select(ID_REGISTRO, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, ENTIDAD_RES, MUNICIPIO_RES,
         SEXO, EDAD, mortalidad, EMBARAZO, HABLA_LENGUA_INDIG,
         NEUMONIA, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION,
         CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, OBESIDAD, TABAQUISMO, ORIGEN, SECTOR)


# Fechas con datos recientes
Junio$Fecha_inicio <- as.Date("2020-06-01")
Junio$Fecha_final <- as.Date("2020-06-30")

Junio$No_Lifespan <- Junio$Fecha_final - Junio$FECHA_SINTOMAS
Junio$Lifespan <- Junio$FECHA_DEF - Junio$FECHA_SINTOMAS

# Ahora necesito saber la contribution de tiempo de cada uno
Junio <- as.data.table(Junio)

Junio$Contribucion_dias[Junio$mortalidad==0] <- Junio$Fecha_final[Junio$mortalidad==0] - Junio$FECHA_SINTOMAS[Junio$mortalidad==0]
Junio$Contribucion_dias[Junio$mortalidad==1] <- Junio$FECHA_DEF[Junio$mortalidad==1] - Junio$FECHA_SINTOMAS[Junio$mortalidad==1]

Junio$Contribucion_dias <- as.numeric(Junio$Contribucion_dias)

# --- Creamos variable Evento
Junio$Evento[Junio$mortalidad==0] <- 0
Junio$Evento[Junio$mortalidad==1] <- 1

table(Junio$Evento,Junio$mortalidad)

table(Junio$Evento)

#Junio$Evento <- factor(Junio$Evento, levels = c(0,1),
#                               labels = c("Sobreviviente","Falleció"))


# --- Sex
Junio$sex[Junio$SEXO==1] <-0
Junio$sex[Junio$SEXO==2] <-1

Junio$sex <- factor(Junio$sex, levels = c(0,1),
                    labels = c("Mujeres","Hombres"))


# --- Edad
Junio$Edad_agru[Junio$EDAD>=0 & Junio$EDAD<=39] <- 39
Junio$Edad_agru[Junio$EDAD>=40 & Junio$EDAD<=59] <- 59
Junio$Edad_agru[Junio$EDAD>=60 & Junio$EDAD<=69] <- 69
Junio$Edad_agru[Junio$EDAD>=70 & Junio$EDAD<=79] <- 79
Junio$Edad_agru[Junio$EDAD>=80 & Junio$EDAD<=120] <- 80

Junio$Edad_agru <- as.factor(Junio$Edad_agru)

# ----- EMBARAZO 
Junio$Embarazo[Junio$EMBARAZO==2] <- 0
Junio$Embarazo[Junio$EMBARAZO==1] <- 1

# ---- Lengua Indigena
Junio$Lengua_Ind[Junio$HABLA_LENGUA_INDIG==2] <- 0
Junio$Lengua_Ind[Junio$HABLA_LENGUA_INDIG==1] <- 1

# -- Neumonia
Junio$Neumonia[Junio$NEUMONIA==2] <- 0
Junio$Neumonia[Junio$NEUMONIA==1] <- 1

# -- EPOC
Junio$Epoc[Junio$EPOC==2] <- 0
Junio$Epoc[Junio$EPOC==1] <- 1

# -- ASMA
Junio$Asma[Junio$ASMA==2] <- 0
Junio$Asma[Junio$ASMA==1] <- 1

# -- Respiratorias
# Numero de complicaciones respirtaorias
Junio$Respiratorias <- Junio$Neumonia + Junio$Epoc + Junio$Asma
table(Junio$Respiratorias)

# Problemas respiratorios
Junio$Prob_respiratorios[Junio$Respiratorias==0] <-0
Junio$Prob_respiratorios[Junio$Respiratorias>=1 & Junio$Respiratorias<=3] <-1

table(Junio$Prob_respiratorios)

####
# --- Enfermedades crónicas
####

# Diabetes
Junio$Diabetes[Junio$DIABETES==2] <-0 
Junio$Diabetes[Junio$DIABETES==1] <-1

# Hipertension
Junio$Hipertension[Junio$HIPERTENSION==2] <-0 
Junio$Hipertension[Junio$HIPERTENSION==1] <-1 

# Cardiovascular
Junio$Cardiovascular[Junio$CARDIOVASCULAR==2] <-0 
Junio$Cardiovascular[Junio$CARDIOVASCULAR==1] <-1 

# Problemas renal cronicos
Junio$Renal_cro[Junio$RENAL_CRONICA==2] <-0 
Junio$Renal_cro[Junio$RENAL_CRONICA==1] <-1 


# Comorbilidades cronicas
Junio$Sum_enf <- Junio$Diabetes + Junio$Hipertension + Junio$Cardiovascular + Junio$Renal_cro
table(Junio$Sum_enf)

Junio$Enf_cronica[Junio$Sum_enf==0] <- 0
Junio$Enf_cronica[Junio$Sum_enf>=1 & Junio$Sum_enf<=4] <- 1

# Inmunosuprecion
Junio$Inmunosuprecion[Junio$INMUSUPR==2] <-0 
Junio$Inmunosuprecion[Junio$INMUSUPR==1] <-1 

####
# Conducta salud
####

# - Obesidad
Junio$Obesidad[Junio$OBESIDAD==2] <-0 
Junio$Obesidad[Junio$OBESIDAD==1] <-1

# - Tabaquismo
Junio$Fumar[Junio$TABAQUISMO==2] <-0 
Junio$Fumar[Junio$TABAQUISMO==1] <-1

#####
# Variables de servicios de salud
#####

# USMER
Junio$USMER[Junio$ORIGEN==2] <-0 
Junio$USMER[Junio$ORIGEN==1] <-1 

# Institución que brindo atención
# 0	IMSS 	IMSS-BIENESTAR	ISSSTE
# 1	Cruz roja	DIF	ESTATAL	MUNICIPA	UNIVERSITAORIO
# 2	SSA				
# 3	PEMEX	SEDENA	SEMAR		
# 4	Privado				
#
Junio$Institucion[Junio$SECTOR>=4 & Junio$SECTOR<=6] <- 0
Junio$Institucion[Junio$SECTOR==1 | Junio$SECTOR==2 | Junio$SECTOR==3 |
                    Junio$SECTOR==7 | Junio$SECTOR==13] <- 1
Junio$Institucion[Junio$SECTOR==12] <- 2
Junio$Institucion[Junio$SECTOR==8 | Junio$SECTOR==10 | Junio$SECTOR==11] <- 3
Junio$Institucion[Junio$SECTOR==9] <- 4

# Checamos 
table(Junio$SECTOR, Junio$Institucion)

# -- Data pre survival
Junio_Pre_survival <- Junio %>% 
  select(ID_REGISTRO, FECHA_SINTOMAS, FECHA_DEF, Fecha_inicio, Fecha_final, Contribucion_dias,
         sex, Edad_agru, Evento, Embarazo,Lengua_Ind, ENTIDAD_RES, MUNICIPIO_RES,
         Neumonia, Epoc,Asma, Respiratorias, Prob_respiratorios,
         Diabetes, Hipertension, Cardiovascular, Renal_cro, Sum_enf, Enf_cronica,
         Inmunosuprecion, USMER, Institucion,
         Obesidad,Fumar) %>% 
  filter(Contribucion_dias>=1 & Contribucion_dias<=30)


########################################################
# ----- Julio
########################################################

Julio <- data %>% 
  filter(FECHA_INGRESO>="2020-07-01" & FECHA_INGRESO<="2020-07-31") %>% # n = 147,367
  select(ID_REGISTRO, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, ENTIDAD_RES, MUNICIPIO_RES,
         SEXO, EDAD, mortalidad, EMBARAZO, HABLA_LENGUA_INDIG,
         NEUMONIA, DIABETES, EPOC, ASMA, INMUSUPR, HIPERTENSION,
         CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, OBESIDAD, TABAQUISMO, ORIGEN, SECTOR)


# Fechas con datos recientes
Julio$Fecha_inicio <- as.Date("2020-07-01")
Julio$Fecha_final <- as.Date("2020-07-31")

Julio$No_Lifespan <- Julio$Fecha_final - Julio$FECHA_SINTOMAS
Julio$Lifespan <- Julio$FECHA_DEF - Julio$FECHA_SINTOMAS

# Ahora necesito saber la contribution de tiempo de cada uno
Julio <- as.data.table(Julio)

Julio$Contribucion_dias[Julio$mortalidad==0] <- Julio$Fecha_final[Julio$mortalidad==0] - Julio$FECHA_SINTOMAS[Julio$mortalidad==0]
Julio$Contribucion_dias[Julio$mortalidad==1] <- Julio$FECHA_DEF[Julio$mortalidad==1] - Julio$FECHA_SINTOMAS[Julio$mortalidad==1]

Julio$Contribucion_dias <- as.numeric(Julio$Contribucion_dias)

# --- Creamos variable Evento
Julio$Evento[Julio$mortalidad==0] <- 0
Julio$Evento[Julio$mortalidad==1] <- 1

table(Julio$Evento,Julio$mortalidad)

table(Julio$Evento)

#Julio$Evento <- factor(Julio$Evento, levels = c(0,1),
#                               labels = c("Sobreviviente","Falleció"))


# --- Sex
Julio$sex[Julio$SEXO==1] <-0
Julio$sex[Julio$SEXO==2] <-1

Julio$sex <- factor(Julio$sex, levels = c(0,1),
                    labels = c("Mujeres","Hombres"))


# --- Edad
Julio$Edad_agru[Julio$EDAD>=0 & Julio$EDAD<=39] <- 39
Julio$Edad_agru[Julio$EDAD>=40 & Julio$EDAD<=59] <- 59
Julio$Edad_agru[Julio$EDAD>=60 & Julio$EDAD<=69] <- 69
Julio$Edad_agru[Julio$EDAD>=70 & Julio$EDAD<=79] <- 79
Julio$Edad_agru[Julio$EDAD>=80 & Julio$EDAD<=120] <- 80

Julio$Edad_agru <- as.factor(Julio$Edad_agru)

# ----- EMBARAZO 
Julio$Embarazo[Julio$EMBARAZO==2] <- 0
Julio$Embarazo[Julio$EMBARAZO==1] <- 1

# ---- Lengua Indigena
Julio$Lengua_Ind[Julio$HABLA_LENGUA_INDIG==2] <- 0
Julio$Lengua_Ind[Julio$HABLA_LENGUA_INDIG==1] <- 1

# -- Neumonia
Julio$Neumonia[Julio$NEUMONIA==2] <- 0
Julio$Neumonia[Julio$NEUMONIA==1] <- 1

# -- EPOC
Julio$Epoc[Julio$EPOC==2] <- 0
Julio$Epoc[Julio$EPOC==1] <- 1

# -- ASMA
Julio$Asma[Julio$ASMA==2] <- 0
Julio$Asma[Julio$ASMA==1] <- 1

# -- Respiratorias
# Numero de complicaciones respirtaorias
Julio$Respiratorias <- Julio$Neumonia + Julio$Epoc + Julio$Asma
table(Julio$Respiratorias)

# Problemas respiratorios
Julio$Prob_respiratorios[Julio$Respiratorias==0] <-0
Julio$Prob_respiratorios[Julio$Respiratorias>=1 & Julio$Respiratorias<=3] <-1

table(Julio$Prob_respiratorios)

####
# --- Enfermedades crónicas
####

# Diabetes
Julio$Diabetes[Julio$DIABETES==2] <-0 
Julio$Diabetes[Julio$DIABETES==1] <-1

# Hipertension
Julio$Hipertension[Julio$HIPERTENSION==2] <-0 
Julio$Hipertension[Julio$HIPERTENSION==1] <-1 

# Cardiovascular
Julio$Cardiovascular[Julio$CARDIOVASCULAR==2] <-0 
Julio$Cardiovascular[Julio$CARDIOVASCULAR==1] <-1 

# Problemas renal cronicos
Julio$Renal_cro[Julio$RENAL_CRONICA==2] <-0 
Julio$Renal_cro[Julio$RENAL_CRONICA==1] <-1 


# Comorbilidades cronicas
Julio$Sum_enf <- Julio$Diabetes + Julio$Hipertension + Julio$Cardiovascular + Julio$Renal_cro
table(Julio$Sum_enf)

Julio$Enf_cronica[Julio$Sum_enf==0] <- 0
Julio$Enf_cronica[Julio$Sum_enf>=1 & Julio$Sum_enf<=4] <- 1

# Inmunosuprecion
Julio$Inmunosuprecion[Julio$INMUSUPR==2] <-0 
Julio$Inmunosuprecion[Julio$INMUSUPR==1] <-1 

####
# Conducta salud
####

# - Obesidad
Julio$Obesidad[Julio$OBESIDAD==2] <-0 
Julio$Obesidad[Julio$OBESIDAD==1] <-1

# - Tabaquismo
Julio$Fumar[Julio$TABAQUISMO==2] <-0 
Julio$Fumar[Julio$TABAQUISMO==1] <-1

#####
# Variables de servicios de salud
#####

# USMER
Julio$USMER[Julio$ORIGEN==2] <-0 
Julio$USMER[Julio$ORIGEN==1] <-1 

# Institución que brindo atención
# 0	IMSS 	IMSS-BIENESTAR	ISSSTE
# 1	Cruz roja	DIF	ESTATAL	MUNICIPA	UNIVERSITAORIO
# 2	SSA				
# 3	PEMEX	SEDENA	SEMAR		
# 4	Privado				
#
Julio$Institucion[Julio$SECTOR>=4 & Julio$SECTOR<=6] <- 0
Julio$Institucion[Julio$SECTOR==1 | Julio$SECTOR==2 | Julio$SECTOR==3 |
                    Julio$SECTOR==7 | Julio$SECTOR==13] <- 1
Julio$Institucion[Julio$SECTOR==12] <- 2
Julio$Institucion[Julio$SECTOR==8 | Julio$SECTOR==10 | Julio$SECTOR==11] <- 3
Julio$Institucion[Julio$SECTOR==9] <- 4

# Checamos 
table(Julio$SECTOR, Julio$Institucion)

# -- Data pre survival
Julio_Pre_survival <- Julio %>% 
  select(ID_REGISTRO, FECHA_SINTOMAS, FECHA_DEF, Fecha_inicio, Fecha_final, Contribucion_dias,
         sex, Edad_agru, Evento, Embarazo,Lengua_Ind, ENTIDAD_RES, MUNICIPIO_RES,
         Neumonia, Epoc,Asma, Respiratorias, Prob_respiratorios,
         Diabetes, Hipertension, Cardiovascular, Renal_cro, Sum_enf, Enf_cronica,
         Inmunosuprecion, USMER, Institucion,
         Obesidad,Fumar) %>% 
  filter(Contribucion_dias>=1 & Contribucion_dias<=31)


# -------------------------------------------------------------------------------------- #
#       ----        Analisis descriptivo
# -------------------------------------------------------------------------------------- #

# Pegamos las bases
Data_final <- rbind(Marzo_Pre_survival, Abril_Pre_survival,
                    Mayo_Pre_survival, Junio_Pre_survival, Julio_Pre_survival)


# Estadísticas descriptivas Sociodemograficas
table(Data_final$sex, Data_final$Evento)
table(Data_final$Edad_agru, Data_final$Evento)
table(Data_final$Lengua_Ind, Data_final$Evento)

# Servicios de salud
table(Data_final$USMER, Data_final$Evento)
table(Data_final$Institucion, Data_final$Evento)

# Estado de salud (no respiratorias)
table(Data_final$Diabetes, Data_final$Evento)
table(Data_final$Hipertension, Data_final$Evento)
table(Data_final$Cardiovascular, Data_final$Evento)
table(Data_final$Renal_cro, Data_final$Evento)
table(Data_final$Sum_enf, Data_final$Evento)
table(Data_final$Enf_cronica, Data_final$Evento)

# Estado de salud Respitaroarias
table(Data_final$Neumonia, Data_final$Evento)
table(Data_final$Epoc, Data_final$Evento)
table(Data_final$Asma, Data_final$Evento)
table(Data_final$Respiratorias, Data_final$Evento)
table(Data_final$Prob_respiratorios, Data_final$Evento)


# Conductas de salud
table(Data_final$Fumar, Data_final$Evento)
table(Data_final$Obesidad, Data_final$Evento)


# Tasa de letalidad
table(Data_final$ENTIDAD_RES,Data_final$Evento)

TFR <- data.frame(table(Data_final$ENTIDAD_RES,Data_final$Evento))
write.csv(TFR, file = "R Codes/Publicacion Mex/TFR.csv")


# -------------------------------------------------------------------------------------- #
#       ----        3. Kaplan Meier
# -------------------------------------------------------------------------------------- #



Model_sex <- survfit(Surv(Contribucion_dias, Evento) ~ sex, data = Data_final)
print(Model_sex, print.rmean=TRUE)
Survival_Sexo <- ggsurvplot(Model_sex, 
                            #risk.table = TRUE,
                            pval = TRUE,
                            conf.int = TRUE,
                            ggtheme = theme_bw(), 
                            palette = c("darkred", "darkblue"),
                            title = "Todos los casos",
                            xlab = "Dias", 
                            ylab = "Funcion de sobrevivencia")
Survival_Sexo

Model_edad <- survfit(Surv(Contribucion_dias, Evento) ~ Edad_agru, data = Data_final)
print(Model_edad, print.rmean=TRUE)
Survival_EDAD <- ggsurvplot(Model_edad, 
                            #risk.table = TRUE,
                            pval = TRUE,
                            conf.int = TRUE,
                            ggtheme = theme_bw(), 
                            #palette = c("darkred", "darkblue"),
                            title = "Todos los casos",
                            xlab = "Dias", 
                            ylab = "Funcion de sobrevivencia")
Survival_EDAD


Model_inst <- survfit(Surv(Contribucion_dias, Evento) ~ Institucion, data = Data_final)
print(Model_inst, print.rmean=TRUE)
Survival_Institucion <- ggsurvplot(Model_inst, 
                            #risk.table = TRUE,
                            pval = TRUE,
                            size = 1.5,
                            conf.int = TRUE,
                            conf.int.style = "step",
                            ggtheme = theme_classic(), 
                            break.time.by = 5,
                            legend.labs =
                              c("IMSS-ISSSTE", "Otras Estado",
                                "SSA","PEMEX, SEDENA SEMAR", "Privado"), 
                            palette = c("#B2182B", "#F4A582",  "#1B9E77","#D1E5F0" , "#2166AC"),
                            #title = "Función de sobrevivencia Kaplan-Meier por Insitución de atención medica",
                            xlab = "Dias desde ingreso al hospital", 
                            ylab = "Porcentaje de sobreiviventes",
                            font.legend = 15,
                            font.labs = 15,
                            font.title=15,
                            font.caption =25,
                            font.x = 15,
                            font.y=15,
                            legend = "bottom"
                            )
Survival_Institucion
ggsave(filename = "Kaplan Meier Institucion.pdf", path= "R Codes/Publicacion Mex/Graficas publicacion/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")


# -------------------------------------------------------------------------------------- #
#       ----        4. Cox model analisis todos los casos
# -------------------------------------------------------------------------------------- #

# --------
# Sociodemografico
# --------

# -- Sexo
Modelo_sexo <- coxph(Surv(Contribucion_dias, Evento) ~ factor(sex),
                                 data = Data_final)
summary(Modelo_sexo)
summary(Modelo_sexo)$coeff[,c(2:5)]

# -- Sexo y Edad
Modelo_Sexo_Edad <- coxph(Surv(Contribucion_dias, Evento) ~ factor(sex) + factor(Edad_agru),
                     data = Data_final)
summary(Modelo_Sexo_Edad)
summary(Modelo_Sexo_Edad)$coeff[,c(2:5)]

# -- Sexo Edad y Habla indigena
Modelo_Sociodemografico<- coxph(Surv(Contribucion_dias, Evento) ~ factor(sex) + factor(Edad_agru) + factor(Lengua_Ind) ,
                          data = Data_final)
summary(Modelo_Sociodemografico)
summary(Modelo_Sociodemografico)$coeff[,c(2:5)]


# -- Interaccion Sexo y Edad
Modelo_Sexo_Edad_Int <- coxph(Surv(Contribucion_dias, Evento) ~ factor(sex) * factor(Edad_agru),
                          data = Data_final)
summary(Modelo_Sexo_Edad_Int)
summary(Modelo_Sexo_Edad_Int)$coeff[,c(2:5)]



# --------
# Modelo de servicios de salud
# --------

Modelo_USMER <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + USMER,
                         data = Data_final)
summary(Modelo_USMER)$coeff[,c(2:5)]

Modelo_Institucion <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + factor(Institucion),
                      data = Data_final)
summary(Modelo_Institucion)$coeff[,c(2:5)]

Modelo_Servicios_Salud <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + USMER + factor(Institucion),
                            data = Data_final)
summary(Modelo_Servicios_Salud)
summary(Modelo_Servicios_Salud)$coeff[,c(2:5)]

# --------
# Enfermedades cronicas
# --------


# Diabetes
Modelo_diabetes <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + Diabetes,
                          data = Data_final)
summary(Modelo_diabetes)$coeff[,c(2:5)]

# Hipertension
Modelo_hiperstion <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + Hipertension,
                         data = Data_final)
summary(Modelo_hiperstion)$coeff[,c(2:5)]

# Cardiovascular
Modelo_cardiovascular <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + Cardiovascular,
                           data = Data_final)
summary(Modelo_cardiovascular)$coeff[,c(2:5)]

# Renal
Modelo_renal <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + Renal_cro,
                               data = Data_final)
summary(Modelo_renal)$coeff[,c(2:5)]

# Enfermedades por separado
Modelo_cronicas1 <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + Diabetes + Hipertension + Cardiovascular + Renal_cro,
                          data = Data_final)
summary(Modelo_cronicas1)
summary(Modelo_cronicas1)$coeff[,c(2:5)]

# Numero de enfermedades cronicas
Modelo_cronicas2 <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + factor(Sum_enf),
                          data = Data_final)
summary(Modelo_cronicas2)
summary(Modelo_cronicas2)$coeff[,c(2:5)]

# Prevalencia de enfermedades cronicas
Modelo_cronicas3 <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru +  Enf_cronica,
                          data = Data_final)
summary(Modelo_cronicas3)$coeff[,c(2:5)]


# --------
# Enfermedades respirtaorias
# --------

# Neumonia
Modelo_neumonia <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + Neumonia,
                         data = Data_final)
summary(Modelo_neumonia)$coeff[,c(2:5)]

# Epoc
Modelo_EPOC <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + Epoc,
                           data = Data_final)
summary(Modelo_EPOC)$coeff[,c(2:5)]

# Asma
Modelo_asma <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + Asma,
                               data = Data_final)
summary(Modelo_asma)$coeff[,c(2:5)]

# Enfermedades respiratorias
Modelo_enf_respiratorias <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + Neumonia + Epoc + Asma,
                      data = Data_final)
summary(Modelo_enf_respiratorias)
summary(Modelo_enf_respiratorias)$coeff[,c(2:5)]

# Numero de Respiratorias
Modelo_num_respiratoriasl <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + factor(Respiratorias),
                      data = Data_final)
summary(Modelo_num_respiratoriasl)$coeff[,c(2:5)]

# Algun problema respiratorio
Modelo_prob_respiratorio <- coxph(Surv(Contribucion_dias, Evento) ~ sex + Edad_agru + Prob_respiratorios,
                          data = Data_final)
summary(Modelo_prob_respiratorio)$coeff[,c(2:5)]

# --------
# Conductas en Salud
# --------

# -- Obesidad
Modelo_Obesidad <- coxph(Surv(Contribucion_dias, Evento) ~ factor(sex) + factor(Edad_agru) + factor(Obesidad),
                          data = Data_final)
summary(Modelo_Obesidad)
summary(Modelo_Obesidad)$coeff[,c(2:5)]


# -- Fumar
Modelo_Fumar <- coxph(Surv(Contribucion_dias, Evento) ~ factor(sex) + factor(Edad_agru) + factor(Fumar),
                         data = Data_final)
summary(Modelo_Fumar)
summary(Modelo_Fumar)$coeff[,c(2:5)]

# -- Conductas
Modelo_Conductas <- coxph(Surv(Contribucion_dias, Evento) ~ factor(sex) + factor(Edad_agru) + factor(Obesidad) + factor(Fumar),
                      data = Data_final)
summary(Modelo_Conductas)
summary(Modelo_Conductas)$coeff[,c(2:5)]


# --------
# Modelo total
# --------

# -- Total
Modelo_total <- coxph(Surv(Contribucion_dias, Evento) ~ factor(sex) + factor(Edad_agru) + USMER + factor(Institucion)
                      + factor(Sum_enf) + Neumonia + Epoc + Asma + factor(Obesidad) + factor(Fumar),
                          data = Data_final)
summary(Modelo_total)
summary(Modelo_total)$coeff[,c(2:5)]

Modelo_total_neumonia <- coxph(Surv(Contribucion_dias, Evento) ~ factor(sex) + factor(Edad_agru) + USMER + factor(Institucion)
                      + factor(Sum_enf) +  Epoc + Asma + factor(Obesidad) + factor(Fumar),
                      data = Data_final)
summary(Modelo_total_neumonia)
summary(Modelo_total_neumonia)$coeff[,c(2:5)]










