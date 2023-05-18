###################### LEER LAS BASES DE DATOS #################################

encuesta<- "C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Encuesta Multiproposito\\Microdatos\\Multiproposito.rds"
EM21 <- readRDS(encuesta)

adicionales <- "C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Encuesta Multiproposito\\Microdatos\\Adicionales.rds"
EM21_plus <- readRDS(adicionales)

base<- "C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Encuesta Multiproposito\\Microdatos\\Multiproposito_completa.rds"
EM21F <- readRDS(base)

library(dplyr)

######################### NOMBRES ##############################################

EM21F$MPIO_NAME <- vector(mode='character',length=dim(EM21F)[1])
EM21F$MPIO_NAME[EM21F$MPIO == 11001] <- "Bogotá"
EM21F$MPIO_NAME[EM21F$MPIO == 25740] <- "Sibaté"
EM21F$MPIO_NAME[EM21F$MPIO == 25473] <- "Mosquera"
EM21F$MPIO_NAME[EM21F$MPIO == 25290] <- "Fusagasugá"
EM21F$MPIO_NAME[EM21F$MPIO == 25214] <- "Cota"
EM21F$MPIO_NAME[EM21F$MPIO == 25175] <- "Chía"
EM21F$MPIO_NAME[EM21F$MPIO == 25758] <- "Sopó"
EM21F$MPIO_NAME[EM21F$MPIO == 25785] <- "Tabio"
EM21F$MPIO_NAME[EM21F$MPIO == 25898] <- "Zipacón"
EM21F$MPIO_NAME[EM21F$MPIO == 25754] <- "Soacha"
EM21F$MPIO_NAME[EM21F$MPIO == 25126] <- "Cajicá"
EM21F$MPIO_NAME[EM21F$MPIO == 25817] <- "Tocancipá"
EM21F$MPIO_NAME[EM21F$MPIO == 25430] <- "Madrid"
EM21F$MPIO_NAME[EM21F$MPIO == 25286] <- "Funza"
EM21F$MPIO_NAME[EM21F$MPIO == 25260] <- "El Rosal"
EM21F$MPIO_NAME[EM21F$MPIO == 25099] <- "Bojacá"
EM21F$MPIO_NAME[EM21F$MPIO == 25799] <- "Tenjo"
EM21F$MPIO_NAME[EM21F$MPIO == 25899] <- "Zipaquirá"
EM21F$MPIO_NAME[EM21F$MPIO == 25269] <- "Facatativá"
EM21F$MPIO_NAME[EM21F$MPIO == 25769] <- "Subachoque"
EM21F$MPIO_NAME[EM21F$MPIO == 25377] <- "La Calera"
EM21F$MPIO_NAME[EM21F$MPIO == 25295] <- "Gachancipá"

EM21F <- rename(EM21F, Estrato = NVCBP11AA)
EM21F <- rename(EM21F, Sexo = NPCEP5)
EM21F <- rename(EM21F, Edad = NPCEP4)

########################### FILTROS ############################################

Poblacion venezolana <- filter(NPCEP13D==1 | NPCEP16D_1==1)
Municipio <- group_by(MPIO_NAME)
Localidad <- group_by(NOMBRE_LOCALIDAD)
UPZ <- group_by(NOMBRE_UPZ_GRUPO)
Estrato <- group_by(Estrato)
Sexo <- group_by(Sexo)
Edad <- group_by(Edad)
Retornados <- filter(NPCEP_5==1 | NPCEP_5==2 | NPCEP_5==3)  *Poseen documentos colombianos

library(tidyr)

######################## VIVIENDA ##############################################
Viv <- EM21F %>% distinct(DIRECTORIO, .keep_all = TRUE)


Tipo <- Viv %>% group_by(NOMBRE_LOCALIDAD, NVCBP10) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) %>% arrange(no)

a <- Viv$NVCBP10 %>% pivot_wider(names_from = MPIO_NAME, values_from = NVCBP10)
tabla <- d %>%
  pivot_wider(names_from = Respuesta, values_from = Respuesta, values_fn = length)









########################### HOGARES ############################################
Hog <- EM21F %>% distinct(DIRECTORIO_HOG, .keep_all = TRUE)

Tenencia <- Hog %>% group_by(MPIO_NAME, NHCCP1) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) %>% arrange(no)








########################### PERSONAS ###########################################










############### EDUCACIÓN POR FUERA DEL MUNICIPIO ##############################
EST <- EM21F[EM21F$NPCHP2==1,]

Bog <- EST %>% filter(MPIO == 11001) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Mos <- EST %>% filter(MPIO == 25473) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
Chi <- EST %>% filter(MPIO == 25175) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
Cot <- EST %>% filter(MPIO == 25214) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
ERS <- EST %>% filter(MPIO == 25260) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
FUN <- EST %>% filter(MPIO == 25286) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
Fus <- EST %>% filter(MPIO == 25290) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
LCL <- EST %>% filter(MPIO == 25377) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
MAD <- EST %>% filter(MPIO == 25430) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
SOA <- EST %>% filter(MPIO == 25754) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
SOP <- EST %>% filter(MPIO == 25758) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
TAB <- EST %>% filter(MPIO == 25785) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))
TOC <- EST %>% filter(MPIO == 25817) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C)) 
ZIP <- EST %>% filter(MPIO == 25899) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCHP13==2) %>% group_by(NPCHP13B) %>% summarise(no = sum(FEX_C))


############ TRABAJO POR FUERA DEL MUNICIPIO
OCUP <- EM21F[EM21F$N_ocupados==1,]

Bog <- OCUP %>% filter(MPIO == 11001) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Chia <- OCUP %>% filter(MPIO == 25175) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Boj <- OCUP %>% filter(MPIO == 25099) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Caj <- OCUP %>% filter(MPIO == 25126) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Cot <- OCUP %>% filter(MPIO == 25214) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
ElR <- OCUP %>% filter(MPIO == 25260) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Fac <- OCUP %>% filter(MPIO == 25269) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Fun <- OCUP %>% filter(MPIO == 25286) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Fus <- OCUP %>% filter(MPIO == 25290) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Gac <- OCUP %>% filter(MPIO == 25295) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Lac <- OCUP %>% filter(MPIO == 25377) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Mad <- OCUP %>% filter(MPIO == 25430) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Mos <- OCUP %>% filter(MPIO == 25473) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Sib <- OCUP %>% filter(MPIO == 25740) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Soa <- OCUP %>% filter(MPIO == 25754) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Sop <- OCUP %>% filter(MPIO == 25758) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Sub <- OCUP %>% filter(MPIO == 25769) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Tab <- OCUP %>% filter(MPIO == 25785) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Ten <- OCUP %>% filter(MPIO == 25799) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Toc <- OCUP %>% filter(MPIO == 25817) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Zip <- OCUP %>% filter(MPIO == 25898) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
Zipq <- OCUP %>% filter(MPIO == 25899) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NPCKPA46==2) %>% group_by(NPCKP46AC) %>% summarise(no = sum(FEX_C)) %>% arrange(no)
