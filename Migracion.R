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

Municipio <- Viv %>% group_by(MPIO_NAME, NVCBP16) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) %>% arrange(no)

Localidad <- Viv %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_LOCALIDAD, NVCBP16) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C))

UPZ <- Viv %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_UPZ_GRUPO, NVCBP16) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C))

Estrato <- Viv %>% group_by(Estrato, NVCBP13) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) %>% arrange(no)


clase <- CLASE
Tipo de viv <- NVCBP10
material pared <- NVCBP12
Material piso <- NVCBP13
Negocio en la vivienda <- NVCBP9
Hogares por vivienda <- NVCBP16



Tipo <- Viv %>% group_by(MPIO_NAME, NVCBP10) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) %>% arrange(no)

Tipo <- Viv %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_UPZ_GRUPO) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(NVCBP10==4) %>% summarise(no = sum(FEX_C)) %>% arrange(no)



########################### HOGARES ############################################
Hog <- EM21F %>% distinct(DIRECTORIO_HOG, .keep_all = TRUE)


Municipio <- Hog %>% group_by(MPIO_NAME) %>% filter(NHCCP44==1) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) %>% arrange(no)

Localidad <- Hog %>% filter(MPIO_NAME=="Bogotá")  %>% filter(NHCCP44==1) %>% group_by(NOMBRE_LOCALIDAD) %>% filter(NPCEP13D==1 | NPCEP16D_1==1)  %>% summarise(no = sum(FEX_C))

UPZ <- Hog %>% filter(MPIO_NAME=="Bogotá") %>% filter(NHCCP44==1) %>% group_by(NOMBRE_UPZ_GRUPO) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) 

Estrato <- Hog %>% group_by(Estrato, NHCCPCTRL2) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) 



Municipio <- Hog %>% filter(NHCCP44==1) %>% group_by(MPIO_NAME, NHCCP44A) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) %>% arrange(no)

Localidad <- Hog %>% filter(MPIO_NAME=="Bogotá") %>% filter(NHCCP44==1) %>% group_by(NOMBRE_LOCALIDAD, NHCCP44A) %>% filter(NPCEP13D==1 | NPCEP16D_1==1)  %>% summarise(no = sum(FEX_C))

UPZ <- Hog %>% filter(MPIO_NAME=="Bogotá") %>% filter(NHCCP44==1) %>% group_by(NOMBRE_UPZ_GRUPO, NHCCP44A) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) 






Estrato <- Hog %>% group_by(NOMBRE_UPZ_GRUPO, NPCEP6) %>% summarise(no = sum(FEX_C)) %>% arrange(no)


Tenencia <- NHCCP1
Acueducto <- NHCDP1
Alcantarillado <- NHCDP3
Basura <- NHCDP5
Energia <- NHCDP9
Gas Natural <- NHCDP15 
Internet <- NHCDP27
Nacionalidad <- NPCEP11A
Personas por hogar <- NHCCPCTRL2
Deficit habita <- N_deficit_habitacional
Deficit cuali <- N_deficit_cualitativo
Deficit cuanti <- N_deficit_cuantitativo
Componentes deficit cuanti <- N_tipo_vivienda, N_deficit_paredes, N_cohabitacion, N_hacinamiento_critico
Componentes deficit cuali <- N_hacinamientomit_jer, N_pisos_jer, N_cocina_jer, N_agua_jer, N_alcantarillado_jer,
                             N_energia_jer, N_recoleccion_jer
Carro <- NHCCP41
Moto <- NHCCP44




########################### PERSONAS ###########################################

Municipio <- EM21F %>% group_by(MPIO_NAME) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) %>% arrange(no)

Localidad <- EM21F %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_LOCALIDAD) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C))

UPZ <- EM21F %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_UPZ_GRUPO) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C))

Estrato <- EM21F %>% group_by(Estrato) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C)) %>% arrange(no)


Hijos <- EM21F %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_LOCALIDAD, DIRECTORIO_HOG) %>% filter(NPCEP6==3) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = n())
Hijoss <- EM21F %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_LOCALIDAD, DIRECTORIO_HOG) %>% filter(NPCEP6==3) %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% summarise(no = sum(FEX_C))


install.packages("writexl")
library(writexl)

write_xlsx(Hijos, "C:/Users/karme/Desktop/Prácticas/Datos/Encuesta Multiproposito/Microdatos/hijos.xlsx")
write_xlsx(Hijoss, "C:/Users/karme/Desktop/Prácticas/Datos/Encuesta Multiproposito/Microdatos/hijoss.xlsx")

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
