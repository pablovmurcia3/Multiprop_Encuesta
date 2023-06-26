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


######################## VIVIENDA ##############################################

Viv <- EM21F %>% distinct(DIRECTORIO, .keep_all = TRUE)

a
clase <- CLASE
Tipo de viv <- NVCBP10
material pared <- NVCBP12
Material piso <- NVCBP13
Negocio en la vivienda <- NVCBP9
Hogares por vivienda <- NVCBP16

Municipio <- Viv  %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% group_by(MPIO_NAME, NVCBP16) %>% summarise(no = sum(FEX_C)) %>% arrange(no)

Localidad <- Viv %>% filter(MPIO_NAME=="Bogotá") %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% group_by(NOMBRE_LOCALIDAD, NVCBP16) %>% summarise(no = sum(FEX_C))

UPZ <- Viv %>% filter(MPIO_NAME=="Bogotá") %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% group_by(NOMBRE_UPZ_GRUPO, NVCBP16)  %>% summarise(no = sum(FEX_C))

Estrato <- Viv %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% group_by(Estrato, NVCBP13) %>% summarise(no = sum(FEX_C)) %>% arrange(no)




########################### HOGARES ############################################

Hog_BS <- Hog %>% filter(MPIO==25754 | MPIO==11001)
Hog <- EM21F %>% distinct(DIRECTORIO_HOG, .keep_all = TRUE)
Hog_ven <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1)
Venez <- EM21F %>% filter(NPCEP13D==1 | NPCEP16D_1==1)


# Tenencia <- NHCCP1
# Acueducto <- NHCDP1
# Alcantarillado <- NHCDP3
# Basura <- NHCDP5
# Energia <- NHCDP9
# Gas Natural <- NHCDP15 
# Internet <- NHCDP27
# Nacionalidad <- NPCEP11A
# Personas por hogar <- NHCCPCTRL2
# Deficit habita <- N_deficit_habitacional
# Deficit cuali <- N_deficit_cualitativo
# Deficit cuanti <- N_deficit_cuantitativo
# Componentes deficit cuanti <- N_tipo_vivienda, N_deficit_paredes, N_cohabitacion, N_hacinamiento_critico
# Componentes deficit cuali <- N_hacinamientomit_jer, N_pisos_jer, N_cocina_jer, N_agua_jer, N_alcantarillado_jer,
# N_energia_jer, N_recoleccion_jer
# Carro <- NHCCP41
# Moto <- NHCCP44
# Condiciones de vida <- NHCLP3
# Condiciones de vida hace 5 <- NHCLP5
# Hacinamiento mit <- N_hacinamiento_mitigable



Municipio <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(N_hacinamiento_mitigable==1) %>% filter(CLASE==2 | CLASE==3) %>% group_by(MPIO_NAME)  %>% summarise(no = sum(FEX_C)) 

Localidad <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_LOCALIDAD, NHCLP5)  %>% summarise(no = sum(FEX_C))

UPZ <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%  filter(MPIO_NAME=="Bogotá") %>% filter(N_deficit_cualitativo==1) %>% group_by(NOMBRE_UPZ_GRUPO)  %>% summarise(no = sum(FEX_C)) 


Estrato <- Hog_BS %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(N_deficit_cualitativo==1) %>% group_by(Estrato, MPIO) %>% summarise(no = sum(FEX_C)) 


########################### PERSONAS ###########################################

# retornado (nacionalidad) <- NPCEP11D
# afiliacion salud <- NPCFP1
# regimen salud <- NPCFP2
# Discapacidad motora <- NPCFP21A1==1 | NPCFP21A1==2 | NPCFP21A2==1 | NPCFP21A2==2
# Discapacidad visual <-  NPCFP21A3==1 | NPCFP21A3==2
# Discapacidad auditiva <- NPCFP21A4==1 | NPCFP21A4==2 
# Discapacidad al hablar <-NPCFP21A5==1 | NPCFP21A5==2 
# Discapacidad mental <- NPCFP21A6==1 | NPCFP21A6==2 | NPCFP21A7==1 | NPCFP21A7==2
# Nivel educativo <- NPCHP4
# Principal actividad <- NPCKP1
# Lugar de trabajo <- NPCKP44a
# Tiempo de viaje al trabajo <- NPCKP46B
# Tiempo de viaje al estudio <- NPCHP19  



Municipio <- EM21F %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% group_by(MPIO_NAME, NPCKP44a) %>% summarise(no = sum(FEX_C)) 

Localidad <- EM21F %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_LOCALIDAD) %>% summarise(no = sum(FEX_C))

UPZ <- EM21F %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(MPIO_NAME=="Bogotá") %>% filter(NPCEP5==1) %>% group_by(NOMBRE_UPZ_GRUPO) %>% summarise(no = sum(FEX_C))

Estrato <- EM21F %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% group_by(Estrato) %>% summarise(no = sum(FEX_C)) %>% arrange(no)






Municipio <- EM21F %>% filter(N_ocupados==1) %>% group_by(MPIO_NAME) %>% summarise(no = weighted.mean(NPCKP46B, FEX_C, na.rm = TRUE)) 

Localidad <- EM21F %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_LOCALIDAD) %>% summarise(no = weighted.mean(NPCHP19, FEX_C, na.rm = TRUE))

UPZ <- EM21F %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_UPZ_GRUPO) %>% summarise(no = weighted.mean(NPCHP19, FEX_C, na.rm = TRUE))




install.packages("writexl")
library(writexl)

write_xlsx(Municipio, "C:/Users/karme/Desktop/Prácticas/Datos/Encuesta Multiproposito/Microdatos/municipio.xlsx")
write_xlsx(Localidad, "C:/Users/karme/Desktop/Prácticas/Datos/Encuesta Multiproposito/Microdatos/localidad.xlsx")
write_xlsx(UPZ, "C:/Users/karme/Desktop/Prácticas/Datos/Encuesta Multiproposito/Microdatos/upz.xlsx")


############### EDUCACIÓN POR FUERA DEL MUNICIPIO ##############################
