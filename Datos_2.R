###################### LEER LAS BASES DE DATOS #################################
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


########################### HOGARES ############################################

Hog <- EM21F %>% distinct(DIRECTORIO_HOG, .keep_all = TRUE)


Hac_mit_urb <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(N_hacinamiento_mitigable==1) %>% filter(CLASE==1) %>% group_by(MPIO_NAME)  %>% summarise(no = sum(FEX_C)) 
Hac_mit_rur <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(N_hacinamiento_mitigable==1) %>% filter(CLASE==2 | CLASE==3) %>% group_by(MPIO_NAME)  %>% summarise(no = sum(FEX_C)) 

Hac_nomit_urb <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(N_hacinamiento_critico==1) %>% filter(CLASE==1) %>% group_by(MPIO_NAME)  %>% summarise(no = sum(FEX_C)) 
Hac_nomit_rur <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(N_hacinamiento_critico==1) %>% filter(CLASE==2 | CLASE==3) %>% group_by(MPIO_NAME)  %>% summarise(no = sum(FEX_C)) 

Def_cuali <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(N_deficit_cualitativo==1) %>% group_by(Estrato, MPIO_NAME) %>% summarise(no = sum(FEX_C)) 
Def_cuanti <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% filter(N_deficit_cuantitativo==1) %>% group_by(Estrato, MPIO_NAME) %>% summarise(no = sum(FEX_C)) 


Hog$Ingresopc <- cut(Hog$N_ingpcugarr,
                           breaks = c(-1 , 250000, 500000, 750000, 1000000, 1500000, 2000000,
                                      2500000, 3000000, 3500000, 4000000, 4500000,
                                      5000000, 33000000))

Ingresopc <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% group_by(Ingresopc, MPIO_NAME) %>% 
  summarise(no = sum(FEX_C)) %>%
  mutate(freq = round(no/sum(no),3))

write_xlsx(Ingresopc,paste0("ing.xlsx"))


