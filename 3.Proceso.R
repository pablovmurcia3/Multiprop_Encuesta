
######################## VIVIENDA ##############################################

Viv <- EM21F %>% distinct(DIRECTORIO, .keep_all = TRUE)


Municipio <- Viv  %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(MPIO_NAME, NVCBP16) %>% summarise(no = sum(FEX_C)) %>% arrange(no)

Localidad <- Viv %>% filter(MPIO_NAME=="Bogotá") %>% 
  filter(NPCEP13D==1 | NPCEP16D_1==1) %>% group_by(NOMBRE_LOCALIDAD, NVCBP16) %>%
  summarise(no = sum(FEX_C))

UPZ <- Viv %>% filter(MPIO_NAME=="Bogotá") %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_UPZ_GRUPO, NVCBP16)  %>%
  summarise(no = sum(FEX_C))

Estrato <- Viv %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(Estrato, NVCBP13) %>% summarise(no = sum(FEX_C)) %>% arrange(no)

######################## HOGARES ##############################################

Hog <- EM21F %>% distinct(DIRECTORIO_HOG, .keep_all = TRUE)


Municipio <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(MPIO_NAME, N_pobre_monetario)  %>% summarise(no = sum(FEX_C)) 


# con porcentaje################
Municipio <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(MPIO_NAME, N_pobre_monetario)  %>% summarise(no = sum(FEX_C)) %>%
  mutate(freq = round(no/sum(no),3))
#################


Localidad <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_LOCALIDAD, N_pobre_monetario)  %>%
  summarise(no = sum(FEX_C))

# con porcentaje################
Localidad <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_LOCALIDAD, N_pobre_monetario)  %>% summarise(no = sum(FEX_C)) %>%
  mutate(freq = round(no/sum(no),3))
#################

UPZ <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% 
  filter(MPIO_NAME=="Bogotá") %>% group_by(NOMBRE_UPZ_GRUPO, NHCLP5)  %>%
  summarise(no = sum(FEX_C)) 

# con porcentaje################
UPZ <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_UPZ_GRUPO, N_pobre_monetario)  %>% summarise(no = sum(FEX_C)) %>%
  mutate(freq = round(no/sum(no),3))
#################

Estrato <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% 
  group_by(Estrato, NHCCPCTRL2) %>% summarise(no = sum(FEX_C)) 



# promedios

Hog <- EM21F %>% distinct(DIRECTORIO_HOG, .keep_all = TRUE)

Municipio <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(MPIO_NAME) %>% 
  summarise(no = weighted.mean(N_ingtotugarr, FEX_C, na.rm =TRUE))

Localidad <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_LOCALIDAD) %>% 
  summarise(no = weighted.mean(N_ingtotugarr, FEX_C, na.rm =TRUE))

UPZ <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_UPZ_GRUPO) %>% 
  summarise(no = weighted.mean(N_ingtotugarr, FEX_C, na.rm =TRUE))


# gasto 

Hog <- EM21F %>% distinct(DIRECTORIO_HOG, .keep_all = TRUE)


alimentos <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_UPZ_GRUPO, directorio_hog) %>% 
  summarise(no = weighted.mean(N_gm_alimentos, FEX_C, na.rm =TRUE)) %>% 
  group_by(NOMBRE_UPZ_GRUPO) %>% 
  summarise(alimentos = mean(no,  na.rm =TRUE))

bebidas <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_UPZ_GRUPO, directorio_hog) %>% 
  summarise(no = weighted.mean(N_gm_bebidas, FEX_C, na.rm =TRUE)) %>% 
  group_by(NOMBRE_UPZ_GRUPO) %>% 
  summarise(bebidas = mean(no,  na.rm =TRUE))

Vivienda <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_UPZ_GRUPO, directorio_hog) %>% 
  summarise(no = weighted.mean(N_gm_vivienda, FEX_C, na.rm =TRUE)) %>% 
  group_by(NOMBRE_UPZ_GRUPO) %>% 
  summarise(Vivienda = mean(no,  na.rm =TRUE))

recreación <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_UPZ_GRUPO, directorio_hog) %>% 
  summarise(no = weighted.mean(N_gm_recrea, FEX_C, na.rm =TRUE)) %>% 
  group_by(NOMBRE_UPZ_GRUPO) %>% 
  summarise(recreación = mean(no,  na.rm =TRUE))

educación <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_UPZ_GRUPO, directorio_hog) %>% 
  summarise(no = weighted.mean(N_gm_educ_hog, FEX_C, na.rm =TRUE)) %>% 
  group_by(NOMBRE_UPZ_GRUPO) %>% 
  summarise(educación = mean(no,  na.rm =TRUE))

durables <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(NOMBRE_UPZ_GRUPO, directorio_hog) %>% 
  summarise(no = weighted.mean(N_gm_durable, FEX_C, na.rm =TRUE)) %>% 
  group_by(NOMBRE_UPZ_GRUPO) %>% 
  summarise(durables = mean(no,  na.rm =TRUE))


df<- list(alimentos, bebidas, Vivienda, recreación, educación, durables)
upz <-Reduce(function(x,y) merge(x,y, by = c("NOMBRE_UPZ_GRUPO"),all =TRUE), df)





######################## PERSONAS ##############################################


Municipio <- EM21F  %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>%
  group_by(MPIO_NAME, Edad) %>% summarise(no = sum(FEX_C)) %>% arrange(no)

Localidad <- EM21F %>% filter(MPIO_NAME=="Bogotá") %>% 
  filter(NPCEP13D==1 | NPCEP16D_1==1) %>% group_by(NOMBRE_LOCALIDAD, Edad) %>%
  summarise(no = sum(FEX_C))

UPZ <- EM21F %>% filter(MPIO_NAME=="Bogotá") %>%
  filter(NPCEP13D==1 | NPCEP16D_1==1) %>% group_by(NOMBRE_UPZ_GRUPO, SEC)  %>%
  summarise(no = sum(FEX_C))

Estrato <- EM21F %>% filter(NPCEP13D==1 | NPCEP16D_1==1) %>% 
  group_by(Estrato) %>% summarise(no = sum(FEX_C)) %>% arrange(no)



##### escritura

write_xlsx(Municipio,paste0("M.xlsx"))
write_xlsx(Localidad,paste0("L.xlsx"))
write_xlsx(UPZ,paste0("U.xlsx"))
