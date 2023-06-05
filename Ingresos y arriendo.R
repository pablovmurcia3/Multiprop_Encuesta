Hog <- EM21F %>% distinct(DIRECTORIO_HOG, .keep_all = TRUE)

Migrantes <- Hog %>% filter(NPCEP13D==1 | NPCEP16D_1==1)


# N_ingpcugarr 

quantile(Migrantes$N_ingpcugarr)


Migrantes$Ingresopc <- cut(Migrantes$N_ingpcugarr,
                           breaks = c(-1 , 250000, 500000,1000000, 33000000))

Ingresopc <- Migrantes %>% group_by(Ingresopc) %>% 
  summarise(no = sum(FEX_C)) %>%
  mutate(freq = round(no/sum(no),3))

weighted.mean(Migrantes$N_ingpcugarr, Migrantes$FEX_C, na.rm =TRUE)
sd(Migrantes$N_ingpcugarr,na.rm =TRUE)
write_xlsx(Ingresopc,paste0("ing.xlsx"))


# NHCCP10 

quantile(Migrantes$NHCCP10, na.rm = TRUE)

table(is.na(Migrantes$NHCCP10))

max(Migrantes$NHCCP10,na.rm = TRUE)

table(Migrantes$arriendo)

Migrantes$arriendo <- cut(Migrantes$NHCCP10,
                          breaks = c(-1 , 250000, 500000,1000000, 22000000))

Arriendo <- Migrantes %>% group_by(arriendo) %>% 
  summarise(no = sum(FEX_C)) %>%
  mutate(freq = round(no/sum(no),3))


weighted.mean(Migrantes$NHCCP10, Migrantes$FEX_C, na.rm =TRUE)
sd(Migrantes$NHCCP10,na.rm =TRUE)

write_xlsx(Arriendo,paste0("arriend.xlsx"))

