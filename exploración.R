library(dplyr)

################################################################################
                                      #  Leer datos
################################################################################
file.choose()

filename <- "C:\\Users\\pablo\\OneDrive\\Escritorio\\R proyects\\Multiprop_Encuesta\\Data\\EM2021\\EM2021.rds"
EM21 <- readRDS(filename)

filename <- "C:\\Users\\pablo\\OneDrive\\Escritorio\\R proyects\\Multiprop_Encuesta\\Data\\EM2021\\EM2021_PLUS.rds"
EM21_plus <- readRDS(filename)


# ViviendA
length(unique(EM21$DIRECTORIO))
unique(EM21$DIRECTORIO)
a <- EM21[EM21$DIRECTORIO == 323974,]
a$DIRECTORIO_HOG

# Hogares
unique(EM21$DIRECTORIO_HOG)
length(unique(EM21$DIRECTORIO_HOG))

# Personas
length(unique(EM21$DIRECTORIO_PER))


length(unique(EM21_plus$directorio_per))

EM21$DIRECTORIO_PER[1:10]
EM21$DIRECTORIO_HOG[1:10]

EM21_plus$directorio_hog[1:10]
EM21_plus$directorio_per[1:10]


names(EM21_plus)[grep("directorio_per", names(EM21_plus))] <- "DIRECTORIO_PER"

EM21F <- merge(EM21, EM21_plus, 
                  by = c("DIRECTORIO_PER"),
                  all = TRUE)


#sectores 


  
EM21F$NPCKP16_COD4_c <- sprintf("%04d", as.numeric(EM21F$NPCKP16_COD4))

EM21F$DIV <- substr(EM21F$NPCKP16_COD4_c,1,2)

a <- EM21F[c("NPCKP16_COD4","DIV")]


length(unique(EM21F$DIV))



EM21F$DIV[EM21F$DIV == "00"] <- "00"
EM21F$DIV[EM21F$DIV == "01"] <- "Agricultura, ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "02"] <- "Silvicultura y extracción de madera"
EM21F$DIV[EM21F$DIV == "03"] <- "Pesca y acuicultura"
EM21F$DIV[EM21F$DIV == "05"] <- "Extracción de carbón de piedra y lignito"
EM21F$DIV[EM21F$DIV == "06"] <- "00"
EM21F$DIV[EM21F$DIV == "07"] <- "00"
EM21F$DIV[EM21F$DIV == "08"] <- "00"
EM21F$DIV[EM21F$DIV == "09"] <- "00"
EM21F$DIV[EM21F$DIV == "10"] <- "00"
EM21F$DIV[EM21F$DIV == "11"] <- ", ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "12"] <- "00"
EM21F$DIV[EM21F$DIV == "13"] <- "00"
EM21F$DIV[EM21F$DIV == "14"] <- "00"
EM21F$DIV[EM21F$DIV == "15"] <- "00"
EM21F$DIV[EM21F$DIV == "16"] <- "00"
EM21F$DIV[EM21F$DIV == "17"] <- "00"
EM21F$DIV[EM21F$DIV == "18"] <- "00"
EM21F$DIV[EM21F$DIV == "19"] <- "00"
EM21F$DIV[EM21F$DIV == "20"] <- "00"
EM21F$DIV[EM21F$DIV == "21"] <- ", ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "22"] <- "00"
EM21F$DIV[EM21F$DIV == "23"] <- "00"
EM21F$DIV[EM21F$DIV == "24"] <- "00"
EM21F$DIV[EM21F$DIV == "25"] <- "00"
EM21F$DIV[EM21F$DIV == "26"] <- "00"
EM21F$DIV[EM21F$DIV == "27"] <- "00"
EM21F$DIV[EM21F$DIV == "28"] <- "00"
EM21F$DIV[EM21F$DIV == "29"] <- "00"
EM21F$DIV[EM21F$DIV == "30"] <- "00"
EM21F$DIV[EM21F$DIV == "31"] <- ", ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "32"] <- "00"
EM21F$DIV[EM21F$DIV == "33"] <- "00"
EM21F$DIV[EM21F$DIV == "35"] <- "00"
EM21F$DIV[EM21F$DIV == "36"] <- "00"
EM21F$DIV[EM21F$DIV == "37"] <- "00"
EM21F$DIV[EM21F$DIV == "38"] <- "00"
EM21F$DIV[EM21F$DIV == "39"] <- "00"
EM21F$DIV[EM21F$DIV == "41"] <- ", ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "42"] <- "00"
EM21F$DIV[EM21F$DIV == "43"] <- "00"
EM21F$DIV[EM21F$DIV == "44"] <- "00"
EM21F$DIV[EM21F$DIV == "45"] <- "00"
EM21F$DIV[EM21F$DIV == "46"] <- "00"
EM21F$DIV[EM21F$DIV == "47"] <- "00"
EM21F$DIV[EM21F$DIV == "49"] <- "00"
EM21F$DIV[EM21F$DIV == "50"] <- "00"
EM21F$DIV[EM21F$DIV == "51"] <- ", ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "52"] <- "00"
EM21F$DIV[EM21F$DIV == "53"] <- "00"
EM21F$DIV[EM21F$DIV == "55"] <- "00"
EM21F$DIV[EM21F$DIV == "56"] <- "00"
EM21F$DIV[EM21F$DIV == "58"] <- "00"
EM21F$DIV[EM21F$DIV == "59"] <- "00"
EM21F$DIV[EM21F$DIV == "60"] <- "00"
EM21F$DIV[EM21F$DIV == "61"] <- ", ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "62"] <- "00"
EM21F$DIV[EM21F$DIV == "63"] <- "00"
EM21F$DIV[EM21F$DIV == "64"] <- "00"
EM21F$DIV[EM21F$DIV == "65"] <- "00"
EM21F$DIV[EM21F$DIV == "66"] <- "00"
EM21F$DIV[EM21F$DIV == "68"] <- "00"
EM21F$DIV[EM21F$DIV == "69"] <- "00"
EM21F$DIV[EM21F$DIV == "70"] <- "00"
EM21F$DIV[EM21F$DIV == "71"] <- ", ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "72"] <- "00"
EM21F$DIV[EM21F$DIV == "73"] <- "00"
EM21F$DIV[EM21F$DIV == "74"] <- "00"
EM21F$DIV[EM21F$DIV == "75"] <- "00"
EM21F$DIV[EM21F$DIV == "77"] <- "00"
EM21F$DIV[EM21F$DIV == "78"] <- "00"
EM21F$DIV[EM21F$DIV == "79"] <- "00"
EM21F$DIV[EM21F$DIV == "80"] <- "00"
EM21F$DIV[EM21F$DIV == "81"] <- ", ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "82"] <- "00"
EM21F$DIV[EM21F$DIV == "83"] <- "00"
EM21F$DIV[EM21F$DIV == "84"] <- "00"
EM21F$DIV[EM21F$DIV == "85"] <- "00"
EM21F$DIV[EM21F$DIV == "86"] <- "00"
EM21F$DIV[EM21F$DIV == "87"] <- "00"
EM21F$DIV[EM21F$DIV == "88"] <- "00"
EM21F$DIV[EM21F$DIV == "90"] <- ", ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "91"] <- ", ganadería, caza y actividades de servicios conexas" 
EM21F$DIV[EM21F$DIV == "92"] <- "00"
EM21F$DIV[EM21F$DIV == "93"] <- "00"
EM21F$DIV[EM21F$DIV == "94"] <- "00"
EM21F$DIV[EM21F$DIV == "95"] <- "00"
EM21F$DIV[EM21F$DIV == "96"] <- "00"
EM21F$DIV[EM21F$DIV == "97"] <- "00"
EM21F$DIV[EM21F$DIV == "98"] <- "00"
EM21F$DIV[EM21F$DIV == "99"] <- "00"



################################################################################
                                #  diccionarios
################################################################################


EM21$MPIO_NAME <- vector(mode='character',length=dim(EM21)[1])
EM21$MPIO_NAME[EM21$MPIO == 11001] <- "Bogotá"
EM21$MPIO_NAME[EM21$MPIO == 25740] <- "Sibaté"
EM21$MPIO_NAME[EM21$MPIO == 25473] <- "Mosquera"
EM21$MPIO_NAME[EM21$MPIO == 25290] <- "Fusagasugá"
EM21$MPIO_NAME[EM21$MPIO == 25214] <- "Cota"
EM21$MPIO_NAME[EM21$MPIO == 25175] <- "Chía"
EM21$MPIO_NAME[EM21$MPIO == 25758] <- "Sopó"
EM21$MPIO_NAME[EM21$MPIO == 25785] <- "Tabio"
EM21$MPIO_NAME[EM21$MPIO == 25898] <- "Zipacón"
EM21$MPIO_NAME[EM21$MPIO == 25754] <- "Soacha"
EM21$MPIO_NAME[EM21$MPIO == 25126] <- "Cajicá"
EM21$MPIO_NAME[EM21$MPIO == 25817] <- "Tocancipá"
EM21$MPIO_NAME[EM21$MPIO == 25430] <- "Madrid"
EM21$MPIO_NAME[EM21$MPIO == 25286] <- "Funza"
EM21$MPIO_NAME[EM21$MPIO == 25260] <- "El Rosal"
EM21$MPIO_NAME[EM21$MPIO == 25099] <- "Bojacá"
EM21$MPIO_NAME[EM21$MPIO == 25799] <- "Tenjo"
EM21$MPIO_NAME[EM21$MPIO == 25899] <- "Zipaquira"
EM21$MPIO_NAME[EM21$MPIO == 25269] <- "Facatativa"
EM21$MPIO_NAME[EM21$MPIO == 25769] <- "Subachoque"
EM21$MPIO_NAME[EM21$MPIO == 25377] <- "La Calera"
EM21$MPIO_NAME[EM21$MPIO == 25295] <- "Gachancipá"


################################################################################
                              #  Mercado laboral
################################################################################
sum(EM21$, na.rm = TRUE)
EM21$FL
class(EM21$NPCEP4)


sort(unique(EM21[EM21$FL == 1,]$NPCEP4))

# por municipios

list <- split(EM21,EM21$MPIO_NAME) 
variable <- sapply(list, function(x) {
  x <- x[x$CLASE == 1,]
  is.na(x$DES) <- 0
  is.na(x$FL) <- 0
  ft <- sum(x[x$FL == 1,]$FEX_C, na.rm = TRUE)
  d <- sum(x[x$DES == 1,]$FEX_C, na.rm = TRUE)
  td_g <- d/ft*100
  
  ft_h <- sum(x[x$FL == 1 & x$NPCEP5 == 1 ,]$FEX_C, na.rm = TRUE)
  d_h <- sum(x[x$DES == 1 & x$NPCEP5 == 1 ,]$FEX_C, na.rm = TRUE)
  td_h <- d_h/ft_h*100
  
  ft_m <- sum(x[x$FL == 1 & x$NPCEP5 == 2,]$FEX_C, na.rm = TRUE)
  d_m <- sum(x[x$DES == 1 & x$NPCEP5 == 2,]$FEX_C, na.rm = TRUE)
  td_m <- d_m/ft_m*100
  
  ft_j <- sum(x[x$FL == 1 & x$NPCEP4 <=28,]$FEX_C, na.rm = TRUE)
  d_j <- sum(x[x$DES == 1 & x$NPCEP4 <=28,]$FEX_C, na.rm = TRUE)
  td_j <- d_j/ft_j*100
  
  
  t <- c(td_g, td_h, td_m, td_j)
}) 



data.frame(t(variable))


