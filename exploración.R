library(dplyr)

################################################################################
                                      #  Leer datos
################################################################################
file.choose()

filename <- "C:\\Users\\pablo\\OneDrive\\Escritorio\\R proyects\\Multiprop_Encuesta\\Data\\EM2021\\EM2021_RDS.rds"

EM21 <- readRDS(filename)
f <- 1

# ViviendA
length(unique(EM21$DIRECTORIO))
unique(EM21$DIRECTORIO)


a <- EM21[EM21$DIRECTORIO == 323974,]

a$DIRECTORIO_HOG

# Hogares
unique(EM21$DIRECTORIO_HOG)
length(unique(EM21$DIRECTORIO_HOG))



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


