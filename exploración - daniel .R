memory.limit(size = 10000)
library(dplyr)
library(openxlsx)


################################################################################
                                      #  Leer datos
################################################################################

encuesta<- "C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Encuesta Multiproposito\\Microdatos\\Multiproposito.rds"
EM21 <- readRDS(encuesta)

adicionales <- "C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Encuesta Multiproposito\\Microdatos\\Adicionales.rds"
EM21_plus <- readRDS(adicionales)

base<- "C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Encuesta Multiproposito\\Microdatos\\Multiproposito_completa.rds"
EM21F <- readRDS(base)



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

#  Unión de las bases

names(EM21_plus)[grep("directorio_per", names(EM21_plus))] <- "DIRECTORIO_PER"

EM21F <- merge(EM21, EM21_plus, 
                  by = c("DIRECTORIO_PER"),
                  all = TRUE)


#sectores 


EM21F$NPCKP16_COD4_c <- sprintf("%04d", as.numeric(EM21F$NPCKP16_COD4))

EM21F$DIV <- substr(EM21F$NPCKP16_COD4_c,1,2)

sort(unique(EM21F$NPCKP16_COD4))



sort(unique(EM21F$DIV))

table(EM21F$NPCKP16_COD4)
table(EM21F$DIV)



EM21F$DIV_n <- vector(mode='character',length=dim(EM21F)[1])

EM21F$DIV_n[EM21F$DIV == "00"] <- "00"
EM21F$DIV_n[EM21F$DIV == "01"] <- "Agricultura, ganadería, caza y actividades de servicios conexas" 
EM21F$DIV_n[EM21F$DIV == "02"] <- "Silvicultura y extracción de madera"
EM21F$DIV_n[EM21F$DIV == "03"] <- "Pesca y acuicultura"
EM21F$DIV_n[EM21F$DIV == "05"] <- "Extracción de carbón de piedra y lignito"
EM21F$DIV_n[EM21F$DIV == "06"] <- "Extracción de petróleo crudo y gas natural"
EM21F$DIV_n[EM21F$DIV == "07"] <- "Extracción de minerales metalíferos"
EM21F$DIV_n[EM21F$DIV == "08"] <- "Extracción de otras minas y canteras"
EM21F$DIV_n[EM21F$DIV == "09"] <- "Actividades de servicios de apoyo para la explotación de minas y canteras"
EM21F$DIV_n[EM21F$DIV == "10"] <- "Elaboración de productos alimenticios"
EM21F$DIV_n[EM21F$DIV == "11"] <- "Elaboración de bebidas" 
EM21F$DIV_n[EM21F$DIV == "12"] <- "Elaboración de productos de tabaco"
EM21F$DIV_n[EM21F$DIV == "13"] <- "Fabricación de productos textiles"
EM21F$DIV_n[EM21F$DIV == "14"] <- "Confección de prendas de vestir"
EM21F$DIV_n[EM21F$DIV == "15"] <- "Curtido y recurtido de cueros; fabricación de calzado;fabricación de artículos de viaje, maletas, bolsos de mano y artículos similares, y fabricación de artículos de talabartería y guarnicionería; adobo y teñido de pieles"
EM21F$DIV_n[EM21F$DIV == "16"] <- "Transformación de la madera y fabricación de productos de madera y de corcho, excepto muebles; fabricación de artículos de cestería y espartería"
EM21F$DIV_n[EM21F$DIV == "17"] <- "Fabricación de papel, cartón y productos de papel y cartón"
EM21F$DIV_n[EM21F$DIV == "18"] <- "Actividades de impresión y de producción de copias a partir de grabaciones originales"
EM21F$DIV_n[EM21F$DIV == "19"] <- "Coquización, fabricación de productos de la refinación del petróleo y actividad de mezcla de combustibles"
EM21F$DIV_n[EM21F$DIV == "20"] <- "Fabricación de sustancias y productos químicos"
EM21F$DIV_n[EM21F$DIV == "21"] <- "Fabricación de productos farmacéuticos, sustancias químicas medicinales y productos botánicos de uso farmacéutico" 
EM21F$DIV_n[EM21F$DIV == "22"] <- "Fabricación de productos de caucho y de plástico"
EM21F$DIV_n[EM21F$DIV == "23"] <- "Fabricación de otros productos minerales no metálicos"
EM21F$DIV_n[EM21F$DIV == "24"] <- "Fabricación de productos metalúrgicos básicos"
EM21F$DIV_n[EM21F$DIV == "25"] <- "Fabricación de productos elaborados de metal, excepto maquinaria y equipo"
EM21F$DIV_n[EM21F$DIV == "26"] <- "Fabricación de productos informáticos, electrónicos y ópticos"
EM21F$DIV_n[EM21F$DIV == "27"] <- "Fabricación de aparatos y equipo eléctrico"
EM21F$DIV_n[EM21F$DIV == "28"] <- "Fabricación de maquinaria y equipo n.c.p."
EM21F$DIV_n[EM21F$DIV == "29"] <- "Fabricación de vehículos automotores, remolques y semirremolques"
EM21F$DIV_n[EM21F$DIV == "30"] <- "Fabricación de otros tipos de equipo de transporte"
EM21F$DIV_n[EM21F$DIV == "31"] <- "Fabricación de muebles, colchones y somieres" 
EM21F$DIV_n[EM21F$DIV == "32"] <- "Otras industrias manufactureras"
EM21F$DIV_n[EM21F$DIV == "33"] <- "Instalación, mantenimiento y reparación especializado de maquinaria y equipo"
EM21F$DIV_n[EM21F$DIV == "35"] <- "Suministro de electricidad, gas, vapor y aire acondicionado"
EM21F$DIV_n[EM21F$DIV == "36"] <- "Captación, tratamiento y distribución de agua"
EM21F$DIV_n[EM21F$DIV == "37"] <- "Evacuación y tratamiento de aguas residuales"
EM21F$DIV_n[EM21F$DIV == "38"] <- "Recolección, tratamiento y disposición de desechos,recuperación de materiales"
EM21F$DIV_n[EM21F$DIV == "39"] <- "Actividades de saneamiento ambiental y otros servicios de gestión de desechos"
EM21F$DIV_n[EM21F$DIV == "41"] <- "Construcción de edificios" 
EM21F$DIV_n[EM21F$DIV == "42"] <- "Obras de ingeniería civil"
EM21F$DIV_n[EM21F$DIV == "43"] <- "Actividades especializadas para la construcción de edificios y obras de ingeniería civil"
EM21F$DIV_n[EM21F$DIV == "44"] <- "44"
EM21F$DIV_n[EM21F$DIV == "45"] <- "Comercio, mantenimiento y reparación de vehículos automotores y motocicletas, sus partes, piezas y accesorios"
EM21F$DIV_n[EM21F$DIV == "46"] <- "Comercio al por mayor y en comisión o por contrata, excepto el comercio de vehículos automotores y motocicletas"
EM21F$DIV_n[EM21F$DIV == "47"] <- "Comercio al por menor (incluso el comercio al por menor de combustibles), excepto el de vehículos automotores y motocicletas"
EM21F$DIV_n[EM21F$DIV == "49"] <- "Transporte terrestre; transporte por tuberías"
EM21F$DIV_n[EM21F$DIV == "50"] <- "Transporte acuático"
EM21F$DIV_n[EM21F$DIV == "51"] <- "Transporte aéreo" 
EM21F$DIV_n[EM21F$DIV == "52"] <- "Almacenamiento y actividades complementarias al transport"
EM21F$DIV_n[EM21F$DIV == "53"] <- "Correo y servicios de mensajería"
EM21F$DIV_n[EM21F$DIV == "55"] <- "Alojamiento"
EM21F$DIV_n[EM21F$DIV == "56"] <- "Actividades de servicios de comidas y bebidas"
EM21F$DIV_n[EM21F$DIV == "58"] <- "Actividades de edición"
EM21F$DIV_n[EM21F$DIV == "59"] <- "Actividades cinematográficas, de video y producción de programas de televisión, grabación de sonido y edición de música"
EM21F$DIV_n[EM21F$DIV == "60"] <- "Actividades de programación, transmisión y/o difusión"
EM21F$DIV_n[EM21F$DIV == "61"] <- "Telecomunicaciones" 
EM21F$DIV_n[EM21F$DIV == "62"] <- "Desarrollo de sistemas informáticos (planificación, análisis, diseño, programación, pruebas), consultoría informática y actividades relacionadas"
EM21F$DIV_n[EM21F$DIV == "63"] <- "Actividades de servicios de información"
EM21F$DIV_n[EM21F$DIV == "64"] <- "Actividades de servicios financieros, excepto las de seguros y de pensiones"
EM21F$DIV_n[EM21F$DIV == "65"] <- "Seguros (incluso el reaseguro), seguros sociales y fondos de pensiones, excepto la seguridad social"
EM21F$DIV_n[EM21F$DIV == "66"] <- "Actividades auxiliares de las actividades de servicios financieros"
EM21F$DIV_n[EM21F$DIV == "68"] <- "Actividades inmobiliarias"
EM21F$DIV_n[EM21F$DIV == "69"] <- "Actividades jurídicas y de contabilidad"
EM21F$DIV_n[EM21F$DIV == "70"] <- "Actividades de administración empresarial; actividades de consultoría de gestión"
EM21F$DIV_n[EM21F$DIV == "71"] <- "Actividades de arquitectura e ingeniería; ensayos y análisis técnicos" 
EM21F$DIV_n[EM21F$DIV == "72"] <- "Investigación científica y desarrollo"
EM21F$DIV_n[EM21F$DIV == "73"] <- "Publicidad y estudios de mercado"
EM21F$DIV_n[EM21F$DIV == "74"] <- "Otras actividades profesionales, científicas y técnicas"
EM21F$DIV_n[EM21F$DIV == "75"] <- "Actividades veterinarias"
EM21F$DIV_n[EM21F$DIV == "77"] <- "Actividades de alquiler y arrendamiento"
EM21F$DIV_n[EM21F$DIV == "78"] <- "Actividades de empleo"
EM21F$DIV_n[EM21F$DIV == "79"] <- "Actividades de las agencias de viajes, operadores turísticos,servicios de reserva y actividades relacionadas"
EM21F$DIV_n[EM21F$DIV == "80"] <- "Actividades de seguridad e investigación privada"
EM21F$DIV_n[EM21F$DIV == "81"] <- "Actividades de servicios a edificios y paisajismo (jardines, zonas verdes)" 
EM21F$DIV_n[EM21F$DIV == "82"] <- "Actividades administrativas y de apoyo de oficina y otras actividades de apoyo a las empresas"
EM21F$DIV_n[EM21F$DIV == "83"] <- "83"
EM21F$DIV_n[EM21F$DIV == "84"] <- "Administración pública y defensa; planes de seguridad social de afiliación obligatoria"
EM21F$DIV_n[EM21F$DIV == "85"] <- "Educación"
EM21F$DIV_n[EM21F$DIV == "86"] <- "Actividades de atención de la salud humana"
EM21F$DIV_n[EM21F$DIV == "87"] <- "Actividades de atención residencial medicalizada"
EM21F$DIV_n[EM21F$DIV == "88"] <- "Actividades de asistencia social sin alojamiento"
EM21F$DIV_n[EM21F$DIV == "90"] <- "Actividades creativas, artísticas y de entretenimiento" 
EM21F$DIV_n[EM21F$DIV == "91"] <- "Actividades de bibliotecas, archivos, museos y otras actividades culturales" 
EM21F$DIV_n[EM21F$DIV == "92"] <- "Actividades de juegos de azar y apuestas"
EM21F$DIV_n[EM21F$DIV == "93"] <- "Actividades deportivas y actividades recreativas y de esparcimiento"
EM21F$DIV_n[EM21F$DIV == "94"] <- "Actividades de asociaciones"
EM21F$DIV_n[EM21F$DIV == "95"] <- "Mantenimiento y reparación de computadores, efectos personales y enseres domésticos"
EM21F$DIV_n[EM21F$DIV == "96"] <- "Otras actividades de servicios personales"
EM21F$DIV_n[EM21F$DIV == "97"] <- "Actividades de los hogares individuales como empleadores de personal doméstico"
EM21F$DIV_n[EM21F$DIV == "98"] <- "Actividades no diferenciadas de los hogares individuales como productores de bienes y servicios para uso propio"
EM21F$DIV_n[EM21F$DIV == "99"] <- "Actividades de organizaciones y entidades extraterritoriales"


EM21F$SEC <- vector(mode='character',length=dim(EM21F)[1])


EM21F$SEC[EM21F$DIV == "01" | EM21F$DIV == "02"| EM21F$DIV == "03"] <- "Agricultura, ganadería, caza, silvicultura y pesca"

EM21F$SEC[EM21F$DIV == "05" | EM21F$DIV == "06"| EM21F$DIV == "07" | EM21F$DIV == "08" | EM21F$DIV == "09" ] <- "Explotación de minas y canteras"

EM21F$SEC[EM21F$DIV == "10"| EM21F$DIV == "11" | EM21F$DIV == "12" | EM21F$DIV == "13" | EM21F$DIV == "14" | 
            EM21F$DIV == "15" |  EM21F$DIV == "16" |  EM21F$DIV == "17" |  EM21F$DIV == "18" |  EM21F$DIV == "19" |
            EM21F$DIV == "20" |  EM21F$DIV == "21"|  EM21F$DIV == "22" |  EM21F$DIV == "23" |  EM21F$DIV == "24" |
            EM21F$DIV == "25" | EM21F$DIV == "26" |EM21F$DIV == "27" | EM21F$DIV == "28" | EM21F$DIV == "29" |
            EM21F$DIV == "30" |EM21F$DIV == "31" |EM21F$DIV == "32" |EM21F$DIV == "33"] <- "Industrias manufactureras"

EM21F$SEC[EM21F$DIV == "35"] <- "Suministro de electricidad, gas, vapor, y aire acondicionado"

EM21F$SEC[EM21F$DIV == "36"| EM21F$DIV == "37" | EM21F$DIV == "38" | EM21F$DIV == "39" ] <- "Distribución de agua; evacuación y tratamiento de aguas residuales, gestión de desechos y actividades de saneamiento ambiental"

EM21F$SEC[EM21F$DIV == "41"| EM21F$DIV == "42" | EM21F$DIV == "43"] <- "Construcción"

EM21F$SEC[EM21F$DIV == "45"| EM21F$DIV == "46" | EM21F$DIV == "47" ] <- "Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas"

EM21F$SEC[EM21F$DIV == "49"| EM21F$DIV == "50" | EM21F$DIV == "51" | EM21F$DIV == "52" | EM21F$DIV == "53"] <- "Transporte y almacenamiento"

EM21F$SEC[EM21F$DIV == "55" | EM21F$DIV == "56"] <- "Alojamiento y servicios de comida"

EM21F$SEC[EM21F$DIV == "58"| EM21F$DIV == "59" | EM21F$DIV == "60" | EM21F$DIV == "61" | EM21F$DIV == "62" | EM21F$DIV == "63"] <- "Información y comunicaciones"

EM21F$SEC[EM21F$DIV == "64" |  EM21F$DIV == "65" |  EM21F$DIV == "66"] <- "Actividades financieras y de seguros"

EM21F$SEC[EM21F$DIV == "68"] <- "Actividades inmobiliarias"

EM21F$SEC[EM21F$DIV == "69"| EM21F$DIV == "70" | EM21F$DIV == "71" | EM21F$DIV == "72" | EM21F$DIV == "73" | EM21F$DIV == "74" |
            EM21F$DIV == "75"] <- "Actividades profesionales, científicas y técnicas"

EM21F$SEC[EM21F$DIV == "77"| EM21F$DIV == "78" | EM21F$DIV == "79" | EM21F$DIV == "80" | EM21F$DIV == "81" | EM21F$DIV == "82"] <- "Actividades de servicios administrativos y de poyo"

EM21F$SEC[EM21F$DIV == "84"] <- "Administración pública y defensa; planes de seguridad social de afiliación obligatoria"

EM21F$SEC[EM21F$DIV == "85"] <- "Educación"

EM21F$SEC[EM21F$DIV == "86"| EM21F$DIV == "87" | EM21F$DIV == "88" ] <- "Actividades de atención de la salud humana y de asistencia social"

EM21F$SEC[EM21F$DIV == "90"| EM21F$DIV == "91" | EM21F$DIV == "92" | EM21F$DIV == "93" ] <- "Actividades artísticas, de entretenimiento y recreación"

EM21F$SEC[EM21F$DIV == "94"| EM21F$DIV == "95" | EM21F$DIV == "96" ] <- "Otras actividades de servicios"

EM21F$SEC[EM21F$DIV == "97"| EM21F$DIV == "98"] <- "Actividades de los hogares en calidad de empleadores; actividades no diferenciadas de los hogares individuales como productores de bienes y servicios para uso propio"

EM21F$SEC[EM21F$DIV == "99"] <- "Actividades de organizaciones y entidades extraterritoriales"


sort(table(EM21F$SEC))



###############################################################################
                  #Juventud
###############################################################################

EM_JOV <- EM21[EM21$NPCEP4 >= 14 & EM21$NPCEP4 <= 28 ,]


sort(table(EM_JOV$SEC))

library(dplyr)

a <- EST %>% filter(NPCHP13==2) %>% group

tablita_sec <- EM_JOV %>% group_by(SEC) %>% summarise(no = length(SEC))  %>% arrange(no)


tablita_div <- EM_JOV %>% group_by(DIV_n) %>% summarise(no = length(DIV_n))  %>% arrange(no)


tablita_div_c <- EM_JOV %>% filter(DIV_n == "Comercio al por menor (incluso el comercio al por menor de combustibles), excepto el de vehículos automotores y motocicletas") %>%
  group_by(NPCKP16_COD4)%>% summarise(no = length(DIV_n))  %>% arrange(no)


tablita_div_a <- EM_JOV %>% filter(DIV_n ==  "Actividades de servicios de comidas y bebidas") %>%
  group_by(NPCKP16_COD4)%>% summarise(no = length(DIV_n))  %>% arrange(no)


tablita_div_OFI <- EM_JOV %>% filter(DIV_n ==  "Actividades administrativas y de apoyo de oficina y otras actividades de apoyo a las empresas") %>%
  group_by(NPCKP16_COD4)%>% summarise(no = length(DIV_n))  %>% arrange(no)
  

sum(tablita_div_a$no)


tablita_CLASE <- EM_JOV %>% group_by(NPCKP16_COD4)%>% summarise(no = length(DIV_n))  %>% arrange(no)


tablita_CLASE <- EM_JOV %>% filter() %>% group_by(NPCKP16_COD4)%>% summarise(no = length(DIV_n))  %>% arrange(no)


################################################################################
                                #  diccionarios
################################################################################


EM21F$MPIO_NAME <- vector(mode='character',length=dim(EM21F)[1])
EM21F$MPIO_NAME[EM21F$MPIO == 11001] <- "Bogota"
EM21F$MPIO_NAME[EM21F$MPIO == 25740] <- "Sibate"
EM21F$MPIO_NAME[EM21F$MPIO == 25473] <- "Mosquera"
EM21F$MPIO_NAME[EM21F$MPIO == 25290] <- "Fusagasuga"
EM21F$MPIO_NAME[EM21F$MPIO == 25214] <- "Cota"
EM21F$MPIO_NAME[EM21F$MPIO == 25175] <- "Chia"
EM21F$MPIO_NAME[EM21F$MPIO == 25758] <- "Sopo"
EM21F$MPIO_NAME[EM21F$MPIO == 25785] <- "Tabio"
EM21F$MPIO_NAME[EM21F$MPIO == 25898] <- "Zipacon"
EM21F$MPIO_NAME[EM21F$MPIO == 25754] <- "Soacha"
EM21F$MPIO_NAME[EM21F$MPIO == 25126] <- "Cajica"
EM21F$MPIO_NAME[EM21F$MPIO == 25817] <- "Tocancipa"
EM21F$MPIO_NAME[EM21F$MPIO == 25430] <- "Madrid"
EM21F$MPIO_NAME[EM21F$MPIO == 25286] <- "Funza"
EM21F$MPIO_NAME[EM21F$MPIO == 25260] <- "El Rosal"
EM21F$MPIO_NAME[EM21F$MPIO == 25099] <- "Bojaca"
EM21F$MPIO_NAME[EM21F$MPIO == 25799] <- "Tenjo"
EM21F$MPIO_NAME[EM21F$MPIO == 25899] <- "Zipaquira"
EM21F$MPIO_NAME[EM21F$MPIO == 25269] <- "Facatativa"
EM21F$MPIO_NAME[EM21F$MPIO == 25769] <- "Subachoque"
EM21F$MPIO_NAME[EM21F$MPIO == 25377] <- "La Calera"
EM21F$MPIO_NAME[EM21F$MPIO == 25295] <- "Gachancipa"


################################################################################
                              #  Separación de la data
################################################################################

# por municipios

list <- split(EM21F,EM21F$MPIO_NAME)
bog <- as.data.frame(list$Bogota)
chia <- as.data.frame(list$Chia)

# por localidades 
lista <- split(bog, bog$NOMBRE_LOCALIDAD)
usaquen <- as.data.frame(lista$Usaquén)

#  
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


tabla_a <- as.data.frame(tapply(bog$NPCKP23, bog$NOMBRE_LOCALIDAD, mean (bog$NPCKP23,na.rm = TRUE)))

mean(EM21F$NPCKP23,na.rm = TRUE)

tabla1 <- bog %>% group_by(bog$NOMBRE_LOCALIDAD) %>% summarise(Mean = mean(bog$NPCKP23, na.rm = T))
tabla_a <- EM21F %>% group_by(EM21F$MPIO_NAME) %>% summarise(Mean = mean(NPCKP23, na.rm = T))


###############################################################################
            # Condiciones de Vida
###############################################################################

# Número de personas por hogar
tabla1 <- EM21F %>% group_by(EM21F$MPIO_NAME, EM21F$NVCBP11AA)  %>% summarise(Promedio = mean(NHCCPCTRL2, na.rm = T))
write.xlsx(tabla1, 'C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Encuesta Multiproposito', colNames= TRUE, overwrite = TRUE)

tabla2 <- bog %>% group_by(bog$NOMBRE_LOCALIDAD, bog$NVCBP11AA)  %>% summarise(Promedio = mean(NHCCPCTRL2, na.rm = T))
write.xlsx(tabla1, 'C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Encuesta Multiproposito', colNames= TRUE, overwrite = TRUE)

EM21F$CLASE <- vector(mode='character',length=dim(EM21F)[1])
EM21F$CLASE[EM21F$NPCKP23>0] <- "Clase alta"

tabla2 <- bog %>% group_by(bog$NOMBRE_LOCALIDAD, bog$NVCBP16) %>% summarise(Promedio = sum(NHCCPCTRL2, na.rm = T))

municipios <- c(table(EM21F$MPIO_NAME))


######### EDUCACIÓN POR FUERA DEL MUNICIPIO 
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










