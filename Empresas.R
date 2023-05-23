library(dplyr)
library(openxlsx)
library(foreign)

Empresas <- read.csv("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Presentación SURA\\Empresas_2021.csv")

Empresas_bog <- read.dbf("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Presentación SURA\\Empresas__Bta.dbf", as.is = F)

###############################################################################
                    #CUNDINAMARCA
###############################################################################

CUN <- Empresas %>% filter(Empresas$Region=="Cundinamarca")

CUN <- CUN %>% filter(CUN$City=="Chía" | CUN$City=="Cajicá" | CUN$City=="El Rosal" | CUN$City=="Facatativá" | CUN$City=="Funza" | CUN$City=="Guasca" | 
                                          CUN$City=="Gachancipá" | CUN$City=="Guasca" | CUN$City=="La Calera" | CUN$City=="Madrid" | CUN$City=="Mosquera" | 
                                          CUN$City=="Sibaté" | CUN$City=="Soacha" | CUN$City=="Sopó" | CUN$City=="Tocancipá" | CUN$City=="Zipaquirá")

tablaa <- as.data.frame(table(CUN$City))

############### DIVISIONES #####################################################

CUN$USER_CIIU_ID_CIIU_4_c <- sprintf("%04d", as.numeric(CUN$USER_CIIU_ID_CIIU_4))
CUN$DIV <- substr(CUN$USER_CIIU_ID_CIIU_4_c,1,2)
CUN$DIV_n <- vector(mode="character",length=dim(CUN)[1])

CUN$DIV_n[CUN$DIV == "00"] <- "00"
CUN$DIV_n[CUN$DIV == "01"] <- "Agricultura, ganadería, caza y actividades de servicios conexas" 
CUN$DIV_n[CUN$DIV == "02"] <- "Silvicultura y extracción de madera"
CUN$DIV_n[CUN$DIV == "03"] <- "Pesca y acuicultura"
CUN$DIV_n[CUN$DIV == "05"] <- "Extracción de carbón de piedra y lignito"
CUN$DIV_n[CUN$DIV == "06"] <- "Extracción de petróleo crudo y gas natural"
CUN$DIV_n[CUN$DIV == "07"] <- "Extracción de minerales metalíferos"
CUN$DIV_n[CUN$DIV == "08"] <- "Extracción de otras minas y canteras"
CUN$DIV_n[CUN$DIV == "09"] <- "Actividades de servicios de apoyo para la explotación de minas y canteras"
CUN$DIV_n[CUN$DIV == "10"] <- "Elaboración de productos alimenticios"
CUN$DIV_n[CUN$DIV == "11"] <- "Elaboración de bebidas" 
CUN$DIV_n[CUN$DIV == "12"] <- "Elaboración de productos de tabaco"
CUN$DIV_n[CUN$DIV == "13"] <- "Fabricación de productos textiles"
CUN$DIV_n[CUN$DIV == "14"] <- "Confección de prendas de vestir"
CUN$DIV_n[CUN$DIV == "15"] <- "Curtido y recurtido de cueros; fabricación de calzado;fabricación de artículos de viaje, maletas, bolsos de mano y artículos similares, y fabricación de artículos de talabartería y guarnicionería; adobo y teñido de pieles"
CUN$DIV_n[CUN$DIV == "16"] <- "Transformación de la madera y fabricación de productos de madera y de corcho, excepto muebles; fabricación de artículos de cestería y espartería"
CUN$DIV_n[CUN$DIV == "17"] <- "Fabricación de papel, cartón y productos de papel y cartón"
CUN$DIV_n[CUN$DIV == "18"] <- "Actividades de impresión y de producción de copias a partir de grabaciones originales"
CUN$DIV_n[CUN$DIV == "19"] <- "Coquización, fabricación de productos de la refinación del petróleo y actividad de mezcla de combustibles"
CUN$DIV_n[CUN$DIV == "20"] <- "Fabricación de sustancias y productos químicos"
CUN$DIV_n[CUN$DIV == "21"] <- "Fabricación de productos farmacéuticos, sustancias químicas medicinales y productos botánicos de uso farmacéutico" 
CUN$DIV_n[CUN$DIV == "22"] <- "Fabricación de productos de caucho y de plástico"
CUN$DIV_n[CUN$DIV == "23"] <- "Fabricación de otros productos minerales no metálicos"
CUN$DIV_n[CUN$DIV == "24"] <- "Fabricación de productos metalúrgicos básicos"
CUN$DIV_n[CUN$DIV == "25"] <- "Fabricación de productos elaborados de metal, excepto maquinaria y equipo"
CUN$DIV_n[CUN$DIV == "26"] <- "Fabricación de productos informáticos, electrónicos y ópticos"
CUN$DIV_n[CUN$DIV == "27"] <- "Fabricación de aparatos y equipo eléctrico"
CUN$DIV_n[CUN$DIV == "28"] <- "Fabricación de maquinaria y equipo n.c.p."
CUN$DIV_n[CUN$DIV == "29"] <- "Fabricación de vehículos automotores, remolques y semirremolques"
CUN$DIV_n[CUN$DIV == "30"] <- "Fabricación de otros tipos de equipo de transporte"
CUN$DIV_n[CUN$DIV == "31"] <- "Fabricación de muebles, colchones y somieres" 
CUN$DIV_n[CUN$DIV == "32"] <- "Otras industrias manufactureras"
CUN$DIV_n[CUN$DIV == "33"] <- "Instalación, mantenimiento y reparación especializado de maquinaria y equipo"
CUN$DIV_n[CUN$DIV == "35"] <- "Suministro de electricidad, gas, vapor y aire acondicionado"
CUN$DIV_n[CUN$DIV == "36"] <- "Captación, tratamiento y distribución de agua"
CUN$DIV_n[CUN$DIV == "37"] <- "Evacuación y tratamiento de aguas residuales"
CUN$DIV_n[CUN$DIV == "38"] <- "Recolección, tratamiento y disposición de desechos,recuperación de materiales"
CUN$DIV_n[CUN$DIV == "39"] <- "Actividades de saneamiento ambiental y otros servicios de gestión de desechos"
CUN$DIV_n[CUN$DIV == "41"] <- "Construcción de edificios" 
CUN$DIV_n[CUN$DIV == "42"] <- "Obras de ingeniería civil"
CUN$DIV_n[CUN$DIV == "43"] <- "Actividades especializadas para la construcción de edificios y obras de ingeniería civil"
CUN$DIV_n[CUN$DIV == "44"] <- "44"
CUN$DIV_n[CUN$DIV == "45"] <- "Comercio, mantenimiento y reparación de vehículos automotores y motocicletas, sus partes, piezas y accesorios"
CUN$DIV_n[CUN$DIV == "46"] <- "Comercio al por mayor y en comisión o por contrata, excepto el comercio de vehículos automotores y motocicletas"
CUN$DIV_n[CUN$DIV == "47"] <- "Comercio al por menor (incluso el comercio al por menor de combustibles), excepto el de vehículos automotores y motocicletas"
CUN$DIV_n[CUN$DIV == "49"] <- "Transporte terrestre; transporte por tuberías"
CUN$DIV_n[CUN$DIV == "50"] <- "Transporte acuático"
CUN$DIV_n[CUN$DIV == "51"] <- "Transporte aéreo" 
CUN$DIV_n[CUN$DIV == "52"] <- "Almacenamiento y actividades complementarias al transport"
CUN$DIV_n[CUN$DIV == "53"] <- "Correo y servicios de mensajería"
CUN$DIV_n[CUN$DIV == "55"] <- "Alojamiento"
CUN$DIV_n[CUN$DIV == "56"] <- "Actividades de servicios de comidas y bebidas"
CUN$DIV_n[CUN$DIV == "58"] <- "Actividades de edición"
CUN$DIV_n[CUN$DIV == "59"] <- "Actividades cinematográficas, de video y producción de programas de televisión, grabación de sonido y edición de música"
CUN$DIV_n[CUN$DIV == "60"] <- "Actividades de programación, transmisión y/o difusión"
CUN$DIV_n[CUN$DIV == "61"] <- "Telecomunicaciones" 
CUN$DIV_n[CUN$DIV == "62"] <- "Desarrollo de sistemas informáticos (planificación, análisis, diseño, programación, pruebas), consultoría informática y actividades relacionadas"
CUN$DIV_n[CUN$DIV == "63"] <- "Actividades de servicios de información"
CUN$DIV_n[CUN$DIV == "64"] <- "Actividades de servicios financieros, excepto las de seguros y de pensiones"
CUN$DIV_n[CUN$DIV == "65"] <- "Seguros (incluso el reaseguro), seguros sociales y fondos de pensiones, excepto la seguridad social"
CUN$DIV_n[CUN$DIV == "66"] <- "Actividades auxiliares de las actividades de servicios financieros"
CUN$DIV_n[CUN$DIV == "68"] <- "Actividades inmobiliarias"
CUN$DIV_n[CUN$DIV == "69"] <- "Actividades jurídicas y de contabilidad"
CUN$DIV_n[CUN$DIV == "70"] <- "Actividades de administración empresarial; actividades de consultoría de gestión"
CUN$DIV_n[CUN$DIV == "71"] <- "Actividades de arquitectura e ingeniería; ensayos y análisis técnicos" 
CUN$DIV_n[CUN$DIV == "72"] <- "Investigación científica y desarrollo"
CUN$DIV_n[CUN$DIV == "73"] <- "Publicidad y estudios de mercado"
CUN$DIV_n[CUN$DIV == "74"] <- "Otras actividades profesionales, científicas y técnicas"
CUN$DIV_n[CUN$DIV == "75"] <- "Actividades veterinarias"
CUN$DIV_n[CUN$DIV == "77"] <- "Actividades de alquiler y arrendamiento"
CUN$DIV_n[CUN$DIV == "78"] <- "Actividades de empleo"
CUN$DIV_n[CUN$DIV == "79"] <- "Actividades de las agencias de viajes, operadores turísticos,servicios de reserva y actividades relacionadas"
CUN$DIV_n[CUN$DIV == "80"] <- "Actividades de seguridad e investigación privada"
CUN$DIV_n[CUN$DIV == "81"] <- "Actividades de servicios a edificios y paisajismo (jardines, zonas verdes)" 
CUN$DIV_n[CUN$DIV == "82"] <- "Actividades administrativas y de apoyo de oficina y otras actividades de apoyo a las empresas"
CUN$DIV_n[CUN$DIV == "83"] <- "83"
CUN$DIV_n[CUN$DIV == "84"] <- "Administración pública y defensa; planes de seguridad social de afiliación obligatoria"
CUN$DIV_n[CUN$DIV == "85"] <- "Educación"
CUN$DIV_n[CUN$DIV == "86"] <- "Actividades de atención de la salud humana"
CUN$DIV_n[CUN$DIV == "87"] <- "Actividades de atención residencial medicalizada"
CUN$DIV_n[CUN$DIV == "88"] <- "Actividades de asistencia social sin alojamiento"
CUN$DIV_n[CUN$DIV == "90"] <- "Actividades creativas, artísticas y de entretenimiento" 
CUN$DIV_n[CUN$DIV == "91"] <- "Actividades de bibliotecas, archivos, museos y otras actividades culturales" 
CUN$DIV_n[CUN$DIV == "92"] <- "Actividades de juegos de azar y apuestas"
CUN$DIV_n[CUN$DIV == "93"] <- "Actividades deportivas y actividades recreativas y de esparcimiento"
CUN$DIV_n[CUN$DIV == "94"] <- "Actividades de asociaciones"
CUN$DIV_n[CUN$DIV == "95"] <- "Mantenimiento y reparación de computadores, efectos personales y enseres domésticos"
CUN$DIV_n[CUN$DIV == "96"] <- "Otras actividades de servicios personales"
CUN$DIV_n[CUN$DIV == "97"] <- "Actividades de los hogares individuales como empleadores de personal doméstico"
CUN$DIV_n[CUN$DIV == "98"] <- "Actividades no diferenciadas de los hogares individuales como productores de bienes y servicios para uso propio"
CUN$DIV_n[CUN$DIV == "99"] <- "Actividades de organizaciones y entidades extraterritoriales"

tabla_div <- as.data.frame(table(CUN$DIV_n))

############### SECTORES #####################################################

CUN$SEC <- vector(mode='character',length=dim(CUN)[1])


CUN$SEC[CUN$DIV == "01" | CUN$DIV == "02"| CUN$DIV == "03"] <- "Agricultura, ganadería, caza, silvicultura y pesca"

CUN$SEC[CUN$DIV == "05" | CUN$DIV == "06"| CUN$DIV == "07" | CUN$DIV == "08" | CUN$DIV == "09" ] <- "Explotación de minas y canteras"

CUN$SEC[CUN$DIV == "10"| CUN$DIV == "11" | CUN$DIV == "12" | CUN$DIV == "13" | CUN$DIV == "14" | 
          CUN$DIV == "15" |  CUN$DIV == "16" |  CUN$DIV == "17" |  CUN$DIV == "18" |  CUN$DIV == "19" |
          CUN$DIV == "20" |  CUN$DIV == "21"|  CUN$DIV == "22" |  CUN$DIV == "23" |  CUN$DIV == "24" |
          CUN$DIV == "25" | CUN$DIV == "26" |CUN$DIV == "27" | CUN$DIV == "28" | CUN$DIV == "29" |
          CUN$DIV == "30" |CUN$DIV == "31" |CUN$DIV == "32" |CUN$DIV == "33"] <- "Industrias manufactureras"

CUN$SEC[CUN$DIV == "35"] <- "Suministro de electricidad, gas, vapor, y aire acondicionado"

CUN$SEC[CUN$DIV == "36"| CUN$DIV == "37" | CUN$DIV == "38" | CUN$DIV == "39" ] <- "Distribución de agua; evacuación y tratamiento de aguas residuales, gestión de desechos y actividades de saneamiento ambiental"

CUN$SEC[CUN$DIV == "41"| CUN$DIV == "42" | CUN$DIV == "43"] <- "Construcción"

CUN$SEC[CUN$DIV == "45"| CUN$DIV == "46" | CUN$DIV == "47" ] <- "Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas"

CUN$SEC[CUN$DIV == "49"| CUN$DIV == "50" | CUN$DIV == "51" | CUN$DIV == "52" | CUN$DIV == "53"] <- "Transporte y almacenamiento"

CUN$SEC[CUN$DIV == "55" | CUN$DIV == "56"] <- "Alojamiento y servicios de comida"

CUN$SEC[CUN$DIV == "58"| CUN$DIV == "59" | CUN$DIV == "60" | CUN$DIV == "61" | CUN$DIV == "62" | CUN$DIV == "63"] <- "Información y comunicaciones"

CUN$SEC[CUN$DIV == "64" |  CUN$DIV == "65" |  CUN$DIV == "66"] <- "Actividades financieras y de seguros"

CUN$SEC[CUN$DIV == "68"] <- "Actividades inmobiliarias"

CUN$SEC[CUN$DIV == "69"| CUN$DIV == "70" | CUN$DIV == "71" | CUN$DIV == "72" | CUN$DIV == "73" | CUN$DIV == "74" |
          CUN$DIV == "75"] <- "Actividades profesionales, científicas y técnicas"

CUN$SEC[CUN$DIV == "77"| CUN$DIV == "78" | CUN$DIV == "79" | CUN$DIV == "80" | CUN$DIV == "81" | CUN$DIV == "82"] <- "Actividades de servicios administrativos y de poyo"

CUN$SEC[CUN$DIV == "84"] <- "Administración pública y defensa; planes de seguridad social de afiliación obligatoria"

CUN$SEC[CUN$DIV == "85"] <- "Educación"

CUN$SEC[CUN$DIV == "86"| CUN$DIV == "87" | CUN$DIV == "88" ] <- "Actividades de atención de la salud humana y de asistencia social"

CUN$SEC[CUN$DIV == "90"| CUN$DIV == "91" | CUN$DIV == "92" | CUN$DIV == "93" ] <- "Actividades artísticas, de entretenimiento y recreación"

CUN$SEC[CUN$DIV == "94"| CUN$DIV == "95" | CUN$DIV == "96" ] <- "Otras actividades de servicios"

CUN$SEC[CUN$DIV == "97"| CUN$DIV == "98"] <- "Actividades de los hogares en calidad de empleadores; actividades no diferenciadas de los hogares individuales como productores de bienes y servicios para uso propio"

CUN$SEC[CUN$DIV == "99"] <- "Actividades de organizaciones y entidades extraterritoriales"

tabla_sec <- as.data.frame(table(CUN$SEC))

lista <- split(CUN, CUN$City)

EMP_CAJ <- as.data.frame(lista$Cajicá)
EMP_CHI <- as.data.frame(lista$Chía)
EMP_ROS <- as.data.frame(lista$"El Rosal")
EMP_FAC <- as.data.frame(lista$Facatativá)
EMP_FUN <- as.data.frame(lista$Funza)
EMP_GAC <- as.data.frame(lista$Gachancipá)
EMP_GUA <- as.data.frame(lista$Guasca)
EMP_CAL <- as.data.frame(lista$"La Calera")
EMP_MAD <- as.data.frame(lista$Madrid)
EMP_MOS <- as.data.frame(lista$Mosquera)
EMP_SIB <- as.data.frame(lista$Sibaté)
EMP_SOA <- as.data.frame(lista$Soacha)
EMP_SOP <- as.data.frame(lista$Sopó)
EMP_TOC <- as.data.frame(lista$Tocancipá)
EMP_ZIP <- as.data.frame(lista$Zipaquirá)

tablita_CAJ <- EMP_CAJ %>% group_by(DIV_n) %>% summarize(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_CHI <- EMP_CHI %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_ROS <- EMP_ROS %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_FAC <- EMP_FAC %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_FUN <- EMP_FUN %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_GAC <- EMP_GAC %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_GUA <- EMP_GUA %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_CAL <- EMP_CAL %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_MAD <- EMP_MAD %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_MOS <- EMP_MOS %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_SIB <- EMP_SIB %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_SOA <- EMP_SOA %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_SOP <- EMP_SOP %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_TOC <- EMP_TOC %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_ZIP <- EMP_ZIP %>% group_by(DIV_n) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

library(writexl)
write_xlsx(tablita_CAJ,paste0("Cajica.xlsx"))
write_xlsx(tablita_CHI,paste0("Chia.xlsx"))
write_xlsx(tablita_CAL,paste0("Calera.xlsx"))
write_xlsx(tablita_ROS,paste0("Rosales.xlsx"))
write_xlsx(tablita_FAC,paste0("Facatativa.xlsx"))
write_xlsx(tablita_FUN,paste0("Funza.xlsx"))
write_xlsx(tablita_GAC,paste0("Gachancipa.xlsx"))
write_xlsx(tablita_GUA,paste0("Guasca.xlsx"))
write_xlsx(tablita_MAD,paste0("Madrid.xlsx"))
write_xlsx(tablita_MOS,paste0("Mosquera.xlsx"))
write_xlsx(tablita_SIB,paste0("Sibate.xlsx"))
write_xlsx(tablita_SOA,paste0("Soacha.xlsx"))
write_xlsx(tablita_SOP,paste0("Sopo.xlsx"))
write_xlsx(tablita_TOC,paste0("Tocancipa.xlsx"))
write_xlsx(tablita_ZIP,paste0("Zipaquira.xlsx"))

tablita_CAJ <- EMP_CAJ %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarize(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_CHI <- EMP_CHI %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_ROS <- EMP_ROS %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_FAC <- EMP_FAC %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_FUN <- EMP_FUN %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_GAC <- EMP_GAC %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_GUA <- EMP_GUA %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_CAL <- EMP_CAL %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_MAD <- EMP_MAD %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_MOS <- EMP_MOS %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_SIB <- EMP_SIB %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_SOA <- EMP_SOA %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_SOP <- EMP_SOP %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_TOC <- EMP_TOC %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

tablita_ZIP <- EMP_ZIP %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarise(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)







###############################################################################
                        #Bogotá 
###############################################################################

BOG <- Empresas %>% filter(Empresas$City=="Bogotá, D.C.")


############### DIVISIONES #####################################################
BOG$USER_CIIU_ID_CIIU_4_c <- sprintf("%04d", as.numeric(BOG$USER_CIIU_ID_CIIU_4))
BOG$DIV <- substr(BOG$USER_CIIU_ID_CIIU_4_c,1,2)
BOG$DIV_n <- vector(mode="character",length=dim(BOG)[1])


BOG$DIV_n[BOG$DIV == "00"] <- "00"
BOG$DIV_n[BOG$DIV == "01"] <- "Agricultura, ganadería, caza y actividades de servicios conexas" 
BOG$DIV_n[BOG$DIV == "02"] <- "Silvicultura y extracción de madera"
BOG$DIV_n[BOG$DIV == "03"] <- "Pesca y acuicultura"
BOG$DIV_n[BOG$DIV == "05"] <- "Extracción de carbón de piedra y lignito"
BOG$DIV_n[BOG$DIV == "06"] <- "Extracción de petróleo crudo y gas natural"
BOG$DIV_n[BOG$DIV == "07"] <- "Extracción de minerales metalíferos"
BOG$DIV_n[BOG$DIV == "08"] <- "Extracción de otras minas y canteras"
BOG$DIV_n[BOG$DIV == "09"] <- "Actividades de servicios de apoyo para la explotación de minas y canteras"
BOG$DIV_n[BOG$DIV == "10"] <- "Elaboración de productos alimenticios"
BOG$DIV_n[BOG$DIV == "11"] <- "Elaboración de bebidas" 
BOG$DIV_n[BOG$DIV == "12"] <- "Elaboración de productos de tabaco"
BOG$DIV_n[BOG$DIV == "13"] <- "Fabricación de productos textiles"
BOG$DIV_n[BOG$DIV == "14"] <- "Confección de prendas de vestir"
BOG$DIV_n[BOG$DIV == "15"] <- "Curtido y recurtido de cueros; fabricación de calzado;fabricación de artículos de viaje, maletas, bolsos de mano y artículos similares, y fabricación de artículos de talabartería y guarnicionería; adobo y teñido de pieles"
BOG$DIV_n[BOG$DIV == "16"] <- "Transformación de la madera y fabricación de productos de madera y de corcho, excepto muebles; fabricación de artículos de cestería y espartería"
BOG$DIV_n[BOG$DIV == "17"] <- "Fabricación de papel, cartón y productos de papel y cartón"
BOG$DIV_n[BOG$DIV == "18"] <- "Actividades de impresión y de producción de copias a partir de grabaciones originales"
BOG$DIV_n[BOG$DIV == "19"] <- "Coquización, fabricación de productos de la refinación del petróleo y actividad de mezcla de combustibles"
BOG$DIV_n[BOG$DIV == "20"] <- "Fabricación de sustancias y productos químicos"
BOG$DIV_n[BOG$DIV == "21"] <- "Fabricación de productos farmacéuticos, sustancias químicas medicinales y productos botánicos de uso farmacéutico" 
BOG$DIV_n[BOG$DIV == "22"] <- "Fabricación de productos de caucho y de plástico"
BOG$DIV_n[BOG$DIV == "23"] <- "Fabricación de otros productos minerales no metálicos"
BOG$DIV_n[BOG$DIV == "24"] <- "Fabricación de productos metalúrgicos básicos"
BOG$DIV_n[BOG$DIV == "25"] <- "Fabricación de productos elaborados de metal, excepto maquinaria y equipo"
BOG$DIV_n[BOG$DIV == "26"] <- "Fabricación de productos informáticos, electrónicos y ópticos"
BOG$DIV_n[BOG$DIV == "27"] <- "Fabricación de aparatos y equipo eléctrico"
BOG$DIV_n[BOG$DIV == "28"] <- "Fabricación de maquinaria y equipo n.c.p."
BOG$DIV_n[BOG$DIV == "29"] <- "Fabricación de vehículos automotores, remolques y semirremolques"
BOG$DIV_n[BOG$DIV == "30"] <- "Fabricación de otros tipos de equipo de transporte"
BOG$DIV_n[BOG$DIV == "31"] <- "Fabricación de muebles, colchones y somieres" 
BOG$DIV_n[BOG$DIV == "32"] <- "Otras industrias manufactureras"
BOG$DIV_n[BOG$DIV == "33"] <- "Instalación, mantenimiento y reparación especializado de maquinaria y equipo"
BOG$DIV_n[BOG$DIV == "35"] <- "Suministro de electricidad, gas, vapor y aire acondicionado"
BOG$DIV_n[BOG$DIV == "36"] <- "Captación, tratamiento y distribución de agua"
BOG$DIV_n[BOG$DIV == "37"] <- "Evacuación y tratamiento de aguas residuales"
BOG$DIV_n[BOG$DIV == "38"] <- "Recolección, tratamiento y disposición de desechos,recuperación de materiales"
BOG$DIV_n[BOG$DIV == "39"] <- "Actividades de saneamiento ambiental y otros servicios de gestión de desechos"
BOG$DIV_n[BOG$DIV == "41"] <- "Construcción de edificios" 
BOG$DIV_n[BOG$DIV == "42"] <- "Obras de ingeniería civil"
BOG$DIV_n[BOG$DIV == "43"] <- "Actividades especializadas para la construcción de edificios y obras de ingeniería civil"
BOG$DIV_n[BOG$DIV == "44"] <- "44"
BOG$DIV_n[BOG$DIV == "45"] <- "Comercio, mantenimiento y reparación de vehículos automotores y motocicletas, sus partes, piezas y accesorios"
BOG$DIV_n[BOG$DIV == "46"] <- "Comercio al por mayor y en comisión o por contrata, excepto el comercio de vehículos automotores y motocicletas"
BOG$DIV_n[BOG$DIV == "47"] <- "Comercio al por menor (incluso el comercio al por menor de combustibles), excepto el de vehículos automotores y motocicletas"
BOG$DIV_n[BOG$DIV == "49"] <- "Transporte terrestre; transporte por tuberías"
BOG$DIV_n[BOG$DIV == "50"] <- "Transporte acuático"
BOG$DIV_n[BOG$DIV == "51"] <- "Transporte aéreo" 
BOG$DIV_n[BOG$DIV == "52"] <- "Almacenamiento y actividades complementarias al transport"
BOG$DIV_n[BOG$DIV == "53"] <- "Correo y servicios de mensajería"
BOG$DIV_n[BOG$DIV == "55"] <- "Alojamiento"
BOG$DIV_n[BOG$DIV == "56"] <- "Actividades de servicios de comidas y bebidas"
BOG$DIV_n[BOG$DIV == "58"] <- "Actividades de edición"
BOG$DIV_n[BOG$DIV == "59"] <- "Actividades cinematográficas, de video y producción de programas de televisión, grabación de sonido y edición de música"
BOG$DIV_n[BOG$DIV == "60"] <- "Actividades de programación, transmisión y/o difusión"
BOG$DIV_n[BOG$DIV == "61"] <- "Telecomunicaciones" 
BOG$DIV_n[BOG$DIV == "62"] <- "Desarrollo de sistemas informáticos (planificación, análisis, diseño, programación, pruebas), consultoría informática y actividades relacionadas"
BOG$DIV_n[BOG$DIV == "63"] <- "Actividades de servicios de información"
BOG$DIV_n[BOG$DIV == "64"] <- "Actividades de servicios financieros, excepto las de seguros y de pensiones"
BOG$DIV_n[BOG$DIV == "65"] <- "Seguros (incluso el reaseguro), seguros sociales y fondos de pensiones, excepto la seguridad social"
BOG$DIV_n[BOG$DIV == "66"] <- "Actividades auxiliares de las actividades de servicios financieros"
BOG$DIV_n[BOG$DIV == "68"] <- "Actividades inmobiliarias"
BOG$DIV_n[BOG$DIV == "69"] <- "Actividades jurídicas y de contabilidad"
BOG$DIV_n[BOG$DIV == "70"] <- "Actividades de administración empresarial; actividades de consultoría de gestión"
BOG$DIV_n[BOG$DIV == "71"] <- "Actividades de arquitectura e ingeniería; ensayos y análisis técnicos" 
BOG$DIV_n[BOG$DIV == "72"] <- "Investigación científica y desarrollo"
BOG$DIV_n[BOG$DIV == "73"] <- "Publicidad y estudios de mercado"
BOG$DIV_n[BOG$DIV == "74"] <- "Otras actividades profesionales, científicas y técnicas"
BOG$DIV_n[BOG$DIV == "75"] <- "Actividades veterinarias"
BOG$DIV_n[BOG$DIV == "77"] <- "Actividades de alquiler y arrendamiento"
BOG$DIV_n[BOG$DIV == "78"] <- "Actividades de empleo"
BOG$DIV_n[BOG$DIV == "79"] <- "Actividades de las agencias de viajes, operadores turísticos,servicios de reserva y actividades relacionadas"
BOG$DIV_n[BOG$DIV == "80"] <- "Actividades de seguridad e investigación privada"
BOG$DIV_n[BOG$DIV == "81"] <- "Actividades de servicios a edificios y paisajismo (jardines, zonas verdes)" 
BOG$DIV_n[BOG$DIV == "82"] <- "Actividades administrativas y de apoyo de oficina y otras actividades de apoyo a las empresas"
BOG$DIV_n[BOG$DIV == "83"] <- "83"
BOG$DIV_n[BOG$DIV == "84"] <- "Administración pública y defensa; planes de seguridad social de afiliación obligatoria"
BOG$DIV_n[BOG$DIV == "85"] <- "Educación"
BOG$DIV_n[BOG$DIV == "86"] <- "Actividades de atención de la salud humana"
BOG$DIV_n[BOG$DIV == "87"] <- "Actividades de atención residencial medicalizada"
BOG$DIV_n[BOG$DIV == "88"] <- "Actividades de asistencia social sin alojamiento"
BOG$DIV_n[BOG$DIV == "90"] <- "Actividades creativas, artísticas y de entretenimiento" 
BOG$DIV_n[BOG$DIV == "91"] <- "Actividades de bibliotecas, archivos, museos y otras actividades culturales" 
BOG$DIV_n[BOG$DIV == "92"] <- "Actividades de juegos de azar y apuestas"
BOG$DIV_n[BOG$DIV == "93"] <- "Actividades deportivas y actividades recreativas y de esparcimiento"
BOG$DIV_n[BOG$DIV == "94"] <- "Actividades de asociaciones"
BOG$DIV_n[BOG$DIV == "95"] <- "Mantenimiento y reparación de computadores, efectos personales y enseres domésticos"
BOG$DIV_n[BOG$DIV == "96"] <- "Otras actividades de servicios personales"
BOG$DIV_n[BOG$DIV == "97"] <- "Actividades de los hogares individuales como empleadores de personal doméstico"
BOG$DIV_n[BOG$DIV == "98"] <- "Actividades no diferenciadas de los hogares individuales como productores de bienes y servicios para uso propio"
BOG$DIV_n[BOG$DIV == "99"] <- "Actividades de organizaciones y entidades extraterritoriales"

tabla_div_bog <- as.data.frame(table(BOG$DIV_n))

tablita_BOG <- BOG %>% group_by(DIV_n) %>% summarize(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)
write_xlsx(tablita_BOG,paste0("Bogota.xlsx"))

tablita_BOG <- BOG %>% group_by(USER_CIIU_ID_CIIU_4) %>% summarize(no = n())  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)


############### SECTORES #####################################################

BOG$SEC <- vector(mode='character',length=dim(BOG)[1])


BOG$SEC[BOG$DIV == "01" | BOG$DIV == "02"| BOG$DIV == "03"] <- "Agricultura, ganadería, caza, silvicultura y pesca"

BOG$SEC[BOG$DIV == "05" | BOG$DIV == "06"| BOG$DIV == "07" | BOG$DIV == "08" | BOG$DIV == "09" ] <- "Explotación de minas y canteras"

BOG$SEC[BOG$DIV == "10"| BOG$DIV == "11" | BOG$DIV == "12" | BOG$DIV == "13" | BOG$DIV == "14" | 
          BOG$DIV == "15" |  BOG$DIV == "16" |  BOG$DIV == "17" |  BOG$DIV == "18" |  BOG$DIV == "19" |
          BOG$DIV == "20" |  BOG$DIV == "21"|  BOG$DIV == "22" |  BOG$DIV == "23" |  BOG$DIV == "24" |
          BOG$DIV == "25" | BOG$DIV == "26" |BOG$DIV == "27" | BOG$DIV == "28" | BOG$DIV == "29" |
          BOG$DIV == "30" |BOG$DIV == "31" |BOG$DIV == "32" |BOG$DIV == "33"] <- "Industrias manufactureras"

BOG$SEC[BOG$DIV == "35"] <- "Suministro de electricidad, gas, vapor, y aire acondicionado"

BOG$SEC[BOG$DIV == "36"| BOG$DIV == "37" | BOG$DIV == "38" | BOG$DIV == "39" ] <- "Distribución de agua; evacuación y tratamiento de aguas residuales, gestión de desechos y actividades de saneamiento ambiental"

BOG$SEC[BOG$DIV == "41"| BOG$DIV == "42" | BOG$DIV == "43"] <- "Construcción"

BOG$SEC[BOG$DIV == "45"| BOG$DIV == "46" | BOG$DIV == "47" ] <- "Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas"

BOG$SEC[BOG$DIV == "49"| BOG$DIV == "50" | BOG$DIV == "51" | BOG$DIV == "52" | BOG$DIV == "53"] <- "Transporte y almacenamiento"

BOG$SEC[BOG$DIV == "55" | BOG$DIV == "56"] <- "Alojamiento y servicios de comida"

BOG$SEC[BOG$DIV == "58"| BOG$DIV == "59" | BOG$DIV == "60" | BOG$DIV == "61" | BOG$DIV == "62" | BOG$DIV == "63"] <- "Información y comunicaciones"

BOG$SEC[BOG$DIV == "64" |  BOG$DIV == "65" |  BOG$DIV == "66"] <- "Actividades financieras y de seguros"

BOG$SEC[BOG$DIV == "68"] <- "Actividades inmobiliarias"

BOG$SEC[BOG$DIV == "69"| BOG$DIV == "70" | BOG$DIV == "71" | BOG$DIV == "72" | BOG$DIV == "73" | BOG$DIV == "74" |
          BOG$DIV == "75"] <- "Actividades profesionales, científicas y técnicas"

BOG$SEC[BOG$DIV == "77"| BOG$DIV == "78" | BOG$DIV == "79" | BOG$DIV == "80" | BOG$DIV == "81" | BOG$DIV == "82"] <- "Actividades de servicios administrativos y de poyo"

BOG$SEC[BOG$DIV == "84"] <- "Administración pública y defensa; planes de seguridad social de afiliación obligatoria"

BOG$SEC[BOG$DIV == "85"] <- "Educación"

BOG$SEC[BOG$DIV == "86"| BOG$DIV == "87" | BOG$DIV == "88" ] <- "Actividades de atención de la salud humana y de asistencia social"

BOG$SEC[BOG$DIV == "90"| BOG$DIV == "91" | BOG$DIV == "92" | BOG$DIV == "93" ] <- "Actividades artísticas, de entretenimiento y recreación"

BOG$SEC[BOG$DIV == "94"| BOG$DIV == "95" | BOG$DIV == "96" ] <- "Otras actividades de servicios"

BOG$SEC[BOG$DIV == "97"| BOG$DIV == "98"] <- "Actividades de los hogares en calidad de empleadores; actividades no diferenciadas de los hogares individuales como productores de bienes y servicios para uso propio"

BOG$SEC[BOG$DIV == "99"] <- "Actividades de organizaciones y entidades extraterritoriales"

tabla_sec_bog <- as.data.frame(table(BOG$SEC))



