###############################################################################
                          # BASE y preparación #

################################################################################
  
  
  ###################### LEER LAS BASES DE DATOS #################################
  
  
  filename <- "C:\\Users\\pablo\\OneDrive\\Escritorio\\R proyects\\Multiprop_Encuesta\\Data\\EM2021\\EM2021.rds"
  EM21 <- readRDS(filename)
  filename <- "C:\\Users\\pablo\\OneDrive\\Escritorio\\R proyects\\Multiprop_Encuesta\\Data\\EM2021\\EM2021_PLUS.rds"
  EM21_plus <- readRDS(filename)
  
  names(EM21_plus)[grep("directorio_per", names(EM21_plus))] <- "DIRECTORIO_PER"
  
  EM21F <- merge(EM21, EM21_plus, by = c("DIRECTORIO_PER"),all =TRUE)
  
  
  ###################### paquetes #################################
  
  install.packages("data.table") # Install data.table package
  library("data.table")          # Load data.table
  library(dplyr)
  library(writexl)
  
  
  ###################### municipios #################################
  
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
  EM21F$MPIO_NAME[EM21F$MPIO == 25899] <- "Zipaquira"
  EM21F$MPIO_NAME[EM21F$MPIO == 25269] <- "Facatativa"
  EM21F$MPIO_NAME[EM21F$MPIO == 25769] <- "Subachoque"
  EM21F$MPIO_NAME[EM21F$MPIO == 25377] <- "La Calera"
  EM21F$MPIO_NAME[EM21F$MPIO == 25295] <- "Gachancipá"
  
  
  
  ###################### edad #################################
  
  EM21F$Edad <- cut(EM21F$NPCEP4, breaks = c(0,13,28,39,49, 59, 130))
  
  ###################### sectores #################################
  
  EM21F$NPCKP16_COD4_c <- sprintf("%04d", as.numeric(EM21F$NPCKP16_COD4))
  
  EM21F$DIV <- substr(EM21F$NPCKP16_COD4_c,1,2)
  
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
  
  ###################### estudio #################################
  
  EM21F$NPCHP13[EM21F$NPCHP13 == "1"] <- "En su municipio"
  EM21F$NPCHP13[EM21F$NPCHP13 == "2"] <- "En  otro municipio"
  EM21F$NPCHP13[is.na(EM21F$NPCHP13)] <- "No se encuentra estudiando"
  
  ###################### trabajo #################################
  
  EM21F$NPCKPA46[EM21F$NPCKPA46 == "1"] <- "En su municipio"
  EM21F$NPCKPA46[EM21F$NPCKPA46 == "2"] <- "En otro municipio"
  EM21F$NPCKPA46[is.na(EM21F$NPCKPA46)] <- "No se encuentra trabajando"
  
  
  rm(EM21)
  rm(EM21_plus)
  
  
