# Función para crear objetos date en base a un string o número
create_date <- function(string) {
  
  get_date <- function(element) {
    element <- stringr::str_remove_all(element, "[//.* ]")
    
    if(stringr::str_detect(element, '[A-z]+', negate = TRUE)) {
      
      return(
        janitor::excel_numeric_to_date(as.numeric(element))
      )
      
    } else if(stringr::str_detect(element, "^[A-z]+")) {
      
      date <- lubridate::make_date(
        stringr::str_replace(element, ".+([0-9]{2})$", "20\\1"),
        crear_mes(stringr::str_extract(element, "[A-z]+")),
        1
      )
      
      return(date)
      
    } else if(stringr::str_detect(element, "^[0-9]+-[A-z]+")) {
      date <- lubridate::make_date(
        stringr::str_replace(element, ".+([0-9]{2})$", "20\\1"),
        crear_mes(stringr::str_extract(element, "[A-z]+")),
        stringr::str_extract(element, "^[0-9]+")
      )
      
      return(date)
    } else {
      stop('Tipo de fecha de reconocido')
    }
  }
  
  
  date <- purrr::map_dbl(string, get_date)
  
  class(date) <- 'Date'
  
  date
}


# Función para crear meses en base a un string o número
crear_mes <- function(mes, type = "text_to_number") {
  # creating the pipe
  `%>%` <- magrittr::`%>%`
  
  if(type == "number_to_text") {
    
    new_mes <- dplyr::recode(mes,
                             `1` = "Enero",
                             `2` = "Febrero",
                             `3` = "Marzo",
                             `4` = "Abril",
                             `5` = "Mayo",
                             `6` = "Junio",
                             `7` = "Julio",
                             `8` = "Agosto",
                             `9` = "Septiembre",
                             `10` = "Octubre",
                             `11` = "Noviembre",
                             `12` = "Diciembre")
    
  }
  
  if(type == "number_to_shorttext"){
    
    new_mes <- dplyr::recode(mes,
                             `1` = "Ene",
                             `2` = "Feb",
                             `3` = "Mar",
                             `4` = "Abr",
                             `5` = "May",
                             `6` = "Jun",
                             `7` = "Jul",
                             `8` = "Ago",
                             `9` = "Sep",
                             `10` = "Oct",
                             `11` = "Nov",
                             `12` = "Dic")
    
  }
  
  if(type == "text_to_number"){
    
    mes  <-  stringr::str_to_title(mes)
    
    new_mes <- dplyr::recode(mes,
                             "Jan" = 01,
                             "Ene" = 01,
                             "Feb" = 02,
                             "Mar" = 03,
                             "Abr" = 04,
                             "May" = 05,
                             "Jun" = 06,
                             "Jul" = 07,
                             "Ago" = 08,
                             "Sep" = 09,
                             "Sept" = 09,
                             "Oct" = 10,
                             "Nov" = 11,
                             "Dic" = 12,
                             
                             "Enero" = 01,
                             "Febrero" = 02,
                             "Marzo" = 03,
                             "Abril" = 04,
                             "Mayo" = 05,
                             "Junio" = 06,
                             "Julio" = 07,
                             "Agosto" = 08,
                             "Septiembre" = 09,
                             "Octubre" = 10,
                             "Noviembre" = 11,
                             "Diciembre" = 12,
                             
                             "January" = 01,
                             "February" = 02,
                             "March" = 03,
                             "April" = 04,
                             "May" = 05,
                             "June" = 06,
                             "July" = 07,
                             "August" = 08,
                             "September" = 09,
                             "October" = 10,
                             "November" = 11,
                             "December" = 12)
  }
  
  return(new_mes)
}

date_label <- function(date = Sys.Date(), year_in_new_line = FALSE) {
  if(year_in_new_line) {
    return(
      paste(stringr::str_to_title(lubridate::month(date, label = TRUE)), lubridate::year(date), sep = "\n")
    )
  }
  paste(stringr::str_to_title(lubridate::month(date, label = TRUE)), lubridate::year(date))
}

# Función para descargar las estadísticas monetarias
get_monetary_stats <- function() {
  `%>%` <- magrittr::`%>%`
  
  # URL de descarga
  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "sector-monetario-y-financiero/documents/serie_indicadores_bcrd.xlsx",
    "?v=1637785656143"
  )
  
  # path temporal
  temp_path <- tempfile(pattern = "", fileext = ".xlsx")
  
  # descargando el archivo
  download.file(url, temp_path, quiet = TRUE, mode = "wb")
  
  # Importando la data
  suppressMessages(
    indicadores_bcrd <- readxl::read_excel(
      path = temp_path,
      skip = 4,
      col_names = TRUE, n_max = 53, na = c("n.d.")
    )
  )
  
  # detalles de las variables 
  {
    elements <- tibble::tribble(
      ~original_names,                                                  ~labels,                         ~short_names,                   ~categoria, ~nivel,                       ~direct_parent,
      "ACTIVOS INTERNACIONALES BRUTOS (Activos Externos) (US$) (1)",                                       "Activos externos",     "activos_internacionales_brutos",           "Indicadores BCRD",     1L,                                   NA,
      "RESERVAS INTERNACIONALES BRUTAS (Activos de Reserva Oficial) (US$) (1)",                        "Reservas internacionales brutas",    "reservas_internacionales_brutas",           "Indicadores BCRD",     1L,                                   NA,
      "RESERVAS INTERNACIONALES NETAS   (US$) (1)",                         "Reservas internacionales netas",     "reservas_internacionales_netas",           "Indicadores BCRD",     1L,                                   NA,
      "ACTIVOS INTERNOS",                                       "Activos internos",                   "activos_internos",           "Indicadores BCRD",     1L,                                   NA,
      "Activos frente al Gobierno Central (2)",                     "Activos frente al gobierno central",            "activos_internos_vs_gob",           "Indicadores BCRD",     2L,                   "Activos internos",
      "De los cuales: Bono de Capitalización (3)",             "Bonos de capitalización frente al gobierno", "activos_internos_capitalizacion_bc",           "Indicadores BCRD",     3L, "Activos frente al gobierno central",
      "Activos frente al Sector Privado (4)",                       "Activos frente al sector privado",       "activos_internos_vs_sprivado",           "Indicadores BCRD",     2L,                   "Activos internos",
      "Crédito a Otras Sociedades de Depósito (5)",                                          "Crédito a OSD",                        "credito_osd",           "Indicadores BCRD",     2L,                   "Activos internos",
      "VALORES EN CIRCULACION (6)",                                 "Valores en circulación",             "valores_en_circulacion",           "Indicadores BCRD",     1L,                                   NA,
      "Descuento en Valores Emitidos",                          "Descuento en valores emitidos",               "descuento_en_valores",           "Indicadores BCRD",     2L,             "Valores en circulación",
      "DEPOSITOS REMUNERADOS DE CORTO PLAZO",                               "Depósitos remunerados CP",        "depositos_remunerados_corto",           "Indicadores BCRD",     1L,                                   NA,
      "BASE MONETARIA RESTRINGIDA",                             "Base monetaria restringida",         "base_monetaria_restringida",             "Base monetaria",     1L,                                   NA,
      "Billetes y Monedas Emitidos",                            "Billetes y monedas emitidos",                   "billetes_monedas",             "Base monetaria",     2L,         "Base monetaria restringida",
      "De los que: Tenencias OSD en MN",                              "Billetes y monedas en OSD",            "billetes_monedas_osd_mn",             "Base monetaria",     3L,        "Billetes y monedas emitidos",
      "Depósitos Encaje Legal y Saldos de Compensación de OSD en BC (MN)",                           "Depositos encaje legal en MN",   "depositos_encaje_compensacion_mn",             "Base monetaria",     2L,          "Billetes y monedas en OSD",
      "Valores del BCRD en posesión de las OSD para fines de encaje legal (MN)",                    "Valores del BCRD en OSD para encaje",      "valores_bcrd_en_osd_encaje_mn",             "Base monetaria",     2L,          "Billetes y monedas en OSD",
      "BASE MONETARIA AMPLIADA",                                "Base monetaria ampliada",            "base_monetaria_ampliada",             "Base monetaria",     1L,                                   NA,
      "Base Monetaria Restringida",                             "Base monetaria restringida",                   "base_restringida",             "Base monetaria",     2L,            "Base monetaria ampliada",
      "Depósitos Encaje Legal y Saldos de Compensación de OSD en BC (ME)",                           "Depósitos encaje legal en ME",   "depositos_encaje_compensacion_me",             "Base monetaria",     2L,            "Base monetaria ampliada",
      "Depósitos Remunerados de Corto Plazo (Overnight)",                                    "Depósitos Overnight",    "depositos_remunerados_overnight",             "Base monetaria",     2L,            "Base monetaria ampliada",
      "Depósitos Remunerados de Corto Plazo en ME",                         "Depósitos remunerados CP en ME",           "depositos_remunerados_me",             "Base monetaria",     2L,            "Base monetaria ampliada",
      "Otros Depósitos de OSD en BCRD",                         "Otros depósitos de OSD en BCRD",          "otros_depositos_osd_en_bc",             "Base monetaria",     2L,            "Base monetaria ampliada",
      "Valores a Corto Plazo emitidos por BC en manos de las OSD (MN y ME)",                          "Valores de CP del BCRD en OSD",     "valores_corto_plazo_bc_en_osde",             "Base monetaria",     2L,            "Base monetaria ampliada",
      "MEDIO CIRCULANTE (M1)",                                       "Medio círculante",                                 "m1",       "Agregados monetarios",     1L,                                   NA,
      "Billetes y monedas en poder del público",                 "Billetes y monedas en poder de público",                "billetes_monedas_pp",       "Agregados monetarios",     2L,                   "Medio círculante",
      "Depósitos Transferibles en MN",                          "Depósitos transferibles en MN",         "depositos_transferibles_mn",       "Agregados monetarios",     2L,                   "Medio círculante",
      "OFERTA MONETARIA AMPLIADA (M2)",                              "Oferta monetaria ampliada",                                 "m2",       "Agregados monetarios",     1L,                                   NA,
      "Medio Circulante (M1)",                                                     "M1",                                 "m1",       "Agregados monetarios",     2L,          "Oferta monetaria ampliada",
      "Otros depósitos M/N",                                  "Otros depósitos en MN",                 "otros_depositos_mn",       "Agregados monetarios",     2L,          "Oferta monetaria ampliada",
      "Valores distintos de acciones MN-emitidos por OSD",      "Valores distintos de acciones MN-emitidos por OSD",      "valores_no_acciones_de_osd_mn",       "Agregados monetarios",     2L,          "Oferta monetaria ampliada",
      "Valores distintos de acciones MN- emitidos por el BCRD", "Valores distintos de acciones MN- emitidos por el BCRD",     "valores_no_acciones_de_bcrd_mn",       "Agregados monetarios",     2L,          "Oferta monetaria ampliada",
      "DINERO EN SENTIDO AMPLIO (M3)",                               "Dinero en sentido amplio",                                 "m3",       "Agregados monetarios",     1L,                                   NA,
      "Oferta Monetaria Ampliada (M2)",                              "Oferta monetaria ampliada",                                 "m2",       "Agregados monetarios",     2L,           "Dinero en sentido amplio",
      "Otros depósitos M/E",                                  "Otros depósitos en ME",                 "otros_depositos_me",       "Agregados monetarios",     2L,           "Dinero en sentido amplio",
      "Valores distintos de acciones ME",                    "Valores distintos de acciones en ME",             "valores_no_acciones_me",       "Agregados monetarios",     2L,           "Dinero en sentido amplio",
      "K2.1 = M1/BM restringida",                                                     "K1",                               "k2.1", "Multiplicadores monetarios",     1L,                                   NA,
      "K2.2 = M2/BM restringida",                                                     "K2",                               "k2.2", "Multiplicadores monetarios",     1L,                                   NA,
      "K2.3 = M3/BM ampliada",                                                     "K3",                               "k2.3", "Multiplicadores monetarios",     1L,                                   NA,
      "TASA DE CAMBIO",                                         "Tasa de cambio",                        "tasa_cambio",             "Tipo de cambio",     1L,                                   NA
    )
  }
  
  # Limpiando los datos (quitando filas sin datos y títulos)
  indicadores_bcrd <- indicadores_bcrd %>%
    dplyr::filter(!is.na(`INDICADORES BANCO CENTRAL`)) %>%
    dplyr::rowwise() %>%
    #Commets
    dplyr::mutate(remove = any(!is.na(dplyr::c_across(-`INDICADORES BANCO CENTRAL`)))) %>%
    dplyr::filter(remove) %>%
    dplyr::select(-remove) %>%
    dplyr::bind_cols(elements)
  
  indicadores_bcrd %>%
    dplyr::rename(descripcion =  `INDICADORES BANCO CENTRAL`) %>%
    tidyr::pivot_longer(
      cols = -c(descripcion, short_names, categoria, nivel, original_names, 
                labels, direct_parent),
      names_to = "date", values_to = "values") %>%
    dplyr::mutate(
      date = create_date(date)
    )
  
}

# Función para descargar las estadísticas del IPC
get_ipc_data <- function(){
  
  # Asignando el pipe para usarlo sin cargar dplyr
  `%>%` <- magrittr::`%>%`
  
  # Descarga el ipc general  ---------------------------
  url_descarga <- paste0("https://cdn.bancentral.gov.do/documents/",
                         "estadisticas/precios/documents/",
                         "ipc_base_2019-2020.xls")
  # directorio de descarga
  file_path <- tempfile(pattern = "", fileext = ".xls")
  
  # descarga el archivo
  download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)
  
  suppressMessages(
    # leer el archivo
    ipc_general <- readxl::read_excel(
      file_path,
      sheet = 1,
      col_names = FALSE,
      skip = 7)
  )
  
  # Adecuando el archivo
  ipc_general <- ipc_general %>%
    janitor::clean_names() %>%
    dplyr::select(x1:x7) %>%
    setNames(
      c("year", "mes", "ipc","ipc_vm", "ipc_vd", "ipc_vi", "ipc_p12")
    ) %>%
    dplyr::filter(!is.na(mes)) %>%
    dplyr::mutate(
      fecha = seq(lubridate::ymd("1984/01/01"),
                  by = "month",
                  length.out = nrow(.)),
      year = lubridate::year(fecha)
    ) %>%
    dplyr::select(fecha, year, mes, everything())
  
  return(ipc_general)
  
}

