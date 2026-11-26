# ============================================================================
# OMR VISION SUITE — MOTOR R DESDE CERO
# Version: 1.0.1 (integración GitHub Actions)
#
# Contrato de entrada:
#   respuestas_omr_<evaluacion>_sesion_<N>.csv
#   mapa_preguntas_sesion_<N>.csv
#
# Modelos seleccionados en OMR Vision Suite:
#   regular   = porcentaje directo de aciertos
#   normative = Rasch (1PL) + estandarización estilo UNAL
#   tri       = TRI 2PL o 3PL, según configuración explícita del mapa
#
# Reglas:
# - El motor NUNCA selecciona ni sustituye el modelo por tamaño de muestra.
# - Respuesta en blanco = desacierto (0).
# - Pregunta anulada = excluida (NA), nunca acierto ni desacierto.
# - Las sesiones se procesan por separado y conservan sus códigos de ítem.
# - Puntajes e indicadores finales se exportan como enteros.
# - Parámetros psicométricos TRI pueden conservar decimales.
# ============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(mirt)
})

VERSION_MOTOR <- "1.0.1"
CONFIG <- list(
  # Parámetros normativos iniciales basados en el esquema publicado por UNAL.
  media_area = 10,
  sd_area = 1,
  media_global = 500,
  sd_global = 100,
  # Parámetros de escala de la plantilla TRI compartida por el usuario.
  tri_media = 50,
  tri_sd = 18,
  tri_min = 0,
  tri_max = 100,
  # Control de estimación. No es un umbral para cambiar de modelo.
  max_cycles = 3000,
  semilla = 2026
)

redondear_entero <- function(x) as.integer(floor(x + 0.5))

normalizar_nombre <- function(x) {
  tolower(trimws(as.character(x)))
}

leer_csv_seguro <- function(path) {
  if (!file.exists(path)) stop("No existe el archivo: ", path, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal",
                  na = c("", "NA", "N/A"), trim_ws = TRUE,
                  locale = locale(encoding = "UTF-8"))
}

validar_columnas <- function(df, requeridas, archivo) {
  faltan <- setdiff(requeridas, names(df))
  if (length(faltan)) {
    stop("Faltan columnas en ", basename(archivo), ": ",
         paste(faltan, collapse = ", "), call. = FALSE)
  }
}

validar_respuestas <- function(df, archivo) {
  validar_columnas(df, "documento", archivo)
  ids <- trimws(as.character(df$documento))
  if (any(is.na(ids) | ids == "")) stop("Hay documentos vacíos en ", basename(archivo), call. = FALSE)
  if (anyDuplicated(ids)) stop("Hay documentos duplicados en ", basename(archivo), call. = FALSE)
  df$documento <- ids
  cols <- grep("^p[0-9]+$", normalizar_nombre(names(df)), value = TRUE)
  # El CSV de la aplicación suele exportar P1, P2...; readr conserva mayúsculas.
  original <- names(df)[match(cols, normalizar_nombre(names(df)))]
  if (!length(original)) stop("No se encontraron columnas P1, P2... en ", basename(archivo), call. = FALSE)
  posiciones <- as.integer(sub("^p", "", cols))
  if (!identical(sort(posiciones), seq_len(max(posiciones)))) {
    stop("Las columnas de respuestas deben ser consecutivas desde P1. Archivo: ", basename(archivo), call. = FALSE)
  }
  names(df)[match(original, names(df))] <- paste0("P", posiciones)
  original <- paste0("P", sort(posiciones))
  for (nm in original) {
    raw <- df[[nm]]
    val <- suppressWarnings(as.numeric(as.character(raw)))
    # NA representa blanco en la fuente; se convierte a desacierto, salvo anuladas,
    # que se enmascaran posteriormente a partir del mapa.
    invalid <- !is.na(raw) & (is.na(val) | !(val %in% c(0, 1)))
    if (any(invalid)) stop("Valores distintos de 0/1 en ", basename(archivo), "$", nm, call. = FALSE)
    val[is.na(val)] <- 0
    df[[nm]] <- as.integer(val)
  }
  df
}

validar_mapa <- function(map, archivo) {
  requeridas <- c("evaluacion_id", "evaluacion", "modelo_calificacion", "sesion",
                  "posicion_respuesta", "codigo_pregunta", "area", "orden_area",
                  "pregunta_en_area", "anulada")
  validar_columnas(map, requeridas, archivo)
  map <- map %>% mutate(
    evaluacion_id = as.character(evaluacion_id),
    modelo_calificacion = normalizar_nombre(modelo_calificacion),
    sesion = as.integer(sesion),
    posicion_respuesta = as.integer(posicion_respuesta),
    codigo_pregunta = trimws(as.character(codigo_pregunta)),
    area = trimws(as.character(area)),
    orden_area = as.integer(orden_area),
    pregunta_en_area = as.integer(pregunta_en_area),
    anulada = tolower(as.character(anulada)) %in% c("true", "1", "sí", "si", "yes")
  )
  if (nrow(map) == 0) stop("El mapa está vacío: ", basename(archivo), call. = FALSE)
  if (any(is.na(map$evaluacion_id) | map$evaluacion_id == "")) stop("evaluacion_id vacío en el mapa.", call. = FALSE)
  if (length(unique(map$evaluacion_id)) != 1) stop("El mapa mezcla evaluaciones.", call. = FALSE)
  if (length(unique(map$modelo_calificacion)) != 1) stop("El mapa mezcla modelos de calificación.", call. = FALSE)
  if (!unique(map$modelo_calificacion) %in% c("regular", "normative", "tri")) {
    stop("Modelo no reconocido en el mapa: ", unique(map$modelo_calificacion), call. = FALSE)
  }
  if (length(unique(map$sesion)) != 1 || anyNA(map$sesion)) stop("El mapa debe contener una sola sesión válida.", call. = FALSE)
  if (anyNA(map$posicion_respuesta) || any(map$posicion_respuesta < 1)) stop("posicion_respuesta inválida.", call. = FALSE)
  if (anyDuplicated(map$posicion_respuesta)) stop("Hay posiciones de respuesta duplicadas en el mapa.", call. = FALSE)
  if (anyDuplicated(map$codigo_pregunta)) stop("Hay códigos de pregunta duplicados en el mapa.", call. = FALSE)
  if (any(is.na(map$codigo_pregunta) | map$codigo_pregunta == "" | is.na(map$area) | map$area == "")) {
    stop("Cada pregunta debe tener código y área.", call. = FALSE)
  }
  map <- map %>% arrange(posicion_respuesta)
  modelo <- unique(map$modelo_calificacion)
  if (modelo == "normative") {
    extra <- c("metodo_normativo", "referencia_normativa", "media_area", "sd_area", "media_global", "sd_global")
    validar_columnas(map, extra, archivo)
    if (length(unique(as.character(map$metodo_normativo))) != 1 || unique(as.character(map$metodo_normativo)) != "rasch_unal") stop("El modelo Normativo requiere metodo_normativo=rasch_unal, constante en todo el mapa.", call. = FALSE)
    if (length(unique(as.character(map$referencia_normativa))) != 1 || unique(as.character(map$referencia_normativa)) != "cohorte_evaluada") stop("Por ahora la referencia normativa debe ser cohorte_evaluada, constante en todo el mapa.", call. = FALSE)
  }
  if (modelo == "tri") {
    validar_columnas(map, c("modelo_tri", "tri_media", "tri_sd", "tri_min", "tri_max"), archivo)
    mt <- unique(toupper(as.character(map$modelo_tri)))
    if (length(mt) != 1 || is.na(mt) || !mt %in% c("2PL", "3PL")) stop("TRI requiere un único modelo_tri explícito: 2PL o 3PL.", call. = FALSE)
  }
  if (!identical(map$posicion_respuesta, seq_len(nrow(map)))) {
    stop("posicion_respuesta debe ser consecutiva desde 1 y coincidir con P1...Pn.", call. = FALSE)
  }
  map
}

obtener_archivos <- function(input_dir) {
  resp <- list.files(input_dir, pattern = "^respuestas_omr_.*_sesion_[0-9]+\\.csv$",
                     full.names = TRUE, ignore.case = TRUE)
  mapas <- list.files(input_dir, pattern = "^mapa_preguntas_sesion_[0-9]+\\.csv$",
                      full.names = TRUE, ignore.case = TRUE)
  if (!length(resp)) stop("No se encontraron CSV respuestas_omr_*_sesion_N.csv en la carpeta.", call. = FALSE)
  if (!length(mapas)) stop("No se encontraron mapa_preguntas_sesion_N.csv en la carpeta.", call. = FALSE)
  extraer_sesion <- function(x) as.integer(sub(".*_sesion_([0-9]+)\\.csv$", "\\1", basename(x), ignore.case = TRUE))
  list(respuestas = resp, mapas = mapas, sesion = extraer_sesion)
}

calcular_kr20 <- function(x) {
  x <- as.data.frame(x)
  if (ncol(x) < 2) return(NA_real_)
  m <- as.matrix(x)
  m[is.na(m)] <- 0
  k <- ncol(m); total <- rowSums(m)
  vt <- stats::var(total)
  if (!is.finite(vt) || vt <= 0) return(NA_real_)
  p <- colMeans(m); q <- 1 - p
  (k / (k - 1)) * (1 - sum(p * q) / vt)
}

correlacion_item_total <- function(x, total) {
  vapply(seq_len(ncol(x)), function(j) {
    item <- x[[j]]
    resto <- total - item
    ok <- is.finite(item) & is.finite(resto)
    if (sum(ok) < 3 || length(unique(item[ok])) < 2 || stats::sd(resto[ok]) == 0) return(NA_real_)
    suppressWarnings(stats::cor(item[ok], resto[ok]))
  }, numeric(1))
}

preparar_matriz <- function(respuestas, mapa, archivo) {
  respuestas <- validar_respuestas(respuestas, archivo)
  n <- nrow(mapa)
  esperadas <- paste0("P", seq_len(n))
  validar_columnas(respuestas, esperadas, archivo)
  extra <- setdiff(grep("^P[0-9]+$", names(respuestas), value = TRUE), esperadas)
  if (length(extra)) stop("El CSV contiene columnas de preguntas que no aparecen en el mapa: ",
                          paste(extra, collapse = ", "), call. = FALSE)
  if (ncol(respuestas[, esperadas, drop = FALSE]) != n) stop("Cantidad de preguntas incompatible con el mapa.", call. = FALSE)
  x <- as.data.frame(respuestas[, esperadas, drop = FALSE])
  names(x) <- mapa$codigo_pregunta
  # El mapa es la autoridad sobre anulaciones: incluso si el CSV contiene 0,
  # una pregunta anulada se convierte en NA y queda fuera de todos los cálculos.
  anuladas <- mapa$codigo_pregunta[mapa$anulada]
  if (length(anuladas)) x[anuladas] <- NA_integer_
  list(ids = respuestas$documento, x = x, mapa = mapa, anuladas = anuladas)
}

resumen_tct <- function(x, mapa, evaluacion_id, sesion, componente) {
  validas <- !mapa$anulada
  xv <- x[, validas, drop = FALSE]
  if (!ncol(xv)) return(tibble())
  total <- rowSums(xv, na.rm = TRUE)
  p <- vapply(xv, function(z) mean(z, na.rm = TRUE), numeric(1))
  rit <- correlacion_item_total(xv, total)
  kr20_pct <- redondear_entero(calcular_kr20(xv) * 100)
  tibble(
    evaluacion_id = evaluacion_id, sesion = as.integer(sesion), componente = componente,
    codigo_pregunta = names(xv),
    dificultad_tct_pct = redondear_entero(p * 100),
    # El CSV binario colapsa blanco e incorrecto en 0; no inventar una tasa de omisión.
    omision_pct = NA_integer_,
    correlacion_item_total_pct = redondear_entero(rit * 100),
    KR20_area_pct = kr20_pct,
    anulada = FALSE
  )
}

ajustar_mirt <- function(x, itemtype, etiqueta) {
  if (!requireNamespace("mirt", quietly = TRUE)) stop("Falta instalar el paquete mirt.", call. = FALSE)
  if (ncol(x) < 2) stop(etiqueta, ": se requieren al menos dos ítems no anulados.", call. = FALSE)
  variacion <- vapply(x, function(z) length(unique(stats::na.omit(z))) > 1, logical(1))
  if (any(!variacion)) {
    stop(etiqueta, ": ítems sin variación impiden la calibración: ",
         paste(names(x)[!variacion], collapse = ", "), call. = FALSE)
  }
  if (anyNA(x)) stop(etiqueta, ": hay valores faltantes inesperados después de tratar blancos y anulaciones.", call. = FALSE)
  set.seed(CONFIG$semilla)
  fit <- tryCatch(
    mirt::mirt(data = x, model = 1, itemtype = itemtype, method = "EM",
               technical = list(NCYCLES = CONFIG$max_cycles), verbose = FALSE),
    error = function(e) stop(etiqueta, ": falló la estimación del modelo ", itemtype,
                             ". No se cambiará a otro modelo. Detalle: ", conditionMessage(e), call. = FALSE)
  )
  if (!isTRUE(tryCatch(mirt::extract.mirt(fit, "converged"), error = function(e) TRUE))) {
    stop(etiqueta, ": el modelo ", itemtype, " no convergió. No se sustituirá por otro modelo.", call. = FALSE)
  }
  fit
}

estimar_habilidad <- function(fit, etiqueta) {
  fs <- tryCatch(mirt::fscores(fit, method = "EAP", full.scores.SE = FALSE),
                 error = function(e) stop(etiqueta, ": no fue posible estimar habilidad EAP: ", conditionMessage(e), call. = FALSE))
  as.numeric(fs[, 1])
}

calificar_regular <- function(datos_sesion, evaluacion_id, sesion) {
  x <- datos_sesion$x; mapa <- datos_sesion$mapa
  estudiantes <- tibble(documento = datos_sesion$ids)
  detalle <- list(); auditoria <- list()
  grupos <- split(seq_len(nrow(mapa)), interaction(mapa$orden_area, mapa$area, drop = TRUE, lex.order = TRUE))
  for (idx in grupos) {
    m <- mapa[idx, , drop = FALSE]
    area <- m$area[1]
    validas <- !m$anulada
    codigos <- m$codigo_pregunta[validas]
    if (!length(codigos)) stop("Sesión ", sesion, ", área ", area, ": todas las preguntas están anuladas.", call. = FALSE)
    aciertos <- rowSums(x[, codigos, drop = FALSE] == 1, na.rm = TRUE)
    n <- length(codigos)
    pct <- redondear_entero(aciertos / n * 100)
    estudiantes[[paste0("S", sesion, "_", make.names(area), "_aciertos")]] <- as.integer(aciertos)
    estudiantes[[paste0("S", sesion, "_", make.names(area), "_preguntas")]] <- as.integer(n)
    estudiantes[[paste0("S", sesion, "_", make.names(area), "_puntaje")]] <- pct
    detalle[[length(detalle) + 1]] <- tibble(documento = datos_sesion$ids,
      evaluacion_id = evaluacion_id, sesion = as.integer(sesion), area = area,
      aciertos = as.integer(aciertos), preguntas_validas = as.integer(n),
      preguntas_anuladas = as.integer(sum(m$anulada)), puntaje = pct)
    auditoria[[length(auditoria) + 1]] <- resumen_tct(x[, m$codigo_pregunta, drop = FALSE], m,
      evaluacion_id, sesion, area)
  }
  valid_codes <- mapa$codigo_pregunta[!mapa$anulada]
  total <- rowSums(x[, valid_codes, drop = FALSE] == 1, na.rm = TRUE)
  n_total <- length(valid_codes)
  estudiantes[[paste0("S", sesion, "_aciertos_total")]] <- as.integer(total)
  estudiantes[[paste0("S", sesion, "_preguntas_validas")]] <- as.integer(n_total)
  estudiantes[[paste0("S", sesion, "_preguntas_anuladas")]] <- as.integer(sum(mapa$anulada))
  estudiantes[[paste0("S", sesion, "_puntaje_global")]] <- redondear_entero(total / n_total * 100)
  list(resultados = estudiantes, detalle = bind_rows(detalle), auditoria = bind_rows(auditoria))
}

calificar_rasch_normativo <- function(datos_sesion, evaluacion_id, sesion, norm_cfg) {
  x <- datos_sesion$x; mapa <- datos_sesion$mapa
  estudiantes <- tibble(documento = datos_sesion$ids)
  theta_componentes <- list(); detalle <- list(); auditoria <- list()
  grupos <- split(seq_len(nrow(mapa)), interaction(mapa$orden_area, mapa$area, drop = TRUE, lex.order = TRUE))
  for (idx in grupos) {
    m <- mapa[idx, , drop = FALSE]; area <- m$area[1]
    codigos <- m$codigo_pregunta[!m$anulada]
    if (length(codigos) < 2) stop("Normativo/Rasch: sesión ", sesion, ", área ", area,
      " tiene menos de dos ítems no anulados. No se cambiará de modelo.", call. = FALSE)
    sub <- x[, codigos, drop = FALSE]
    fit <- ajustar_mirt(sub, "Rasch", paste0("Normativo Rasch — sesión ", sesion, " — ", area))
    theta <- estimar_habilidad(fit, paste0("Normativo Rasch — sesión ", sesion, " — ", area))
    sd_theta <- stats::sd(theta)
    if (!is.finite(sd_theta) || sd_theta == 0) stop("Normativo/Rasch: no se puede estandarizar ", area,
      " porque la habilidad no tiene variación en la cohorte.", call. = FALSE)
    z <- (theta - mean(theta)) / sd_theta
    escala_area <- redondear_entero(norm_cfg$media_area + norm_cfg$sd_area * z)
    key <- paste0("S", sesion, "_", make.names(area))
    estudiantes[[paste0(key, "_puntaje")]] <- escala_area
    theta_componentes[[key]] <- z
    detalle[[length(detalle) + 1]] <- tibble(documento = datos_sesion$ids,
      evaluacion_id = evaluacion_id, sesion = as.integer(sesion), area = area,
      preguntas_validas = as.integer(length(codigos)),
      preguntas_anuladas = as.integer(sum(m$anulada)), puntaje_normativo = escala_area)
    auditoria[[length(auditoria) + 1]] <- resumen_tct(sub, m[!m$anulada, , drop = FALSE],
      evaluacion_id, sesion, area)
  }
  zmat <- as.data.frame(theta_componentes)
  total_z <- rowSums(zmat)
  sd_total <- stats::sd(total_z)
  if (!is.finite(sd_total) || sd_total == 0) stop("Normativo/Rasch: el puntaje compuesto no tiene variación; no es posible estandarizarlo.", call. = FALSE)
  total_std <- (total_z - mean(total_z)) / sd_total
  estudiantes[[paste0("S", sesion, "_puntaje_global_normativo")]] <- redondear_entero(norm_cfg$media_global + norm_cfg$sd_global * total_std)
  list(resultados = estudiantes, detalle = bind_rows(detalle), auditoria = bind_rows(auditoria))
}

calificar_tri <- function(datos_sesion, evaluacion_id, sesion, modelo_tri, tri_cfg) {
  modelo_tri <- toupper(trimws(modelo_tri))
  if (!modelo_tri %in% c("2PL", "3PL")) stop("TRI: el mapa debe especificar modelo_tri como 2PL o 3PL. No se inferirá por tamaño de muestra.", call. = FALSE)
  itemtype <- if (modelo_tri == "2PL") "2PL" else "3PL"
  x <- datos_sesion$x; mapa <- datos_sesion$mapa
  estudiantes <- tibble(documento = datos_sesion$ids)
  detalle <- list(); auditoria <- list()
  grupos <- split(seq_len(nrow(mapa)), interaction(mapa$orden_area, mapa$area, drop = TRUE, lex.order = TRUE))
  for (idx in grupos) {
    m <- mapa[idx, , drop = FALSE]; area <- m$area[1]
    codigos <- m$codigo_pregunta[!m$anulada]
    if (length(codigos) < 3) stop("TRI: sesión ", sesion, ", área ", area,
      " requiere al menos tres ítems no anulados para esta implementación.", call. = FALSE)
    sub <- x[, codigos, drop = FALSE]
    fit <- ajustar_mirt(sub, itemtype, paste0("TRI ", modelo_tri, " — sesión ", sesion, " — ", area))
    theta <- estimar_habilidad(fit, paste0("TRI ", modelo_tri, " — sesión ", sesion, " — ", area))
    score <- redondear_entero(pmin(pmax(tri_cfg$media + tri_cfg$sd * theta,
                                         tri_cfg$min), tri_cfg$max))
    key <- paste0("S", sesion, "_", make.names(area))
    estudiantes[[paste0(key, "_puntaje_TRI")]] <- score
    co <- tryCatch(mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)$items,
                   error = function(e) NULL)
    if (is.null(co)) stop("TRI: no se pudieron extraer parámetros de ítems para ", area, call. = FALSE)
    # mirt suele nombrar la discriminación a1 en modelos unidimensionales;
    # algunas versiones/formatos la exponen como a. No se presupone un único alias.
    a_col <- intersect(c("a", "a1"), colnames(co))[1]
    if (is.na(a_col) || !length(a_col) || !("b" %in% colnames(co))) {
      stop("TRI: la salida de mirt no contiene parámetros de discriminación/dificultad reconocibles.", call. = FALSE)
    }
    a <- co[, a_col]
    b <- co[, "b"]
    c_col <- intersect(c("g", "c"), colnames(co))[1]
    c <- if (length(c_col) && !is.na(c_col)) co[, c_col] else rep(0, nrow(co))
    # Conversión de discriminación a métrica normal de la plantilla (D = 1.7).
    a_normal <- a / 1.7
    tct <- resumen_tct(sub, m[!m$anulada, , drop = FALSE], evaluacion_id, sesion, area)
    tct$modelo_tri <- modelo_tri
    tct$TRI_a <- as.numeric(a_normal[match(tct$codigo_pregunta, rownames(co))])
    tct$TRI_b <- as.numeric(b[match(tct$codigo_pregunta, rownames(co))])
    tct$TRI_c <- as.numeric(c[match(tct$codigo_pregunta, rownames(co))])
    tct$alerta <- ifelse(is.na(tct$TRI_a) | tct$TRI_a <= 0.2,
      "REVISAR: discriminación baja", ifelse(is.na(tct$correlacion_item_total_pct) |
      tct$correlacion_item_total_pct < 15, "REVISAR: correlación ítem-total baja",
      ifelse(!is.na(tct$omision_pct) & tct$omision_pct > 30, "ALERTA: omisión superior al 30%",
      ifelse(tct$TRI_c > 0.40, "ALERTA: pseudo-azar elevado", "OK"))))
    detalle[[length(detalle) + 1]] <- tibble(documento = datos_sesion$ids,
      evaluacion_id = evaluacion_id, sesion = as.integer(sesion), area = area,
      preguntas_validas = as.integer(length(codigos)),
      preguntas_anuladas = as.integer(sum(m$anulada)), puntaje_TRI = score, modelo_TRI = modelo_tri)
    auditoria[[length(auditoria) + 1]] <- tct
  }
  score_cols <- grep("_puntaje_TRI$", names(estudiantes), value = TRUE)
  estudiantes[[paste0("S", sesion, "_puntaje_global_TRI")]] <- redondear_entero(rowMeans(estudiantes[, score_cols, drop = FALSE]))
  list(resultados = estudiantes, detalle = bind_rows(detalle), auditoria = bind_rows(auditoria))
}

combinar_resultados <- function(lista) {
  Reduce(function(a, b) full_join(a, b, by = "documento"), lista)
}

exportar <- function(resultado, output_dir, eval_id, modelo) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  prefijo <- paste0("OMR_", gsub("[^A-Za-z0-9_-]", "_", eval_id), "_", modelo)
  # Garantía: columnas de puntajes y contadores exportados como enteros.
  for (nm in names(resultado$resultados)) {
    if (grepl("(puntaje|aciertos|preguntas_validas|preguntas_anuladas|preguntas|_pct)$", nm)) {
      resultado$resultados[[nm]] <- as.integer(round(resultado$resultados[[nm]]))
    }
  }
  readr::write_csv(resultado$resultados, file.path(output_dir, paste0(prefijo, "_resultados.csv")), na = "")
  readr::write_csv(resultado$detalle, file.path(output_dir, paste0(prefijo, "_detalle.csv")), na = "")
  readr::write_csv(resultado$auditoria, file.path(output_dir, paste0(prefijo, "_auditoria_items.csv")), na = "")
  invisible(TRUE)
}

leer_configuracion_mapa <- function(mapa, modelo) {
  tomar <- function(nombre, default = NA_real_) {
    valores <- unique(mapa[[nombre]])
    valores <- valores[!is.na(valores) & trimws(as.character(valores)) != ""]
    if (length(valores) == 0) return(default)
    if (length(valores) != 1) stop("El mapa contiene configuraciones contradictorias en ", nombre, ".", call. = FALSE)
    valores[[1]]
  }
  if (modelo == "normative") {
    cfg <- list(media_area = as.numeric(tomar("media_area")), sd_area = as.numeric(tomar("sd_area")),
                media_global = as.numeric(tomar("media_global")), sd_global = as.numeric(tomar("sd_global")))
    if (any(!is.finite(unlist(cfg))) || cfg$sd_area <= 0 || cfg$sd_global <= 0) {
      stop("Configuración normativa inválida: las medias deben ser numéricas y las desviaciones positivas.", call. = FALSE)
    }
    return(cfg)
  }
  if (modelo == "tri") {
    cfg <- list(media = as.numeric(tomar("tri_media")), sd = as.numeric(tomar("tri_sd")),
                min = as.numeric(tomar("tri_min")), max = as.numeric(tomar("tri_max")))
    if (any(!is.finite(unlist(cfg))) || cfg$sd <= 0 || cfg$min >= cfg$max) {
      stop("Configuración de escala TRI inválida.", call. = FALSE)
    }
    return(cfg)
  }
  NULL
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    stop("Uso: Rscript OMR_Vision_Suite_motor_R_desde_cero.R <carpeta_csv> [carpeta_salida]", call. = FALSE)
  }
  input_dir <- normalizePath(args[1], mustWork = TRUE)
  output_dir <- if (length(args) >= 2) args[2] else file.path(input_dir, "resultados_R")
  files <- obtener_archivos(input_dir)
  sesiones_r <- vapply(files$respuestas, files$sesion, integer(1))
  sesiones_m <- vapply(files$mapas, files$sesion, integer(1))
  if (anyDuplicated(sesiones_r)) stop("Hay más de un CSV de respuestas para una misma sesión en la carpeta.", call. = FALSE)
  if (anyDuplicated(sesiones_m)) stop("Hay más de un mapa para una misma sesión en la carpeta.", call. = FALSE)
  if (!setequal(sesiones_r, sesiones_m)) stop("No coinciden las sesiones de los CSV de respuestas y los mapas.", call. = FALSE)
  resultados_sesion <- list(); eval_ids <- character(); modelos <- character(); modelos_tri <- character()
  for (s in sort(sesiones_r)) {
    rp <- files$respuestas[match(s, sesiones_r)]
    mp <- files$mapas[match(s, sesiones_m)]
    mapa <- validar_mapa(leer_csv_seguro(mp), mp)
    if (unique(mapa$sesion) != s) stop("La sesión declarada en el mapa no coincide con el nombre del archivo: ", basename(mp), call. = FALSE)
    respuestas <- leer_csv_seguro(rp)
    if (nrow(respuestas) == 0) stop("CSV sin estudiantes: ", basename(rp), call. = FALSE)
    dat <- preparar_matriz(respuestas, mapa, rp)
    eval_ids <- c(eval_ids, unique(mapa$evaluacion_id))
    modelos <- c(modelos, unique(mapa$modelo_calificacion))
    # Modelo interno TRI: el mapa ampliado debe declararlo. No se selecciona por N.
    mt <- if ("modelo_tri" %in% names(mapa)) unique(toupper(as.character(mapa$modelo_tri))) else character(0)
    modelos_tri <- c(modelos_tri, mt)
    model <- unique(mapa$modelo_calificacion)
    model_cfg <- leer_configuracion_mapa(mapa, model)
    calc <- switch(model,
      regular = calificar_regular(dat, unique(mapa$evaluacion_id), s),
      normative = calificar_rasch_normativo(dat, unique(mapa$evaluacion_id), s, model_cfg),
      tri = calificar_tri(dat, unique(mapa$evaluacion_id), s, mt, model_cfg),
      stop("Modelo no implementado: ", model, call. = FALSE))
    resultados_sesion[[as.character(s)]] <- calc
    message("Sesión ", s, " procesada con modelo ", model, ".")
  }
  if (length(unique(eval_ids)) != 1) stop("La carpeta contiene más de una evaluación.", call. = FALSE)
  if (length(unique(modelos)) != 1) stop("Las sesiones de una evaluación tienen modelos distintos.", call. = FALSE)
  modelo <- unique(modelos)
  combinado <- list(
    resultados = combinar_resultados(lapply(resultados_sesion, `[[`, "resultados")),
    detalle = bind_rows(lapply(resultados_sesion, `[[`, "detalle")),
    auditoria = bind_rows(lapply(resultados_sesion, `[[`, "auditoria"))
  )
  exportar(combinado, output_dir, unique(eval_ids), modelo)
  message("Proceso finalizado. Evaluación: ", unique(eval_ids), " | Modelo: ", modelo,
          " | Estudiantes consolidados: ", nrow(combinado$resultados),
          " | Motor: ", VERSION_MOTOR)
  message("Archivos guardados en: ", normalizePath(output_dir, mustWork = FALSE))
}

if (sys.nframe() == 0) main()
