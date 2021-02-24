#' Ler dados das APIs do BCB e IPEADATA
#'
#' @return Uma tabela com dados usados de APIs
#' @export
#'
#' @examples
ler_apis <- function() {
  series <- c(
    4380, # PIB mensal - Valores correntes (R$ milhões)
    4382, # PIB acumulado dos últimos 12 meses - Valores correntes (R$ milhões)
    24363,#	Índice de Atividade Econômica do Banco Central - IBC-Br
    24364,# Índice de Atividade Econômica do Banco Central (IBC-Br) - com ajuste sazonal
    4390, # Taxa de juros - Selic acumulada no mês
    4189, # Taxa de juros - Selic acumulada no mês anualizada base 252
    25433,# Taxa média mensal de juros das operações de crédito - Total
    25434,# Taxa média mensal de juros das operações de crédito - Pessoas jurídicas - Total
    11752,# Índice da taxa de câmbio real efetiva (IPCA) - Jun/1994=100
    11753,# Índice da taxa de câmbio real (IPCA) - Jun/1994=100 - Dólar americano
    20360,# Índice da taxa de câmbio efetiva nominal - Jun/1994=100
    433,  # Índice nacional de preços ao consumidor-amplo (IPCA)
    27815 # Meios de pagamento amplos - M4 (saldo em final de período)
  )

  bcb <- rbcb::get_series(code = series, start_date = "2014-01-01") %>%
    purrr::reduce(left_join, by = "date")

  embi <- ipeadatar::ipeadata(code = "JPM366_EMBI366") %>%
    dplyr::mutate(mes = lubridate::month(date),
           ano = lubridate::year(date),
           date = as.Date(paste0(ano, "-", formatC(x = mes, width = 2, flag = 0),"-01"))) %>%
    dplyr::filter(ano>=2014) %>%
    dplyr::group_by(code, date) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    tidyr::spread(key = code, value = value)

  ocup <- ipeadatar::ipeadata(code = "PNADC12_PO12") %>%
    dplyr::filter(date>="2014-01-01") %>%
    dplyr::select(code, date, value) %>%
    tidyr::spread(key = code, value = value)

  ipea <- dplyr::full_join(embi, ocup)

  ibov1 <- tseries::get.hist.quote(
    instrument = "^BVSP",
    start = "2014-01-01",
    quote = "Close",
    provider = "yahoo",
    origin = "1970-01-01",
    compression = "m",
    retclass = "zoo")

  ibov <- tibble::tibble(
    date = zoo::index(ibov1),
    ibov = unclass(ibov1$Close)
  )

  dplyr::full_join(bcb, ibov, "date") %>%
    dplyr::full_join(ipea, "date")
}

#' Ler Cade em Numeros
#'
#' @return Uma tabela com CN compilado
#' @export
ler_cn <- function(dir = NULL) {
  if (is.null(dir)) {
    dir <- system.file("ext", package = "previsaoAC")
  }
  arq <- list.files(path = dir, full.names = TRUE)
  lista <- lapply(arq, readxl::read_excel)

  nomes <- dir(dir) %>%
    substr(start = 1, stop = 4)

  purrr::map2_df(lista, nomes, ~dplyr::mutate(.x, ano = .y)) %>%
    setNames(c("mes", "acs", "ano")) %>%
    dplyr::mutate(int = paste0(ano, "-", mes,"-01"),
           date = as.Date(int, format = "%Y-%b-%d")) %>%
    dplyr::select(date, acs) %>%
    # Remover último registro que contém informação parcial do mês atual
    dplyr::filter(date != max(date))

}

#' Consolidar dados API e CN
#'
#' @param api Tabela de dados das apis
#' @param acs Tabela com dados do Cade em Numeros
#'
#' @return A tabela usada para modelagem e previsoes
#' @export
consolidar_base <- function(api, acs) {
  nomes <- c(
    "data", "pibm", "pibac", "ibcm", "ibcsz", "slicm",
    "slica", "jmto", "jmpj", "cbef", "cbre", "cbnm",
    "ipca", "m4sld", "ibov", "embi", "pnad", "ac"
  )

  api %>%
    # as_tibble() %>%
    dplyr::full_join(acs, "date") %>%
    setNames(nomes) %>%
    dplyr::mutate(ac12c = c(rep(NA, 11), zoo::rollsum(ac, 12)),
           ano = lubridate::year(x = data),
           mes = lubridate::month(x = data),
           anomes = paste0(ano, "-", formatC(x = mes, width = 2, flag = "0")))
}
