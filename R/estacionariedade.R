#' Teste de estacionaridade das variaveis
#'
#' @param df Data frame com variaveis para testar
#'
#' @return Data frame com resultado dos testes
#' @export
estacionariedade <- function(df) {
  variaveis <- c(
    "pibm", "pibac", "ibcm", "ibcsz", "slicm",
    "slica", "jmto", "jmpj", "cbef", "cbre",
    "cbnm", "ipca", "m4sld", "ibov", "embi", "pnad"
  )

  testes0 <- lapply(X = df[variaveis], FUN = teste_12_lags) %>%
    # obs: não filtrei linhas por período comum a ACs
    purrr::map(~as_tibble(t(.x))) %>%
    # Seleciona o menor lag da variável x onde os testes são bem sucedidos
    purrr::map(filter, as.logical(resumo)) %>%
    purrr::map_df(summarise_all, first, .id = "cod")

  testes1 <- df[variaveis] %>%
    purrr::map(diff) %>%
    purrr::map(teste_12_lags) %>%
    purrr::map(~as_tibble(t(.x))) %>%
    # Seleciona o menor lag da variável x onde os testes são bem sucedidos
    purrr::map(filter, as.logical(resumo)) %>%
    purrr::map_df(summarise_all, first, .id = "cod")


  testes0 %>%
    select(var = cod, estac_0 = resumo) %>%
    left_join(
      testes1 %>%
        select(var = cod, estac_1 = resumo),
      by = "var"
    ) %>%
    mutate_at(vars(2:3), as.logical)

}

