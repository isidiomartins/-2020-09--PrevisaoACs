# Funções script 03
#' Calcula os testes para a variável x em todos 12 lags (sem lag também)
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
teste_12_lags <- function(x) {
  sapply(X = 0:12, function(y) tau(x = x, lags = y))
  }

# Funções script 04
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
paste_mais <- function(x) {
  paste0(x, collapse = " + ")
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
criar_linhas_lags <- function(data){

  # Criando 12 novas linhas vazias
  n <- nrow(data)
  new <- (n+1):(n+12)
  data[new,] <- NA

  # Criando datas para estes espaços vazios
  last<- max(which(!is.na(data$data)))
  data$data[(last+1):nrow(data)]<- data$data[last] %m+% months(1:12)

  # Simplesmente preenchendo os outros campos de data
  data$ano<- lubridate::year(x = data$data)
  data$mes<- lubridate::month(x = data$data)
  data$anomes<- paste0(data$ano, "-", formatC(x = data$mes, width = 2, flag = "0"))

  data
}

#' Title
#'
#' @param i
#' @param form
#'
#' @return
#' @export
#'
#' @examples
prever_12 <- function(i, form) {
  modelo <- lm(formula = formula(form), data = base[1:i,])

  # acho que seria melhor fazer isso fora da funcao
  obs_prev <- base %>%
    as_tibble() %>%
    filter(between(row_number(), i + 1, i + 12))

  obs_prev %>%
    select(ano, mes, anomes, ac) %>%
    mutate(prev = predict(modelo, obs_prev))
}

#' Title
#'
#' @param formula
#'
#' @return
#' @export
#'
#' @examples
janela_previsao <- function(formula) {
  inicio <- which(base$data == "2017-12-01") # Neste ponto podem ser realizadas alterações
  final <- max(which(!is.na(base$ac)))
  lapply(X = inicio:final, FUN = prever_12, formula)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
rmse <- function(x){
  erro <- (x$ac - x$prev)
  sqrt(mean(erro ^ 2, na.rm = TRUE))
}

# Funções script 05

