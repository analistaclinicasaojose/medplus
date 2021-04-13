#' Padroniza o valor do DF
#'
#'
#' Entrada deve ser um vetor de valores.
#' Essa funcao substitui virgula por ponto e transforma em numerico.
#'
#' @param vetor do tipo string = 200,000 | 200,00 | 200,0 
#' 
#' @import stringr
#' 
#' @return vetor numerico = 200
#' 
padronizar_valor <- function(vetor){
  # O banco tem que ter formado xslx
  vetor  = vetor  %>% str_replace(',', '.')
  vetor <- as.numeric(vetor)
  return(vetor)
}

