#' Arrumar formato da data
#'
#'
#' Entrada deve ser um vetor data no formado DD/MM/AAAA 00:00:00.
#' Essa funcao arruma o padrao data do medplus para o padrao data do r.
#'
#' @param  vetor_data = 'DD/MM/AAAA 00:00:00' string
#'
#' @import stringr

padronizar_data <- function(vetor_data){
  
  vetor_data <-  paste0(str_sub(vetor_data,start = 7, end = 10),
                               str_sub(vetor_data,start = 3, end = 6),
                               str_sub(vetor_data,start = 1, end = 2)) 
  
  vetor_data <- str_replace_all(vetor_data,"/", "-")
  return(vetor_data)
}

