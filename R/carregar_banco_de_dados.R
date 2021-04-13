#' Carregar banco de dados
#'
#'
#' Entrada deve ser um banco de dados xlsx do medplus.
#' Essa funcao retira as 3 primeiras linhas e atualiza o nome das colunas
#'
#' @param  banco_de_dados = 'exemplo.xlsx'
#'
#' @import readxl
#' 
carregar_banco <- function(banco_de_dados){
  # O banco tem que ter formado xslx
  banco <- read_excel(banco_de_dados)
  colunas <- as.vector(t(banco[3,]))
  colnames(banco) <- colunas
  banco <- banco[-c(1,2,3),]
  return(banco)
}

