#' Limpeza para dados do relatorio procedimentos - mantendo apenas os dados validos 
#'
#'
#' Essa funcao funciona para os dados do relatorio procedimentos e retira os dados que poluem o banco de dados.
#' 
#' Dados do tipo 'A- REPETICAO', 'GRATUIDADE', 'Retorno' sao retirados
#' 
#' Plano = 'Convenio Cortesia C/ Cobranca de Honorarios' sao retirados
#' 
#' Mantem apenas os StatusAgen: 'Atendido' e 'Cumprido e Recebido'
#' 
#' @import tidyverse
#' 
#' @param Dataframe
#' 
limpeza_procedimentos <- function(dados){
  
  dados <- filter(dados, StatusAgen == "Atendido" | StatusAgen == "Cumprido e Recebido")
  dados <- filter(dados, Plano != 'Convenio Cortesia C/ Cobranca de Honorarios')
  dados <- dados[!(dados$Tipo %in% c('A- REPETICAO','Bloqueado','GRATUIDADE','Retorno')),]
  
  return(dados)
}

