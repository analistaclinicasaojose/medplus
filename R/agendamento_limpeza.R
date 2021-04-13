#' Limpeza para dados do relatorio agendamentos - mantendo apenas os dados validos 
#'
#'
#' Essa funcao funciona para os dados do relatorio agendamentos e retira os dados que poluem o banco de dados.
#' 
#' 
#' Plano = 'Convenio Cortesia C/ Cobranca de Honorarios' sao retirados
#' 
#' Procedimentos do tipo 'A- REPETICAO', 'GRATUIDADE', 'Retorno' sao retirados
#' 
#' Procedimentos do tipo 'Bloqueado' sao substituidos para 'Atendidos'
#' 
#' Pacientes com o nome 'Nao agendar' ou 'Teste' sao retirados
#' 
#' E substituido o NomeStatus = 'Chegou', 'Na Espera', 'Sem Status', 'Em Consulta' para 'Atendido'
#' 
#' E removido o NomeStatus = 'Confirmado'
#' 
#' @param Dataframe
#'
#' @import tidyverse
#' @import stringr
#' 
#' 
limpeza_agendamentos <- function(dados){
  
  dados <- filter(dados, Plano != 'Convenio Cortesia C/ Cobranca de Honorarios')
  
  dados <- dados[!(dados$Tipo %in% c('A- REPETICAO','Bloqueado','GRATUIDADE','Retorno')),]
  
  dados <- filter(dados, Paciente != 'NAO AGENDAR')  
  dados<- dados[!(dados$Descricao=='NAO AGENDAR'),]  

  dados<- dados[!(toupper(substr(dados$Paciente, start=1, stop=5)) =='TESTE'),]

  dados$NomeStatus<- dados$NomeStatus %>% 
    str_replace_all("Bloqueado", "Atendido")
  
  #Trocando faltou, remarcou,desistiu ou desmarcado para no_show
  #Retirando os 'bloqueados' e os 'Chegou'
  
  dados<-dados[!(dados$NomeStatus=="Confirmado"),]
  
  dados$NomeStatus <- dados$NomeStatus %>% 
    str_replace_all("Bloqueado","Atendido")%>% 
    str_replace_all("Chegou","Atendido")%>% 
    str_replace_all("Na Espera","Atendido")%>% 
    str_replace_all("Sem Status","Atendido")%>%
    str_replace_all('Em Consulta', 'Atendido')
  
  dados<-dados[!(dados$nomeUsuario=="TELE - Triagem"),]
  
  dados<- dados[!(dados$Descricao=='NAO AGENDAR'),] 
  dados<- dados[!(dados$Descricao=='Horário Bloqueado'),] 
  dados<- dados[!(dados$NomeStatus=='Remarcado'),] 
  
  return(dados)
}

