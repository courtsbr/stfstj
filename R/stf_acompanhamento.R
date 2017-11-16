#' Function stf_acompanhamento
#'
#' Essa função retorna a tabela com o andamento processual da jurispruência buscada com a função stf_metadados.R
#' @param url_acompanhamento url dos andamento
#' @keywords stf, jurisprudência, andamento processual
#' @import rvest
#' @import xml2
#' @import magrittr
#' @return tabela com os andamentos processuais
#' @examples
#' andamento de um processo da telefônica
#' url<- "http://www.stf.jus.br/portal/processo/verProcessoAndamento.asp?numero=897105&classe=ARE-AgR&codigoClasse=0&origem=JUR&recurso=0&tipoJulgamento=M"
#' andamento<-stf_acompanhamento(url) 

#' @export

stf_acompanhamento<-function(url){
  url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath="//table[@class='resultadoAndamentoProcesso']") %>%
    rvest::html_table() %>% 
    magrittr::extract2(1)
  
}