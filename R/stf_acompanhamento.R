#' Function stf_acompanhamento
#'
#' Essa função retorna a tabela com o andamento processual da jurispruência buscada com a função stf_metadados.R
#' @param url_acompanhamento url dos andamento
#' @keywords stf, jurisprudência, andamento processual
#' @import rvest
#' @import xml2
#' @import magrittr
#' @return tabela com os andamentos processuais

#' @export
stf_acompanhamento<-function(url){
  url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath="//table[@class='resultadoAndamentoProcesso']") %>%
    rvest::html_table() %>% 
    magrittr::extract2(1)
  
}