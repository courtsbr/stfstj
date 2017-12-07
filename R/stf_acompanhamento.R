#' Function stf_acompanhamento
#'
#' Essa função retorna a tabela com o andamento processual da jurispruência buscada com a função stf_metadados.R
#' @param url url dos andamentos
#' @keywords stf, jurisprudência, andamento processual
#' @return tabela com os andamentos processuais
#' @export
stf_acompanhamento <- function(url) {
  xp <- "//table[@class='resultadoAndamentoProcesso']"
  url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = xp) %>%
    rvest::html_table() %>%
    magrittr::extract2(1)
}
