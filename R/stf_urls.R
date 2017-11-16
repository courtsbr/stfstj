#' Function stf_urls
#'
#' Essa função retorna as urls dos processos baseadas na pesquisa livre da jurisprudência do STF
#' @param pesquisa_livre palavras a serem buscadas
#' @keywords stf, jurisprudência
#' @import httr
#' @import xml2
#' @import stringr
#' @import magrittr
#' @return vetor com urls dos processos agrupados de 10 em 10

#' @export

stf_urls<-function(pesquisa_livre){
  url1<-stringr::str_c("http://www.stf.jus.br/portal/jurisprudencia/listarConsolidada.asp?txtPesquisaLivre=",pesquisa_livre,"&base=baseAcordaos")
  numero_tinyurl<-httr::GET(url1) %>% 
    httr::content() %>% 
    xml2::xml_find_all("//*[@class='linkPagina']|//*[@class='linkPagina']/@href") %>%
    xml2::xml_text()
  paginas<-stringr::str_extract(numero_tinyurl[[1]],"\\d+") %>% 
    as.numeric() %>% 
    magrittr::divide_by(10) %>% 
    ceiling()
  tinyURL<-numero_tinyurl[[2]]
  urls<-stringr::str_c("http://www.stf.jus.br/portal/jurisprudencia/",tinyURL,"&pagina=",1:paginas)
  return(urls)
}
