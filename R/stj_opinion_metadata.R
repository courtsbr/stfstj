#' Gets metadada from Brazilian Federal High Court precedents
#' 
#' @param open_search Words to be searched
#' @param phrase logical, if TRUE, default, the search parameter will be treated as phrase.

#' @return Data frame with the metadata
#' @export
stj_opinion_metadata <- function(open_search,phrase=TRUE){
  
  abjutils::rm_accent(open_search)
  
  if (phrase == TRUE) {
    open_search <- deparse(open_search)
  }
  
  #open_search <- utils::URLencode(open_search)
  
  url1 <- "http://www.stj.jus.br/SCON/"
  url2 <- "http://www.stj.jus.br/SCON/pesquisar.jsp"
  h <- httr::GET(url1)
  

 ####corpo_fim####
  
  
  
  
  content<-httr::GET("http://www.stj.jus.br",
            path = "/SCON/decisoes/toc.jsp",
            query = list(tipo_visualizacao = "",
                         livre = open_search,
                         b = "ACOR",
                         p = "true",
                         l = 10,
                         t = 'JURIDICO',
                         i = 1),
            httr::add_headers(`Referer` = url2)) %>% 
    httr::content()
  

## Get the number of pages to loop over each one and add them to the data.frame
  pages <- content %>% 
    xml2::xml_find_first(xpath = '//div[@id="infopesquisa"]/span[@class="labellinha"]/following-sibling::span[1]') %>%
    xml2::xml_text() %>% 
    as.numeric()

  ## Might be important to inform the exact request time in Brasilia 
  ## in order to allow others to replicate the research.
 
  search_time<-lubridate::now(tz="America/Sao_Paulo")
  
 df<- seq(1,pages,10) %>% 
     purrr::map_dfr(~{
  res2 <- httr::GET("http://www.stj.jus.br",
                  path = "/SCON/jurisprudencia/toc.jsp",
                  query = list(tipo_visualizacao = "",
                             livre = open_search,
                             b = "ACOR",
                             p = "true",
                             l = 10,
                             t = 'JURIDICO',
                             i = .x),
                  httr::add_headers(`Referer` = url2))
  
  page<-.x
  
  principal <- res2 %>%
    httr::content()
  
  processo <- principal %>% 
    xml2::xml_find_all("//div[@class='docTexto']/text()[following-sibling::br][1]") %>% 
    xml2::xml_text() %>% 
    stringr::str_trim()
  
  
  origem <- processo %>% 
    stringr::str_extract("\\w{2}$")
  
  classe <- principal %>% 
    xml2::xml_find_all("//div[@class='docTexto']/text()[following-sibling::br][2]") %>% 
    xml2::xml_text() %>% 
    stringr::str_trim()
  
  processo_stj <- principal %>% 
    xml2::xml_find_all("//div[@class='docTexto']/text()[preceding-sibling::br][2]") %>% 
    xml2::xml_text() %>% 
    stringr::str_trim()
  
  relator <- principal %>% 
    xml2::xml_find_all("//div/h4[text()='Relator(a)']/following-sibling::pre[@class='docTexto']") %>% 
    xml2::xml_text() %>%
    stringr::str_extract("(?<=Ministr[ao]\\s).*(?=\\s\\()")
  
  orgao_julgador <- principal %>% 
    xml2::xml_find_all("//div/h4[text()='\u00D3rg\u00E3o Julgador']/following-sibling::pre[@class='docTexto']") %>% 
    xml2::xml_text()
  
  data_julgamento <- principal %>% 
    xml2::xml_find_all("//div/h4[text()='Data do Julgamento']/following-sibling::pre[@class='docTexto']") %>% 
    xml2::xml_text() %>% 
    lubridate::dmy()
  
  publicacao <- principal %>% 
    xml2::xml_find_all("//div/h4[text()='Data da Publica\u00E7\u00E3o/Fonte']/following-sibling::pre[@class='docTexto']") %>% 
    xml2::xml_text()
  
  fonte <- publicacao %>% stringr::str_extract("\\w+")
  
  data_publicacao <- pt_time_extract(publicacao)
  
  ementa <- principal %>% 
    xml2::xml_find_all("//div/h4[text()='Ementa']/following-sibling::pre[@class='docTexto']") %>%
    xml2::xml_text()
  
  decisao <- principal %>% 
    xml2::xml_find_all("//div/h4[text()='Ac\u00F3rd\u00E3o']/following-sibling::pre[@class='docTexto']") %>%
    xml2::xml_text()
  
  tibble::tibble(page,processo,origem,classe,processo_stj,relator,orgao_julgador,data_julgamento,fonte,data_publicacao,ementa,decisao)
       })
 
 df<-dplyr::bind_cols(search_time=rep(search_time,nrow(df)),df)
}
