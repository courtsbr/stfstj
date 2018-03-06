#' Get metadata from Brazilian highest Federal Court's (STJ) monocratic decisions
#'
#' @param open_search search parameter
#' @param phrase logical, if TRUE, default, the search parameter will be treated as phrase.
#'
#' @return data frame with the metadata
#' @export
#'
#' @examples
#' family_doctors<- stj_mono_metadata("m\u00e9dico de fam\u00edlia")

stj_mono_metadata <- function(open_search,phrase=TRUE){
  
  open_search<-abjutils::rm_accent(open_search)
  
  if (phrase == TRUE) {
    open_search <- deparse(open_search)
  }
  

  url1 <- "http://www.stj.jus.br/SCON/"
  url2 <- "http://www.stj.jus.br/SCON/pesquisar.jsp"
  h <- httr::GET(url1)
  
  content<-httr::GET("http://www.stj.jus.br",
                     path = "/SCON/decisoes/toc.jsp",
                     query = list(tipo_visualizacao = "",
                                  livre = open_search,
                                  b = "DTXT",
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
                        path = "/SCON/decisoes/toc.jsp",
                        query = list(tipo_visualizacao = "",
                                     livre = open_search,
                                     b = "DTXT",
                                     p = "true",
                                     l = 10,
                                     t = 'JURIDICO',
                                     i = .x),
                        httr::add_headers(`Referer` = url1))
      
      page<-.x
      
      principal <- res2 %>%
        httr::content()
      
      processo <- principal %>% 
        xml2::xml_find_all("//div/h4[contains(.,'Processo')]/following-sibling::p[1]") %>% 
        xml2::xml_text() %>% 
        stringr::str_trim()
      
      
      relator <- principal%>% 
        xml2::xml_find_all("//div/h4[text()='Relator(a)']/following-sibling::p[1]") %>% 
        xml2::xml_text() %>%
        stringr::str_extract("(?<=Ministr[ao]\\s).*")
      
     
      
      data_publicacao <- principal %>% 
        xml2::xml_find_all("//div/h4[text()='Data da Publica\u00e7\u00e3o']/following-sibling::p[1]") %>% 
        xml2::xml_text() %>% 
        lubridate::dmy()
      
      
      decisao <- principal%>% 
        xml2::xml_find_all("//div/h4[text()='Decis\u00e3o']/following-sibling::pre[1]") %>%
        xml2::xml_text()
      
      tibble::tibble(page,processo,relator,data_publicacao,decisao)
    })
  
  df<-dplyr::bind_cols(search_time=rep(search_time,nrow(df)),df)
}