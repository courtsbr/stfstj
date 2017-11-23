#' Function stf_monocratica
#'
#' Essa função baixa as decisões monocráticas do STF
#' @param urls  vetor de urls das decisões monocráticas obtidas com a função stf_urls
#' @keywords stf, jurisprudência, inteiro teor, decisão
#' @return dataframe com metadados e decisão monocrática
#' @export

stf_monocratica<-function(urls){
  urls %>%
    
    purrr::map_dfr(purrr::possibly(~{
    
    principal<- .x %>% 
      httr::GET() %>% 
      httr::content() 
    
    recurso<-principal %>% 
      xml2::xml_find_all("//div[@class='processosJurisprudenciaAcordaos']/p[1]/strong") %>% 
      xml2::xml_text() %>% 
      stringr::str_split("\n")
    
    processo<-recurso %>%
      purrr::map_chr(~stringr::str_extract(.x[[1]],".*?(?=\\/)"))
    
    origem<-recurso %>% 
      purrr::map_chr(~stringr::str_extract(.x[[1]],"(?<=\\/).*")) %>% 
      stringr::str_trim()
    
    classe<-recurso %>% 
      purrr::map_chr(~stringr::str_trim(.x[[2]]))
    
    relator<-recurso %>% 
      purrr::map_chr(~{
        .x[[3]] %>% 
          stringr::str_extract("(?<=Relator\\(a\\)\\:).*?(?=Relator|Julgamento)") %>% 
          stringr::str_extract("(?<=Min\\.\\s).*")
      })
    
    
    data_julgamento<-recurso %>% 
      purrr::map_chr(~{
        .x[[3]] %>% 
          stringr::str_extract("\\d{2}\\/\\d{2}\\/\\d{4}")
      })
    
    data_publicacao<-principal %>% 
      xml2::xml_find_all("//p[strong='Publicação']/following-sibling::*[1]") %>% 
      xml2::xml_text() %>%
      stringr::str_extract("(?<=PUBLIC\\s|DJ\\s)\\d{2}.\\d{2}.\\d{4}")
    
    partes<-principal %>% 
      xml2::xml_find_all("//p[strong='Partes']/following-sibling::pre[1]") %>% 
      xml2::xml_text() %>% 
      stringr::str_split("\r\n") %>% 
      purrr::modify_depth(1,~{setNames(.,str_extract(.,".*?(?=\\s)"))})
    partes<-dplyr::bind_rows(!!!partes)
    
    
    decisao<-principal %>% 
      xml2::xml_find_all("//p[strong='Decisão']/following-sibling::pre[1]") %>% 
      xml2::xml_text()
    
  
    data.frame(url=.x,processo,origem,classe,relator,data_julgamento,data_publicacao,partes,decisao,stringsAsFactors = F)
  },data.frame(url=NA_character_,processo=NA_character_,origem=NA_character_,classe=NA_character_,relator=NA_character_,data_julgamento=NA_character_,data_publicacao=NA_character_,partes=NA_character_,decisao=NA_character_)
  ))
}


