#' Function stf_metadados
#'
#' Essa função retorna os metadados da jurisprudencia do STF conforme a url extraída com a função stf_urls.R
#' @param urls endereços eletrônicos dos processos agrupados de 10 em 10
#' @keywords stf, jurisprudência
#' @return Tabela com metadados dos processos
#' @export
stf_metadados<-function(urls){
  urls %>% purrr::map_dfr(purrr::possibly(~{

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
      purrr::map_chr(~stringr::str_trim(.x[[6]]))
    
    relator<-recurso %>% 
      purrr::map_chr(~{
        .x[[7]] %>% 
          stringr::str_extract("(?<=Relator\\(a\\)\\:).*?(?=Relator|Julgamento)") %>% 
          stringr::str_extract("(?<=Min\\.\\s).*")
      })
    
    relator_acordao<- recurso %>% 
      purrr::map_chr(~{
        .x[[7]] %>% 
          stringr::str_extract("(?<=Relator\\(a\\)\\sp\\/\\sAcórdão\\:).*(?=Julgamento)") %>% 
          stringr::str_extract("(?<=Min\\.\\s).*")
      })
    
    data_julgamento<-recurso %>% 
      purrr::map_chr(~{
        .x[[7]] %>% 
          stringr::str_extract("\\d{2}\\/\\d{2}\\/\\d{4}")
      })
    
    publicacao<-principal %>% 
      xml2::xml_find_all("//p[strong='Publicação']/following-sibling::*[1]") %>% 
      xml2::xml_text()
    
    data_publicacao<- publicacao %>%
      stringr::str_extract("(?<=PUBLIC\\s|DJ\\s)\\d{2}.\\d{2}.\\d{4}")
    
    eletronico<-stringr::str_detect(publicacao,"ELETRÔNICO")
    
    partes<-principal %>% 
      xml2::xml_find_all("//p[strong='Parte(s)']/following-sibling::pre") %>% 
      xml2::xml_text() %>% 
      stringr::str_split("\r\n") %>% 
      purrr::modify_depth(1,~{setNames(.,str_extract(.,".*?(?=\\s)"))})
    partes<-dplyr::bind_rows(!!!partes)
    
    ementa<- principal %>% 
      xml2::xml_find_all("//div[contains(@style,'line-height: 150%;text-align: justify;')]") %>% 
      xml2::xml_text()
    
    
    decisao_tag<-principal %>% 
      xml2::xml_find_all("//strong[div/@style='line-height: 150%;text-align: justify;']/following-sibling::p[1]") %>% 
      xml2::xml_text()
    
    decisao<-principal %>% 
      xml2::xml_find_all("//strong[div/@style='line-height: 150%;text-align: justify;']/following-sibling::p[1]/../div[1]") %>% 
      xml2::xml_text()
    
    decisao<-ifelse(decisao_tag=="Decisão",decisao,"inexistente")
    
    voto<-decisao %>% purrr::map_chr(~{
      if
      (stringr::str_detect(.x,"maioria")){
        "maioria"
      }else if (stringr::str_detect(.x,"unanime")){
        "unanime"
      }else
        NA
    })
    
    url_inteiro_teor<-principal %>% 
      xml2::xml_find_all("//li/a[contains(@href,'obterInteiroTeor')]") %>% 
      xml2::xml_attrs() %>%
      stringr::str_extract("inteiroTeor.*") %>% 
      stringr::str_c("http://www.stf.jus.br/portal/",.)
    
    url_andamento<-principal %>% 
      xml2::xml_find_all("//div[@class='abasAcompanhamento']/ul[@class='abas']/li/a[contains(@href,'verProcessoAndamento')]") %>%
      xml2::xml_attrs() %>%
      stringr::str_extract("numero.*") %>%
      stringr::str_c("http://www.stf.jus.br/portal/processo/verProcessoAndamento.asp?",.)
    
    
    
    data.frame(url=.x,processo,origem,classe,relator,relator_acordao,data_julgamento,data_publicacao,eletronico,partes,ementa,voto,decisao,url_inteiro_teor,url_andamento,stringsAsFactors = F)
  },data.frame(url=NA_character_,processo=NA_character_,origem=NA_character_,classe=NA_character_,relator=NA_character_,relator_acordao=NA_character_,data_julgamento=NA_character_,data_publicacao=NA_character_,eletronico=NA,partes=NA_character_,ementa=NA_character_,voto=NA_character_,decisao=NA_character_,url_inteiro_teor=NA_character_,url_andamento=NA_character_)
  ))
}
