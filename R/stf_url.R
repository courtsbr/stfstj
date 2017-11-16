pesquisa_livre<-"EXCESSO+ADJ2+PRAZO"


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

url_excesso<-stf_urls(pesquisa_livre)


base<-url_excesso[c(1:100,201:291)] %>%
  purrr::map_dfr(possibly(~{

principal<-.x %>% 
  httr::GET() %>% 
  httr::content() 

recurso<-principal %>% 
  xml2::xml_find_all("//div[@class='processosJurisprudenciaAcordaos']/p[1]/strong") %>% 
  xml2::xml_text() %>% 
  str_split("\n")

processo<-recurso %>%
  map_chr(~str_extract(.x[[1]],".*?(?=\\/)"))

origem<-recurso %>% 
  map_chr(~str_extract(.x[[1]],"(?<=\\/).*")) %>% 
  str_trim()

classe<-recurso %>% 
        map_chr(~str_trim(.x[[6]]))
                
relator<-recurso %>% 
         map_chr(~{
           .x[[7]] %>% 
            str_extract("(?<=Relator\\(a\\)\\:).*?(?=Relator|Julgamento)") %>% 
           str_extract("(?<=Min\\.\\s).*")
           })

relator_acordao<- recurso %>% 
          map_chr(~{
          .x[[7]] %>% 
            str_extract("(?<=Relator\\(a\\)\\sp\\/\\sAcórdão\\:).*(?=Julgamento)") %>% 
            str_extract("(?<=Min\\.\\s).*")
        })

data_julgamento<-recurso %>% 
  map_chr(~{
    .x[[7]] %>% 
      str_extract("\\d{2}\\/\\d{2}\\/\\d{4}")
  })

publicacao<-principal %>% 
  xml2::xml_find_all("//p[strong='Publicação']/following-sibling::*[1]") %>% 
  xml2::xml_text() %>%
  str_extract("(?<=PUBLIC\\s|DJ\\s)\\d{2}.\\d{2}.\\d{4}")
  

partes<-principal %>% 
  xml2::xml_find_all("//p[strong='Parte(s)']/following-sibling::pre") %>% 
  xml2::xml_text() %>% 
  stringr::str_split("\r\n") %>% 
  purrr::modify_depth(1,~{setNames(.,str_extract(.,".*?(?=\\s)"))})
partes<-bind_rows(!!!partes)

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

voto<-decisao %>% map_chr(~{
  if
  (stringr::str_detect(.x,"maioria")){
  "maioria"
}else if (stringr::str_detect(.x,"unanime")){
  "unanime"
}else
    NA
})

url_inteiro_teor<-principal %>% 
  xml_find_all("//li/a[contains(@href,'obterInteiroTeor')]") %>% 
  xml_attrs() %>%
  str_extract("inteiroTeor.*") %>% 
  str_c("http://www.stf.jus.br/portal/",.)

data.frame(url=.x,processo,origem,classe,relator,relator_acordao,data_julgamento,publicacao,partes,ementa,voto,decisao,url_inteiro_teor)
},data.frame(url=NA_character_,processo=NA_character_,origem=NA_character_,classe=NA_character_,relator=NA_character_,relator_acordao=NA_character_,data_julgamento=NA_character_,publicacao=NA_character_,partes=NA_character_,ementa=NA_character_,voto=NA_character_,decisao=NA_character_,url_inteiro_teor=NA_character_)
))


stf_inteiro_teor<-function(url,baixar=FALSE){
  tmp_file<-tempfile(pattern="inteiro",fileext = ".pdf")
  url %>% 
    purrr::map_chr(~{
    .x %>% 
    httr::GET(url, write_disk(path="inteiro.pdf",overwrite = T))
     txt<- textreadr::read_document("inteiro.pdf",combine=T)

    })
} 
