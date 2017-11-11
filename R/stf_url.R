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


base<-url_excesso[1:2] %>% purrr::map(~{

principal<- url_excesso[2] %>% 
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
        map_chr(~str_trim(.x[[6]])
                
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

publicacao<-principal %>% 
  xml2::xml_find_all("//p[strong='Publicação']/following-sibling::*[1]") %>% 
  xml2::xml_text() 

partes<-principal %>% 
  xml2::xml_find_all("//p[strong='Parte(s)']/following-sibling::pre") %>% 
  xml2::xml_text() %>% 
  stringr::str_split("\r\n") %>% 
  purrr::modify_depth(1,~{setNames(.,str_extract(.,".*?(?=\\s)"))})
parte<-bind_rows(!!!partes)

ementa<- principal %>% 
  xml2::xml_find_all("//div[contains(@style,'line-height: 150%;text-align: justify;')]") %>% 
  xml2::xml_text()

decisao<-principal %>% 
  xml2::xml_find_all("//div[contains(@style,'text-align:justify; color: #385260; font-weight: normal; font-size: 11px')]") %>% 
  xml2::xml_text()




  

  
  
  
  

  
  
  map(t) %>% 
pa<-partes %>% 
  do.call(rbind,.)

pa1<-map(pa,t)

pa2<-do.call(rbind,pa1)

ementa<- principal %>% 
  xml2::xml_find_all("//div[contains(@style,'line-height: 150%;text-align: justify;')]") %>% 
  xml2::xml_text()

decisao<-principal %>% 
  xml2::xml_find_all("//div[contains(@style,'text-align:justify; color: #385260; font-weight: normal; font-size: 11px')]") %>% 
  xml2::xml_text()
final<-data.frame()
})

b<-base %>% purrr::reduce(rbind)



url_andamento<-principal %>% 
  xml2::xml_find_all("//p/strong/a/@href") %>% 
  xml2::xml_text() %>% 
  str_extract("numero.*") %>% 
  str_c("http://www.stf.jus.br/portal/processo/verProcessoAndamento.asp?",.)


  
  


recurso<-url_andamento[1] %>% 
  httr::GET() %>% 
  httr::content() %>% 
  xml_find_first("//h3/strong") %>% 
  xml_text()

origem<-url_andamento[1] %>% 
  httr::GET() %>% 
  httr::content() %>% 
  xml_find_first("//tr[contains(td,'Origem:')]//strong") %>% 
  xml_text()

relator_atual<-url_andamento[1] %>% 
  httr::GET() %>% 
  httr::content() %>% 
  xml_find_first("//tr[contains(td,'Relator atual')]//strong") %>% 
  xml_text()

requerente<-url_andamento[1] %>% 
  httr::GET() %>% 
  httr::content() %>% 
  xml_find_first("//tr[contains(td,'RECTE.')]//strong") %>% 
  xml_text()

paciente<-url_andamento[1] %>% 
  httr::GET() %>% 
  httr::content() %>% 
  xml_find_first("//tr[contains(td,'PACTE.')]//strong") %>% 
  xml_text()

impetrante<-url_andamento[1] %>% 
  httr::GET() %>% 
  httr::content() %>% 
  xml_find_first("//tr[contains(td,'IMPTE.')]//strong") %>% 
  xml_text()

coator<-url_andamento[1] %>% 
  httr::GET() %>% 
  httr::content() %>% 
  xml_find_first("//tr[contains(td,'COATOR')]//strong") %>% 
  xml_text()

andamento<-url_andamento[1] %>% 
  xml2::read_html() %>% 
  rvest::html_nodes(xpath="//table[@class='resultadoAndamentoProcesso']") %>% 
  rvest::html_table(header=T,fill=TRUE) %>% 
  magrittr::extract2(1)


inteiro_teor<-url_andamento[1] %>% 
  httr::GET() %>% 
  httr::content() %>% 
  xml_find_first("//td") %>% 
  xml_text()
  
  http://www.stf.jus.br/portal/processo/verProcessoPeca.asp?id=313194342&tipoApp=.pdf
href="verProcessoPeca.asp?id=313194342&tipoApp=.pdf"
  
