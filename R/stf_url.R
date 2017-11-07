pesquisa_livre<-"EXCESSO+ADJ2+PRAZO"

stf_urls<-function(pesquisa_livre){
  url1<-str_c("http://www.stf.jus.br/portal/jurisprudencia/listarConsolidada.asp?txtPesquisaLivre=",pesquisa_livre,"&base=baseAcordaos")
  numero_tinyurl<-httr::GET(url1) %>% 
    httr::content() %>% 
    xml2::xml_find_all("//*[@class='linkPagina']|//*[@class='linkPagina']/@href") %>%
    xml2::xml_text()
  paginas<-stringr::str_extract(numero_tinyurl[[1]],"\\d+") %>% 
    as.numeric() %>% 
    divide_by(10) %>% 
    ceiling()
  tinyURL<-numero_tinyurl[[2]]
  urls<-str_c("http://www.stf.jus.br/portal/jurisprudencia/",tinyURL,"&pagina=",1:paginas)
 return(urls)
  }







s1<-GET(url3) %>% 
  content() %>% 
  xml_find_all("//*[@class='linkPagina']|//*[@class='linkPagina']/@href") %>%
  xml_text()

numero<-str_extract(s1[[1]],"\\d+")
tinyurl<-s1[[2]]           

http://www.stf.jus.br/portal/jurisprudencia/listarJurisprudencia.asp?s1=%28EXCESSO+ADJ2+PRAZO%29&pagina=2&base=baseAcordaos&url=http://tinyurl.com/y7aw5tyj
%>% 
  str_extract("\\d+")
