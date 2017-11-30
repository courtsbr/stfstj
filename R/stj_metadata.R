#' Function stj_metadata
#'
#' This function returns metadada from Brazilian Federal High Court precedents
#' @param open_search Words to be searched
#' @param database Character string with one of these seven options:"acordaos","sumulas","monocraticas","presidencia",
#'    "sumulas_vinculantes","repercussao_geral","questoes_ordem".
#'    Default is "acordaos".
#' @keywords stj, precedents, metadata
#' @return Dataframe with the metadata

stj_metadata<-function(open_search,database="ACOR"){

if(phrase==TRUE){open_search<-deparse(open_search)}

open_search<-URLencode(open_search)

url1<-"http://www.stj.jus.br/SCON/"
url2<-"http://www.stj.jus.br/SCON/pesquisar.jsp"
url3<-"http://www.stj.jus.br/SCON/jurisprudencia/toc.jsp?tipo_visualizacao=&livre=&b=&p=true&t=JURIDICO&l=10&i=10"

h<-httr::GET(url1)

body<-list(
acao="pesquisar",
novaConsulta="true",
i="1",
data="",
livre=open_search,
ref="",
opAjuda="SIM",
tipo_visualizacao="null",
thesaurus="null",
p="true",
operador="e",
processo="",
livreMinistro="",
relator="",
data_inicial="",
data_final="",
tipo_data="DTDE",
livreOrgaoJulgador="",
orgao="",
ementa="",
siglajud="",
numero_leg="",
tipo1="",
numero_art1="",
tipo2="",
numero_art2="",
tipo3="",
numero_art3="",
nota="",
b=database)

res <- httr::POST(url2,body=body,encode="form",
           httr::add_headers(`Referer`="http://www.stj.jus.br/SCON/"))
                    
 
content_res<-res %>%
       httr::content()

link_acor<-content_res %>% 
  xml2::xml_find_first(xpath="//span/a[contains(@href,'jurisprudencia') and contains(@href,'ACOR')]") %>%
  xml2::xml_attrs() %>% 
  unlist() %>% 
  stringr::str_c("http://www.stj.jus.br",.)

pages_acor<-content_res %>% 
  xml2::xml_find_first(xpath="//span/a[contains(@href,'jurisprudencia') and contains(@href,'ACOR')]") %>%
  xml2::xml_text() %>% 
  stringr::str_extract("\\d+") %>% 
  as.numeric() %>% 
  divide_by(10) %>% 
  ceiling()



  res2<-httr::GET("http://www.stj.jus.br",
                path="/SCON/jurisprudencia/toc.jsp",
                query=list(tipo_visualizacao="",
                           livre=open_search,
                           b=field,
                           p="true",
                           l=10,
                           t='JURIDICO',
                           i=.x),
     httr::add_headers(`Referer`=url2))

principal_acor <-res2 %>%
  httr::content()

processo<-principal_acor %>% 
  xml2::xml_find_all("//div[@class='docTexto']/text()[following-sibling::br][1]") %>% 
  xml2::xml_text() %>% 
  stringr::str_trim()

origem<-processo %>% 
  stringr::str_extract("\\w{2}$")

classe<-principal_acor %>% 
  xml2::xml_find_all("//div[@class='docTexto']/text()[following-sibling::br][2]") %>% 
  xml2::xml_text() %>% 
  stringr::str_trim()

processo_stj<-principal_acor %>% 
  xml2::xml_find_all("//div[@class='docTexto']/text()[preceding-sibling::br][2]") %>% 
  xml2::xml_text() %>% 
  stringr::str_trim()

relator<-principal_acor %>% 
  xml2::xml_find_all("//div/h4[text()='Relator(a)']/following-sibling::pre[@class='docTexto']") %>% 
  xml2::xml_text() %>%
  stringr::str_extract("(?<=Ministro\\s).*(?=\\s\\()")

orgao_julgador<-principal_acor %>% 
  xml2::xml_find_all("//div/h4[text()='\u00D3rg\u00E3o Julgador']/following-sibling::pre[@class='docTexto']") %>% 
  xml2::xml_text()

data_julgamento<-principal_acor %>% 
  xml2::xml_find_all("//div/h4[text()='Data do Julgamento']/following-sibling::pre[@class='docTexto']") %>% 
  xml2::xml_text() %>% 
  lubridate::dmy()

publicacao<-principal_acor %>% 
  xml2::xml_find_all("//div/h4[text()='Data da Publica\u00E7\u00E3o/Fonte']/following-sibling::pre[@class='docTexto']") %>% 
  xml2::xml_text()

fonte<-publicacao %>% stringr::str_extract("\\w+")

data_publicacao<-lubridate::dmy(publicacao)

ementa<-principal_acor %>% 
  xml2::xml_find_all("//div/h4[text()='Ementa']/following-sibling::pre[@class='docTexto']") %>%
  xml2::xml_text()

decisao<-principal_acor %>% 
  xml2::xml_find_all("//div/h4[text()='Ac\u00F3rd\u00E3o']/following-sibling::pre[@class='docTexto']") %>%
  xml2::xml_text()

url_inteiro_teor<-httr::GET("https://ww2.stj.jus.br/processo/revista/inteiroteor/?num_registro=201701547105&dt_publicacao=14/11/2017",
                            httr::set_config( httr::config( ssl_verifypeer = 0L )))

url_acompanhamento<-httr::GET("https://ww2.stj.jus.br/webstj/Processo/justica/jurisprudencia.asp?valor=201702579361",
                              httr::set_config( httr::config( ssl_verifypeer = 0L )))


}


