#' Function stj_metadata
#'
#' This function returns metadada from Brazilian Federal High Court precedents
#' @param open_search Words to be searched
#' @param database Character string with one of these seven options:"acordaos","sumulas",
#'    "monocraticas',"informativo"
#'    Default is "acordaos".
#' @param phrase Logical. If TRUE, it will deparse the sentence.
#' @param start_date Optional. Character string  in the
#'     format: "dd/mm/yyyy" with the start of period to search for.
#' @param end_date  Optional. Character string  in the
#'     format: "dd/mm/yyyy" with the end of period to search for.
#' @keywords stj, precedents, metadata
#' @return Dataframe with the metadata

stj_metadata <- function(open_search,database = "acordaos",phrase=FALSE,start_date="",end_date=""){
  
  
  dates<-sprintf("@DTDE >= %s e @DTDE <= %s",
                 start_date %>% lubridate::dmy() %>% stringr::str_replace_all("\\D",""),
                 
                 end_date %>% lubridate::dmy() %>% stringr::str_replace_all("\\D",""))
  
  ## Switch the database value according to argument option.
  database <- switch(database,
                     acordaos = "ACOR",
                     sumulas = "SUMU",
                     monocraticas = "DTXT",
                     informativo = "INFJ"
  )
  
  ## Deparse the sentence when the user wants to search for the whole phrase
  if (phrase == TRUE) {
    open_search <- deparse(open_search)
  }
  
  ## Urls needed to connect
  url1 <- "http://www.stj.jus.br/SCON/"
  url2 <- "http://www.stj.jus.br/SCON/pesquisar.jsp"
  url3 <- "http://www.stj.jus.br/SCON/jurisprudencia/toc.jsp?tipo_visualizacao=&livre=&b=&p=true&t=JURIDICO&l=10&i=10"
  
  ## This the body that receives the parameters from user choices
  body <- list(
    acao = "pesquisar",
    novaConsulta = "true",
    i = "1",
    data = dates,
    livre = open_search,
    ref = "",
    opAjuda = "SIM",
    tipo_visualizacao = "null",
    thesaurus = "null",
    p = "true",
    operador = "e",
    processo = "",
    livreMinistro = "",
    relator = "",
    data_inicial = start_date,
    data_final = end_date,
    tipo_data = "DTDE",
    livreOrgaoJulgador = "",
    orgao = "",
    ementa = "",
    siglajud = "",
    numero_leg = "",
    tipo1 = "",
    numero_art1 = "",
    tipo2 = "",
    numero_art2 = "",
    tipo3 = "",
    numero_art3 = "",
    nota = "",
    b = database)
  
  ## Connect to the webpage and grabs the number of results
  content_res <- httr::POST(url2,body = body,encode = "form",httr::add_headers(`Referer` = url1)) %>% 
    httr::content()
  
  ## Extracts the number of results and creates a character vector with the pages' number
  pages_acor <- content_res %>% 
    xml2::xml_find_first(xpath = "//span/a[contains(@href,'jurisprudencia') and contains(@href,'ACOR')]") %>%
    xml2::xml_text() %>% 
    stringr::str_extract("\\d+") %>% 
    as.numeric() %>% 
    seq(1,.,10) %>% 
    as.character()
  
  
  pages_acor %>% 
    
    purrr::map_dfr(purrr::possibly(~{   
    
    principal_acor <- httr::GET("http://www.stj.jus.br",
                                path = "/SCON/jurisprudencia/toc.jsp",
                                query = list(tipo_visualizacao = "",
                                             livre = open_search,
                                             b = database,
                                             data=dates,
                                             p = "true",
                                             l = 10,
                                             t = 'JURIDICO',
                                             i = .x),
                                httr::add_headers(`Referer` = url2)) %>% 
      httr::content()
    
    
    processo <- principal_acor %>% 
      xml2::xml_find_all("//div[@class='docTexto']/text()[following-sibling::br][1]") %>% 
      xml2::xml_text() %>% 
      stringr::str_trim()
    
    origem <- processo %>% 
      stringr::str_extract("\\w{2}$")
    
    ## Extracts the 
    classe <- principal_acor %>% 
      xml2::xml_find_all("//div[@class='docTexto']/text()[following-sibling::br][2]") %>% 
      xml2::xml_text() %>% 
      stringr::str_trim()
    
    ## Extracts the docket number of the case
    processo <- principal_acor %>% 
      xml2::xml_find_all("//div[@class='docTexto']/text()[preceding-sibling::br][2]") %>% 
      xml2::xml_text() %>% 
      stringr::str_trim()
    
    ## Extracts the name of opinion's reporter
    relator <- principal_acor %>% 
      xml2::xml_find_all("//div/h4[text()='Relator(a)']/following-sibling::pre[@class='docTexto']") %>% 
      xml2::xml_text() %>%
      stringr::str_match("(?:.*?\\s)(.*)(?:\\s\\(.*)") %>% 
      magrittr::extract(,2)
    
    ## Extracts the panel's name
    orgao_julgador <- principal_acor %>% 
      xml2::xml_find_all("//div/h4[text()='\u00D3rg\u00E3o Julgador']/following-sibling::pre[@class='docTexto']") %>% 
      xml2::xml_text()
    
    ## Extracts the date the case was decided 
    data_julgamento <- principal_acor %>% 
      xml2::xml_find_all("//div/h4[text()='Data do Julgamento']/following-sibling::pre[@class='docTexto']") %>% 
      xml2::xml_text() %>% 
      lubridate::dmy()
    
    ## Extracts publication's source and date
    publicacao <- principal_acor %>% 
      xml2::xml_find_all("//div/h4[text()='Data da Publica\u00E7\u00E3o/Fonte']/following-sibling::pre[@class='docTexto']") %>% 
      xml2::xml_text()
    
    fonte <- publicacao %>% stringr::str_extract("\\w+")
    
    data_publicacao <- publicacao %>%
      stringr::str_extract("\\d{2}.\\d{2}.\\d{4}") %>% 
      lubridate::dmy()
    
    ## Extracts the syllabus or headnote from the opinion
    ementa <- principal_acor %>% 
      xml2::xml_find_all("//div/h4[text()='Ementa']/following-sibling::pre[@class='docTexto']") %>%
      xml2::xml_text()
    
    ## Extracts the decision  
    decisao <- principal_acor %>% 
      xml2::xml_find_all("//div/h4[text()='Ac\u00F3rd\u00E3o']/following-sibling::pre[@class='docTexto']") %>%
      xml2::xml_text()
    
    ## Extracts the link of the  opinion   
    url_inteiro_teor<-principal_acor %>% 
      xml2::xml_find_all("//div[@id='acoesdocumento']/a[1]/@href") %>% 
      xml2::xml_text() %>% 
      {stringr::str_c("https://ww2.stj.jus.br/processo/revista/inteiroteor/?",stringr::str_extract(.,"num_.*?(?=')"))}
    
    url_andamento<- principal_acor %>% 
      xml2::xml_find_all("//div[@id='acoesdocumento']/a[2]/@href") %>% 
      xml2::xml_text() %>% 
      {stringr::str_c("http://www.stj.jus.br/webstj/Processo/justica/jurisprudencia.asp?valor=",stringr::str_extract(.,"\\d+"))}
    

  data.frame(processo,origem,classe,relator,orgao_julgador,data_julgamento,
             data_publicacao,fonte,ementa,decisao,url_inteiro_teor,url_andamento,stringsAsFactors = FALSE)  
  },
  data.frame(processo=NA_character_,origem=NA_character_,classe=NA_character_,relator=NA_character_,orgao_julgador=NA_character_,data_julgamento=NA_character_,
             data_publicacao=NA_character_,fonte=NA_character_,ementa=NA_character_,decisao=NA_character_,url_inteiro_teor=NA_character_,url_andamento=NA_character_,stringsAsFactors = FALSE)  
  ))

  }
  
  
  
  
  
  
  