## This function gets the urls according to the search parameters provided for
## stf_metadata 

stf_url<-function(x,y){
  
  y<-switch(y,
            acordaos="&base=baseAcordaos",
            monocraticas="&base=baseMonocraticas",
            sumulas="&base=baseSumulas",
            informatico="&base=basePresidencia",
            repercussao_geral="&base=baseRepercussao",
            sumulas_vinculantes="&base=baseSumulasVinculantes",
            questoes_ordem="&base=baseQuestoes"
            
            
  )
  
  url1<-stringr::str_c("http://www.stf.jus.br/portal/jurisprudencia/listarConsolidada.asp?txtPesquisaLivre=",x,y)
  url1<-URLencode(url1)
  numero_tinyurl<-httr::GET(url1) %>% 
    httr::content() %>% 
    xml2::xml_find_all("//*[@class='linkPagina']|//*[@class='linkPagina']/@href") %>%
    xml2::xml_text()
  paginas<-stringr::str_extract(numero_tinyurl[[1]],"\\d+") %>% 
    as.numeric() %>% 
    magrittr::divide_by(10) %>% 
    ceiling()
  tinyURL<-numero_tinyurl[[2]]
  urls <- stringr::str_c("http://www.stf.jus.br/portal/jurisprudencia/",tinyURL,"&pagina=",1:paginas)
}


stf_urls<-purrr::possibly(stf_url,"ignore")


stf_parts_names<-function(z){
  z %>% 
    purrr::map_chr(~{
      .x %>% 
        stringr::str_replace(stringr::regex("(adv|dpu).*\\s*",ignore_case = T),"Advogado") %>% 
        stringr::str_replace(stringr::regex("AG.*E.*\\s*",ignore_case = T),"Agravante") %>% 
        stringr::str_replace(stringr::regex("AG.*(o|a).*\\s*",ignore_case = T),"Agravado") %>% 
        stringr::str_replace(stringr::regex(".*(COATOR|coatro|autoridade).*\\s*",ignore_case = T),"Coator") %>% 
        stringr::str_replace(stringr::regex("emb.*(o|a).*\\s*",ignore_case = T),"Embargado") %>% 
        stringr::str_replace(stringr::regex("emb.*e.*\\s*",ignore_case = T),"Embargante") %>% 
        stringr::str_replace(stringr::regex("EXT.*\\s*",ignore_case = T),"Extraditando") %>% 
        stringr::str_replace(stringr::regex("imp.*d.*\\s*",ignore_case = T),"Impetrado") %>% 
        stringr::str_replace(stringr::regex("imp.*t.*\\s*|IMPRE\\s*",ignore_case = T),"Impetrante") %>% 
        stringr::str_replace(stringr::regex("^p(a|c|e|t).*\\s*",ignore_case = T),"Paciente") %>% 
        stringr::str_replace(stringr::regex(".*rec.*e.*\\s*",ignore_case = T),"Recorrente") %>% 
        stringr::str_replace(stringr::regex(".*rec.*(o|a).*\\s*",ignore_case = T),"Recorrido") %>% 
        stringr::str_replace(stringr::regex(".*req.*e.*\\s*",ignore_case = T),"Requerente") %>% 
        stringr::str_replace(stringr::regex(".*req.*(o|a).*\\s*",ignore_case = T),"Requerido") %>%
        stringr::str_replace(stringr::regex("^proc.*\\s*",ignore_case = T),"Procurador") %>% 
        stringr::str_replace(stringr::regex("^sus.*e.*\\s*",ignore_case = T),"Suscitante") %>% 
        stringr::str_replace(stringr::regex("^sus.*(o|a).*\\s*",ignore_case = T),"Suscitado") %>% 
        stringr::str_replace(stringr::regex(".*curiae.*\\s*",ignore_case = T),"Amicus_curiae") %>% 
        stringr::str_replace(stringr::regex("rc.*e.*\\s*",ignore_case = T),"Reclamante") %>% 
        stringr::str_replace(stringr::regex("rc.*(o|a).*\\s*",ignore_case = T),"Reclamado") %>% 
        stringr::str_replace(stringr::regex("intd(o|a).*\\s*",ignore_case = T),"Interessado") %>% 
        stringr::str_replace(stringr::regex("r\u00E9u*.*\\s*",ignore_case = T),"R\u00E9u") %>% 
        stringr::str_replace(stringr::regex("autor.*",ignore_case = T),"Autor") %>% 
        stringr::str_replace(stringr::regex("litis.*pass.*",ignore_case = T),"Listisconsorte_passivo") %>% 
        stringr::str_replace(stringr::regex("litis.*at.*",ignore_case = T),"Listisconsorte_ativo") %>% 
        stringr::str_replace(stringr::regex("DND(oa).*",ignore_case = T),"Denunciado") %>% 
        stringr::str_replace(stringr::regex("inves(oa).*",ignore_case = T),"Investigado")
      
    })
}


#' Function stf_metadata
#'
#' This function returns metadada from Brazilian Supreme Court precedents
#' @param open_search Words to be searched
#' @param database Character string with one of these seven options:
#'    "acordaos","sumulas","monocraticas","presidencia",
#'    "sumulas_vinculantes","repercussao_geral","questoes_ordem".
#'    Default is "acordaos".
#' @param parts_names Logical. If TRUE (default), it will attempt to fix 
#'    parts prefixes.
#' @keywords stf, precedents, metadata
#' @return Dataframe with the metadata
#' @export
stf_metadata<-function(open_search,database="acordaos",parts_names=TRUE){
  
  urls<-stf_urls(x=open_search,y=database)
  
  assertthat::assert_that(urls[1]!="ignore",msg="No file was found in the chosen database")
  
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
    
    if(database=="acordaos"){
      classe<-recurso %>% 
        purrr::map_chr(~stringr::str_trim(.x[[6]]))
    }else{
      classe<-recurso %>% 
        purrr::map_chr(~stringr::str_trim(.x[[2]]))
    }
    
    
    if(database=="acordaos"){
      relator<-recurso %>% 
        purrr::map_chr(~{
          .x[[7]] %>% 
            stringr::str_extract("(?<=Relator\\(a\\)\\:).*?(?=Relator|Julgamento)") %>% 
            stringr::str_extract("(?<=Min\\.\\s).*")
        })
    }else{
      relator<-recurso %>% 
        purrr::map_chr(~{
          .x[[3]] %>% 
            stringr::str_extract("(?<=Relator\\(a\\)\\:).*?(?=Relator|Julgamento)") %>% 
            stringr::str_extract("(?<=Min\\.\\s).*")
        })
    }
    
    if(database=="acordaos"){
      relator_acordao<- recurso %>% 
        purrr::map_chr(~{
          .x[[7]] %>% 
            stringr::str_extract("(?<=Relator\\(a\\)\\sp\\/\\sAc\u00F3rd\u00E3o\\:).*(?=Julgamento)") %>% 
            stringr::str_extract("(?<=Min\\.\\s).*")
        })
    }
    
    
    
    if(database=="acordaos"){
      data_julgamento<- recurso %>% 
        purrr::map_chr(~{
          .x[[7]] %>% 
            stringr::str_extract("\\d{2}\\/\\d{2}\\/\\d{4}")
        })
    }else{
      data_julgamento<- recurso %>% 
        purrr::map_chr(~{
          .x[[3]] %>% 
            stringr::str_extract("\\d{2}\\/\\d{2}\\/\\d{4}")
        })
    }
    
    
    if(database=="acordaos"){
      orgao_julgador<- recurso %>% 
        purrr::map_chr(~{
          .x[[7]] %>% 
            stringr::str_extract("(?<=\u00D3rg\u00E3o\\sJulgador\\:).*")
        })
    }
    
    publicacao<-principal %>% 
      xml2::xml_find_all("//p[strong='Publica\u00E7\u00E3o']/following-sibling::*[1]") %>% 
      xml2::xml_text()
    
    data_publicacao<- publicacao %>%
      stringr::str_extract("(?<=PUBLIC\\s|DJ\\s)\\d{2}.\\d{2}.\\d{4}")
    
    if(database=="acordaos"){
      eletronico<-publicacao %>% 
        stringr::str_detect(stringr::regex("ELETR\u00D4NICO",ignore_case=TRUE))
    }
    
    partes<-principal %>% 
      xml2::xml_find_all("//p[strong[contains(.,'Parte')]]/following-sibling::pre[1]") %>% 
      xml2::xml_text() %>% 
      stringr::str_extract_all("\\w.*\\:.*(\r\n)*\\w*?") %>% 
      
      purrr::modify_depth(1,~{
        .x %>% 
          setNames(stringr::str_extract(.,".*(?=\\:)"))
      })
    
    partes<-dplyr::bind_rows(!!!partes)
    
    partes<-partes %>%
      purrr::map_dfr(~stringr::str_replace(.x,".*?(\\:\\s)","")) 
    partes<-partes %>% 
      dplyr::select(-dplyr::matches(stringr::regex("rela|red.*",ignore_case=TRUE))) 
    
    if(parts_names){
      names(partes)<-stf_parts_names(z=names(partes))
    } 
    
    if(database=="acordaos"){
      ementa<- principal %>% 
        
        xml2::xml_find_all("//div[contains(@style,'line-height: 150%;text-align: justify;')]") %>% 
        xml2::xml_text()
    }
    
    if(database=="acordaos"){
      decisao_tag<-principal %>% 
        xml2::xml_find_all("//strong[div/@style='line-height: 150%;text-align: justify;']/following-sibling::p[1]") %>% 
        xml2::xml_text()
      
      
      decisao<-principal %>% 
        xml2::xml_find_all("//strong[div/@style='line-height: 150%;text-align: justify;']/following-sibling::p[1]/../div[1]") %>% 
        xml2::xml_text()
      
      
      decisao<-ifelse(decisao_tag=="Decis\u00E3o",decisao,"inexistente")
    }
    
    if(database=="monocraticas"){
      decisao<-principal %>% 
        xml2::xml_find_all("//p[strong='Decis\u00E3o']/following-sibling::pre") %>% 
        xml2::xml_text()
    }
    
    if(database=="acordaos"){
      voto<-decisao %>% purrr::map_chr(~{
        if
        (stringr::str_detect(.x,stringr::regex("maioria",ignore_case=TRUE))){
          "maioria"
        }else if (stringr::str_detect(.x,stringr::regex("un(a|\u00E2)nim.*",ignore_case=TRUE))){
          "un\u00E2nime"
        }else if (stringr::str_detect(.x,stringr::regex("empate",ignore_case=TRUE))){
          "empate"
        }else
          NA
      })
    }
    
    if(database=="acordaos"){
      url_inteiro_teor<-principal %>% 
        xml2::xml_find_all("//li/a[contains(@href,'obterInteiroTeor')]") %>% 
        xml2::xml_attrs() %>%
        stringr::str_extract("inteiroTeor.*") %>% 
        stringr::str_c("http://www.stf.jus.br/portal/",.)
    }
    
    
    url_andamento<-principal %>% 
      xml2::xml_find_all("//div[@class='abasAcompanhamento']/ul[@class='abas']/li/a[contains(@href,'verProcessoAndamento')]") %>%
      xml2::xml_attrs() %>%
      stringr::str_extract("numero.*") %>%
      stringr::str_c("http://www.stf.jus.br/portal/processo/verProcessoAndamento.asp?",.)
    
    
    if (database == "acordaos") {
      data.frame(processo, origem, classe, relator, relator_acordao, 
                 data_julgamento, data_publicacao, orgao_julgador,
                 eletronico, ementa, voto,decisao, url_inteiro_teor,
                 url_andamento, partes, stringsAsFactors = FALSE)
    }else{
      data.frame(processo,origem,classe,relator,data_julgamento,data_publicacao,decisao,url_andamento,partes,stringsAsFactors = FALSE)
    }
  },data.frame(processo=NA_character_,origem=NA_character_,classe=NA_character_,relator=NA_character_,relator_acordao=NA_character_,data_julgamento=NA_character_,data_publicacao=NA_character_,orgao_julgador=NA_character_,eletronico=NA,ementa=NA_character_,voto=NA_character_,decisao=NA_character_,url_inteiro_teor=NA_character_,url_andamento=NA_character_,partes=NA_character_),
  quiet = FALSE
  ))
}




