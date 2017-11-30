#' Function stf_thesaurusDF
#'
#' This function returns a data frame with STF thesaurus
#' @param letter specify the letter
#' @param term specify the word or phrase to be searched
#' @keywords stj, thesaurus
#' @return Dataframe with the thesaurus


stf_thesaurusDF<-function(letter="",term=""){
url1<-"http://www.stf.jus.br/portal/jurisprudencia/pesquisarVocabularioJuridico.asp"
url2<-"http://www.stf.jus.br/portal/jurisprudencia/listarTesauro.asp"
term<-deparse(term)
if(length(letter)==1){

httr::POST(url2,body=list(passaletra=letter,
                              txtPesquisaLivre=term),
               encode="form",
               httr::add_headers(`Referer`=url1)) %>% 
  httr::content() %>%
  rvest::html_node(xpath="//table") %>% 
  rvest::html_table(header=FALSE,fill=T)
}else{
  letter %>% 
    purrr::map_dfr(~{
    httr::POST(url2,body=list(passaletra=.x,
                                txtPesquisaLivre=term),
                 encode="form",
                 httr::add_headers(`Referer`=url1)) %>% 
    httr::content() %>%
    rvest::html_node(xpath="//table") %>% 
    rvest::html_table(header=FALSE,fill=T)
    })
}
}

