#' Function stf_thesaurus
#'
#' This function displays STF's thesaurus. 
#' @keywords stf, thesaurus
#' @return This function has only side effect, it doesn't return anything.
#' @examples 
#' \dontrun{
#' stf_thesaurus()
#' }

#' @export
stf_thesaurus<-function(){
  url<-"http://www.stf.jus.br/portal/jurisprudencia/pesquisarVocabularioJuridico.asp"
  getOption("viewer")(url)
}


