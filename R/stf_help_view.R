#' Function stf_help_view
#'
#' This function displays STF's help page on Rstudio viewer pane. 
#' @keywords stf, help
#' @return This function has only side effect, it doesn't return anything.
#' @examples 
#' \dontrun{
#' stf_help_view()
#' }

#' @export
stf_help_view<-function(){
  url<-"http://www.stf.jus.br/portal/cms/verTexto.asp?servico=jurisprudenciaPesquisaGeral&pagina=ajudaPesquisaJurisprudencia&popup=S"
  getOption("viewer")(url)
  
}


