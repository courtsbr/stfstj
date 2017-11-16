#' Function stf_inteiro_teor
#'
#' Essa função baixa e lê os acórdãos do stf conforme url extraída via função stf_metadata
#' @param urls vetor de urls dos pdfs
#' @keywords stf, jurisprudência, inteiro teor, decisão
#' @import httr
#' @import xml2
#' @import purrr
#' @import magrittr
#' @import textreadr
#' @return vetor com urls dos processos agrupados de 10 em 10

#' @export

stf_inteiro_teor<-function(url){
  tmp_file<-tempfile(pattern="inteiro",fileext = ".pdf")
  url %>% 
    purrr::map_chr(~{
      .x %>% 
        httr::GET(httr::write_disk(path=tmp_file,overwrite = T))
      textreadr::read_document(tmp_file,combine=T)
      
    })
} 
