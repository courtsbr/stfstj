#' Function stf_acordaos
#'
#' Essa função baixa e lê os acórdãos do stf conforme url extraída via função stf_metadata
#' @param df dataframe com duas colunas, uma com o vetor de urls dos pdfs e outra com  vetor lógico chamado eletronico. 
#'     Deve-se informar quais são pdfs são eletrônicos (TRUE) e quais,
#'     escaneados (FALSE).
#' @keywords stf, jurisprudência, inteiro teor, decisão
#' @return vetor de textos após conversão dos pdfs
#' @export
stf_acordaos<-function(df){
  stopifnot(is.logical(df$eletronico))
  diretorio<-getwd()
  setwd(tempdir())
  tmp_file<-tempfile(pattern="inteiro",fileext = ".pdf")
  on.exit(setwd(diretorio),unlink(tmp_file))
  purrr::map2_chr(df$url,df$eletronico,~{
    if(.y==TRUE){
          pdftools::pdf_text(.x) %>% 
        paste(sep="\n",collapse="")
        }else{
      httr::GET(.x,httr::write_disk(path=tmp_file),overwrite=T)
          files<-pdftools::pdf_convert(tmp_file,dpi=400)
         files %>% 
          purrr::map_chr(~{
            .x %>% 
              tesseract::ocr(engine=tesseract::tesseract("por"))
            paste(sep="\n",collapse="")
        })
   
        }
   }
     )

}


