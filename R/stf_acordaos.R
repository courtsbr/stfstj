#' Function stf_acordaos
#'
#' This functions reads the pdfs with the whole decisions according with the metadata
#'     obtained by the stf_metadata function.
#' @param df data frame with at least two columns, one with the url,
#'    and the other, named electronic, with a logical vector informing wether the pdf is text(TRUE)
#'    or image(FALSE).
#' @keywords stf, precedents, inteiro teor, decision
#' @return vector with the whole content of the decisions.
#' @export
stf_acordaos<-function(df){
  stopifnot(is.logical(df$eletronico))
  diretorio<-getwd()
  setwd(tempdir())
  tmp_file<-tempfile(pattern="inteiro",fileext = ".pdf")
  on.exit(setwd(diretorio),unlink(tmp_file))
  purrr::map2_chr(df$url_inteiro_teor,df$eletronico,purrr::possibly(~{
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
   },NA_character_
     ))

}


