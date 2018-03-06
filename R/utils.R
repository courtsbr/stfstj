# Extracts dates in Portuguese formats from strings and convert them to class Date.
pt_time_extract<-function(string){
  string %>% 
  stringr::str_extract("\\d{2}/\\d{2}/\\d+") %>% 
  lubridate::dmy()
}


