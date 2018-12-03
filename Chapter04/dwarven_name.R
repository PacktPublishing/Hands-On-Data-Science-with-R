dwarven_name <- function(nametype = 2, gender = 1, surnametype = 2){
  if(!require(httr)){install.packages('httr')}
  library(httr)
  
  form_dt <- list(
    nametype = nametype,
    numnames = 1,
    gender = gender,
    surnametype = surnametype,
    namegenraceid = 1
    )
  
  url <- 'http://www.rdinn.com/generators/1/dwarven_name_generator.php'
  post <- POST(url, body = form_dt)
  page <- content(post, as = 'text')
  
  name <- substr(page,
                 start = regexpr('<td width=\"25%\">',page) + nchar('<td width=\"25%\">'),
                 stop = regexpr('</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;', page) - 1)
  
  return(name)
}