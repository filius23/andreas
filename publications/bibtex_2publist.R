# The function: bibtex_2academic
library(tidyverse)

bibtex_2list <- function(bibfile) {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(RefManageR, dplyr, stringr, anytime, tidyr, stringi)
  
  options(encoding="UTF-8")
  # avoid umlaut probelms -----
  uml_repl <- function(x) {
    stringi::stri_replace_all_fixed(
      x, 
      c("â€™","Ã¤","Ã¶","Ã¼","â›”","Â©","â€“","Ãœ"), 
      c( "'", "ä", "ö", "ü",   "",  "©", "-" ,"Ü"), 
      vectorize_all = FALSE
    )
  }
  
  
  # Import the bibtex file and convert to data.frame- ------
  mypubs <- RefManageR::ReadBib(bibfile, check = "warn", .Encoding = "UTF-8")
  mypubs <- mypubs %>%
    as_tibble() %>%
    mutate(across(everything(),~uml_repl(.x)),
           across(everything(),~stringr::str_squish(.x)),
           rnames = mypubs %>% as.data.frame() %>% rownames() )
  
  
  # Customize for more than one editor
  mypubs$editor<- gsub(
    pattern = (' and '),
    replacement = ', ',
    x = mypubs$editor
  ) %>%  stri_replace_last_fixed(., ',', ' &')
  
  
  # create helper var for when number or volume is missing to avoid ugly NAs
  mypubs$vol_num <- coalesce(mypubs$volume,mypubs$number)
  
  # assign "categories" to the different types of publications
  mypubs   <- mypubs %>%
    dplyr::mutate(
      pubtype = dplyr::case_when(bibtype == "Article" ~ "2",
                                 bibtype == "Article in Press" ~ "2",
                                 bibtype == "InProceedings" ~ "1",
                                 bibtype == "Proceedings" ~ "1",
                                 bibtype == "Conference" ~ "1",
                                 bibtype == "Conference Paper" ~ "1",
                                 bibtype == "MastersThesis" ~ "3",
                                 bibtype == "PhdThesis" ~ "3",
                                 bibtype == "Manual" ~ "4",
                                 bibtype == "TechReport" ~ "4",
                                 bibtype == "Book" ~ "5",
                                 bibtype == "InCollection" ~ "6",
                                 bibtype == "InBook" ~ "6",
                                 bibtype == "Misc" ~ "0",
                                 TRUE ~ "0"),
      
      # add preprint repo as journal for preprints
      journal = ifelse(pubtype==0 & is.na(journal),publisher,journal)
      )
  #   ex <- mypubs[6,]
  
  
  # prepare authors
  auth_fun <- function(w1) {
    w2=str_split(w1,pattern = " ",n = 2) %>% unlist(.)
    rev_order = length(w2):1
    # reversed characters
    reversed_chars = w2[rev_order]
    # collapse reversed characters
    paste(reversed_chars, collapse = ", ")
  }
  
  
  mypubs$authors <- 
    map_chr(mypubs$author, function(x){
      authors <-  str_split(x, "\\sand\\s") %>% unlist(.) 
      if(length(authors) == 2) paste0(authors,collapse = " & ") 
      else paste0(authors,collapse = ", ") 
        # map(.,~auth_fun(.x)) %>% unlist(.) 
    }) 
  
  # prepare Links 
  mypubs$link <- coalesce(mypubs$url,
                          paste0("www.doi.org/",mypubs$doi),
                          ) %>% 
    str_remove(.,"www.doi.org/NA")
  
  

  pub_rmd <- 
    mypubs %>% 
      mutate(date = lubridate::ymd(paste0(year,"-",month,"-1"))) %>% 
      arrange(desc(date),desc(pubtype)) %>% 
      group_nest(row_number()) %>% 
      pull(data) %>% 
      map_chr(function(x){

        templat1 <- case_when(
          x$pubtype %in% 6 ~                  "[**{title}**]({link}). \n\n {authors}. ({year}) In {editor}: {booktitle}. {pages}, {publisher}.\n\n",
          x$pubtype %in% 0 ~                  "[**{title}**]({link}). \n\n {authors}. ({year}) {journal} [working paper].\n\n",   # add [working paper] to title for preprints
          is.na(x$number) & is.na(x$volume) ~ "[**{title}**]({link}). \n\n {authors}. ({year}) {journal}, {pages}.\n\n",
          is.na(x$number) | is.na(x$volume) ~ "[**{title}**]({link}). \n\n {authors}. ({year}) {journal}, {vol_num}, {pages}.\n\n",
          TRUE ~                              "[**{title}**]({link}). \n\n {authors}. ({year}) {journal}, ({volume}) {number}, {pages}.\n\n"
          )
        templat2 <- case_when(
          x$pubtype %in% 6 ~                  "[**{title}**]({link}). \n\n {authors} \n\n In {editor}: {booktitle}. {pages}. ({year}) \n\n",
          x$pubtype %in% 0 ~                  "[**{title}**]({link}). \n\n {authors} \n\n {journal} [working paper] ({year}) \n\n",   # add [working paper] to title for preprints
          is.na(x$number) & is.na(x$volume) ~ "[**{title}**]({link}). \n\n {authors} \n\n {journal} ({year}) \n\n",
          is.na(x$number) | is.na(x$volume) ~ "[**{title}**]({link}). \n\n {authors} \n\n {journal} ({year}) \n\n",
          TRUE ~                              "[**{title}**]({link}). \n\n {authors} \n\n {journal} ({year}) \n\n"
          )
        
        glue::glue_data(x, templat2)
      }) %>% 
      str_remove_all(.,"NA, ")
  
  pub_rmd
}

# Run the function
# bibtex_2list(bibfile  = "./publications/eigene.bib") %>% 
#   str_replace_all(.,"Ã¶","ö") %>% 
#   str_replace_all(.,"Ã¤","ä") %>% 
#   str_replace_all(.,"Ã¼","ü") %>% 
#   str_replace_all(.,"â€™","'") #%>% 
  #kableExtra::kable(.,format = "markdown")
