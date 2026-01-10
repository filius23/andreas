if (!require("pacman")) install.packages("pacman")
pacman::p_load(RefManageR, dplyr, stringr, anytime, tidyr, stringi)
options(encoding="UTF-8")
  
  # avoid umlaut problems -----
  uml_fix <- function(x) {
    stringi::stri_replace_all_fixed(
      x, 
      c("â€™","Ã¤","Ã¶","Ã¼","â›”","Â©","â€“","Ãœ"), 
      c( "'", "ä", "ö", "ü",   "",  "©", "-" ,"Ü"), 
      vectorize_all = FALSE
    )}

  uml_repl <- function(x) {
  stringi::stri_replace_all_fixed(
    x, 
    c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
    c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
    vectorize_all = FALSE
  )}


# function to create entries --------------
create_qmd <- function(ex,overwrite=T,out_fold) {
  
  # create directory & file name from date and title (first 3 words)
  if (is.na(ex[["year"]]))  ex[["year"]] <- format(Sys.Date(), '%Y') # replace missing date with current year
  
  pub_name <- paste(
    ex[["year"]],
    ex[["title"]] %>% stringr::word(.,start = 1,end = 3) %>% # first 3 words of title
      str_remove_all(.,pattern = "[:punct:]") %>% 
      tolower(.) %>% 
      gsub(.,pattern = "\\s",replacement = "_"),
    sep = "_") %>% 
    uml_repl()
  
  # full file name --> now qmd file!
  filename <- here::here(out_fold,pub_name,"index.qmd")
  
  # directory 
  if (dir.exists( here::here(out_fold,pub_name) ) == F) {
    dir.create( here::here(out_fold,pub_name) )
  }
  
  # write file ------
  if (!file.exists(filename) | overwrite) {
    fileConn <-  filename
    write("---", fileConn)
    
    ## Authors-----
    ## collapse into single string
    auth_qmd <- 
      str_split(ex["author"], " and ") %>% 
      unlist(.) %>% 
      paste0("  - ", .) |> 
      paste0(collapse = "\n")
    write(paste0("author:\n",auth_qmd), fileConn, append = T)
    
    ## Title -----
    ex[["title"]] <- gsub(ex[["title"]],pattern = "'",replacement =  "''")
    write(paste0("title: \'", ex[["title"]],"\'"), fileConn, append = T)
    
    ## Date ----
    date_hugo <- anydate(paste0(ex[["year"]],ex[["month"]],"15",collapse = "-")) 
    write(paste0("date: ",        "\'", date_hugo, "\'" ), fileConn, append = T)
    write(paste0("publishDate: ", "\'", date_hugo, "\'" ), fileConn, append = T)
    
    ## Publication type. -------
    # 0 = Uncategorized, 1 = Conference paper, 2 = Journal article
    # 3 = Manuscript, 4 = Report, 5 = Book,  6 = Book section
    write("publication_types: ", fileConn, append = T)
    write(paste0("- \"",ex[["pubtype"]],"\""),fileConn, append = T)
    
    
    if (exists('ex[["note"]]')) {
      note_url <- ( grepl("osf", ex[["note"]]) & grepl("https", ex[["note"]])  )
    } else {
      note_url <- F
    }
    
    ##  links --------------
    if (!is.na(ex[["doi"]]) | !is.na(ex[["url"]]) | note_url  ) {
      write("links:", fileConn, append = T)
    }
    
    
    ### pdf link -----
    if (!is.na(ex[["url"]])){
      write(" - icon: file-pdf", fileConn, append = T)
      write("   icon_pack: fas", fileConn, append = T)
      write("   name: PDF", fileConn, append = T)
      write(paste0("   url:  \'", ex[["url"]],"\'"), fileConn, append = T)
    }  
    
    ### doi -----
    if (!is.na(ex[["doi"]])){
      write(" - icon: doi", fileConn, append = T)
      write("   icon_pack: ai", fileConn, append = T)
      write("   name: Publication", fileConn, append = T)
      write(paste0("   url:  \'https://doi.org/", ex[["doi"]],"\'"), fileConn, append = T)
    }  
    
    
    ### Code / Replication repo -> saved as note in zotero -----
    if ( note_url ) {
      url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
      code_url <- str_extract(ex[["note"]],url_pattern)
      write(" - icon: osf", fileConn, append = T)
      write("   icon_pack: ai", fileConn, append = T)
      write("   name: Code", fileConn, append = T)
      write(paste0("   url: ", code_url), fileConn, append = T)
    }  
    
    ## tags ----
    tags_hugo <- 
      str_split(ex[["keywords"]], "\\,") %>% 
      unlist(.) %>% 
      str_remove_all(.,pattern = "[:punct:]") %>% 
      str_to_title(.) %>% 
      paste0("- ",.)
    if (!is.na(ex[["keywords"]]))   write(c("","tags:",tags_hugo), fileConn, append = T,  sep = "\n")
    
    
    ## Publication details -----------
    # add Editor, Booktitle etc. if applicable
    ## Journal Article: Use Journal Name -----
    if (!is.na(ex[["journal"]])) {
      publication <-  paste0("publication: \'*", ex[["journal"]],"*") # Journal italic
      
      if (!is.na(ex[["volume"]]))     publication <- paste0(publication," ",
                                                            ex[["volume"]])
      if (!is.na(ex[["number"]]))     publication <- paste0(publication,"(",
                                                            ex[["number"]], ")")
      
      if (grepl("-",ex[["pages"]]))     publication <- paste0(publication,", ",
                                                              ex[["pages"]])
      
      publication <-  paste0(publication,"\'") # close quote
      
    }
    
    ## Book chapter ------
    if (!is.na(ex[["booktitle"]])) {
      if (!is.na(ex[["editor"]]))    publication <- paste0("publication: \'In ", 
                                                           ex[["editor"]], ": ")
      if (!is.na(ex[["booktitle"]])) publication <-  paste0(publication, 
                                                            "*", ex[["booktitle"]],"*.") # Book title italic
      if (!is.na(ex[["pages"]]))     publication <- paste0(publication,
                                                           " (pp.", ex[["pages"]], ").")
      if (!is.na(ex[["publisher"]])) publication <- paste0(publication,
                                                           " ", ex[["publisher"]],"\'")
    }
    
    if(!exists("publication") & is.na(ex$journal)) publication <- paste0("publication: \'",ex$publisher,"\'")
    
    write(publication, fileConn, append = T)
    
    ## Subtitle -----
    # to highlight journal etc on publication page 
    publication_clean <- coalesce(ex$journal,ex$booktitle,ex$publisher)
    write(paste0("subtitle: \'", publication_clean,"\'"), fileConn, append = T)
    
    # generate further fields (empty) ------
    # other possible fields are kept empty. They can be customized later by
    # editing the created md
    
    # write("url_code: ", fileConn, append = T)
    # write("image_preview: ", fileConn, append = T)
    # write("selected: false", fileConn, append = T)
    # write("projects: []", fileConn, append = T)
    
    #links
    # write("url_preprint: \"\"", fileConn, append = T)
    # write("url_dataset: ", fileConn, append = T)
    # write("url_project: ", fileConn, append = T)
    # write("url_slides: ", fileConn, append = T)
    # write("url_video: ", fileConn, append = T)
    # write("url_poster: ", fileConn, append = T)
    
    #other stuff
    # write("math: true", fileConn, append = T)
    # write("highlight: true", fileConn, append = T)
    # Featured image
    # write("[header]", fileConn, append = T)
    # write("image: ", fileConn, append = T)
    # write("caption: ", fileConn, append = T)
    
    write("---", fileConn, append = T) # end YAML
    
    # Abstract ----
    write("#### Abstract", fileConn, append = T) # Abstract header
    write(ex[["abstract"]], fileConn, append = T) # Abstract 
    
    
    write(" ", fileConn, append = T) # add space
    write("**[Back to publication list](/publication)**", fileConn, append = T) # add back link
    
    
    
    # export bibtex-file for citation ----
      ex_df <- as.data.frame(ex) 
      print(class(ex_df))
      print(ex[["rnames"]])
      RefManageR::WriteBib(bib =  RefManageR::as.BibEntry(ex_df),
                           file =  here::here(out_fold,pub_name,paste0("cite.bib")) )
    
  }
}

# prep bibtex file ------------------
  
  my_bibfile <- "publications/eigene.bib"
  # Import the bibtex file and convert to data.frame- ------
  mypubs <- 
    ReadBib(my_bibfile, check = "warn", .Encoding = "UTF-8") |> 
    as_tibble() %>%
    mutate(across(everything(),~uml_fix(.x)),
           across(everything(),~stringr::str_squish(.x)),
           rnames   = names(names(RefManageR::ReadBib(my_bibfile,.Encoding = "UTF-8"))),
           mainref  = journal,
           mainref  = mainref %>% replace_na(''), #otherwise it appears "NA" in post
           abstract = abstract %>% replace_na('(Abstract not available)'), #otherwise it appears "NA" in post
           abstract = gsub(x = abstract,pattern = ('\\\\'),replacement = ''),
            mainref = gsub( x = mainref,pattern = ('\\\\'),replacement = '' ),
          # Customize for more than one editor
          editor = gsub(x = editor,pattern = (' and '),replacement = ', '),
          editor = stri_replace_last_fixed(editor, ',', ' &'),
          # add characters between tags for properly render
          keywords = gsub(x = keywords, pattern = (','), replacement = '","'),
          # assign "categories" to the different types of publications
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
                                     TRUE ~ "0")
          )
  
  
  
  
# create a function which populates the md template based on the info
# about a publication
# apply the "create_md" function to the publications list to generate
purrr::walk(seq_len(nrow(mypubs)), ~create_qmd(ex = mypubs[.x, , drop = FALSE],
                                               out_fold = "publications",
                                               overwrite = T))


create_qmd(ex = mypubs[19, , drop = FALSE],
           out_fold = "publications",
           overwrite = T)
