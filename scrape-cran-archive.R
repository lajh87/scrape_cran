#' scrape cran
#' 13/06/2021

library(magrittr)
library(httr)

#+ Scrape src/contrib -----
req <- GET("https://cran.r-project.org/src/contrib/")
contrib <- content(req) %>%
  xml2::xml_find_all("//table//tr//td") %>%
  xml2::xml_text() %>%
  matrix(nrow = 5) %>% 
  t() %>%
  dplyr::as_tibble()


#+ scrape archive ----
req <- GET("https://cran.r-project.org/src/contrib/Archive/")

archive_dirs <- content(req) %>%
  xml2::xml_find_all("//table//tr//td//a") %>%
  xml2::xml_attr("href")


archive_dirs_spliced <- archive_dirs[3:length(archive_dirs)]


test <- archive_dirs_spliced[1:4] %>%
  purrr::map_df(function(x){
    url <- file.path("https://cran.r-project.org/src/contrib/Archive/", x)
    req <- GET(url)
    content(req) %>%
      xml2::xml_find_all("//table//tr//td") %>%
      xml2::xml_text() %>%
      matrix(nrow = 5) %>% 
      t() %>%
      dplyr::as_tibble(.name_repair = "unique") %>%
      dplyr::mutate(package = x)
  })


filters_packages_by_letter_num <- function(start, stop){
  archive_dirs_spliced %>%
  substr(1,1) %>%
  tolower() %in% c(letters[start:stop])
}


packages_a_f <- archive_dirs_spliced[filters_packages_by_letter_num(1,6)]

archive_a_f <- packages_a_f %>%
  purrr::map_df(function(x){
    which_ob <- match(x, packages_a_f)
    print(paste(x, which_ob, length(packages_a_f)))
    url <- file.path("https://cran.r-project.org/src/contrib/Archive/", x)
    req <- GET(url)
    tryCatch({
      content(req) %>%
        xml2::xml_find_all("//table//tr//td") %>%
        xml2::xml_text() %>%
        matrix(nrow = 5) %>% 
        t() %>%
        dplyr::as_tibble(.name_repair = "unique") %>%
        dplyr::mutate(package = x)
    }, error = function(e){dplyr::tibble(package = x)})
  })

packages_g_z <- archive_dirs_spliced[filters_packages_by_letter_num(7,26)]
length(packages_a_f) + length(packages_g_z) == length(archive_dirs_spliced)

archive_g_z <- packages_g_z %>%
  purrr::map_df(function(x){
    which_ob <- match(x, packages_a_f)
    print(paste(x, which_ob, length(packages_a_f)))
    url <- file.path("https://cran.r-project.org/src/contrib/Archive/", x)
    req <- GET(url)
    tryCatch({
      content(req) %>%
        xml2::xml_find_all("//table//tr//td") %>%
        xml2::xml_text() %>%
        matrix(nrow = 5) %>% 
        t() %>%
        dplyr::as_tibble(.name_repair = "unique") %>%
        dplyr::mutate(package = x)
    }, error = function(e){dplyr::tibble(package = x)})
  })

# End
