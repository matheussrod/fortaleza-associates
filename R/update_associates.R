
url <- "https://www.sociofortaleza.com.br/"
html <- rvest::read_html(x = url)
datetime <- format(as.POSIXct(Sys.time(), tz = "Ameriza/Brazil"), "%Y-%m-%d %H:%M:%S")

associates_text <- html |> 
  rvest::html_node("strong") |> 
  rvest::html_text()

associates_number <- as.integer(sub(pattern = "\\.", replacement = "", x = associates_text))

associates_new <- data.frame("data" = datetime, "associates" = associates_number)
associates_old <- read.csv2(file = here::here("data/associates.csv"))
associates <- rbind(associates_old, associates_new)

write.csv2(x = associates, file = here::here("data/associates.csv"), quote = FALSE, row.names = FALSE)
