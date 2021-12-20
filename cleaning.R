library(pdftools)
library(tidyverse)

discurso <- pdftools::pdf_text(pdf = "discurso.pdf")

discurso_texto <- discurso |> 
  str_c(collapse = "\n") |>  
  str_remove_all("[:space:]Domingo 19 de diciembre de 2021[:space:]+[:space:]") |> 
  str_remove_all("[:space:]Discurso Presidente Gabriel Boric Font\n\n\n") |> 
  str_replace_all("\\\n", " ") |> 
  str_replace_all("  +", " ") |> 
  str_remove_all("[:digit:]\\s") |> 
  str_replace_all("  ", " ") |> 
  str_remove_all(" \\d")


discurso_texto |> 
  readr::write_file("discurso_presidencial_boric2021.txt")
