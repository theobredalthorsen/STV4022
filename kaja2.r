tre <- readxl::read_excel("tre.xlsx")

tre_01 <- tre |> 
  mutate(across(where(is.numeric), ~ (. - min(.)) / (max(.) - min(.))))

tre_01 <- tre_01 |> select(!lav)

tre_indeks <- tre_01 %>% 
  rowwise() %>% 
  mutate(indeks = mean(c_across(where(is.numeric)), na.rm = TRUE)) %>% 
  ungroup()

tre_indeks_stor <- tre_indeks |>
  rename(gjennomsnitt = indeks) |> 
  filter(str_ends(tresort, "_stor"))

tre_indeks_liten <- tre_indeks |> 
  filter(str_ends(tresort, "_liten")) |> 
  select(!stammeskader)

tinytable::tt(tre_indeks_stor) |> 
  save_tt("indeks_stor.docx")

tinytable::tt(tre_indeks_liten) |> 
  save_tt("indeks_liten.docx")
  
