

tre <- readxl::read_excel("tre.xlsx")


tre_m_snitt <- tre |> 
  group_by(tresort) |> 
summarise(gjsnitt = mean(c_across(where(is.numeric)), na.rm = TRUE))


tre <- left_join(tre, tre_m_snitt, by = "tresort")

tre_minmax <- tre %>% 
  select(!stammeomkrets) |> 
  mutate(across(where(is.numeric), ~ (. - min(.)) / (max(.) - min(.))))

tre_minmax <- tre_minmax %>% 
  rowwise() %>% 
  mutate(indeks = mean(c_across(where(is.numeric)), na.rm = TRUE)) %>% 
  ungroup()



library(ggplot2)
png(filename = "plot_indeks.png", height = 500, width = 500)
plot_indeks <- ggplot(data = tre_minmax, aes(x =reorder(tresort,
                                                        -indeks), y = indeks)) + 
  geom_col(col = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
    theme(axis.text.x = element_text(angle = 45,
                                     vjust = 1,
                                     hjust = 1)) +
  labs(x = "Tresort",
       y = "Indeks")
plot_indeks

dev.off()




vars <- c("bladfarge", "tilvekst", "bladskader", "stammeomkrets",
          "angrep_skadedyr", "lav", "stammeskader", "helhet")

for (v in vars) {
  assign(v, tre %>% select(tresort, all_of(v)))
}


all_tabeller <- c(angrep_skadedyr, bladfarge, bladskader, helhet, lav, stammeomkrets, stammeskader, tilvekst)


blad_split_liten <- bladfarge |> 
  filter(str_ends(tresort, "_liten"))

blad_split_stor <- bladfarge |> 
  filter(str_ends(tresort, "_stor"))



vars <- c("bladfarge", "tilvekst", "bladskader", "stammeomkrets",
          "angrep_skadedyr", "lav", "stammeskader", "helhet")

# lager en liste med ett element per variabel
all_tabeller <- list()

for (v in vars) {
  # hent bare tresort og denne variabelen
  df <- tre %>% select(tresort, all_of(v))
  
  # lag delsett
  liten <- df %>% filter(str_ends(tresort, "_liten"))
  stor  <- df %>% filter(str_ends(tresort, "_stor"))
  
  # legg alt i listen
  all_tabeller[[v]] <- list(
    full  = df,
    liten = liten,
    stor  = stor

  )
}



for (v in names(all_tabeller)) {
  
  cat("\n\n###", v, " — liten\n")
  print(tt(all_tabeller[[v]]$liten))
  
  cat("\n\n###", v, " — stor\n")
  print(tt(all_tabeller[[v]]$stor))
}