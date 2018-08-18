source(here::here("R/03_base2.R"))

ua_df <- union_all(x,y) # %>% arrange(id)
ua_df <-
  bind_rows(
    proc_data2(ua_df, "x"),
    proc_data2(ua_df, "y")
  ) %>%
 filter(!(.id == "x" & .y %in% c(-4,-5) ) & !(.id == "y" & .y %in% c(-1,-2,-3))) %>%
  mutate(frame = 2, .x = .x + 2)

ua <-
  initial_dfs %>%
  bind_rows(ua_df) %>%
  arrange(desc(frame)) %>%
  filter(!(label %in% c("id","id2"))) %>%
  plot_data("union_all(x, y)") %>%
  animate_plot()

ua <- animate(ua)
ua
anim_save(here::here("images", "union_all.gif"), ua)

ua_g <- union_all(x, y) %>% # %>% arrange(id)
  proc_data2() %>%  filter(label != "id") %>%
  plot_data("union_all(x, y)")

save_static_plot(ua_g, "union_all")
