get_speech(spidx$link[1]) %>%
  select(content) %>%
  unnest_tokens(tag, content, token = pos) %>%
  filter(grepl("vv\\+",tag)) %>%
  separate(tag, into =c("text", "tag"), sep = "/") %>%
  distinct(tag)

spidx$link[1:100] %>%
  map_dfr(get_speech) %>%
  select(content) %>%
  unnest_tokens(tag, content, token = pos) %>%
  filter(grepl("/n|/v(v|a)", tag)) -> root

root %>%
  filter(grepl("/n", tag)) %>%
  mutate(tag = gsub("/n.+$", "", tag)) -> n_done

root %>%
  filter(grepl("v(v|a)\\+etm", tag)) -> rofi

rofi %>%
  mutate(
    tag = gsub("/.*$", "", tag)
  ) %>%
  mutate(tag = tag %>% map_chr(rm_batchim_done))

root %>%
  filter(grepl("v(v|a)\\+ef", tag)) %>%
  mutate(
    tag = gsub("/.*$", "", tag)
  ) -> ef_done

root %>%
  filter(grepl("v(v|a)$", tag)) %>%
  mutate(
    tag = gsub("/.*$", "다", tag)
  ) -> v_done


root %>%
  filter(!grepl("/n", tag)) %>%
  filter(!grepl("v(v|a)\\+etm", tag)) %>%
  filter(!grepl("v(v|a)\\+ef", tag)) %>%
  filter(!grepl("v(v|a)$", tag)) %>%
