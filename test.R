library(tidyverse)
library(tidytext)
library(RcppMeCab)
library(presidentSpeechKr)

get_speech(spidx$link[1]) %>%
  select(content) %>%
  unnest_tokens(tag, content, token = pos) %>%
  filter(grepl("v(v|a)\\+",tag)) %>%
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
  filter(grepl("v(v|a)\\+etm", tag)) %>%
  mutate(
    tag = gsub("/.*$", "", tag)
  ) %>%
  mutate(tag = tag %>% map_chr(rm_etm)) -> etm_done

root %>%
  filter(grepl("v(v|a)\\+etm", tag)) %>%
  mutate(
    tag = gsub("/.*$", "", tag)
  ) %>%
  unique() %>%
  mutate(tag=ifelse(chk_niun_done(tag),rm_niun_done(tag),tag)) %>%
  mutate(tag=ifelse(chk_liul_done(tag),rm_liul_done(tag),tag)) %>%
  mutate(tag=ifelse(chk_eu_done(tag),add_liul_done(tag),tag)) %>%
  mutate(tag = paste0(tag, "다"))

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
