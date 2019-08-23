library(tidyverse)
library(extrafont)
theme_set(theme_minimal(base_family = "Roboto Condensed")+
            theme(text = element_text(size = 19, family = "Brill")))
library(waffle)
library(qrcode)
qrcode_gen("https://github.com/agricolamz/2019.08.21-25_SLE_Leipzig/raw/master/2019.08.21-25_SLE_Leipzig.pdf")
qrcode_gen("https://github.com/agricolamz/2019.08.21-25_SLE_Leipzig/raw/master/2019.08.21-25_SLE_Leipzig.pdf",
           wColor = "#0099CC", bColor = "white")


lingtypology::glottolog.modified %>% 
  select(affiliation, language, area) %>% 
  na.omit() %>% 
  filter(affiliation != "Artificial Language",
         affiliation != "Unclassified") %>% 
  mutate(affiliation = str_replace(affiliation, "Deaf Sign Language", "Sign Language"),
         affiliation = str_replace(affiliation, "Deaf sign language", "Sign Language"),
         affiliation = ifelse(str_detect(affiliation, ","), str_extract(affiliation, ".*?,"), affiliation),
         affiliation = str_remove(affiliation, ",")) ->
  all_langs


  count(affiliation, area, sort = TRUE) %>% 
  mutate(rank = 1:n()) ->
  all_langs

set.seed(42)

all_langs <- tibble(group = "total = 7420\nvalue 1 = 1891\nθ = 0.254",
                    value = sample(c("value 1", "value 2"),
                                   7420,
                                   replace = TRUE,
                                   prob = c(0.25, 0.75)))

all_langs %>% 
  count(group, value)

all_langs %>%
  sample_n(200) %>%
#  count(group, value)
  mutate(group = "total = 200\nvalue 1 = 62\nθ = 0.235") %>%
  rbind(all_langs) %>%
  count(group, value) ->
  simple_sample_results

simple_sample_results %>%
  ggplot(aes(fill = value, values = n)) +
  geom_waffle(color = "white", size = .3, n_rows = 70, flip = TRUE, show.legend = FALSE) +
  facet_wrap(~group, nrow = 1, strip.position = "bottom")+
  scale_x_discrete()+
  scale_y_continuous(labels = function(x) x * 70, # make this multiplyer the same as n_rows
                                 expand = c(0,0))+
  theme(panel.grid = element_blank())+
  ggthemes::scale_fill_tableau(name=NULL)

ggsave(filename = "03_simple_sample.jpeg", 
       width = 100, 
       height = 150, 
       units = "mm",
       device = "jpeg")

all_langs %>% 
  count(affiliation, area, sort = TRUE) %>% 
  mutate(rank = 1:n()) %>% 
  filter(n > 10) %>% 
  ggplot(aes(rank, n, label = affiliation, color = area))+
  geom_point()+
  ggrepel::geom_text_repel(alpha = 0.9)+
  scale_y_log10()+
  labs(y = "logarithm number of languages")+
  theme(legend.position = "top")

ggsave(filename = "05_families_by_area.jpeg", 
       width = 200, 
       height = 150, 
       units = "mm",
       device = "jpeg")

