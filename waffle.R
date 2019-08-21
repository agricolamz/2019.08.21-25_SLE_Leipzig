library(tidyverse)
library(waffle)

set.seed(42)
all_langs <- tibble(group = "total = 7000, value 1 = 1794, θ = 0.256",
                    value = sample(c("value 1", "value 2"),
                                   7000,
                                   replace = TRUE,
                                   prob = c(0.25, 0.75)))

all_langs %>%
  sample_n(200) %>%
  mutate(group = "total = 200, value 1 = 49, θ = 0.245") %>%
  rbind(all_langs) %>%
  count(group, value) ->
  simple_sample_results

simple_sample_results %>%
  ggplot(aes(fill = value, values = n)) +
  geom_waffle(color = "white", size = .3, n_rows = 70, flip = TRUE, show.legend = FALSE) +
  facet_wrap(~group, nrow = 1, strip.position = "bottom")+
  ggthemes::scale_fill_tableau(name=NULL)+
  theme_minimal(base_family = "Roboto Condensed")+
  scale_x_discrete()+
  scale_y_continuous(labels = function(x) x * 70, # make this multiplyer the same as n_rows
                     expand = c(0,0))+
  theme(panel.grid = element_blank(), axis.ticks.y = element_line())

