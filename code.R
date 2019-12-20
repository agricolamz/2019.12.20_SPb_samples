library(tidyverse)
library(extrafont)
theme_set(theme_minimal(base_family = "Roboto Condensed")+
            theme(text = element_text(size = 19, family = "Brill")))
library(waffle)
library(qrcode)
qrcode_gen("https://github.com/agricolamz/2019.12.20_SPb_samples/raw/master/2019.12.20_SPb_samples.pdf")
qrcode_gen("https://github.com/agricolamz/2019.12.20_SPb_samples/raw/master/2019.12.20_SPb_samples.pdf",
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


set.seed(42)
all_langs %>% 
  mutate(value =  sample(c("value 1", "value 2"),
                           7393, 
                           replace = TRUE,
                           prob = c(0.25, 0.75)),
         group = "total = 7393\nvalue 1 = 1885\nθ = 0.255") ->
  all_langs


all_langs %>% 
  count(value)

set.seed(42)
all_langs %>%
  sample_n(200) %>%
#  count(group, value)
  mutate(group = "total = 200\nvalue 1 = 39\nθ̂ = 0.195") %>%
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
  ggrepel::geom_text_repel(alpha = 0.9, show.legend = FALSE)+
  scale_y_log10()+
  labs(y = "logarithm number of languages", x = "rank by number of languages")+
  theme(legend.position = "top", legend.title = element_blank())

ggsave(filename = "04_families_by_area.jpeg", 
       width = 200, 
       height = 150, 
       units = "mm",
       device = "jpeg")

set.seed(42)
all_langs %>%
  sample_n(200) %>% View()
  select(affiliation) %>% 
  mutate(present = TRUE) %>% 
  count(affiliation, present) %>% 
  rename(n_for_coloring = n) ->
  sampled_langs

set.seed(42)
all_langs %>%
  sample_n(200) %>% 
  count(area) %>% 
  arrange(-n) %>% 
  mutate(text = str_c(area, " — ", n)) %>% 
  select(text) %>% 
  unlist() %>% 
  str_c(., collapse = "\n") ->
  annotation

all_langs %>% 
  count(affiliation, area, sort = TRUE) %>% 
  mutate(rank = 1:n()) %>% 
  filter(n > 10) %>% 
  left_join(sampled_langs) %>% 
#  mutate(n_for_coloring = ifelse(is.na(present), NA, n)) %>% 
  ggplot(aes(rank, n, label = affiliation, color = n_for_coloring))+
  geom_point()+
  ggrepel::geom_text_repel(alpha = 0.9, show.legend = FALSE)+
  scale_color_gradient(na.value = "red", low = "lightblue", high = "navy", name = "number of languaes in the sample:")+
  scale_y_log10()+
  labs(y = "logarithm number of languages", x = "rank by number of languages")+
  theme(legend.position = "top", legend.title = element_text(size = 12))

ggsave(filename = "09_families_by_sample.jpeg", 
       width = 200, 
       height = 150, 
       units = "mm",
       device = "jpeg")

all_langs %>% 
  count(affiliation, area, sort = TRUE) %>% 
  mutate(rank = 1:n()) %>% 
  filter(n > 10) %>% 
  left_join(sampled_langs) %>% 
  #  mutate(n_for_coloring = ifelse(is.na(present), NA, n)) %>% 
  ggplot(aes(rank, n, label = affiliation, color = n_for_coloring))+
  geom_point()+
  ggrepel::geom_text_repel(alpha = 0.9, show.legend = FALSE)+
  annotate("text", y = 600, x = 52, label = annotation, family = "Brill", size = 5)+
  scale_color_gradient(na.value = "red", low = "lightblue", high = "navy", name = "number of languaes in the sample:")+
  scale_y_log10()+
  labs(y = "logarithm number of languages", x = "rank by number of languages")+
  theme(legend.position = "top", legend.title = element_text(size = 12))

ggsave(filename = "10_families_by_sample.jpeg", 
       width = 200, 
       height = 150, 
       units = "mm",
       device = "jpeg")

all_langs %>% 
  count(area, affiliation) %>% 
  ggplot(aes(fill = affiliation, values = n)) +
  geom_waffle(color = "white", size = .3, n_rows = 70, flip = TRUE, show.legend = FALSE) +
  facet_wrap(~area, ncol = 1, scales = "free_y")+
  scale_x_discrete()+
  scale_y_continuous(labels = function(x) x * 70, # make this multiplyer the same as n_rows
                     expand = c(0,0))+
  theme(panel.grid = element_blank())

ggsave(filename = "05_families_by_area.jpeg", 
       width = 100, 
       height = 150, 
       units = "mm",
       device = "jpeg")


library(lingtypology)
read_csv("https://github.com/phoible/dev/raw/master/data/phoible.csv") %>% 
  filter(Source == "upsid") %>% 
  select(Glottocode, LanguageName, Phoneme) %>% 
  mutate(affiliation = aff.lang(lang.gltc(Glottocode)),
         affiliation = ifelse(str_detect(affiliation, ","), str_extract(affiliation, ".*?,"), affiliation),
         affiliation = str_remove(affiliation, ",")) %>% 
  filter(!is.na(affiliation)) %>% 
  write_csv("data/phoible_upsid.csv")

upsid <- read_csv("data/phoible_upsid.csv")

set.seed(11)
upsid %>% 
  mutate(vowel = str_extract(Phoneme, "a|u|i")) %>% 
  count(Glottocode, affiliation, LanguageName, vowel) %>% 
  filter(!is.na(vowel)) %>% 
  mutate(n = 1,
         area = area.lang(lang.gltc(Glottocode))) %>% 
  filter(!is.na(area)) %>% 
  spread(vowel, n, fill = 0) %>% 
  gather(vowel, value, a:u) %>% 
  group_by(area, affiliation, vowel) %>% 
  summarise(n = n(),
            ratio = sum(value)/n) %>% 
  spread(vowel, ratio) %>% 
  ungroup() %>% 
  filter(n > 2) %>% 
  sample_n(10) %>% 
  gather(vowel, ratio, a:u) %>% 
  mutate(affiliation = paste0(affiliation, " (", n, ")"),
         caption = paste("sample of ", sum(n)/3, " languages")) ->
for_plot

for_plot %>% 
  group_by(vowel) %>% 
  summarise(mu = mean(ratio),
            md = median(ratio)) ->
  ms

for_plot  %>% 
  ggplot(aes(n, ratio, label = affiliation, color = area))+
  geom_hline(data = ms, aes(yintercept = md), linetype = 2, alpha = 0.5)+
  ggrepel::geom_text_repel(show.legend = FALSE, size = 4)+
  geom_point(aes(size = n), show.legend = FALSE)+
  facet_wrap(~vowel)+
  labs(x = "number of languages",
       y = "ratio of languages with a sound",
       caption = unique(for_plot$caption))

ggsave(filename = "images/06_families_sample.jpeg", 
       width = 200, 
       height = 150, 
       units = "mm",
       device = "jpeg")

for_plot  %>% 
  ggplot(aes(ratio)) +
  geom_vline(data = ms, aes(xintercept = md), linetype = 2, alpha = 0.5)+
  geom_density()+
  facet_wrap(~vowel, scales = "free_y")+
  labs(caption = unique(for_plot$caption))+
  xlim(0, 1)

ggsave(filename = "images/07_distributions.jpeg", 
       width = 200, 
       height = 150, 
       units = "mm",
       device = "jpeg")

upsid %>% 
  mutate(vowel = str_extract(Phoneme, "a|u|i")) %>% 
  count(Glottocode, affiliation, LanguageName, vowel) %>% 
  filter(!is.na(vowel)) %>% 
  mutate(n = 1,
         area = area.lang(lang.gltc(Glottocode))) %>% 
  filter(!is.na(area)) %>% 
  spread(vowel, n, fill = 0) %>% 
  gather(vowel, value, a:u) %>% 
  group_by(area, affiliation, vowel) %>% 
  summarise(n = n(),
            ratio = sum(value)/n) %>% 
  spread(vowel, ratio) %>% 
  ungroup() %>% 
  filter(n > 2) %>% 
  gather(vowel, ratio, a:u) %>% 
  mutate(affiliation = paste0(affiliation, " (", n, ")"),
         caption = paste("sample of ", sum(n)/3, " languages")) ->
  for_plot_2  

for_plot_2 %>% 
  group_by(vowel) %>% 
  summarise(mu = mean(ratio),
            md = median(ratio)) ->
  ms_2


for_plot_2  %>% 
  ggplot(aes(ratio)) +
  geom_vline(data = ms_2, aes(xintercept = md), linetype = 2, alpha = 0.5)+
  geom_density()+
  facet_wrap(~vowel, scales = "free_y")+
  labs(caption = unique(for_plot_2$caption))+
  xlim(0, 1)

for_plot_2 %>% 
  count(affiliation)

ggsave(filename = "images/08_distributions.jpeg", 
       width = 200, 
       height = 150, 
       units = "mm",
       device = "jpeg")
