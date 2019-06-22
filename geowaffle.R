library(ggplot2)
library(geofacet)
library(dplyr)
source('https://raw.githubusercontent.com/andriy-gazin/median-age/master/custom_grid.R')

df <- read.csv('presidential_elections_2019.csv', stringsAsFactors = F)

## prepare data --------------------------------------------------------------

df_waffle <- df %>% 
  mutate(share = round(share, 0)) %>% 
  rowwise() %>% 
  do(sapply(., rep, .$share) %>% as.data.frame()) %>% 
  select(code:candidate) %>% 
  arrange(region, candidate) %>% 
  ungroup() %>% 
  group_by(code, region) %>% 
  do(cbind.data.frame(candidate = .$candidate, 
                      head(expand.grid(x = 1:10, y = 1:10), nrow(.))))

## annotations ---------------------------------------------------------------

annotations <- df %>% 
  select(-votes) %>% 
  tidyr::spread(candidate, share) %>% 
  rename(zelenskiy = 'Зеленський В.О.', poroshenko = 'Порошенко П.О.') %>% 
  mutate(x =  10.75, 
         zy = zelenskiy %/% 10,
         py = (zelenskiy + poroshenko) %/% 10)

## plot ------------------------------------------------------------------

png('geowaffle.png', width = 1000, height = 750)

ggplot(df_waffle)+
  geom_tile(aes(x = x, y = y, fill = candidate), 
            color = '#F3F7F7', size = 0.5, alpha = 0.8)+
  geom_text(data = annotations,
            aes(x = x, y = zy, 
                label = round(zelenskiy, 1) %>% format(digits = 3)),
            family = 'Ubuntu Condensed', hjust = 0,
            color = '#5D646F', size = 3.5)+
  geom_text(data = annotations,
            aes(x = x, y = py, 
                label = round(poroshenko, 1) %>% format(digits = 3)),
            family = 'Ubuntu Condensed', hjust = 0,
            color = '#5D646F', size = 3.5)+
  facet_geo(~region, grid = ukraine %>% filter(!code %in% c(26, 27)))+
  scale_fill_manual(values = c('#66c2a5', '#fc8d62'),
                    labels = c('Володимир Зеленський', 'Петро Порошенко') %>% 
                      stringr::str_wrap(10))+
  scale_x_continuous(limits = c(0, 13), expand = c(0, 0))+
  guides(fill = guide_legend(title = NULL))+
  labs(
    title = 'Другий тур виборів Президента України 2019 року',
    subtitle = 'Один прямокутник на графіку відповідає одному відсотку голосів виборців',
    caption = 'Дані: ЦВК України | Візуалізація: Textura.in.ua')+
  theme_void(base_family = 'Ubuntu Mono')+
  theme(
    legend.position = c(0, 0.95),
    legend.justification = 'left',
    legend.text = element_text(size = 12, family = 'Ubuntu Condensed',
                               margin = margin(t = 5)),
    legend.direction = 'vertical',
    legend.key.width = unit(5, 'pt'),
    text = element_text(color = '#5D646F'),
    strip.text = element_text(size = 12, family = 'Ubuntu Condensed', hjust = 0,
                              margin = margin(l = 5)),
    plot.title = element_text(size = 36, face = 'bold', margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, margin = margin(b = 20)),
    plot.caption = element_text(size = 12, margin = margin(t = 20)),
    plot.background = element_rect(fill = '#F3F7F7'),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), 'cm')
  )

dev.off()