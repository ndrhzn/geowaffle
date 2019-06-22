library(ggplot2)
library(geofacet)
library(dplyr)
source('https://raw.githubusercontent.com/andriy-gazin/median-age/master/custom_grid.R')

df <- read.csv('presidential_elections_2019.csv', stringsAsFactors = F)

## ------------------------------------------------------------------------

df_waffle <- df %>% 
  rowwise() %>% 
  do(sapply(., rep, round(.$share, 0)) %>% as.data.frame()) %>% 
  select(code:candidate) %>% 
  arrange(region, candidate) %>% 
  ungroup()

## ----------------------------------------------------------------------

df_final = data.frame()

for(i in unique(df_waffle$code)) {
  
  grid = expand.grid(x = 1:10, y = 1:10)
  
  foo = df_waffle %>% 
    filter(code == i)
  
  foo = bind_cols(foo, grid[1:nrow(foo),])
  
  df_final = rbind.data.frame(df_final, foo)
  
}
  
## annotations

annotations <- df %>% 
  select(-votes) %>% 
  tidyr::spread(candidate, share) %>% 
  rename(zelenskiy = 'Зеленський В.О.', poroshenko = 'Порошенко П.О.') %>% 
  mutate(x =  10.75, 
         zy = zelenskiy %/% 10,
         py = (zelenskiy + poroshenko) %/% 10)

## -------------------------------------------------------------------------

png('geowaffle.png', width = 1000, height = 750)

ggplot(df_final)+
  geom_tile(aes(x = x, y = y, fill = candidate), color = '#F3F7F7', size = 0.025)+
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
  scale_x_continuous(limits = c(0, 12))+
  guides(fill = guide_legend(title = NULL))+
  labs(
    title = 'Другий тур виборів Президента України 2019 року',
    subtitle = 'Кожен прямокутник позначає один набраний кандидатом відсоток голосів виборців',
    caption = 'Дані: ЦВК України | Візуалізація: Textura.in.ua')+
  theme_void(base_family = 'Ubuntu Mono')+
  theme(
    legend.position = c(0, 0.95),
    legend.justification = 'left',
    legend.text = element_text(size = 11),
    legend.key.height = unit(5, 'pt'),
    legend.direction = 'vertical',
    text = element_text(color = '#5D646F'),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 36, face = 'bold', margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, margin = margin(b = 10)),
    plot.caption = element_text(size = 12, margin = margin(t = 20)),
    plot.background = element_rect(fill = '#F3F7F7'),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), 'cm')
  )

dev.off()

## postcard -----------------------------------------------------------------
png(filename = 'postcard.png', width = 214, height = 152, units = 'mm', res = 300)

ggplot(df_final)+
  geom_tile(aes(x = x, y = y, fill = candidate))+
  facet_geo(~region, grid = ukraine %>% filter(!code %in% c(26, 27)))+
  scale_fill_manual(values = c('#66c2a5', '#fc8d62'))+
  labs(caption = 'Textura.in.ua')+
  theme_void(base_family = 'Ubuntu Mono')+
  theme(
    legend.position = 'none',
    strip.text = element_blank(),
    plot.caption = element_text(family = 'Ubuntu Mono', 
                                margin = margin(t = 20, b = 5),
                                size = 10, color = '#324759'),
    panel.background = element_rect(fill = '#F3F7F7'),
    plot.background = element_rect(fill = '#F3F7F7'),
    plot.margin = unit(c(1.5, 0.75, 0.75, 0.75), 'cm')
  )

dev.off()
