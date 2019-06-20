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
  

  
## -------------------------------------------------------------------------

png('geowaffle.png', width = 1000, height = 800)

ggplot(df_final)+
  geom_tile(aes(x = x, y = y, fill = candidate), color = '#F3F7F7', size = 0.025)+
  facet_geo(~region, grid = ukraine %>% filter(!code %in% c(26, 27)))+
  scale_fill_manual(values = c('#66c2a5', '#fc8d62'))+
  labs(title = 'Другий тур виборів Президента України 2019 року',
       subtitle = 'Кожен прямокутник позначає один набраний кандидатом відсоток голосів виборців',
       caption = 'Дані: ЦВК України | Візуалізація: Textura.in.ua')+
  theme_void(base_family = 'Ubuntu Mono')+
  theme(
    legend.position = 'top',
    legend.justification = 'left',
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
png(filename = 'postcard.png', width = 210, height = 148, units = 'mm', res = 300)

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
                                margin = margin(t = 20),
                                size = 10, color = '#324759'),
    plot.background = element_rect(fill = '#F3F7F7'),
    plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), 'cm')
  )

dev.off()
