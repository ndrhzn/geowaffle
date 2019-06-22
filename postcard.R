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

## postcard -----------------------------------------------------------------
png(filename = 'postcard.png', width = 214, height = 152, units = 'mm', res = 300)

ggplot(df_waffle)+
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
