#load libraries 
library(tidyverse)
library(nbastatR)
library(data.table)
library(extrafont)
library(magick)
library(paletteer)
library(ggforce)
library(waffle)
library(scales)

#Custom theme
theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

gl <- game_logs(seasons = 2020, result_types = c("player"),  season_types = "Regular Season")

players <- gl %>% group_by(namePlayer) %>% 
  filter(namePlayer != "Stephen Curry") %>%  
  summarise(ppg = sum(pts)/max(numberGamePlayerSeason), na.rm = TRUE, 
            minutes = sum(minutes)) %>% 
  filter(minutes >= 500) %>% 
  top_n(30, ppg)

df <- gl %>% 
  select(namePlayer, fg2m, fg3m, ftm) %>% 
  group_by(namePlayer) %>% 
  summarise(fg2m.pts = sum(fg2m) * 2,
            fg3m.pts = sum(fg3m) * 3, 
            ftm.pts = sum(ftm), 
            fg2m.pct = fg2m.pts / (fg2m.pts + fg3m.pts + ftm.pts), 
            fg3m.pct = fg3m.pts / (fg2m.pts + fg3m.pts + ftm.pts), 
            ftm.pct = ftm.pts / (fg2m.pts + fg3m.pts + ftm.pts))

df <- df %>% filter(namePlayer %in% players$namePlayer) %>% 
  select(namePlayer, fg2m.pct:ftm.pct) %>% gather(Type, Value, -namePlayer)

df$Type <- as.factor(df$Type)
levels(df$Type)
df$Type = factor(df$Type,levels(df$Type)[c(3, 2, 1)])

df <- df %>% group_by(namePlayer) %>% arrange(Type) %>% mutate(total = sum(Value), pcts = round(100*Value / total))
df <- df %>% group_by(namePlayer) %>% mutate(totalsum = 100 - sum(pcts[-length(pcts)]))
df$pcts <- ifelse(df$Type == "fg2m.pct", df$totalsum, df$pcts)

df$namePlayer <- paste0(substr(df$namePlayer, 1, 1), ". ", word(df$namePlayer, -1))
df$namePlayer <- ifelse(df$namePlayer == "G. Antetokounmpo", "Giannis", df$namePlayer)

df <- df %>% ungroup() %>% 
  arrange(Type, desc(Value)) %>%               # sort your dataframe
  mutate(namePlayer = factor(namePlayer, unique(namePlayer))) # reset your factor-column based on that order

p <- df %>% 
  ggplot(aes(fill=Type, values=pcts)) + 
  geom_waffle(color = "white", size=.25, n_rows = 10, flip = T) +
  facet_wrap(~namePlayer, nrow=5, strip.position = "bottom") +
  coord_equal(clip  = 'off') +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_continuous(breaks = function(x) seq(5, max(x), by = 5), 
                     expand = c(0,0))  +
  theme_owen()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  theme(legend.position = 'top') +
  scale_fill_manual(values = c("red", "black", "gray"), labels = c("Fts", "3s", "2s"))  + 
  theme(strip.text.x = element_text(size = 8)) +
  labs(fill = "", 
       title = "Composition of Total Points Scored", 
       subtitle = "Each square represents where one percent of a player's total points come from") +
  theme(plot.title = element_text(face = 'bold', size = 12, hjust = .5), 
        plot.subtitle = element_text(size = 8, hjust = .5), 
        plot.caption = element_text(color = 'gray40')) +
  theme(plot.margin = unit(c(.5, 0, .5, 0), "cm"), 
        panel.border = element_rect(colour = "black", fill=NA, size=.25))   +
  theme(legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0)) 

p <- cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="floralwhite", color = NA))

ggsave("FreeThrowKings.png", p, width = 6, height = 6)
footy <- image_read("footer.png")
graf <- image_read("FreeThrowKings.png")
image_composite(graf, footy, offset = "+0+1745") %>% image_write("FreeThrowKings.png")
