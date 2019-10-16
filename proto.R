library(googledrive)
library(googlesheets4)
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(scales)
library(ggrepel)



my_xp <- 3592282


levels <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSiqyyYi6jGje-v8u_bOPZ6wFzDOHxxJmsSotTy0RB_jkjTgKjdDoe2Eujh0FU3VDKbx6QIKGp9LYFP/pub?gid=0&single=true&output=csv")

#dir <- drive_ls("~/pokemon")

#pokelevels <- drive_get(id=dir %>% filter(name=="PokeLevels") %>% pull(id))

#levels <- read_sheet(pokelevels)

levels_train <- levels %>% filter(Level >= 20)

fit <- glm(log10(Cumulative) ~ Level, family = gaussian, data = levels_train)

to_predict <- data.frame(
  Level = seq.int(41,50,1)
)

predicted <- predict.glm(fit, to_predict, se.fit = TRUE)

pred_df <- data.frame(
  Level = to_predict$Level,
  Cumulative = 10^predicted$fit
  )

my_xp <- if_else(my_xp > max(levels$Cumulative),
                 max(levels$Cumulative),
                 my_xp)
lvl_start <- levels %>% filter(Cumulative <= my_xp) %>% pull(Cumulative) %>% max()
lvl_stop <- levels %>% filter(Cumulative >= my_xp) %>% pull(Cumulative) %>% min() -1

lvl_pct <- round(100*(my_xp - lvl_start) / (lvl_stop - lvl_start), digits = 1)

my_level <- levels %>% filter(Cumulative < my_xp) %>%
  filter(row_number() == n()) %>% pull(Level)

steps_to_go <- round((max(levels$Cumulative) - my_xp) / my_xp, digits=2)

my_df <- tribble(
  ~Level, ~XP, ~label,
  my_level, my_xp, "You are here"
)

my_step_df <- tribble(
  ~Level, ~XP, ~label,
  5,my_xp / 2, 1,
  5, my_xp + (max(levels$Cumulative) - my_xp) / 2, steps_to_go
)

my_nudge_x <- if_else(my_xp > max(levels$Cumulative) / 2, 
                    -1 * max(levels$Cumulative) / 10,
                    1 * max(levels$Cumulative) / 10)

my_nudge_y <- if_else(my_level > max(levels$Level) / 2, 
                      -.1 * my_level,
                      0.1 * my_level)

levels %>%
  ggplot(aes(y=Level, x=Cumulative)) + 
  geom_step() + 
  geom_vline(xintercept = my_xp, linetype=3, color="red" ) +
  geom_hline(yintercept = my_level, linetype=3, color = "red") +
  geom_segment(x = 0, xend = my_xp, y = 5, yend =5, arrow = arrow(ends = "both")) +
  geom_segment(x = my_xp, xend = max(levels$Cumulative), y = 5, yend =5, arrow = arrow(ends = "both")) +
  geom_label_repel(data = my_df, aes(x=XP, y=Level, label=label), 
                   arrow=arrow(type="closed", length = unit(0.15, "inches")), 
                   nudge_x = my_nudge_x, 
                   nudge_y = my_nudge_y)+
  geom_label(data = my_step_df, aes(x=XP, y=Level, label=label)) +
  labs(
    title = "Your Place in the Poke-verse",
    x="Total XP",
    y="Trainer Level"
  ) + ggthemes::theme_few()

levels %>% filter(Level > 1) %>%
  ggplot(aes(y=Level, x=Cumulative)) + 
  geom_step() + 
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = 'b') +
  geom_vline(xintercept = my_xp, linetype=3 ) +
  geom_hline(yintercept = my_level, linetype=3) +
  ggrepel::geom_label_repel(data = my_df, aes(x=XP, y=Level, label=label))+
  labs(
    title = "My Place in the Poke-verse",
    x="log10(Total XP)",
    y="Trainer Level"
  )
  



