# load packages
library(tidyverse)

ph <- 7

pk <- 7

ba <- 10^(ph-pk)

b <- ba/(1+ba)

a <- 1/(ba+1)


d <- if_else(a > 0.001,
             a,
             0.001)

af <- tibble("number" = seq(from = 1, to = d*1000, by = 1))

af <- af %>%
  mutate(specie = "acid",
         spot = 1)

c <- if_else(b > 0.001,
              b,
             0.001)

bf <- tibble("number" = seq(from = 1, to = c*1000, by = 1))


bf <- bf %>%
  mutate(specie = "base",
         spot = 1)

df <- bind_rows(af, bf)

ggplot(df, aes(x = 1, y = spot, color = specie)) +
  geom_point(position = "jitter", alpha = 0.8, size = 2) +
  scale_color_manual(values = c("purple", "darkgreen")) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20, face = "bold")) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01))

ggplot(df, aes(x = specie, fill = specie)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("purple", "darkgreen")) +
  theme_bw() +
  theme(axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 20, face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "specie", y = "relative number of \nmolecules in solution") +
  scale_y_continuous(expand = c(0.01,0), limits = c(0, 1100))

