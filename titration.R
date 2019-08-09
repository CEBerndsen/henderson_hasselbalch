library(tidyverse)

pk <- 7

ps <- 12

barat <- 10^(ps - pk)

phlow <- tibble(btoa = seq(from = 1, to = 999, by = 1))

phhigh <- tibble(btoa = seq(from = 1, to = 999, by = 1))

phex <- tibble(btoa = barat)

phlow <- phlow %>%
  mutate(ratio = btoa/999)

phhigh <- phhigh %>%
  mutate(ratio = 999/btoa)

phex <- phex %>%
  mutate(ratio = barat)

phdf <- bind_rows(phlow, phhigh, phex)

phdf <- phdf %>%
  mutate(ph = pk + log10(ratio),
         equiv = ratio/(1+ratio))

where <- phdf %>%
  filter(round(ph, 2) == ps)

ggplot(phdf, aes(x = equiv, y = ph)) +
  geom_line() +
  geom_vline(xintercept = 0.5, linetype = 2) +
  geom_point(data = where, aes(x = equiv, y = ph), alpha = 0.5, color = "red", size = 5) +
  theme_bw() +
  theme(axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 20, face = "bold"),
        legend.position = "none") +
  labs(x = "Molar equivalents of strong base", y = "pH") +
  scale_x_continuous(expand = c(0,0), breaks = seq(from = 0, to = 1, by = 0.1)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(from = 0, to = 14, by = 1))
