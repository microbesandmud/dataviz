install.packages("lubridate")
install.packages("gganimate")
install.packages("extrafont")
install.packages("ggdark")
install.packages("ggridges")



library(tidyverse)
library(lubridate)
library(extrafont)
library(ggdark)
library(ggridges)
library(tidyverse)

df <- read_csv(
  "20191016_simple_biopiles.csv",
  col_types = "fnnnnn")

p1 <- ggplot(df,
             aes(y = Biopile, x = Months, fill = ..x..)) +
  geom_density_ridges_gradient(
    quantile_lines = TRUE, quantiles = 2,
    jittered_points = TRUE, alpha = 0.5,
    point_size = 0.6, point_color = "black") +
  scale_fill_viridis_c() +
  labs(
    title = "Age Distribution of Amusement Park Injuries in Texas",
    subtitle = "Young men seem to be more injury prone\n(Median value shown as a vertical line)\n#TidyTuesday, 2019-09-10",
    caption = "Source: data.world\n@jmcastagnetto / Jesus M. Castagnetto",
    y = "",
    x = "",
    fill = "Months"
  ) +
  xlim(-10, 80) +
  dark_theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.justification = c(0, 0),
    axis.text.y = element_text(size = 18, color = "yellow"),
    axis.text.x = element_text(size = 12, color = "yellow"),
    strip.text = element_text(family = "Inconsolata", size = 36, face = "bold.italic"),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 18, face = "bold"),
    plot.caption = element_text(family = "Inconsolata", size = 14)
  )

p1

ggsave(
  plot = p1,
  filename = "2019-09-10.png",
  width = 12,
  height = 9
)
