library(magick)
library(tonythemes)
library(tidyverse)
theme_set_tony()

dir <- 's0e04'
image_read(file.path(dir, 'twitch.png'))
df <- read_csv(file.path(dir, 'tweets.csv')) %>% select(-1)

seq(as.POSIXct('2022-03-01 12:00:00'), as.POSIXct('2022-03-01 01:00:00'), by = "minute")
df %>% 
  filter(
    text %>% str_detect("\U1f1ee\U1f1ea|\U1F340|\U2618")
  ) %>% 
  count(created_at) %>% 
  mutate(
    rn = slider::slide_period(created_at, .period = "minute", .f = mean)
  )

compute <- function(rgx) {
  df %>% 
    filter(
      text %>% str_detect(rgx)
    ) %>% 
    mutate(
      h = lubridate::hour(created_at),
      m = lubridate::minute(created_at),
      hms = sprintf("2022-03-22 %2d:%2d:00", h, m) %>% lubridate::ymd_hms()
    ) %>% 
    count(hms) %>% 
    mutate(
      rn = slider::slide_dbl(n, mean, .after = 4)
    )
}
m <- compute("\U1f1ee\U1f1ea|\U1F340|\U2618")
o <- compute("\U1F4B0|\U1F911")
r <- bind_rows(
  m %>% mutate(group = "mcgregor"),
  o %>% mutate(group = "mayweather")
)

r %>% 
  ggplot() +
  aes(x = hms, y = n, group = group, color = group) +
  geom_point() +
  geom_line() +
  geom_hline(
    aes(yintercept = 0)
  ) +
  scale_color_manual(
    values = c(
      'mcgregor' = '#48a949',
      'mayweather' = '#fccd25'
    )
  ) +
  geom_text(
    inherit.aes =FALSE,
    size = 6,
    data = tibble(
      lab = c("Fight Begins", "McGregor does OK\nin the early rounds", "Mayweather takes\nover and wins by\nTKO"),
      hms = as.POSIXct(c("2022-03-22 00:15:00", "2022-03-22 00:30:00", "2022-03-22 01:00:00"), tz = "UTC"),
      y = c(10, 16, 12)
    ),
    aes(x = hms, y = y, label = lab)
  ) +
  guides(
    color = guide_legend(
      "",
      label.position = "right",
      label.hjust = 0,
      override.aes = list(size = 3)
    )
  ) +
  scale_y_continuous(
    labels = c("2", "4", "6", "8 emoji"),
    breaks = c(5, 10, 15, 20)
  ) +
  theme(
    legend.position = 'top',
    legend.justification = c(0, 1),
    axis.title.y = element_text(
      face = 'bold',
      size = 18,
      hjust = 0.5
    ),
    panel.grid.minor = element_line(color = '#d7d7d7'),
    panel.grid.major = element_line(color = '#d7d7d7'),
    plot.title = element_text(size = 22, face = 'bold'),
    plot.subtitle = element_text(size = 16, face = 'plain'),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = '#f0f0f0', colour = '#f0f0f0'),
    panel.background = element_rect(fill = '#f0f0f0', colour = '#f0f0f0')
  ) +
  labs(
    x = NULL,
    y = "Four-minute rolling average",
    title = 'Irish pride vs. The Money Team',
    subtitle = 'Four-minute rolling average of the number of uses of selected emoji in\nsampled tweets during the Mayweather-McGregor fight'
  ) -> p

ggsave(
  plot = p,
  filename = file.path(dir, "plot.png"),
  width = 8,
  height = 8
)
