library(memer)
library(tonythemes)
library(tidyverse)
library(maps)
dir <- 's0e01'
meme_get(filepath = file.path(dir, 'ai.png'))
df <- file.path(dir, 'data.csv') %>% read_csv()
long <- df %>% 
  pivot_longer(
    -c(RespondentID, identify_as_southerner),
    names_to = 'region',
    values_to = 'value'
  ) %>% 
  mutate(
    across(region, tolower)
  ) %>% 
  filter(value != 0)

usa <- map_data('state')
states <- usa %>% 
  filter(!region %in% c('alaska', 'hawaii'))

counts <- long %>% 
  filter(!is.na(RespondentID)) %>% 
  mutate(
    is_southerner = case_when(
      identify_as_southerner %in% c('A lot', 'Some') ~ 'yes',
      TRUE ~ 'no'
    )
  ) %>% 
  count(region, is_southerner) %>% 
  group_by(region) %>% 
  mutate(
    prop = n / sum(n)
  ) %>% 
  ungroup() %>% 
  filter(is_southerner == 'yes')

abbrs <- counts %>% 
  inner_join(states) %>% 
  group_by(region) %>% 
  summarize(
    across(
      c(lat, long),
      mean,
      na.rm = TRUE
    )
  ) %>% 
  ungroup() %>% 
  left_join(
    setNames(state.name, state.abb) %>% enframe('abbr', 'region') %>% mutate(across(region, tolower))
  )
white_abbrs <- abbrs %>% 
  filter(
    abbr %in% c(
      'TX', 'AR', 'LA', 'MS', 'TN', 'KY', 'VA', 'MS', 'AL', 'GA', 'SC', 'NC', 'FL'
    )
  )
black_abbrs <- abbrs %>% anti_join(white_abbrs)
update_geom_defaults('text', list(family = 'Lato'))
p <- ggplot() +
  geom_polygon(
    data = states,
    color = '#c9c9c9',
    fill = 'grey90',
    aes(
      group = region,
      x = long,
      y = lat
    )
  ) +
  geom_polygon(
    data = counts %>% 
      inner_join(states),
    alpha = 0.5,
    aes(
      fill = prop,
      group = region,
      x = long,
      y = lat
    )
  ) +
  geom_text(
    data = white_abbrs,
    color = 'white',
    fontface = 'bold',
    aes(
      label = abbr,
      x = long,
      y = lat
    )
  ) +
  geom_text(
    data = black_abbrs,
    color = 'black',
    fontface = 'bold',
    aes(
      label = abbr,
      x = long,
      y = lat
    )
  ) +
  scale_fill_distiller(
    palette = 'Reds', 
    labels = scales::percent_format(1),
    breaks = seq(0.1, 1, by = 0.1),
    limits = c(0, 1),
    direction = 1
  ) +
  coord_map(
    projection = 'conic',
    lat0 = 30
  ) +
  #A ggthemes::theme_map() 
  guides(
    fill = guide_legend(
      ''
    )
  ) +
  # scale_fill_continuous(
  #   # labels = scales::percent_format(1),
  #   breaks = seq(0.1, 1, by = 0.1),
  #   limits = c(0, 1)
  # ) +
  theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text = element_blank(),
    panel.grid.minor = element_line(color = '#c9c9c9'),
    panel.grid.major = element_line(color = '#c9c9c9'),
    plot.title = element_text(size = 17, face = 'bold', family = 'Lato'),
    plot.subtitle = element_text(size = 16, family = 'Lato'),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = '#f0f0f0', colour = '#f0f0f0')
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "'Which States Do You Consider Part of the South?'",
    subtitle = 'Percentage classifying each state as part of the South, from a survey\nof 1,135 people idenityfing "some" or "a lot" as a Southerner'
  )
p

ggsave(
  plot = p,
  filename = file.path(dir, 'viz.png'),
  width = 7,
  height = 7 / 1.25
)
