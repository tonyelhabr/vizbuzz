library(magick)
library(dplyr)
library(readr)
library(forcats)
library(ggplot2)
theme_set(theme_minimal())

dir <- 's1e01'
df <- file.path(dir, 'bump (1).csv') |> read_csv()

d <- df |> 
  mutate(
    medals = 6 * gold + 2 * silver + 1 * bronze,
    day = dense_rank(date)
  ) |> 
  arrange(day, country) |> 
  group_by(day, country) |> 
  mutate(
    cumu_medals = cumsum(medals)
  ) |> 
  ungroup() |>
  group_by(day) |> 
  mutate(
    rnk = row_number(-cumu_medals)
  ) |> 
  ungroup()
d |> filter(country == 'NOR')
d |> filter(country == 'GER')

agg <- d |> 
  group_by(country) |> 
  summarize(total_medals = sum(medals)) |> 
  ungroup() |> 
  arrange(desc(total_medals))
agg

first_day <- d |> 
  filter(day == 1) |> 
  slice_min(rnk, n = 8)
first_day

last_day <- d |> 
  slice_max(day) |> 
  slice_min(rnk, n = 10)
last_day

dd <- d |> 
  inner_join(
    agg,
    by = join_by(country)
  ) |> 
  mutate(
    country = fct_reorder(country, -total_medals),
    factor(day, levels = as.character(1:16))
  )

dd |> 
  ggplot() +
  aes(
    x = day,
    y = -rnk,
    group = country,
    color = country
  ) +
  geom_line(
    data = dd |> filter(!(country %in% c('NOR', 'GER', 'CAN', 'USA', 'NED'))),
    size = 1.5,
    color = '#DBDBDB'
  ) +
  geom_point(
    data = dd |> filter(!(country %in% c('NOR', 'GER', 'CAN', 'USA', 'NED'))),
    shape = 21,
    size = 3,
    stroke = 2,
    color = '#DBDBDB',
    fill = 'white'
  ) +
  geom_line(
    data = dd |> filter(country %in% c('NOR', 'GER', 'CAN', 'USA', 'NED')),
    size = 1.5,
    aes(color = country),
  ) +
  geom_point(
    data = dd |> filter(country %in% c('NOR', 'GER', 'CAN', 'USA', 'NED')),
    shape = 21,
    size = 3,
    stroke = 2,
    aes(color = country),
    fill = 'white'
  ) +
  scale_color_manual(
    values = c(
      'NED' = '#FDC674',
      'NOR' = '#81B873',
      'GER' = '#818181',
      'CAN' = '#FB7385',
      'USA' = '#778BC2'
    )
  ) +
  guides(
    color = 'none'
  ) +
  scale_x_continuous(
    labels = 1:16,
    breaks = 1:16
  ) +
  scale_y_continuous(
    breaks = -10:-1,
    labels = first_day |>
      mutate(label = sprintf('%s %s', row_number(), country)) |>
      pull(label) |>
      c('9', '10') |> 
      rev(),
    sec.axis = sec_axis(
      ~.,
      breaks = -10:-1,
      labels = last_day |>
        pull(country) |> 
        rev()
    )
  ) +
  coord_cartesian(
    clip = 'on',
    ylim = c(-10, -1)
  ) +
  theme(
    plot.title = element_text(
      face = 'bold',
      size = 20
    ),
    plot.subtitle = element_text(
      size = 16
    ),
    axis.title.x = element_text(
      face = 'bold',
      size = 16
    ),
    axis.title.y = element_text(
      face = 'bold',
      size = 16
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = '#DBDBDB'),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(hjust = 0),
    panel.background = element_rect(fill = 'white', color = 'white'),
    plot.background = element_rect(fill = 'white', color = 'white')
  ) +
  labs(
    title = 'PyeongChang 2018 Olympic Winter Games',
    subtitle = 'Countries ranked by overall medals after each competition day',
    y = 'Rank',
    x = 'Competition days with medals'
  )

ggsave(
  last_plot(),
  filename = file.path(dir, 'plot.png'),
  width = 8,
  height = 8
)
