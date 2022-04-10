
library(ggplot2)
library(ggtext)

sysfonts::font_add_google("Josefin Sans")
showtext::showtext_auto()

associates <- read.csv2(file = here::here("data/associates.csv"), header = TRUE) |> 
  dplyr::mutate(date = as.POSIXct(date)) |> 
  tibble::as_tibble()

associates_thousands <- associates |> 
  dplyr::mutate(thousands = floor(associates/1000)) |>
  dplyr::group_by(thousands) |> 
  dplyr::summarise(date = dplyr::first(date),
                   associates = dplyr::first(associates)) |> 
  dplyr::ungroup()

events <- tibble::tribble(
  ~date_start, ~date_end, ~description,
  as.POSIXct("2021-11-23 00:00:00"), as.POSIXct("2021-11-23 00:00:00"), "(23/11/21)<br><br>Sócio<br>Friday",
  as.POSIXct("2021-12-17 00:00:00"), as.POSIXct("2021-12-17 00:00:00"), "(17/12/21)<br><br>Preto<br>Fiction",
  as.POSIXct("2022-01-06 00:00:00"), as.POSIXct("2022-01-06 00:00:00"), "(06/01/22)<br><br>Promoção<br>Tradição 2021",
  as.POSIXct("2022-03-04 00:00:00"), as.POSIXct("2022-03-04 00:00:00"), "(04/03/22)<br><br>Promoção<br>sócias torcedoras",
  as.POSIXct("2022-03-25 13:00:00"), as.POSIXct("2022-03-25 13:00:00"), "(25/03/22)<br><br>Sorteio<br>Libertadores",
  as.POSIXct("2022-04-07 19:00:00"), as.POSIXct("2022-04-07 19:00:00"), "(07/04/22)<br><br>Estreia na<br>Libertadores"
)

max_associates <- max(associates$associates)
min_associates <- min(associates$associates)
events_associates <- events |> 
  dplyr::mutate(date_start = as.Date(date_start)) |> 
  dplyr::left_join(
    
    associates |>
      dplyr::mutate(date = as.Date(date)) |> 
      dplyr::group_by(date) |> 
      dplyr::summarise(associates = max(associates)) |> 
      dplyr::ungroup(),
    
    by = c("date_start" = "date")
  ) |> 
  dplyr::mutate(associates = ifelse(is.na(associates), min_associates, associates))

red <- "#e2211c"
blue <- "#2962a7"
white <- "#fdffff"
f1 <- "Josefin Sans"

position_max <- round(max(associates$associates), -4) + 5000
p1 <- ggplot(associates) + 
  geom_line(aes(x = date, y = associates), size = 1, color = blue) +
  geom_point(data = dplyr::summarise(associates, date = max(date), associates = max(associates)), aes(x = date, y = associates), color = blue, size = 3) + 
  geom_text(data = dplyr::summarise(associates, date = max(date), associates = max(associates)), aes(x = date, y = associates, label = associates), color = blue, size = 15, vjust = -1, family = f1) + 
  
  geom_segment(data = events_associates, aes(x = date_end, y = associates, xend = date_end, yend = position_max + 5000), color = blue, linetype = 3, size = 0.5, alpha = 0.4) +
  geom_richtext(
    data = events_associates, aes(x = date_end, y = position_max + 5000, label = description),
    color = blue, size = 11, family = f1, vjust = -0.5, fill = NA, lineheight = 0,
    label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  
  scale_x_datetime(name = "data", date_labels = "%b/%y", limits = c(min(associates$date), max(associates$date) + 1e3)) +
  scale_y_continuous(breaks = seq(min(associates$associates), position_max, 5000), limits = c(min(associates$associates) - 250, position_max + 7000)) + 
  labs(
    title = "<span style='color:#e2211c'>Sócios torcedores do Fortaleza Esporte Clube</span>", 
    subtitle = "<i>Evolução da quantidade de sócios de 23/11/21 a 09/04/22</i>",
    caption = "Fonte: www.sociofortaleza.com.br · Gráfico: Matheus S. Rodrigues"
  ) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = white, color = NA),
    plot.title = element_markdown(face = "bold", size = 93, family = f1, margin = margin(t = 10, b = 0)),
    plot.subtitle = element_markdown(size = 40, margin = margin(5, 0, 30, 0), color = "grey40", family = f1),
    plot.caption = element_text(margin = margin(15, 0, 0, 0), size = 30, color = "grey40"),
    plot.margin = margin(rep(20, 4)),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 30, family = f1, color = "grey40"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, size = 30, family = f1, color = "grey40", margin = margin(r = 10)),
    panel.grid.major.y = element_line(color = "gray90", size = 0.5, linetype = 3)
  )

ggsave(plot = p1, filename = here::here("img/plot.png"), width = 10, height = 8, units = "in", dpi = 320)
