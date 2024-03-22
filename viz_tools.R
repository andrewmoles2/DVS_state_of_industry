# State of industry data viz challenge 2023
# Information here: https://www.datavisualizationsociety.org/soti-challenge-2023?utm_source=DVS+members&utm_campaign=c2b2e0316b-EMAIL_CAMPAIGN_2024_02_15_06_22&utm_medium=email&utm_term=0_-c2b2e0316b-%5BLIST_EMAIL_ID%5D&mc_cid=c2b2e0316b
# Data dictionary here: https://docs.google.com/spreadsheets/d/1NVjRvVM5uB4uvG_hFxtm7dqIZJCnQloHYGjGFP5pB-g/edit#gid=1240845135

# load libraries ----
library(tidyverse)
library(janitor)
library(ggtext)
library(MetBrewer)
library(tidytext)
library(geomtextpath)
library(cowplot)

# load data (manually downloaded) ----
set4 <- read_csv("set4_dataviz.csv") %>%
  clean_names()

# wrangling part 1 ----
# pull out only charts and tools columns
dataviz <- set4 %>%
  select(data_viz_roles_collapsed, charts_used:charts_used_other,
         tools_freq:tools_freq_power_bi, tools_liking_top_three)

# try and wrangle fav tools column
fav_tools <- dataviz %>%
  filter(tools_liking_top_three != "[\\unfinished]" & tools_liking_top_three != "[^not answered]")

#fav_tools <- fav_tools %>%
#  separate_wider_delim(tools_liking_top_three, delim = ";", names = c("tool_1", "tool_2", "tool_3"),
#                       too_many = "merge", too_few = "align_end")


# clean up the tools ----
tools <- dataviz %>%
  mutate(tools_freq_ggplot2 = ifelse(tools_freq_ggplot2 == "[^not chosen]", "missing", tools_freq_ggplot2)) %>%
  select(tools_freq_ggplot2)

tools <- dataviz %>%
  mutate(
    across(tools_freq_arc_gis:tools_freq_power_bi, 
           ~ifelse(.x == "[^not chosen]", "missing",
                   ifelse(.x == "[\\unfinished]", "missing", .x)))
  ) %>%
  select(tools_freq_arc_gis:tools_freq_power_bi)

tools_long <- tools %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = str_replace(name, "tools_freq_", "")) %>%
  filter(value != "missing") %>%
  mutate(value = factor(value, levels = c("Never", "Rarely", "Sometimes", "Often")),
         value_num = as.numeric(value))

tools_agg <- tools_long %>%
  group_by(name) %>%
  summarise(
    n_resp = n(),
    mean_use = mean(value_num, na.rm = TRUE),
    std = sd(value_num, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_use)) %>%
  ungroup()

open_source <- c("pen paper", "figma", "ggplot2",
                 "d3", "physical materials", "plotly",
                 "matplotlib", "observable", "arc gis",
                 "leaflet", "highcharts")

tools_agg <- tools_agg %>%
  mutate(name = str_replace_all(name, "_", " ")) %>%
  mutate(open_source = if_else(name %in% open_source, TRUE, FALSE)) %>%
  mutate(tool_col = if_else(name == "ggplot2", "#d8b349", "black")) %>%
  mutate(open_col = if_else(open_source == TRUE, "#d8b349", "black"))


# wrangling part 2 - fav tools -----

fav_tools_tab <- fav_tools %>%
  select(tools_liking_top_three) %>%
  pull()
 
r_prog <- data.frame(tools = fav_tools_tab[grepl("R", fav_tools_tab, ignore.case = FALSE)]) |>
  filter(str_detect(tools, "RawGraphs", negate = TRUE)) |>
  filter(str_detect(tools, "Rawgraphs", negate = TRUE)) |>
  filter(str_detect(tools, "React", negate = TRUE)) |>
  filter(str_detect(tools, "RAW", negate = TRUE)) |>
  filter(!grepl("rstudio", tools, ignore.case = TRUE))
  
fav_tools_df <- fav_tools |>
  unnest_tokens(word, tools_liking_top_three) |>
  anti_join(stop_words) |>
  select(word) |>
  mutate(word = if_else(word == "power", "powerbi", word)) |>
  filter(word != "bi") |>
  filter(word != "js") |>
  mutate(word = case_when(
    grepl("ggplot", word) ~ "R + ggplot2",
    grepl("rstudio", word, ignore.case=T) ~ "R + ggplot2",
    .default = word
  )) |>
  add_row(word = rep("R + ggplot2", nrow(r_prog)))

google <- fav_tools_df |>
  filter(str_detect(word, "google")) |>
  pull()
google[sample(1:length(google), sum(grepl("data studio", fav_tools_tab, ignore.case = TRUE)))] <- "google data studio"
google <- ifelse(google != "google data studio", "google sheets", google)

fav_tools_df <- fav_tools_df |>
  filter(word != "data" & word != "studio" & word != "google" & word != "googlesheets" & word != "	
googlesheet" & word != "sheets") |>
  add_row(word = google) |>
  mutate(word = case_when(
    word == "pen" ~ "pen/pencil + paper",
    word == "pencil" ~ "pen/pencil + paper",
    .default = word
  )) |>
  filter(word != "paper")

fav_tools_df <- fav_tools_df |>
  mutate(word = case_when(
    str_detect(word, "seaborn") ~ "python",
    str_detect(word, "matplo") ~ "python",
    .default = word
  )) |>
  mutate(word = case_when(
    word == "raw" ~ "rawgraphs",
    word == "rawgraph" ~ "rawgraphs",
    .default = word
    )) |>
  mutate(word = case_when(
    word == "d3js" ~ "d3",
    .default = word
  ))

fav_tools_count <- fav_tools_df |>
  count(word) |> 
  arrange(desc(n))

sum(grepl("Power Bi", fav_tools_tab, ignore.case = TRUE))
sum(grepl("R", fav_tools_tab, ignore.case = FALSE) | grepl("ggplot2", fav_tools_tab, ignore.case = TRUE))
n_tableau <- sum(grepl("Tableau", fav_tools_tab, ignore.case = TRUE))

# viz 1 - data viz tool use ----
# keep simple. 
# Annotate which tool made the visual
# mess around with colours - pull colours from DVS website to use
# identify open source tools using colour highlighting
# inverse colours to have dark background
# Tableau is missing - make note on plot about this and maybe give number given in another field
sub_title <- "<span style = 'color:#d8b349;'>Open-source</span> data visualisation tools are highlighted"
f <- "Avenir"

n_tableau_str <- glue::glue("Wait, where is Tableau?!\nTableau is an obvious omission in the survey questions this data pulls from.\nHowever, practioners mention as their favourite tool {n_tableau} times.")

(
  p <- tools_agg %>%
  ggplot(aes(x = reorder(name, mean_use), y = mean_use, colour = open_col)) +
  geom_segment(aes(x = reorder(name, mean_use), y = mean_use, 
                   xend = reorder(name, mean_use), yend = 1),
               linewidth = 2, linetype = 1, lineend = "round") +
  geom_point(size = 10) +
  geom_text(aes(label = round(mean_use, 2)),
            colour = "white", family = f,
            size = 3) +
  annotate("text", x = 10, y = 2.75, label = n_tableau_str,
           size = 5, family = f) +
  coord_flip() +
  scale_y_continuous(limits = c(1, 4), breaks = seq(1, 4, 1),
                     labels = c("1 - Never", "2 - Rarely", "3 - Sometimes", "4 - Often")) +
  scale_colour_identity() +
  labs(
    title = "Average tool use of by data viz practitioners",
    subtitle = sub_title,
    caption = "SOTI challenge 2023 | Graphic: Andrew Moles",
    x = "", y = ""
  ) +
  theme_minimal(base_family = "Avenir",
                base_size = 16) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 20),
        plot.subtitle = element_markdown(),
        plot.caption = element_text(face = "italic", size = 9),
        text = element_text(family = f),
        axis.text.x = element_markdown(color = "black", size = 11),
        axis.text = element_text(size = 14),
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank(),
        axis.ticks = element_blank())
)


# viz 2 - fav tools count ----
os <- c("pen/pencil + paper", "R + ggplot2",
                 "d3", "python", "figma",
                 "svelte", "rawgraphs", "canva",
                 "observable", "qgis", "plotly")
sub_title <- "Top 20 tools are shown. <span style = 'color:#d8b349;'>Open-source</span> data visualisation tools are highlighted"

fav_light <- fav_tools_count |>
  mutate(open_source = if_else(word %in% os, TRUE, FALSE)) |>
  mutate(open_col = if_else(open_source == TRUE, "#d8b349", "black")) |>
  slice_head(n = 20) |>
  ggplot(aes(x = reorder(word, n), y = n, colour = open_col)) +
  geom_segment(aes(x = reorder(word, n), y = n, 
                   xend = reorder(word, n), yend = 1),
               linewidth = 2, linetype = 1, lineend = "round") +
  geom_point(size = 8) +
  geom_text(aes(label = round(n, 2)),
            colour = "white", family = f,
            size = 3) +
  coord_flip() +
  annotate(
    geom = "curve", x = 14.5, y = 152, xend = 18, yend = 110, 
    curvature = .3, arrow = arrow(length = unit(2, "mm")),
    linewidth = 0.75, lineend = "round", colour = "grey10"
  ) +
  annotate(geom = "text", x = 13.5, y = 130, 
           label = "R and ggplot2 were\nused to make this visual", 
           hjust = "left", family = f, size = 4, colour = "black") +
  scale_colour_identity() +
  scale_y_continuous(breaks = seq(0, 200, 25)) +
  labs(
    title = "What are data viz practitioners favourite tools?",
    subtitle = sub_title,
    caption = "SOTI challenge 2023 | Graphic: Andrew Moles",
    x = "", y = "Number of times tool was ranked in top 3 favourite tools"
  ) +
  theme_minimal(base_family = "Avenir",
                base_size = 16) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 20),
        plot.subtitle = element_markdown(),
        plot.caption = element_text(face = "italic", size = 9),
        text = element_text(family = f),
        axis.text.x = element_markdown(color = "black", size = 11),
        axis.text = element_text(size = 14),
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank(),
        axis.ticks = element_blank())
fav_light

fav_dark <- fav_tools_count |>
  mutate(open_source = if_else(word %in% os, TRUE, FALSE)) |>
  mutate(open_col = if_else(open_source == TRUE, "#d8b349", "grey95")) |>
  slice_head(n = 20) |>
  ggplot(aes(x = reorder(word, n), y = n, colour = open_col)) +
  geom_segment(aes(x = reorder(word, n), y = n, 
                   xend = reorder(word, n), yend = 1),
               linewidth = 2, linetype = 1, lineend = "round") +
  geom_point(size = 8) +
  geom_text(aes(label = round(n, 2)),
            colour = "grey5", family = f,
            size = 3) +
  coord_flip() +
  annotate(
    geom = "curve", x = 14.5, y = 152, xend = 18, yend = 110, 
    curvature = .3, arrow = arrow(length = unit(2, "mm")),
    linewidth = 0.75, linetype = 1, lineend = "round", colour = "grey95"
  ) +
  annotate(geom = "text", x = 13.5, y = 130, 
           label = "R and ggplot2 were\nused to make this visual", 
           hjust = "left", family = f, size = 4.5, colour = "grey95") +
  scale_colour_identity() +
  scale_y_continuous(breaks = seq(0, 200, 25)) +
  labs(
    title = "What are data viz practitioners favourite tools?",
    subtitle = sub_title,
    caption = "SOTI challenge 2023 | Graphic: Andrew Moles",
    x = "", y = "Number of times tool was ranked in top 3 favourite tools"
  ) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 20, family = f, color = "grey95"),
        plot.subtitle = element_markdown(family = f, color = "grey95", size = 16),
        plot.caption = element_text(face = "italic", size = 9, family = f, color = "grey95"),
        text = element_text(family = f, color = "grey95", size = 14),
        axis.text.x = element_markdown(color = "grey95", size = 14, family = f),
        axis.text.y = element_markdown(color = "grey95", size = 14, family = f),
        axis.text = element_text(color = "grey95", size = 14, family = f),
        plot.background = element_rect(fill="black", colour="black"),
        panel.background = element_rect(fill = "black"), 
        panel.border = element_blank(), 
        panel.grid = element_line(colour = "grey15"),
        strip.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())
fav_dark


# save visuals ----
ggsave("avg_tool_use.png", p,
       device = ragg::agg_png, dpi = 350, width = 12, height = 10, bg = "white")

ggsave("fav_tools_light.png", fav_light,
       device = ragg::agg_png, dpi = 300, width = 12, height = 10, bg = "white")

ggsave("fav_tools_dark.png", fav_dark,
       device = ragg::agg_png, dpi = 300, 
       width = 12, height = 10, bg = "black")

# flow fields ----
source("flow_field_setup.R")

test <- flow_setup()

curve_stroke <- 1
curve_alpha <- 0.1
limit <- max(test$i)
limit_x <- round(max(test$x_start))
limit_y <- round(max(test$y_start))

test |>
  ggplot() +
  geom_segment(aes(x = x_start, y = y_start,
                   xend = x_end, yend = y_end,
                   colour = l),
               size = curve_stroke, alpha = curve_alpha,
               lineend = "round", linejoin = "bevel") +
  scale_colour_gradient(low = "black", high = "#8CCCE4") +
  scale_fill_gradient(low = "black", high = "#B75347") +
  coord_cartesian(xlim = c(limit * 1.5, limit_x - limit * 1.5), 
                  ylim = c(limit * 1.5, limit_y - limit * 1.5)) + # "crop" to fill the frame
  theme_void() +
  theme(legend.position = "none")

# flow with words
# all tools with 4 or more resp ----
tools_words <- fav_tools_df |>
  count(word) |>
  filter(n > 4) |>
  select(word) |>
  pull()
tools_add <- fav_tools_df |>
  filter(word %in% tools_words) |>
  mutate(l = 0:1128)

less_lines <- flow_setup(noise = "perlin", n_curves = 1200, n_steps = 35,
                         octaves = 3, frequency = 0.00145,
                         pal = coolors("https://coolors.co/palette/edae49-d1495b-00798c"))

less_lines <- less_lines |>
  left_join(tools_add)

flow1 <- less_lines |>
  ggplot(aes(x_start, y = y_start, 
             group = l, colour = colour)) +
  geom_textpath(aes(label = word),
                alpha = 0.65, size = 3.5, 
                text_only = TRUE, spacing = 2) +
  scale_colour_identity() +
  theme_void()

less_lines |>
  ggplot(aes(x = x_start, y = y_start, group = l, 
             colour = colour)) +
  geom_path(size = 0.75, alpha = 0.75, 
            lineend = "round", linejoin = "round") +
  scale_colour_identity() +
  theme_void()

flow1

ggsave("fav_tools_flow_all.png", flow1,
       device = ragg::agg_png, dpi = 325, 
       width = 12, height = 12, bg = "white")

# only top 20 tools ----
twenty <- flow_setup(noise = "perlin", n_curves = 20, n_steps = 45,
                     octaves = 3, frequency = 0.00125, seed = 29,
                     pal = coolors("https://coolors.co/fff0f0-fbefff-ebf3fc-fdffea-edffec"))
# seed 11, 19, 29, 1991 work 
twenty <- fav_tools_count |>
  slice_head(n = 20) |>
  mutate(l = 0:19) |>
  right_join(twenty)

twenty |>
  ggplot(aes(x = x_start, y = y_start, group = l, 
             colour = colour, linewidth = n)) +
  geom_path(size = 0.75, alpha = 0.75, 
            lineend = "round", linejoin = "round") +
  scale_colour_identity() +
  #coord_flip() + 
  theme_void() + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", 
                                       colour = "black"))

flow2 <- twenty |>
  ggplot(aes(x = x_start, y = y_start, 
             group = l, linewidth = n,
             colour = colour)) +
  geom_textpath(aes(label = word),
                alpha = 0.9, 
                padding = unit(0.12, "inch"),
                #colour = "white",
                lineend = "round", linejoin = "round",
                family = 'Avenir') +
  annotate("text", x = 133, y = 165, 
           label = "What are data viz practitioners favourite tools?",
           colour = "white", family = 'Avenir',
           size = 7.5) +
  annotate("text", x = 133, y = 159, 
           label = "Thicker lines = more popular tools",
           colour = "white", family = 'Avenir',
           size = 5.5, fontface = "italic") +
  scale_colour_identity() +
  #coord_flip() + 
  theme_void() + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", 
                                       colour = "black"))

flow2

# I like this version best. Some edits:
# adjust colours. Enjoying the edgy simple 2 or three pal colour with dark background
# add title but do it via ggdraw or something similar - same title as before
# see if sizes can be fixed better

ggsave("fav_tools_flow_20.png", flow2,
       device = ragg::agg_png, dpi = 325, 
       width = 13, height = 13, bg = "black")
