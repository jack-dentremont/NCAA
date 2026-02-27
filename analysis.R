#load necessary library
library(dplyr)

#read data
ncaa <- readxl::read_xlsx("data_clean.xlsx")

#investigate structure 
str(ncaa)

#convert spread and result columns to number format
ncaa$Spread <- as.numeric(trimws(gsub(" \\(.*\\)", "", ncaa$Spread)))
str(ncaa)
any(is.na(ncaa$Spread))
ncaa$Result <- as.numeric(ncaa$Result)
str(ncaa)

#convert spreads to expected score differentials, eliminating need for spreads
ncaa$Expected_Result <- ncaa$Spread * -1
str(ncaa)
ncaa <- ncaa %>%
  select(-Spread)
str(ncaa)

#calculate if team covered spread
ncaa <- ncaa %>%
  mutate(Cover = case_when(
    Result > Expected_Result ~ 1,
    Result < Expected_Result ~ 0,
    TRUE ~ 0
  ))
head(ncaa)
str(ncaa)

#find seed with highest percentage of spreads covered
summary_table <- ncaa %>%
  group_by(Seed) %>%
  summarize(cover_rate = mean(Cover), avg_exp_diff = mean(Expected_Result), avg_result = mean(Result))
summary_table
which.max(summary_table$cover_rate)

#Create visualizations
font_add_google("Inter", "inter")
showtext_auto()

theme_portfolio <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      text              = element_text(family = "inter", color = "#2d2d2d"),
      plot.title        = element_text(face = "bold", size = rel(1.4), margin = margin(b = 4)),
      plot.subtitle     = element_text(color = "#5a5a5a", size = rel(0.95), margin = margin(b = 12)),
      plot.caption      = element_text(color = "#999999", size = rel(0.7), hjust = 0, margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title        = element_text(size = rel(0.85), color = "#5a5a5a"),
      axis.text         = element_text(size = rel(0.85)),
      panel.grid.major  = element_line(color = "#e8e8e8", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      plot.margin       = margin(20, 25, 15, 20),
      plot.background   = element_rect(fill = "white", color = NA)
    )
}

clr_above <- "#1b9e77"
clr_below <- "#d95f02"
clr_accent <- "#7570b3"

p1 <- summary_table %>%
  mutate(
    deviation   = cover_rate - 0.5,
    above_fifty = deviation >= 0,
    label       = paste0(round(cover_rate * 100, 1), "%"),
    seed_label  = factor(Seed)
  ) %>%
  ggplot(aes(x = fct_rev(seed_label), y = deviation, fill = above_fifty)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "#2d2d2d") +
  geom_text(
    aes(
      label = label,
      hjust = ifelse(deviation >= 0, -0.15, 1.15)
    ),
    size = 3.5, fontface = "bold", color = "#2d2d2d", family = "inter"
  ) +
  scale_fill_manual(values = c("TRUE" = clr_above, "FALSE" = clr_below)) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(-0.18, 0.22),
    breaks = seq(-0.15, 0.20, by = 0.05)
  ) +
  coord_flip() +
  labs(
    title    = "Which Seeds Cover the Most First-Round Spreads?",
    subtitle = "Deviation from 50% cover rate across 11 NCAA Tournaments (2014-2025)",
    x        = "Seed",
    y        = "Cover Rate Deviation from 50%",
    caption  = "Jack d'Entremont"
  ) +
  theme_portfolio()

ggsave("cover_rate_vs_.5.png", p1, width = 9, height = 7, dpi = 300, bg = "white")

p2 <- summary_table %>%
  mutate(seed_label = factor(Seed)) %>%
  ggplot(aes(x = avg_exp_diff, xend = avg_result, y = fct_rev(seed_label))) +
  geom_segment(
    aes(x = avg_exp_diff, xend = avg_result, yend = fct_rev(seed_label)),
    color = "#cccccc", linewidth = 1.8
  ) +
  geom_point(aes(x = avg_exp_diff), size = 3.5, color = clr_below) +
  geom_point(aes(x = avg_result),   size = 3.5, color = clr_above) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#999999", linewidth = 0.4) +
  # Legend as annotations (cleaner than a legend box)
  annotate("point", x = 10, y = 3.5, size = 3.5, color = clr_below) +
  annotate("text",  x = 11, y = 3.5, label = "Expected Margin (Spread)",
           hjust = 0, size = 3, color = clr_below, family = "inter") +
  annotate("point", x = 10, y = 2.2, size = 3.5, color = clr_above) +
  annotate("text",  x = 11, y = 2.2, label = "Actual Margin (Result)",
           hjust = 0, size = 3, color = clr_above, family = "inter") +
  scale_x_continuous(breaks = seq(-25, 25, by = 5)) +
  labs(
    title    = "Expected Spread vs. Actual Margin by Seed",
    x        = "Average Point Differential",
    y        = "Seed",
    caption  = "Jack d'Entremont"
  ) +
  theme_portfolio()

ggsave("plot2_dumbbell.png", p2, width = 10, height = 7, dpi = 300, bg = "white")

tbl <- summary_table %>%
  mutate(
    cover_pct   = cover_rate * 100,
    spread_edge = avg_result - avg_exp_diff
  ) %>%
  select(Seed, cover_pct, avg_exp_diff, avg_result, spread_edge) %>%
  gt() %>%
  tab_header(
    title    = md("**First-Round Spread Coverage by Seed**"),
    subtitle = "11 NCAA Tournaments — Average Expected vs. Actual Margins"
  ) %>%
  cols_label(
    Seed         = "Seed",
    cover_pct    = "Cover %",
    avg_exp_diff = "Avg Spread",
    avg_result   = "Avg Result",
    spread_edge  = "Edge (Result − Spread)"
  ) %>%
  fmt_number(columns = c(avg_exp_diff, avg_result, spread_edge), decimals = 1) %>%
  fmt_number(columns = cover_pct, decimals = 1, pattern = "{x}%") %>%
  # Color the cover % column
  data_color(
    columns  = cover_pct,
    palette  = c(clr_below, "white", clr_above),
    domain   = c(30, 70)
  ) %>%
  # Color the edge column
  data_color(
    columns  = spread_edge,
    palette  = c(clr_below, "white", clr_above),
    domain   = c(-5, 5)
  ) %>%
  tab_source_note("Jack d'Entremont") %>%
  tab_options(
    heading.align            = "left",
    table.font.size          = px(14),
    column_labels.font.weight = "bold",
    table.border.top.color   = "white",
    heading.border.bottom.color = "#e0e0e0"
  )

gtsave(tbl, "plot3_table.png", vwidth = 750)
