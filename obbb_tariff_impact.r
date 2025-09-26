#Combined Distributional Effects of the One Big Beautiful Bill Act and of Tariffs
##web-site: https://budgetlab.yale.edu/research/combined-distributional-effects-one-big-beautiful-bill-act-and-tariffs-0

setwd("/Users/takayukitamura/Documents/R_Computing/code_club")
rm(list = ls())
library(tidyverse)
library(showtext)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(readxl)
library(glue)
library(scales)

data_url <- "https://budgetlab.yale.edu/research/combined-distributional-effects-one-big-beautiful-bill-act-and-tariffs-0#:~:text=September%20Data%20Update"

download.file(data_url, "ds.xlsx")

raw <- read_excel("/Users/takayukitamura/Documents/R_Computing/code_club/tbl_sep.xlsx", sheet = "F2", range = "A5:E15") %>% 
  select(decile = "Income Decile", "avg_income_2025" = `Average Income After Transfers and Taxes (2025 dollars)`, obbb = "OBBBA (via CBO)", tariffs_2025 = "2025 Tariff Increases", net_total = Total)

# Make decile an ordered factor so the x-axis is in the right order
dat <- raw %>% 
  mutate(decile = factor(decile, levels = decile))

# long format for the two componet bars
bars <- dat %>% 
  select(decile, obbb, tariffs_2025) %>% 
  pivot_longer(-decile, names_to = "component",
               values_to = "impact") %>% 
  mutate(
    component = recode(component,
                       obbb = "OBBB (via CBO)",
                       tariffs_2025 = "2025 Tariff Increases"),
    component = factor(component, levels = c("OBBB (via CBO)", "2025 Tariff Increases")))

# --- Plot ---
ggplot(bars, aes(x = decile, y = impact, fill = component)) +
  geom_col(position = position_stack(), alpha = 0.6) +
  scale_fill_manual(values = c("#7c4ea5", "#e6b800")) +
  geom_point(data = dat, aes(x = decile, y = net_total),
             shape = 18,
             size = 4, color = "#b30059", alpha = 1, inherit.aes = FALSE) +
  geom_text(data = dat, aes(x = decile, y = net_total,
                            label = dollar(net_total, accuracy = 1)),
            vjust = 0.5, hjust = -0.25, size = 3.0, color = "#b30059",fontface = "bold", inherit.aes = FALSE) +
  annotate(geom = "label",
           x = 7.00,
           y = 13000,
           label = "Only Top 10% of US households,\na household income of $517,000 or more, \ncould get benefits",
           colour = "#b30059",
           size = 3) +
  annotate(geom = "curve",
           x = 10, xend = 8.5,
           y = 8180, yend = 11700,
           curvature = -0.3,
           linewidth = 0.5,
           color = "#b30059") + 
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(breaks = seq(-6000, 15000, 2000),
                     labels = dollar) +
  coord_cartesian(clip = "off", expand = TRUE,
                  xlim = c(1, 10.2)) +
  labs(
    title = "Impacts of OBBB and 2025 Tariffs by Income Decile (2025 USD)",
    subtitle = "Columns show component impacts; dot + label show the net impacts",
    caption = "source: CBO (Congressional Budget Office), The Budget Lab at Yale, CNN, by Takayuki Tamura",
    x = "Income decile",
    y = "Annual impact per household (+ gain / − loss)",
    fill = "Component"
  ) +
  theme(
    plot.title.position = "panel",
    plot.title = element_text(size = 14, face = "bold", margin = margin(t = 10, b = 10)),
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.7),
    legend.title = element_blank(),
    legend.key.size = unit(2.5,"lines"),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "gray80", linewidth = 0.3),
    panel.grid.minor.y = element_line(colour = "gray90", linewidth = 0.2),
    axis.line = element_line(linewidth = 0.5, colour = "gray")
  )

ggsave("obbb_tariff_impact.png", width = 7.5, height = 6.6)

