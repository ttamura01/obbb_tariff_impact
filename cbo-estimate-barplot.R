#Using ggplot2 to demonstrate the regressive nature of Trump's spending bill (CC364)
setwd("/Users/takayukitamura/Documents/R_Computing/code_club")
rm(list = ls())
library(tidyverse)
library(showtext)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(readxl)
library(glue)

font_add_google("Libre Franklin", "franklin", regular.wt = 500)
font_add_google("Libre Franklin", "franklin_light", regular.wt = 300)
showtext_opts(dpi = 300)
showtext_auto()

# Get the data! Link in bottom right corner of plot to the CBO
#   * In MS Excel format
#   * Read in with {readxl}
# 2. Do some data clean up to get in tidy format to make bar plot
#   * create a variable to indicate positive or negative net effect
#   * create bar plot with geom_col()
#   * add text with geom_text() 
#   * stylize the percent have the percent sign and positive/negative sign
#   * color label by direction of effect
#   * match the colors and remove the legend

# 3. Axis work
#   * remove x and y axis lines, text, ticks
#   * remove gridlines and background 
#   * add zero line with geom_hline
#.  * add decile information to top of negative bars, bottom of positive bars
# 4. Add titles
#.  * Need to stylize various elements 

nd_to_range <- function(x){
  
  decile_number <- as.numeric(str_remove(x, "\\w\\w$"))
  upper <- 10 * decile_number
  lower <- 10 * (decile_number - 1)
  
  glue("{lower}th-\n{upper}th")
  
}

decile_to_text <- function(x){
  

  case_when(x == "Lowest" ~ "Bottom\n10%",
            x == "Highest" ~ "Top\n10%",
            TRUE ~ nd_to_range(x))
    
}

data_url <- "https://www.cbo.gov/system/files/2025-06/61387-Data.xlsx"

download.file(data_url, "cbo-data.xlsx")

nudge <- 0.16

read_excel("cbo-data.xlsx", sheet = "Figure 2", range = "A8:F18") %>% 
  select(decile = "Household \r\nincome decile", 
         net_effect = "Net effect") %>% 
  mutate(direction = if_else(net_effect < 0, "negative", "positive"),
         decile = decile_to_text(decile),
         decile = factor(decile, levels = decile),
         decile_y = if_else(net_effect < 0, 0.45,-0.45), 
         label_y = if_else(net_effect < 0, 
                           net_effect - nudge, 
                           net_effect + nudge),
         label = if_else(net_effect < 0,
                         glue("{net_effect} %"),
                         glue("+{net_effect} %"))) %>% 
  ggplot(aes(x = decile, y = net_effect, fill = direction, label = label)) +
  geom_col() +
  geom_text(aes(y = label_y, color = direction),
            family = "franklin", size = 9, size.unit = "pt") +
  geom_text(aes( y = decile_y, label = decile, 
                 color = "gray50", lineheight = 0.9, 
                 family = "franklin")) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_fill_manual(breaks = c("negative", "positive"),
                    values = c("#D35400", "#59A497")) +
  scale_color_manual(breaks = c("negative", "positive"),
                    values = c("#D35400", "#59A497", "white")) +
  scale_x_discrete(expand = expansion(0, 0.48)) +
  labs(title = "How the Bill Would Affect Households at Different Income Ranks",
       subtitle = "Estimated annual average change in resources between 2026-34",
       caption = "Note: Estimated annual average effect of the House version of the One Big Beautiful Bill Act on after-tax\n
income. Groups are based on income adjusted for household size. \u2022 Source: Congressional Budget Office") + 
  theme_void() +
  theme(
    text = element_text(family = "franklin"),
    legend.position = "none",
    # axis.line = element_blank(),
    # axis.title = element_blank(),
    # axis.text = element_blank(),
    # axis.ticks = element_blank(),
    # panel.grid = element_blank(),
    # panel.background = element_blank(),
    # plot.title.position = "plot",
    # plot.caption.position = "plot",
    plot.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(face = "bold", size = 11,
                              margin = margin(l = -10)),
    plot.subtitle = element_text(size = 9.5, colour = "gray40",
                                 margin = margin(l = -10, t = 12, b = 15)),
    plot.caption = element_textbox_simple(size = 8, colour = "gray40",
                                          margin = margin(l = -10, b = 6, t = 15),
                                          lineheight = 1.3),
    plot.margin = margin(t = 8, l = 18,r = 10))

ggsave("cbo-estimate-barblot.png", width = 6, height = 4.22)
