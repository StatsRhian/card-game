## function to make a player card
library(tidyverse)
library(ggplot2)
library(cowplot)
library(magick)


player_card = function(player_info,
                       family = NULL,
                       img_position = list(xmin = -0.02, xmax = 0.02,
                                      ymin = 2, ymax = 4)) {
  img_path = paste0(here::here(), "/", player_info$img_path)
  p = player_info %>%
    ggplot() +
    ggtitle(player_info$title) +
    annotate("text", x = 0, y = 1,
             label = paste0("Primary Skill: ", player_info$primary),
                            family = family) +
    annotate("text", x = 0, y = -0.5,
             label = paste0("Secondary Skill: ", player_info$secondary),
             family = family) +
    ylim(-2, 4) +
    xlim(-0.03, 0.03) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = family),
          aspect.ratio = 4 / 3,
          plot.background = element_rect(fill = "skyblue", colour = "skyblue"),
          panel.background = element_rect(fill = "skyblue", colour = "skyblue"))
  img = img_path %>%
    magick::image_read() %>%
    magick::image_fill("none")
  raster = grDevices::as.raster(img)
  p = p +
    annotation_raster(raster,
                      img_position$xmin, img_position$xmax,
                      img_position$ymin, img_position$ymax)
  p
}
