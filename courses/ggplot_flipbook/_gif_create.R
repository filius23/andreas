library(tidyverse)
pagedown::chrome_print(input = "./courses/ggplot_flipbook/index.html")
# then create gif as follows
magick::image_read_pdf(path = "./courses/ggplot_flipbook/index.pdf", density = 100) %>% # create images
  magick::image_write_gif(path = "./courses/ggplot_flipbook/featured.gif", delay = .4) # images to gif


# https://github.com/quarto-dev/quarto-cli/discussions/1840