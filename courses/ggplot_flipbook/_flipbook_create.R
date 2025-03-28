library(tidyverse)
# full flipbook
rmarkdown::render(input = "./courses/ggplot_flipbook/_index.Rmd",
                  output_file =  "index.html",output_dir = here::here("courses","ggplot_flipbook"))

# render preview 
rmarkdown::render(input = "./courses/ggplot_flipbook/_preview.Rmd",
                  output_file =  "preview.html",output_dir = here::here("courses","ggplot_flipbook"))


pagedown::chrome_print(input = "./courses/ggplot_flipbook/preview.html")
# then create gif as follows
magick::image_read_pdf(path = "./courses/ggplot_flipbook/preview.pdf", density = 100) %>% # create images
  magick::image_write_gif(path = "./courses/ggplot_flipbook/preview.gif", delay = .9) # images to gif


# https://github.com/quarto-dev/quarto-cli/discussions/1840