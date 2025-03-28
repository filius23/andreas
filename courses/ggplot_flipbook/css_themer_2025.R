mycss <- list(
  ".hljs-tomorrow-night-bright .hljs" = list( # code chunks
    "background" = bg_color,
    # "color" = test_col, 
    "border-radius"="5px"),
  ".remark-inline-code" = list(
    "background" = highlight2, # code highlight for inline code
    "color" = bg_color,
    "border-radius" = "4px",
    "padding" = ".05px"),
  ".inverse .remark-inline-code" = list(
    "background" = bg_color,
    "border-radius" = "3px",
    "padding" = "5px"),
  ".smaller" = list("font-size" = "80%", 
                    "color" = text_color1),
  ".vsmall"  = list("font-size" = "40%", 
                    "color" = text_color1),
  
  ".panel1-manual" = list(
    "color" = bg_color,
    "width" = "49%",
    "hight" = "31%",
    "float" = "left",
    "padding-left" = "1%",
    "font-size" = "70%"
  ),
  ".panel2-manual" = list(
    "color" = bg_color,
    "width" = "48%",
    "hight" = "31%",
    "float" = "left",
    "padding-left" = "1%",
    "font-size" = "70%"
  ),
  ".panel3-manual" = list(
    "color" = text_color1,
    "width" = "48%",
    "hight" = "31%",
    "float" = "left",
    "padding-left" = "1%",
    "font-size" = "70%"
  ),
  
  "sup" = list("font-size" = "14px"),
  "h1, h2, h3" = list(
    "margin-top"=".25em", 
    "margin-bottom"=".25em"),
  ".note85" = list("font-size" = "85%", 
                   "position" = "left",
                   "float" = "left"),
  ".note50" = list("font-size" = "50%", 
                   "position" = "left",
                   "float" = "left"),
  "a" = list("text-decoration" = "underline"),
  "body" = list("line-height" = "1.4"),
  ".short" = list("height" = "30%"),
  ".title-slide" = list("background-image" = "",
                        "background-position"="5% 98%",
                        "background-size" ="21%",
                        "background-repeat"="no-repeat")
)
style_solarized_light(
  extra_css = mycss,
  header_color = heading_color,
  text_bold_color = bold_color,
  text_color = text_color1,
  link_color = link_col, #  "#b58900",
  # title_slide_background_position = "bottom",
  # title_slide_background_size = "contain" 
)
