packages <- c("tidyverse", "tidycensus", "leaflet", "mapview", "DT", "sf",
              "knitr", "rmarkdown", "kableExtra", "RColorBrewer", "tigris",
              "directlabels", "officer", "flextable", "zoo", "directlabels",
              "fmsb", "readxl", "wordcloud", "tm", "treemapify", "layer", "extrafont",
              "ggfittext", "xtable")



lapply(packages, library, character.only = TRUE)


#### load color scheme ####

#### color vectors ####

UETHDA_Colors <- c(`orange` = "#ff5100", 
                   `dark grey` = "#808285",
                   `black` = "#000000",
                   `white` = "#ffffff",
                   `dark blue` = "#0b5394",
                   `light grey` = "#cccccc",
                   `cyan` = "00ffff")

uethda_cols <- function(...){
  cols <- c(...)
  
  if(is.null(cols))
    return(UETHDA_Colors)
  
  UETHDA_Colors[cols]
}

UETHDA_palettes <- list(
  `main` = uethda_cols("orange", "dark grey", "black"),
  
  `cool` = uethda_cols("orange", "cyan", "dark blue")
)

uethda_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- UETHDA_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


scale_color_uethda <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- uethda_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("uethda_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_uethda <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- uethda_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("uethda_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#### function for tables ####

table_function <- function(data_frame, caption) {
  #use back ticks for caption to print correctly
  caption<- enquo(caption)
  
  table <- data_frame %>%
    kbl(caption = as_label(caption))%>%
    kable_classic(full_width = FALSE, html_font = "Calibri")%>%
    row_spec(0, font_size=12)%>%
    kable_styling(font_size = 12) %>%
    gsub("font-size: initial !important;", 
         "font-size: 150% !important;", 
         .)
  
  return(table)
}

#### function to remove after comma ####

remove_after_comma_function <- function(string){
  gsub(",.*","",string) 
}


?kbl
