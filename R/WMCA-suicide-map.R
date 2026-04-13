library(tmap)
library(sf)
library(dplyr)

WMCA_LAs <- c("Birmingham",
              "Coventry",
              "Dudley",
              "Sandwell",
              "Solihull",
              "Walsall",
              "Wolverhampton")

suicide_files <- c(
  "data/person-suicide-rate.csv",
  "data/person-suicide-rate-25to44.csv",
  "data/person-suicide-rate-45to64.csv"
)

titles <- c(
  "Directly Standardised Suicide Rate (all ages) per 100,000 (2022-24)",
  "Directly Standardised Suicide Rate (25-44yrs) per 100,000 (2022-24)", 
  "Directly Standardised Suicide Rate (45-64yrs) per 100,000 (2022-24)"
)

save_name <- c(
  "output/WMCA_suicide_map_allages.png",
  "output/WMCA_suicide_map_25to44.png",
  "output/WMCA_suicide_map_45to64.png"
)

for (i in 1:3) {
  suicides <- read.csv(
    suicide_files[i]
  ) %>%
    filter(
      Time.period.Sortable == max(Time.period.Sortable),
      AreaName %in% WMCA_LAs
    )
  
  WMCA_LA_shp <- st_read("data/WMCA-LA-shapes/LAD_MAY_2025_UK_BFC_V2.shp") %>%
    filter(LAD25NM %in% WMCA_LAs) %>%
    left_join(
      suicides,
      by = join_by("LAD25NM"=="AreaName")
    ) %>%
    mutate(
      plotVal = paste0(
        formatC(round(Value, 1), format = "f", digits = 1),
        "\n(", 
        formatC(round(Lower.CI.95.0.limit, 1), format = "f", digits = 1),
        " - ",
        formatC(round(Upper.CI.95.0.limit, 1), format = "f", digits = 1),
        ")"
      )
    )
  
  pallette = ggpubr::get_palette(c( "#FFFFFF", "#E47B12"), 20)
  
  map1 <- tm_shape(WMCA_LA_shp) +
    tm_fill(
      "Value",
      fill.scale = tm_scale_continuous(
        values = pallette
      ),
      fill.legend = tm_legend(
        orientation = "landscape",
        #margins = c(1.5, 0.5, 0.5, 0.5),
        title = titles[i]
      )
    ) +
    tm_borders(lwd = 2)+
    tmap::tm_layout(
      legend.position = c("LEFT", "TOP"),
      scale = 0.8,
      legend.frame.alpha = 0,
      legend.frame.lwd = 0,
      legend.frame = FALSE,
      inner.margins = c(0.08, 0.08, 0.13, 0.08),
      frame = FALSE
    ) +
    tm_credits(
      paste("Produced by Birmingham City Council Public Health Department.",
            "\nOffice for Health Improvement and Disparities. Public health profiles.",
            "\n2026 https://fingertips.phe.org.uk/ © Crown copyright 2026",
            "\nContains OS data \u00A9 Crown copyright and database right",
            # Get current year
            format(Sys.Date(), "%Y"),
            ". Source:\nOffice for National Statistics licensed under the Open Government Licence v.3.0."
      ), 
      size = 0.8,
      position = c(0, 0.12)) +
    tm_text(
      "plotVal"
    )
  map1 
  tmap_save(map1, save_name[i])
  
  
}
