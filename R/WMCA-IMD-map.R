library(readxl)
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

imds <- read_excel(
  "data/File_1_IoD2025 Index of Multiple Deprivation.xlsx",
  sheet = "IMD25"
  ) %>%
  filter(
    `Local Authority District name (2024)` %in% WMCA_LAs
  ) %>%
  rename(
    "IMD Decile" = "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)"
  )

lsoa_shp <- st_read("data/LSOA21-shapes/LSOA_2021_EW_BFC_V10.shp") %>%
  right_join(
    imds,
    by = join_by("LSOA21CD"=="LSOA code (2021)")
  )

WMCA_LA_shp <- st_read("data/WMCA-LA-shapes/LAD_MAY_2025_UK_BFC_V2.shp") %>%
  filter(LAD25NM %in% WMCA_LAs)

WMCA_ward_shp <- st_read("data/WMCA-ward-shapes/WD_DEC_2025_UK_BFC.shp")

pallette = ggpubr::get_palette((c("#E47B12", "#FFFFFF")), 20)

map1 <- tm_shape(lsoa_shp) +
  tm_fill(
    "IMD Decile",
    fill.scale = tm_scale_continuous(
      values = pallette,
      ticks = c(1,5,10),
      labels = c("\n1\n(Most deprived)", "\n5\n", "\n10\n(Least deprived)")
    ),
    fill.legend = tm_legend(
      orientation = "landscape",
      margins = c(1.5, 0.5, 0.5, 0.5),
      title = "Index of Multiple Deprivation (2025)"
    )
  ) +
  tm_shape(WMCA_LA_shp) +
  tm_borders(lwd = 2)+
  tmap::tm_layout(
    legend.position = c("LEFT", "TOP"),
    scale = 0.8,
    legend.frame.alpha = 0,
    legend.frame.lwd = 0,
    legend.frame = FALSE,
    #scalebar.
    #scalebar.position = c("LEFT", "TOP"),
    #scalebar.size = 5,
    inner.margins = 0.08,
    frame = FALSE
  ) +
  tm_credits(
    paste("Produced by Birmingham City Council Public Health Department.",
          "\nContains OS data \u00A9 Crown copyright and database right",
          # Get current year
          format(Sys.Date(), "%Y"),
          ". Source:\nOffice for National Statistics licensed under the Open Government Licence v.3.0."
    ), 
    size = 0.8,
    position = c(0, 0.07))
map1 
tmap_save(map1, "output/WMCA_IMD_map.png")

# Add wards

map2 <- map1 +
  tm_shape(WMCA_ward_shp) +
  tm_borders(
    lw = 1,
    col = "darkgray"
  )
map2