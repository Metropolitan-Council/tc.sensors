library(tigris)
library(sf)
library(hexSticker)
library(councilR)


options(tigris_use_cache = TRUE)
showtext::showtext_auto()
sysfonts::font_paths()
files <- sysfonts::font_files()
sysfonts::font_add("HelveticaNeueLT Std Cn", "HelveticaNeueLTStd-Cn.otf")
sysfonts::font_add("HelveticaNeueLT Std Lt", "HelveticaNeueLTStd-Lt.otf")
sysfonts::font_add("Arial Narrow", "Arial Narrow.ttf")
sysfonts::font_add("HelveticaNeueLT Std Med Cn", "HelveticaNeueLTStd-MdCn.otf")
sysfonts::font_add("Palatino Linotype", "pala.ttf")


counties <- tigris::counties(
  state = "MN",
  class = "sf"
) %>%
  filter(NAME %in% c(
    "Hennepin",
    "Dakota",
    "Carver",
    "Ramsey",
    "Anoka",
    "Scott",
    "Washington"
  ))

roads_sf <- roads(state = "MN",
                  year = 2020,
                  county = c(
                    "Hennepin",
                    "Dakota",
                    "Carver",
                    "Ramsey",
                    "Anoka",
                    "Scott",
                    "Washington")) %>%
  sf::st_simplify()


map_plot <- ggplot() +
  geom_sf(data = counties,
          color = "gray80",
          fill = "white",
          lwd = 0) +
  geom_sf(data = roads_sf %>%
            filter(RTTYP %in% c(
              "U",
              "I",
              "S"
            )),
          mapping = aes(color = MTFCC),
          size = 0.7,
          show.legend = F) +
  scale_color_brewer(direction = -1,
                     palette = "PuBu") +
  theme_void() +
  theme_transparent()

map_plot
ggsave(plot = map_plot,
       width = 7.36,
       height = 6.92,
       filename = "data-raw/subplot.png", device = "png", dpi = 300)

print(sticker(
  subplot = "data-raw/subplot.png",
  package = "tc.sensors",
  p_size = 27,
  p_color = "white",
  h_fill = councilR::colors$councilBlue,
  s_x = 1,
  s_y = 0.8,
  s_height = 0.5,
  s_width = 0.5 * 1.1,
  white_around_sticker = TRUE,
  # dpi = 300,
  h_size = 3,
  h_color = councilR::blue_cascade$level1$background,
  p_x = 1,
  p_y = 1.5,
  p_family = "HelveticaNeueLT Std Lt",
  url = "github.com/Metropolitan-Council/tc.sensors",
  u_color = "gray85",
  u_family = "Palatino Linotype",
  u_size = 4.5,
  dpi = 300
))

# saved manually at 3072 × 1920
