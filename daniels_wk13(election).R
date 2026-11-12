##WK10 Election results
##Natalie Daniels
##Data Wrangling

###In this code, I explore the election results from the 2024 presidential election
###through a variety of ways; state, county, population, race. I also explore the 2026
###election results and the swing from 2024 to 2026. 

socviz.co/index.html #install

setwd("~/DATA WRANG/wk11 election")

#And the election data for his chapters is contained in those packages. You can just load them all.  If you want to be judicious, you can just install packages needed for the chapter:
  
  
install.packages(c("socviz", "sf", "statebins", "ggrepel", "maps"))


#And then load just those packages for his maps chapter code to work (including election data):
  
library(tidyverse)  # ggplot2, dplyr, etc.

library(socviz)   # election24, states_sf, counties_sf, opiates, %nin%, theme_map()

library(sf)     # needed for geom_sf() and the *_sf objects

library(statebins)  # geom_statebins()

library(ggrepel)   # geom_label_repel() in the opiates time-series plot

library(maps) # state outlines

install.packages("ggthemes")   # once
library(ggthemes)

glimpse(election24)
glimpse(states_sf)
glimpse(counties_sf)
glimpse(opiates)

##colors 
party_colors <- c(Harris = "blue", Trump = "red")


###1a.

state_2024 <- election24 %>%
  distinct(st, .keep_all = TRUE)  

p1a <- states_sf %>%
  left_join(state_2024, by = "st") %>%  
  ggplot(aes(fill = winner)) +
  geom_sf(color = "white", linewidth = 0.2) +
  scale_fill_manual(values = party_colors, na.value = "grey85") +
  labs(title = "2024 Presidential Election Winner by State", fill = NULL) +
  theme_map() +
  theme(legend.position = "bottom")
p1a

##make it blocks
state_2024 %>%
ggplot(aes(state = state, fill = winner)) + 
  geom_statebins() +
  scale_fill_manual(values = party_colors) +
  theme_statebins()

###1b

glimpse(election24_county_df)

party_colors <- c(Democratic = "blue", Republican = "red")

counties_sf <- counties_sf %>%
  mutate(fips = str_pad(as.character(fips), 5, pad = "0"))

joined <- counties_sf %>%
  left_join(election24_county_df, by = "fips")

sum(!is.na(joined$winner))

county_winner <- ggplot(joined, aes(fill = winner)) +
  geom_sf(color = "white", linewidth = 0.05) +
  scale_fill_manual(values = party_colors, na.value = "grey85") +
  labs(title = "2024 Presidential Election Winner by County", fill = NULL) +
  theme_map() +
  theme(legend.position = "bottom")
county_winner

###1c

p1c <- ggplot(counties_sf, aes(fill = pop_dens_disc)) +
  geom_sf(color = "white", linewidth = 0.02) +
  scale_fill_brewer(palette = "Blues", na.value = "grey85") +
  labs(title = "Population Density by County", fill = "People per sq. mile") +
  theme_map() + theme(legend.position = "right")
p1c


###1d

p1d <- ggplot(counties_sf, aes(fill = black_disc)) +
  geom_sf(color = "white", linewidth = 0.02) +
  scale_fill_brewer(palette = "Purples", na.value = "grey85") +
  labs(title = "Percent Black by County", fill = "% Black") +
  theme_map() + theme(legend.position = "right")
p1d


###2. 
mid26 <- read_csv("midterms_2026_preliminary.csv")

raw26 <- read_csv("midterms_2026_preliminary.csv",
                  col_types = cols(state_fips = col_character()))

mid26 <- raw26 %>%
  filter(!is.na(candidate)) %>%
  mutate(
    office = case_when(
      str_detect(office, "Senate") ~ "Senate",
      office == "Governor" ~ "Governor"
    ),
    party3 = case_when(
      party == "D" ~ "Democratic",
      party == "R" ~ "Republican",
      TRUE ~ "Other"
    )
  )

winners <- mid26 %>%
  group_by(state_abbr, office) %>%
  mutate(top = vote_pct == max(vote_pct, na.rm = TRUE),
         n_top = sum(top, na.rm = TRUE)) %>%
  filter(top) %>%
  summarize(
    winner     = if_else(n() > 1, "Too close / tied", first(party3)),
    candidate  = paste(candidate, collapse = " / "),
    vote_pct   = first(vote_pct),
    pct_reporting = first(pct_reporting),
    .groups = "drop"
  ) %>%
  rename(st = state_abbr)

win_colors <- c(Democratic = "red", Republican = "blue",
                Other = "purple", "Too close / tied" = "yellow")

map_winner <- function(office_name) {
  states_sf %>%                                 
    left_join(filter(winners, office == office_name), by = "st") %>%
    ggplot(aes(fill = winner)) +
    geom_sf(color = "white", linewidth = 0.3) +
    scale_fill_manual(values = win_colors, na.value = "grey88",
                      breaks = names(win_colors)) +
    labs(title = paste("2026", office_name, "Winner by State"),
         subtitle = "Preliminary results (some states still counting). Grey = no race.",
         caption = "Source: midterms_2026_preliminary.csv",
         fill = NULL) +
    theme_map() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16, face = "bold"))
}

p2_senate <- map_winner("Senate")
p2_senate
p2_gov <- map_winner("Governor")
p2_gov


###3

margin26 <- mid26 %>%
  filter(party3 %in% c("Democratic", "Republican")) %>%
  group_by(state_abbr, office, party3) %>%
  summarize(pct = sum(vote_pct, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = party3, values_from = pct) %>%
  filter(!is.na(Democratic), !is.na(Republican)) %>%
  mutate(margin_2026 = Democratic - Republican) %>%
  rename(st = state_abbr)

summary(election24$pct_harris)

margin24 <- election24 %>%
  transmute(st, margin_2024 = pct_harris - pct_trump)

swing <- margin26 %>%
  inner_join(margin24, by = "st") %>%
  mutate(swing = margin_2026 - margin_2024)

swing %>% arrange(desc(swing)) %>% print(n = 10)   # sanity check

map_swing <- function(office_name) {
  d <- filter(swing, office == office_name)
  lim <- max(abs(d$swing), na.rm = TRUE)
  states_sf %>%
    left_join(d, by = "st") %>%
    ggplot(aes(fill = swing)) +
    geom_sf(color = "white", linewidth = 0.3) +
    scale_fill_gradient2(low = 'red', mid = "white", high = "blue",
                         midpoint = 0, limits = c(-lim, lim),
                         na.value = "grey88",
                         name = "Swing in Democratic margin (pts)") +
    labs(title = paste("Swing: 2024 President to 2026", office_name),
         subtitle = "Blue = shift toward Democrats, red = toward Republicans. Preliminary 2026 data; grey = no comparable race.",
         caption = "Source: socviz::election24; midterms_2026_preliminary.csv") +
    theme_map() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16, face = "bold"),
          legend.key.width = unit(1.5, "cm"))
}

p3_senate <- map_swing("Senate")
p3_senate
p3_gov <- map_swing("Governor")
p3_gov


###4
us <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

#starbuild
ang  <- seq(0, 2 * pi, length.out = 11)[-11] + pi / 2
r    <- rep(c(1.3, 0.5), 5)                    
x    <- -76.1351 + r * cos(ang) / 0.73      
y    <-  43.0392 + r * sin(ang)
star <- st_sfc(st_polygon(list(cbind(c(x, x[1]), c(y, y[1])))), crs = 4326)

p4 <- ggplot() +
  geom_sf(data = us, fill = "grey92", color = "white") +
  geom_sf(data = star, fill = "orange", color = NA) +
  labs(title = "Location of Syracuse University") +
  theme_map()
p4

###5

peaks <- tribble(
  ~rank, ~name,               ~elev_ft, ~prom_ft, ~lat,    ~lon,     ~coord_src,
  1, "Washington",           6288, 6138, 44.2705, -71.3033, "wikipedia",
  2, "Adams",                5774,  804, 44.3206, -71.2914, "wikipedia",
  3, "Jefferson",            5712,  742, 44.3042, -71.3169, "wikipedia",
  4, "Monroe",               5384,  254, 44.2556, -71.3225, "wikipedia",
  5, "Madison",              5367,  466, 44.3288, -71.2768, "wikipedia",
  6, "Lafayette",            5260, 3320, 44.1609, -71.6444, "approx",
  7, "Lincoln",              5089,  180, 44.1494, -71.6447, "approx",
  8, "South Twin",           4902, 1522, 44.1878, -71.5517, "approx",
  9, "Carter Dome",          4832, 2830, 44.2669, -71.1783, "approx",
  10, "Moosilauke",           4802, 2933, 44.0247, -71.8314, "approx",
  11, "Eisenhower",           4780,  350, 44.2377, -71.3516, "approx",
  12, "North Twin",           4761,  299, 44.2008, -71.5542, "approx",
  13, "Carrigain",            4700, 2240, 44.0828, -71.4472, "approx",
  14, "Bond",                 4698,  299, 44.1567, -71.5319, "approx",
  15, "Middle Carter",        4610,  720, 44.2844, -71.1858, "approx",
  16, "West Bond",            4540,  299, 44.1555, -71.5664, "approx",
  17, "Garfield",             4500,  820, 44.1936, -71.6136, "approx",
  18, "Liberty",              4459,  379, 44.1125, -71.6458, "approx",
  19, "South Carter",         4430,  230, 44.2733, -71.1814, "approx",
  20, "Wildcat A",            4422, 1034, 44.2564, -71.2008, "approx",
  21, "Hancock",              4420, 1247, 44.0347, -71.5090, "approx",
  22, "South Kinsman",        4358, 2240, 44.1167, -71.7799, "approx",
  23, "Field",                4340, 1706, 44.1869, -71.4461, "approx",
  24, "Osceola",              4340,  200, 44.0064, -71.5211, "approx",
  25, "Flume",                4328,  408, 44.1067, -71.6528, "approx",
  26, "South Hancock",        4319, 1247, 44.0248, -71.5122, "approx",
  27, "Pierce",               4310,  235, 44.2336, -71.3744, "approx",
  28, "North Kinsman",        4293, 2240, 44.1261, -71.7771, "approx",
  29, "Willey",               4285,  256, 44.1783, -71.4414, "approx",
  30, "Bondcliff",            4265,  299, 44.1458, -71.5461, "approx",
  31, "Zealand",              4260,  246, 44.1944, -71.5114, "approx",
  32, "North Tripyramid",     4180, 1320, 43.9856, -71.4181, "approx",
  33, "Cabot",                4170, 2674, 44.5133, -71.4192, "approx",
  34, "East Osceola",         4156,  316, 44.0042, -71.5033, "approx",
  35, "Middle Tripyramid",    4140, 1320, 43.9794, -71.4217, "approx",
  36, "Cannon",               4100,  740, 44.1567, -71.6988, "approx",
  37, "Hale",                 4054,  614, 44.2128, -71.5256, "approx",
  38, "Jackson",              4052,  331, 44.2017, -71.4139, "approx",
  39, "Tom",                  4051,  331, 44.2128, -71.4525, "approx",
  40, "Wildcat D",            4050, 1034, 44.2400, -71.2022, "approx",
  41, "Moriah",               4049,  922, 44.3556, -71.1114, "approx",
  42, "Passaconaway",         4043,  803, 43.9717, -71.3586, "approx",
  43, "Owl's Head",           4025,  825, 44.1167, -71.5856, "approx",
  44, "Galehead",             4024,  264, 44.1767, -71.5717, "approx",
  45, "Whiteface",            4020,  591, 43.9831, -71.3772, "approx",
  46, "Waumbek",              4006, 1289, 44.3633, -71.5186, "approx",
  47, "Isolation",            4004,  203, 44.2056, -71.3000, "approx",
  48, "Tecumseh",             4003, 1739, 43.9669, -71.5431, "approx"
)
library(tidyverse)
library(sf)
library(maps)
library(ggrepel)

# peaks as points in plain lat/lon
peaks_sf <- peaks %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# New Hampshire only: state outline and counties
nh_state    <- st_as_sf(map("state",  "new hampshire", plot = FALSE, fill = TRUE))
nh_counties <- st_as_sf(map("county", "new hampshire", plot = FALSE, fill = TRUE))

# MAP 1: NH with peaks colored by elevation
pA <- ggplot() +
  geom_sf(data = nh_counties, fill = "grey94", color = "grey75", linewidth = 0.3) +
  geom_sf(data = nh_state, fill = NA, color = "grey30", linewidth = 0.8) +
  geom_sf(data = peaks_sf, aes(color = elev_ft), size = 4) +
  scale_color_viridis_c(option = "magma", direction = -1, name = "Elevation (ft)") +
  coord_sf(xlim = c(-71.95, -71.0), ylim = c(43.92, 44.58), expand = FALSE) +
  labs(title = "New Hampshire's 48 Four-Thousand-Footers",
       subtitle = "Colored by elevation") +
  theme_void()
pA


# MAP 2:
pB <- ggplot() +
  geom_sf(data = nh_counties, fill = "grey94", color = "grey75", linewidth = 0.3) +
  geom_sf(data = nh_state, fill = NA, color = "grey30", linewidth = 0.8) +
  geom_sf(data = peaks_sf, aes(size = prom_ft), color = "#2E5E4E", alpha = 0.7) +
  geom_sf_text(data = filter(peaks_sf, rank <= 6), aes(label = name),
               nudge_y = 0.03, size = 3, fontface = "bold") +
  scale_size_area(max_size = 12, name = "Prominence (ft)") +
  coord_sf(xlim = c(-71.95, -71.0), ylim = c(43.92, 44.58), expand = FALSE) +
  labs(title = "How Independent Is Each Peak?",
       subtitle = "Sized by prominence; top 6 peaks labeled") +
  theme_void()
pB



####dataset for 48 https://www.peakbagger.com/list.aspx?lid=21404
