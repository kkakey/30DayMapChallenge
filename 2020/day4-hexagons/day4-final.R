library(tidyverse)
library(rvest)
library(foreach)
library(geojsonio)
library(sf)
library(rgeos)

### webscrape dairy queen locations ### 
page <- read_html("https://www.dairyqueen.com/us-en/Sitemap/")

addresses.to.geocode <- html_nodes(page, ".paragraph-modules.drip-effect-tan") %>% 
  map_df(~{
    data_frame(
      city = html_nodes(.x, "ul > li") %>% html_text(trim=TRUE)
    )
  }) 

### count number of locations in each state
addresses <- addresses.to.geocode[[1]]
state_count <- c()
for (i in addresses) {
    t <- strsplit(x=i, split = ",")
    t <- unlist(t)
    state_count <- c(state_count, substring(sub(pattern=" ", "", (x=t[length(t)])), 1, 2))

}

us_states <- unique(state_count)[1:49]

dq_states <- data.frame(state = state_count) %>%
  filter(state %in% us_states) %>%
  count(state)
  

### merge data with hexgrid shapefile
spdf <- geojson_read("raw-data/us_states_hexgrid.geojson",  what = "sp")
hex_state <- read_sf("raw-data/us_states_hexgrid/us_states_hexgrid.shp")
hex_state <- rename(hex_state, state = iso3166_2)

dq_states <- hex_state %>% left_join(dq_states, by ="state")

# Calculate the centroid of each hexagon to add the label
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Bin data
dq_states$bin <- cut( dq_states$n , breaks=c(1,50,100,200,300,600), 
                      labels=c( "1-50", "51-100", "101-200", "201-300", "301+"), 
                      include.lowest = TRUE )
# keep only necessary columns
dq_states <- dq_states %>%
  select(state, geometry, n, bin)

# Plot
ggplot() +
  geom_sf(data=dq_states, fill="grey70", color = "grey20") +
  geom_sf(data=na.omit(dq_states), aes(fill=bin, geometry=geometry), color = "grey20") +
  geom_text(data=centers, aes(x=x, y=y, label=id),family="Baskerville-Normal") +
  theme(plot.title = element_text(size=22, family="Baskerville-Normal-Italic", hjust=.5, vjust=-1),
        text=element_text(size=12,  family="Baskerville-Normal"),
        plot.background =  element_rect(fill="#007ac1", color="#007ac1"),
        panel.background =  element_rect(fill="#007ac1", color="#007ac1"),
        legend.background = element_rect(fill = "#007ac1"),
        legend.key = element_rect(fill = "#007ac1"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = c(0.5, 0.9),
        plot.caption = element_text(vjust = -13, size=7)) +
  scale_fill_manual(values=c("#F9AA53","#F5864D","#F26248","#F05045","#EE3E42"),na.value = "grey70",
                    guide = guide_legend(title="", keyheight = unit(3.2, units = "mm"), 
                                         keywidth=unit(12, units = "mm"), label.position = "bottom", 
                                         title.position = 'top', nrow=1)) +
  ggtitle("Dairy Queen Locations by State") +
  labs(caption = "@kkakey | Nov. 2020") +
  theme(plot.margin = unit(c(3, 1, 3, 1), "lines")) +
  ggsave("dq2.png", dpi=400)

