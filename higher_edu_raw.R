## Higher Education Enrollments Ages 18-34 by County, 2016-2019
  
## Processing Census Data
library(ggplot2)      # For plotting
library(tidycensus)   # For downloading Census data
library(pander)       # For customizing tables
library(tmap)         # For creating tmap
library(oldtmaptools) # Contains old (deprecated) functions from tmaptools
library(tmaptools)    # For reading and processing spatial data related to tmap
library(dplyr)        # For data wrangling
library(sf)           # For reading, writing and working with spatial objects

readRenviron("~/.Renviron")

Sys.getenv("CENSUS_API_KEY")

# Queries to view datasets based on specific criteria. 
v16 <- load_variables(year = 2016,
                      dataset = "acs5",                      
                      cache = TRUE)

v19 <- load_variables(year = 2019,
                      dataset = "acs5",                      
                      cache = TRUE) 

### Downloading Census Data: 

hi_ed_enroll = variables = c(male_public_1824="B14004_005", male_public_2534 = "B14004_006", male_private_1824 = "B14004_010", male_private_2534 = "B14004_011", female_public_1824 = "B14004_021", female_public_2534 = "B14004_022", female_private_1824 = "B14004_026", female_private_2534= "B14004_027", male_1819 = "B01001_007", male_20 = "B01001_008", male_21 = "B01001_009", male_2224 = "B01001_010", male_2529 = "B01001_011", male_3034 = "B01001_012", female_1819 = "B01001_031", female_20 = "B01001_032", female_21 = "B01001_033", female_2224 = "B01001_034", female_2529 = "B01001_035", female_3034 = "B01001_036") 

dat16 <- get_acs(geography = "county", hi_ed_enroll,  
  year = 2016, output = "tidy", state = NULL, geometry = FALSE) %>%
  rename(`2016` = estimate) %>%
  select (- NAME, -moe) 

dat19 <- get_acs(geography = "county", hi_ed_enroll,  
  year = 2019, output = "tidy", state = NULL, geometry = TRUE, shift_geo = TRUE) %>%
  rename(`2019` = estimate) %>%
  select(-moe)

### Joining the 2016 and 2019 Data:

dat <- left_join(dat19, dat16, by = c("GEOID", "variable")) 

st_geometry(dat) <- NULL 

### Assigning Enrollment Categories:

dat <- mutate(dat,
              variable = case_when(
                variable %in% c("male_public_1824", "male_public_2534", "male_private_1824", "male_private_2534", "female_public_1824", "female_public_2534", "female_private_1824", "female_private_2534") ~ "enrolled1834", 
                variable %in% c("male_1819", "male_20", "male_21","male_2224", "male_2529", "male_3034", "female_1819", "female_20", "female_21",  "female_2224", "female_2529", "female_3034") ~ "pop1834"))

### Summarizing the Data by County-Year-Category:

# Create long version
dat <- tidyr::gather(dat, year, estimate, c(`2016`, `2019`)) 

# Group the data by our new categories and sum
dat <- group_by(dat, GEOID, NAME, year, variable) %>% 
  summarize(estimate = sum(estimate)) %>%
  ungroup() %>%
  tidyr::spread(variable, estimate) 

### Calculating the Final Estimates: 

dat <- mutate(dat, est = (enrolled1834/pop1834) * 100) %>%
  select(-c(enrolled1834, pop1834)) %>%
  tidyr::spread(year, est) %>%
  mutate(diff = `2019`-`2016`)

## Overview of the Higher Education Enrollment Rate in the U.S

### Distribution by Year:

datlong <- select(dat, -diff) %>%
  tidyr::gather(year, estimate, c(`2016`, `2019`)) %>%
  group_by(year) %>%
  mutate(mean = round(mean(estimate, na.rm = TRUE), 1)) %>%
  mutate(median = round(median(estimate, na.rm = TRUE), 1))

# Compare the county distributions as well as the % mean of enrollment by year. 
ggplot(datlong, aes(estimate)) +
  geom_histogram(fill = "firebrick2", 
    color = "white", bins = 60) +
  labs(x= "Enrolled Adults Ages 18-34 (%) ", y= "Count",
       title="Figure 1: Distribution of Higher Education Enrollment Ages 18-34", 
       subtitle="American Community Survey, 2016-2019", 
       caption= "Source: United States Census Bureau ") +
  theme(plot.subtitle= element_text(face = "italic"))+
  facet_wrap(~year, ncol = 1) +
  geom_vline(aes(xintercept = mean, 
    group = year), lty = "dashed") +
  geom_vline(aes(xintercept = median, 
    group = year), lty = "dashed")+ 
  geom_text(aes(label = paste("Mean = ", mean), x = mean, y = 55))+
  geom_text(aes(label = paste("Median = ", median), x = median, y = 150))

t_mean<- t.test(datlong$estimate[datlong$year==2016],
       datlong$estimate[datlong$year==2019]) 
# At a 95% confidence level, we can reject the null hypothesis because the test statistic is greater than 1.96, the p-value is less than 0.05 and the 95% confidence interval does not include 0. 

panderOptions('table.split.table', Inf) 

pander(t_mean, 

caption = "Two-Sample T-Test for Higher Education Enrollment, 2016-2019")

## Mapping the Continental U.S with Tmap

### Creating a Geographic File: 

shp <- dat19 %>%
  filter(variable == "male_public_1824") %>% # much faster than using distinct()
  select(GEOID, NAME) %>%
  left_join(dat, by = c("GEOID", "NAME")) %>%
  arrange(GEOID) %>%
  rename(enrolled_2016 = `2016`,
    enrolled_2019 = `2019`,
    enrolled_diff = diff)


# Remove the Aleutians West from shp for display purposes.
shp <- filter(shp, GEOID != "02016")

### Mapping: 

# Create a state FIPS field.
shp <- mutate(shp, STFIPS = stringr::str_sub(GEOID, 1, 2))

# Aggregate the county layer by state. 
states <- shp %>%
  aggregate_map(by = "STFIPS") 

cuts <- c(0, 10, 14, 17, 22, 100)

# Allows us to tweak the features that we want like variable name and title.
createMap <- function(.data, varname, statecol, maptitle){ 
mymap <- tm_shape(shp, projection = 2163, unit= "mi") +
  tm_polygons(varname,
    breaks = cuts, 
    palette = "BuPu", 
    title = "Enrolled (%)") +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(title = maptitle,
     title.size = 1.1,
     title.position = c("center", "top"))
 
mymap + tm_shape(states) +
  tm_borders(col = statecol, alpha = 0.5)+
  tm_layout(inner.margins = c(0.06, 0.10, 0.10, 0.08)) +
  tm_scale_bar(color.dark = "gray60",
               position = c("right", "bottom")) + 
  tm_compass(type = "4star", size = 2.5, text.size = 0.5,
     color.dark = "gray60", text.color = "gray60",
     position = c("left", "top"))
}

### Continental Map of Higher Education Enrollment Ages 18-34 by County, 2016-2019:
m1 <- createMap(shp, 
  varname = "enrolled_2016", 
  statecol = "black", 
  maptitle = "Figure 2: Higher Education Enrollment Ages 18-34 by County, 2016")

m1

m2 <- createMap(shp, 
  varname = "enrolled_2019", 
  statecol = "black",
  maptitle = "Figure 3: Higher Education Enrollment Ages 18-34 by County, 2019")

m2

## Taking a Deeper Dive into Higher Education Enrollment Rates by Individual Counties

### Counties with Greatest Change (+/-) in Percent of Higher Education Enrollment:

d10 <- top_n(dat, 10, diff) %>%
  mutate(type = "Enrollment Decreased",
    difftemp = diff)

i10 <- top_n(dat, -10, diff) %>%
  mutate(type = "Enrollment Increased",
    difftemp = abs(diff))

id10 <- bind_rows(list(i10, d10)) %>%
  arrange(desc(difftemp))

ggplot(id10) + 
  geom_col(aes(x = forcats::fct_reorder(NAME, difftemp), 
    y = difftemp, fill = type)) +
  ylim(0,50)+
  coord_flip() +
  scale_fill_manual(values = c("firebrick2", "cyan4")) +
   theme(plot.subtitle= element_text(face = "italic"),
         legend.position = "bottom",
         legend.title = element_blank()) +
  labs(x= "", y= "Difference in % Enrolled 2016-2019",
       title="Figure 4: Counties with the Greatest Change (+/-) \nin Higher Education Enrollment Ages 18-34", 
       subtitle="American Community Survey, 2016-2019", 
       caption= "Source: United States Census Bureau ")

### Exploring Higher Education Enrollment Rates in Texas: 

### Preparing the Texas Data:

# Filter to Texas and create some additional fields for mapping. Since we'll be using
tx <- filter(shp, STFIPS == "48") %>%
  mutate(NAME = stringr::str_remove(NAME, ", Texas"),
         `Higher Education Enrollment in Texas Ages 18-34, 2016-2019` = case_when(
           enrolled_diff < 0  ~ "Enrollment increased",
           enrolled_diff > 0  ~ "Enrollment decreased",
           enrolled_diff == 0 ~ "Enrollment stayed the same"),
         diff2 = round(abs(enrolled_diff), 1),
         popup = ifelse(enrolled_diff != 0,
                        paste0(`Higher Education Enrollment in Texas Ages 18-34, 2016-2019`, " by ", diff2, "%"), 
                        `Higher Education Enrollment in Texas Ages 18-34, 2016-2019`),
         diffrad = as.numeric(cut(diff2, c(0, 5, 10, 20, 30, 45), 
                                  right = FALSE)))

# Remove some unnecessary fields
tx <- select(tx, -c(enrolled_2016, enrolled_2019, enrolled_diff, STFIPS))

### Interactive Map of Higher Education Enrollment in Texas Ages 18-34, 2016-2019: 

# Basemap
carto <- "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}{r}.png"

# Created a "normal" tmap except we'll add the basemap and the `popup.vars` argument.The symbol size of the bubbles will be based on the data so use our calculated field `diffrad` which will apply sizes 1 through 5. Sizes can be further adjusted using the `scale` argument.
txmap <- tm_basemap(carto) +  
  tm_shape(tx) +
  tm_borders(col = "azure2") +
  tm_bubbles("diffrad", 
             col = "Higher Education Enrollment in Texas Ages 18-34, 2016-2019", 
             border.col = "white", 
             scale = 1.5,
             style = "fixed",
             palette = c("coral2", "aquamarine3", "gray"),
             popup.vars = c("County: " = "NAME", "Change: " = "popup"))

tmap_leaflet(txmap)