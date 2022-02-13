# Setting the working directory
setwd("C:/Users/hp/Documents/Research Project/")

# Loading the necessary libraries
# install.packages("ggcorrplot")
library(corrplot)
library(tidyverse)
library(janitor)
library(scales)
library(reshape2)

colomboDistrict <- c("battaramulla", "malabe", "nugegoda", "piliyandala",
                     "pannipitiya", "pita kotte", "kottawa", "colombo 7",
                     "thalawathugoda", "colombo 5", "dehiwala", "pelawatte",
                     "maharagama", "colombo 6", "colombo 3", "hokandara",
                     "homagama", "kohuwala", "kalubowila", "kiribathgoda" , 
                     "kaduwela", "colombo 4", "mattegoda", "koswatta" , 
                     "colombo 9", "padukka", "angoda", "colombo 2", "kolonnawa",
                     "udahamulla", "attidiya", "colombo 10", "nawinna", "boralesgamuwa",
                     "nawala", "athurugiriya", "mount lavinia" , "moratuwa", "colombo 8",
                     "rajagiriya", "ethul kotte" , "rathmalana", "pepiliyana", "thalahena",
                     "mirihana", "katubedda",  "madiwela", "kotikawatta", "polgasowita", 
                     "arangala" , "akuregoda" , "kahatuduwa", "pittugala", "thalapathpitiya",
                     "wijerama" , "colombo 15",  "bellanwila", "colombo 13", "enderamulla",
                     "kawdana", "kumaragewatta" , "dalugama", "ederamulla","embuldeniya",
                     "kahanthota", "kalalgoda" , "makuluduwa","mulleriyawa", "navinna", "pitipana",
                     "raththanapitiya" , "wikramasinghapura battaramulla", 
                     "delkanda", "kesbewa", "lake drive", "meegoda", "wellampitiya", "avissawella" ,
                     "colombo 1" , "gothatuwa", "kosgama",  "thalangama",  "bokundara", "colombo 12",
                     "depanama", "himbutana",  "karagampitiya", "palanwaththa","pamunuwa", 
                     "weliweriya" 
)
gampahaDistrict <- c("wattala", "negombo", "kandana", "kadana", "ja ela",
                     "ragama", "gampaha", "kerawalapitiya", "kadawatha", 
                     "nittambuwa", "katunayake", "kelaniya", "ganemulla", "biyagama",
                     "minuwangoda","mabola",  "divulapitiya", "uswetakeiyawa","kattuwa",
                     "kirindiwela", "pallawatta", "sapugaskanda", "veyangoda" , "welisara",
                     "seeduwa", "ekala" , "kalapaluwawa", "mahabage", "pamunugama", "gampa",
                     "kirillawala", "makola", "mirigama",  "naiwala",  "peliyagoda", "walpola",
                     "yakkala"
)
kandyDistrict <- c("kandy", "alawathugoda", "kadugannawa", "nuwara eliya", "akurana",
                   "digana", "menikhinne", "nugawela", "peradeniya",  "katugastota" , 
                   "aniwatte",  "pilimathalawa"
)
kalutaraDistrict <- c("kalutara", "panadura", "bandaragama", "horana", "ingiriya",
                      "matugama", "wadduwa", "beruwala", "alubomulla", "gonapola",
                      "jawatta", "mathugama", "nugagoda")
ratnapuraDistrict <- c("ratnapura", "balangoda")
galleDistrict <- c("unawatuna", "habaraduwa", "galle", "ahangama", "koggala", "bentota",
                   "gonapinuwala", "kotagedara", "hikkaduwa", "talpe", "ahungalla", "kalahe",
                   "kosgoda" , "manampita")
mataraDistrict <- c("mirissa", "weligama", "matara","devinuwara", "dikwella", "kamburugamuwa")
kurunegalaDistrict <- c("kurunegala", "kanduboda", "pannala")
badullaDistrict <- c("bandarawela", "ella", "welimada")
mataleDistrict <- c("matale", "dambulla" )
kegalleDistrict <- c("kegalle", "mawanella", "gonawala", "warakapola", "yatiyanthota", "rambukkana",
                     "ruwanwella")
anuradhapuraDistrict <- c("anuradhapura", "habarana" )
amparaDistrict <- c("kalmunai", "ampara")
polonnaruwaDistrict <- c("polonnaruwa", "medirigiriya" )
hambantotaDistrict <- c("tangalle",  "hambantota", "beliatta"  )
jaffnaDistrict <- c("ariyalai")
puttalamDistrict <- c("chilaw", "dankotuwa", "marawila", "thalwila" )
trincomaleeDistrict <- c("trincomalee")
vavuniyaDistrict <- c("vavuniya")

data <- read_csv("https://raw.githubusercontent.com/nirshad97/Research-Project/main/web-scraping/FullDataFrame.csv")
data %>% head()

# Getting the missing values as a perrcentage for all the features
pct_na_vals <- data %>% 
  select(- "...1" ) %>% 
  clean_names() %>% 
  is.na() %>% 
  colSums()/dim(data)[1]

pct_na_df <- as.data.frame((round((pct_na_vals*100), 2)))
colnames(pct_na_df) <- c("Percentage of NA values")

pct_na_df <- pct_na_df %>% 
  arrange(desc(`Percentage of NA values`)) %>% 
  mutate(`Percentage of NA values` = paste0(`Percentage of NA values`, "%"))


# To View thew missing percentages
pct_na_df %>% 
  View()


df <- data %>% 
  select(- "...1" ) %>% 
  clean_names() %>% 
  filter(!is.na(property_type)) %>% 
  select(-property_type)


# Changing some characters to factors

df$bedrooms <- as.integer(df$bedrooms)
# df$no_of_floors <- as.factor(df$no_of_floors)
# df$car_parking_spaces <- as.factor(df$car_parking_spaces)
df$area_of_land <- str_to_lower(df$area_of_land)
df$availability <- as.factor(df$availability)

df1 <- df %>% 
  mutate(area = as.character(str_extract(area_of_land,"^[^\\s]+")),
         unit = str_trim(str_extract(area_of_land, "\\s.+")),
         availability = str_trim(availability)) %>% 
  select(-area_of_land) %>% 
  select(location, area, unit, everything())  %>%
  mutate(location = tolower(location),
         district = as.factor(case_when
                              (
                              location %in% colomboDistrict ~ "Colombo",
                              location %in% gampahaDistrict ~ "Gampaha",
                              location %in% kandyDistrict ~ "Kandy",
                              location %in% kalutaraDistrict ~ "Kalutara",
                              location %in% ratnapuraDistrict ~ "Ratnapura",
                              location %in% galleDistrict ~ "Galle",
                              location %in% mataraDistrict ~ "Matara",
                              location %in% kurunegalaDistrict ~ "Kurunegala",
                              location %in% badullaDistrict ~ "Badulla",
                              location %in% mataleDistrict ~ "Matale",
                              location %in% kegalleDistrict ~ "Kegalle",
                              location %in% anuradhapuraDistrict ~ "Anuradhapura",
                              location %in% amparaDistrict ~ "Ampara",
                              location %in% polonnaruwaDistrict ~ "Polonnaruwa",
                              location %in% hambantotaDistrict ~ "Hambantota",
                              location %in% jaffnaDistrict ~ "Jafna",
                              location %in% puttalamDistrict ~ "Puttalam",
                              location %in% trincomaleeDistrict ~ "Trincomalee",
                              location %in% vavuniyaDistrict ~ "Vavuniya"
         )),
         floor_area = as.double(str_extract(floor_area, "\\d+\\.*\\d*")),
         car_parking_spaces = as.factor(case_when(is.na(car_parking_spaces) ~ "None",
                                                  TRUE ~ as.character(car_parking_spaces))))


df2 <- df1 %>% 
  mutate(price = as.numeric(str_extract(price,  "\\d+\\.*\\d*")),
         area=as.numeric(area)) %>% 
  select(-starts_with("nearest")) 

df2 <- df1 %>% 
  mutate(area=as.numeric(area))


outlier1 <- df2 %>%
  mutate(district = fct_lump(district, 5)) %>%  
  ggplot(aes(x=fct_reorder(district, price , .fun = length, .desc = T), 
             y=price)) +
  geom_point(position = position_jitter(width = 0.25), aes(color=district), size=2, alpha=0.43) +
  geom_point(stat="summary", fun=mean, size=3.5, color="#ab0202") + 
  scale_y_log10(name = "Price in Millions (log-scaled)") + 
  scale_color_brewer(type="qual", palette =2, guide="none") + 
  labs(title = "House Prices with Districts",
       subtitle = "Each individual dots represent a house and its respective price. The dark red dots represent the mean prices of houses\nfor that specific district. Moreover, the y-scale of the graph has been scaled logarithmically for visually identifying outliers",
       x="Districts") + 
  theme_minimal(base_family = "PT Sans", base_size = 12) + 
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.title.position = "plot")

outlier1

ggsave("outlier4.png", plot=outlier1, height = 5, width=11, unit="in")
  
df2 %>%
  mutate(district = fct_lump(district, 5)) %>% 
  ggplot(aes(x=price)) + geom_histogram() + 
  facet_wrap(~district)

price_distribution <- df2 %>% 
  ggplot(aes(x=price)) + geom_histogram(fill="#ffcf4a") + 
  labs(title="Distribution of the house prices", 
       x = "Prices in millions (LKR)", 
       y= "") + 
  scale_x_continuous(breaks = seq(0, 1000, 50), minor_breaks = seq(0, 1000, 200)) +
  theme_minimal(base_family = "PT Sans", base_size = 12) + 
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.title.position = "plot") 


df2 %>% 
  ggplot(aes(x=floor_area)) + geom_histogram(fill="#ffcf4a") + 
  labs(title="Distribution of the house prices", 
       x = "Prices in millions (LKR)", 
       y= "") + 
  # scale_x_continuous(breaks = seq(0, 1000, 50), minor_breaks = seq(0, 1000, 200)) +
  theme_minimal(base_family = "PT Sans", base_size = 12) + 
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.title.position = "plot") 


ggsave("price_d2.png", plot=price_distribution, height = 5, width=8, unit="in")


price_distribution

# 
df2 %>% 
  filter(floor_area < 15000) %>%  
  ggplot(aes(x=floor_area)) + geom_histogram(fill="#ffcf4a") +
  labs(title="Distribution of the floor area", 
       x = "Area in sq.ft", 
       y= "") + 
  # scale_x_continuous(breaks = seq(0, 1000, 50), minor_breaks = seq(0, 1000, 200)) +
  theme_minimal(base_family = "PT Sans", base_size = 12) + 
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.title.position = "plot") 

# 
# Q1 <- quantile(df2$floor_area, na.rm=T)[2] 
# Q3 <- quantile(df2$floor_area, na.rm=T)[4]

LOWER_LIMIT <- quantile(df2$floor_area, na.rm=T)[2]  - 1.5*IQR(df2$floor_area, na.rm=T)
UPPER_LIMIT <- quantile(df2$floor_area, na.rm=T)[4] + 1.5*IQR(df2$floor_area, na.rm=T)


floor_area_hist <- df2 %>% 
  filter(floor_area >= LOWER_LIMIT & floor_area <= UPPER_LIMIT) %>% 
  ggplot(aes(x=floor_area)) + geom_histogram(fill="#ffcf4a") +
  labs(title="Distribution of the floor area", 
       x = "", 
       y= "") + 
  # scale_x_continuous(breaks = seq(0, 1000, 50), minor_breaks = seq(0, 1000, 200)) +
  scale_x_continuous(labels = comma_format(suffix=" sq.ft")) + 
  theme_minimal(base_family = "PT Sans", base_size = 12) + 
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        plot.subtitle = element_text(hjust=0.5),
        plot.title.position = "plot") 

ggsave("floor_area_1.png", plot=floor_area_hist, height = 6, width=6, unit="in")

df2 %>% 
  mutate(area = as.numeric(area)) %>% 
  filter(floor_area >= LOWER_LIMIT & floor_area <= UPPER_LIMIT) %>%  
  select(floor_area, price, bathrooms_w_cs) %>% 
  melt() %>% 
  ggplot(aes(variable, value)) + geom_tile()
  
# df2 %>% filter(unit ="acres")

LOWER_LIMIT <- quantile(as.integer(df1$no_of_floors), na.rm=T)[2] - 1.5*IQR(as.integer(df1$no_of_floors), na.rm=T)
UPPER_LIMIT <- quantile(as.integer(df1$no_of_floors), na.rm=T)[4] + 1.5*IQR(as.integer(df1$no_of_floors), na.rm=T)


df$no_of_floors <- as.integer(df1$no_of_floors)
df %>% 
  # filter(no_of_floors > 0 & no_of_floors < 5) %>% 
  ggplot(aes(no_of_floors)) + geom_bar(fill="#ffcf4a")


cor_plot <- df2 %>% select(area, bedrooms, bathrooms_w_cs, floor_area, price) %>% 
  cor(use = "complete.obs")


ggcorrplot::ggcorrplot(cor_plot, type = "lower")

df %>% str()


df2

df2 %>% str()
