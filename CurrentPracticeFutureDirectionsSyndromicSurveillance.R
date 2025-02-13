########################################################################################
# Code to perform analysis and recreate in 
# Current Practice and Future Directions in Syndromic Surveillance for Animal Health: 
# A Scoping Review and Analysis
#
# Script tested for R version 4.1.2
#
# Script updated 13 February 2025
# following manuscript revisions
########################################################################################

########################################################################################
# Set-up
########################################################################################

# Set your working directory

# Load packages. (Install before if necessary)
library(tidyverse)
library(janitor)
library(readxl)
library(maps)
library(viridis)
library(ggalluvial)
library(ggsankey)
library(FactoMineR)
library(factoextra)
library(ggpubr)

# Read in the data:
dat <- read_csv("dat.csv", header=T)

########################################################################################
# Unit of Analysis
########################################################################################
# Summarize the number of papers per unit of analysis (individual, herd/flock, population)
unit.analysis <- dat %>%
  filter(!(Source=="Gray literature")) %>%
  distinct(Title, .keep_all = T) %>%
  group_by(Unit_analysis) %>%
  summarise(num.articles=sum(n()))

# Filter papers analyzing data at the individual level for downstream analysis:
no.ind <- dat %>%
  filter(!(Unit_analysis=="Individual"))

########################################################################################
# Publications per year
########################################################################################
# Plot the number of publications per year (Figure 2) 
# Note: Excludes papers analyzing at the individual level
# Select relevant columns
dat.bar.chart <- no.ind %>%
  select(Title,Year, Authors, Journal, DOI)

# Summarize number of papers per year
papers.per.year <- dat.bar.chart %>%
  filter(!(Source=="Gray literature")) %>%
  distinct(Title, .keep_all=TRUE) %>%
  group_by(Year)%>%
  summarise(num.articles=sum(n()))

#Create barchart of papers per year (Fig. 2)
paper.plot <- ggplot(papers.per.year, aes(y=num.articles, 
                                          x=Year)) + 
  geom_bar(position="stack", stat="identity", fill="light grey", color="black") +
  theme_classic() +
  ylab("Number of Articles") +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=14),
        axis.title=element_text(size=12.5),
        axis.text = element_text(size=11.5))

# Use ggsave function to export plot if desired

########################################################################################
# Data organization and clean up
########################################################################################
# Organize data by system

# Identify and separate named systems
# Assign a unique ID number to each system
named <- no.ind %>%
  filter(!is.na(Surveillance_system)) %>%
  filter(!(Surveillance_system=="NA")) %>%
  group_by(Surveillance_system) %>%
  mutate(syst_id = cur_group_id()) %>%
  select(syst_id, everything())

# Give an ID to each unnamed system
dat.analysis <- no.ind %>%
  filter(Surveillance_system =="NA"| Surveillance_system=="NA") %>%
  group_by(Title) %>%
  mutate(syst_id = cur_group_id()) %>%
  mutate(syst_id = syst_id + 57) %>%
  select(syst_id, everything()) %>%
  ungroup() %>%
  # Bind named systems
  bind_rows(named)%>%
  # Data clean up: change data format to numeric where needed
  mutate(across(starts_with("Num"), as.numeric))%>%
  # Lower case
  mutate(Indicators_simpl = tolower(Indicators_simpl)) %>%
  # Change country names as needed
  mutate(., Country = ifelse(Country == "Korea South", "South Korea",Country))

# Read in World Bank data
wb <- read_excel("wb_cats.xlsx", sheet = 1) %>%
  mutate(., Economy = ifelse(Economy == "United Kingdom", "UK",Economy)) %>%
  mutate(., Economy = ifelse(Economy == "United States", "USA",Economy)) %>%
  mutate(., Economy = ifelse(Economy == "Taiwan, China", "Taiwan",Economy)) %>%
  select(-`Lending category`) %>%
  rename(Country=Economy)

# Join World Bank data to our data
dat.analysis2 <- dat.analysis %>%
  # Add in country income level:
  left_join(wb, by = c('Country')) %>%
  # Perform some data clean up on indicators
  mutate(across('Indicators_simpl', str_replace, 'gi', 'gastrointestinal')) %>%
  mutate(across('Indicators_simpl', str_replace, 'trauma','musculoskeletal')) %>%
  mutate(Num_Indicators_System = ifelse(is.na(Num_Indicators_System), Num_Indicators_Study, Num_Indicators_System))

# Calculate number of articles per system
arts.per.syst <- dat.analysis2 %>%
  filter(!(Source=="Gray literature")) %>%
  group_by(syst_id) %>%
  summarise(num.articles=sum(n()))

# Now, keep only 1 row of data per system (ID)
keep.syst.ana <- dat.analysis2 %>%
  filter(Keep=="Yes")

# Filter by unique title
# Necessary because in some cases there are multiple lines
# per title if sectors / species were treated differently
# E.g. different indicators were collected
keep.unique.title <- keep.syst.ana %>%
  distinct(Title, .keep_all = T)

doub.arts <- anti_join(keep.syst.ana, keep.unique.title)

keep.syst.diff.ind.spp <- keep.unique.title %>%
  bind_rows(doub.arts)

########################################################################################
# Descriptive statistics of systems, by single variable
########################################################################################
# System status (surveillance type)
summ.type <- keep.unique.title %>%
  group_by(Surveillance_Type) %>%
  summarise(n = sum(n())) %>%
  mutate(percent=(n/126)*100) %>%
  arrange(., desc(percent))

# Governance
summ.governance <- keep.unique.title %>%
  filter(Surveillance_Type=='Established'|Surveillance_Type=='Temporary' ) %>%
  group_by(Governance) %>%
  summarise(n = sum(n())) %>%
  mutate(percent=(n/26)*100) %>%
  arrange(., desc(percent))

# Objectives
summ.obj <- keep.unique.title %>%
  group_by(System_Purpose) %>%
  summarise(n = sum(n())) %>%
  mutate(percent=(n/126)*100) %>%
  arrange(., desc(percent))

# Response
summ.resp <- keep.unique.title %>%
  filter(!(Surveillance_Type == "Prospective/Proof of Concept")) %>%
  group_by(Response_alarm) %>%
  summarise(n = sum(n())) %>%
  mutate(percent=(n/42)*100) %>%
  arrange(., desc(percent))

# Geographic scale
summ.geoscale <- keep.unique.title %>%
  group_by(Geographic_Extent) %>%
  summarise(n=n()) %>%
  mutate(percent=n/126*100) %>%
  arrange(desc(percent))

# Summarise by continent
summ.cont <- keep.unique.title %>%
  group_by(Region.x) %>%
  summarise(n=n()) %>%
  mutate(percent=n/126*100) %>%
  arrange(desc(percent))

# OneHealth systems
summ.oh <- keep.unique.title %>%
  group_by(Integration_HumanHealth) %>%
  summarise(n=n()) %>%
  mutate(percent=n/126*100) %>%
  arrange(desc(percent))

# Summarize by animal sectors and species
# For animal sector continue to use:
keep.unique.title

# For species use this to account for systems with different indicators for
# different species:
keep.syst.diff.ind.spp

# Summarize and clean up based on animal sector
summ.anim.type <- keep.unique.title %>%
  group_by(Animal_Type) %>%
  mutate(., Animal_Type = ifelse(str_detect(Animal_Type, ","),"Multiple",Animal_Type)) %>%
  #filter(str_detect(Animal_Type, "Livestock,"))
  summarise(n = n()) %>%
  mutate(percent=n/126*100) %>%
  arrange(., desc(percent)) %>%
  mutate(Animal_Type = factor(Animal_Type,
                              levels=c("Livestock", "Companion","Multiple",
                                       "Wildlife","Fisheries/Aquaculture",
                                       "NA")))

# Summarize by animal species
# Animal species
animal.sp <- keep.syst.diff.ind.spp %>%
  filter(!(is.na(Species_name))) %>%
  mutate(Species_name = ifelse(Animal_Type == "Wildlife", "wildlife", Species_name)) %>%
  mutate(Species_name = ifelse(Animal_Type == "Fisheries/Aquaculture", "fish", Species_name)) %>%
  separate_rows(., Species_name, sep = ", ", convert = FALSE) %>%
  mutate(across(
    .cols = Species_name, 
    .fns = ~ dplyr::if_else(stringr::str_detect(.x, "wild"), "wildlife", .x))) %>%
   mutate(across(
    .cols = Species_name, 
    .fns = ~ dplyr::if_else(stringr::str_detect(.x, "chicken"), "poultry", .x))) %>%
  mutate(across(
    .cols = Species_name, 
    .fns = ~ dplyr::if_else(stringr::str_detect(.x, "Cattle"), "cattle", .x))) %>%
  mutate(across(
    .cols = Species_name, 
    .fns = ~ dplyr::if_else(stringr::str_detect(.x, "domestic bird"), "poultry", .x))) %>%
  mutate(across(
    .cols = Species_name, 
    .fns = ~ dplyr::if_else(stringr::str_detect(.x, "duck"), "poultry", .x))) %>%
  mutate(across(
    .cols = Species_name, 
    .fns = ~ dplyr::if_else(stringr::str_detect(.x, "goat"), "small ruminant", .x))) %>%
  mutate(across(
    .cols = Species_name, 
    .fns = ~ dplyr::if_else(stringr::str_detect(.x, "sheep"), "small ruminant", .x))) %>%
  mutate(across(
    .cols = Species_name, 
    .fns = ~ dplyr::if_else(stringr::str_detect(.x, "small ruminants"), "small ruminant", .x))) %>%
  group_by(Species_name) %>%
  summarise(n=n()) %>%
  filter(!(is.na(Species_name))) %>%
  mutate(Species_name = ifelse(n == 1, "other/unspecified", Species_name)) %>%
  group_by(Species_name) %>%
  summarise(n=sum(n)) %>%
  #arrange(desc(n)) %>% 
  mutate(Species_name = factor(Species_name)) %>%
  mutate(percent=n/126*100) %>%
  arrange(desc(percent))


# Summarize by data provider
summ.datprovider <- keep.unique.title %>%
  group_by(Data_provider) %>%
  summarise(n = n()) %>%
  filter(!is.na(Data_provider)) %>%
  mutate(percent=n/126*100) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Multiple (see notes)", "Multiple",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Member of public", "Member public",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Private company (disposal services, etc)", 
                              "Private company",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Member Study Team", "Study team",Data_provider)) %>%
  arrange(., desc(percent))

# Summarize indicators
# Note: Here use to account for the use of different
# indicators for different species within the same system
keep.syst.diff.ind.spp

# Indicators PER SYSTEM
# Clean up indicator data
ind.setup <- keep.syst.diff.ind.spp %>%
  separate_rows(., Indicators_simpl, sep = ", ", convert = FALSE) %>%
  mutate(across('Indicators_simpl', str_replace, 
                'milk production|milk quality|mastitis|reduced production', 'production')) %>%
  mutate(across('Indicators_simpl', str_replace, 'consultations|prescriptions|admissions|lab tests',
                'consults/prescriptions/tests')) %>%
  distinct()

# Summarize indicators
summ.indicators <- keep.syst.diff.ind.spp %>%
  separate_rows(., Indicators_simpl, sep = ", ", convert = FALSE) %>%
  group_by(Indicators_simpl) %>%
  summarise(n = n()) %>%
  mutate(percent=n/126*100) %>%
  arrange(., desc(percent)) %>%
  mutate(., Indicators_simpl = ifelse(Indicators_simpl == "GI", 
                                      "gastrointestinal",Indicators_simpl))

# Analytical approaches (spatial, temporal, spatio-temporal)
# Note: Analysis at level of article since data from the same
# system could be analyzed differently in each article
analyt.dat <- dat.analysis2 %>%
  filter(!(Source=="Gray literature")) %>%
  distinct(Title, .keep_all = TRUE)

analysis.type <- analyt.dat %>%
  group_by(Analysis_type) %>%
  summarise(n=n()) %>%
  mutate(percent=n/165*100) %>%
  arrange(., desc(percent))

# Statistical framework:
stat.frame <- analyt.dat %>%
  group_by(Statistical_framework) %>%
  summarise(n=n()) %>%
  mutate(percent=n/165*100) %>%
  arrange(., desc(percent))

########################################################################################
# Create a map of syndromic surveillance systems (Figure 3)
########################################################################################
# Load map from "maps" package
world<-map_data("world")

# Summarize number of systems per country
summ.country <- keep.unique.title %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  mutate(percent=n/126*100) %>%
  #mutate(., Country = ifelse(Country == "Korea South", "South Korea",Country)) %>%
  mutate(Systems = n) %>%
  # Remove systems in multiple countries, global, or no country
  filter(!(Country=="NA"))%>%
  filter(!(Country=="Multiple"))%>%
  arrange(., desc(percent))

# Join our data to the map
sys.map <- data.frame(world,data=sample(10,length(unique(world$group)),T)[world$group]) %>%
  left_join(., summ.country, by = c("region" = "Country"))

# Plot the number of systems per country on a map (Figure 3)
map.sys <- ggplot(sys.map)+
  geom_polygon(aes(long,lat,group=group,fill=Systems), color="black", size = 0.35)+
  scale_fill_viridis(begin = 0.6, direction=, option = "turbo", na.value="gray93") +
  theme(panel.background = element_rect(fill = "lightsteelblue1"))
# Save using ggsave if desired
#######################################################################################

########################################################################################
# Create sankey plots linking animal sector, data providers,
# and indicators (Figure 4, Supplementary Figure 1)
########################################################################################
# Use this data set (created above in section "Indicators PER SYSTEM" , L. 284):
ind.setup

# Prepare and clean the data to create sankey plot
alluv.dat <- ind.setup %>%
  select(Animal_Type, Data_provider, Indicators_simpl) %>%
  mutate(., Animal_Type = ifelse(str_detect(Animal_Type, ","),"Multiple sectors",Animal_Type)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Multiple (see notes)", "Multiple providers",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Member of public", "Member public",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Private company (disposal services, etc)", 
                              "Private company",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Member Study Team", "Study team",Data_provider)) %>%
  separate_rows(., Indicators_simpl, sep = ", ", convert = FALSE) %>%
  pivot_wider(names_from = Indicators_simpl, values_from = Indicators_simpl, 
              values_fn = ~1, values_fill = NA) %>%
  pivot_longer(cols=c(3:20),
               names_to='Indicator',
               values_to='Y.N') %>%
  rename(Sector = Animal_Type,
         `Data Provider` = Data_provider) %>%
  select(Sector, `Data Provider`, Indicator, Y.N) %>%
  drop_na()

# Set up the Sankey plot
df <- alluv.dat %>%
  ggsankey::make_long(Sector, `Data Provider`, Indicator)

pl <- ggplot(df, aes(x = x
                     , next_x = next_x
                     , node = node
                     , next_node = next_node
                     , fill = factor(node)
                     , label = node)
)

# Plot (Figure 4)
sankey.plot <- pl +geom_sankey(flow.alpha = 0.5
                        , node.color = "black"
                        ,show.legend = FALSE) +
  scale_fill_viridis_d(option = "turbo", direction = -1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text=element_text(size=14),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
# Save using ggsave if desired

# Repeat with Sector in the middle (Supplementary Figure 1)
df2 <- alluv.dat %>%
  ggsankey::make_long(`Data Provider`, Sector, Indicator)

pl2 <- ggplot(df2, aes(x = x
                       , next_x = next_x
                       , node = node
                       , next_node = next_node
                       , fill = factor(node)
                       , label = node)
)

# Plot (Supplementary Figure 1)
pl2 <- pl2 +geom_sankey(flow.alpha = 0.5
                        , node.color = "black"
                        ,show.legend = FALSE) +
  scale_fill_viridis_d(option = "turbo", direction = -1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text=element_text(size=14),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
#Save with ggsave if desired

## END ##