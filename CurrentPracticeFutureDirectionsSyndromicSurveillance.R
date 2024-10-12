########################################################################################
# Code to perform analysis and recreate in 
# Current Practice and Future Directions in Syndromic Surveillance for Animal Health: 
# A Systematic Review and Analysis
#
# Script tested for R version 4.1.2
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
dat <- read.csv("dat.csv", header=T, sep = ",")

########################################################################################
# Unit of Analysis
########################################################################################
# Papers per Unit of analysis (individual, herd/flock, population)
unit.analysis <- dat %>%
  distinct(Title, .keep_all = T) %>%
  group_by(Unit_analysis) %>%
  summarise(num.articles=sum(n()))

# Filter papers analyzing data at the individual level:
no.ind <- dat %>%
  filter(!(Unit_analysis=="Individual"))

########################################################################################
# Publications per year
########################################################################################
# Plot the number of publications per year (Figure 2) 
# Note: Excludes papers analyzing at the individual level
dat.bar.chart <- no.ind %>%
  select(Title,Year, Authors, Journal, DOI)

# Barchart of original data only
papers.per.year <- dat.bar.chart %>%
  distinct(Title, .keep_all=TRUE) %>%
  group_by(Year)%>%
  summarise(num.articles=sum(n()))

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
  filter(!is.na(Surveillance_system_name)) %>%
  group_by(Surveillance_system_name) %>%
  mutate(syst_id = cur_group_id()) %>%
  select(syst_id, everything())

# Give an ID to each unnamed system
dat.analysis <- no.ind %>%
  filter(is.na(Surveillance_system_name)) %>%
  group_by(Title) %>%
  mutate(syst_id = cur_group_id()) %>%
  mutate(syst_id = syst_id + 60) %>%
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
  group_by(syst_id) %>%
  summarise(num.articles=sum(n()))

# Now, keep only 1 SyS per ID
keep.syst.ana <- dat.analysis2 %>%
  filter(Keep=="Yes")

# Filter by unique title
# Necessary because in some cases there are multiple lines
# per title if sectors / species were treated differently
# E.g. different indicators were collected
keep.unique.title <- keep.syst.ana %>%
  distinct(Title, .keep_all = T)

doub.arts <- anti_join(keep.syst.ana, keep.unique.title)

add.back <- doub.arts %>%
  filter(syst_id != 36)

keep.syst.diff.ind.spp <- keep.unique.title %>%
  bind_rows(add.back)

########################################################################################
# Descriptive statistics of systems, by single variable
########################################################################################
# System status (surveillance type)
summ.type <- keep.unique.title %>%
  group_by(Surveillance_Type) %>%
  summarise(n = sum(n())) %>%
  mutate(percent=(n/131)*100) %>%
  arrange(., desc(percent))

# Geographic scale
summ.geoscale <- keep.unique.title %>%
  group_by(Geographic_Extent) %>%
  summarise(n=n()) %>%
  mutate(percent=n/131*100) %>%
  arrange(desc(percent))

# Summarise by continent + count by continent
summ.cont <- keep.unique.title %>%
  group_by(Region.x) %>%
  summarise(n=n()) %>%
  mutate(percent=n/131*100) %>%
  arrange(desc(percent))

# Animal sectors and species
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
  mutate(percent=n/131*100) %>%
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
  mutate(percent=n/131*100) %>%
  arrange(desc(percent))

# Summarize by data provider
summ.datprovider <- keep.unique.title %>%
  group_by(Data_provider) %>%
  summarise(n = n()) %>%
  filter(!is.na(Data_provider)) %>%
  mutate(percent=n/131*100) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Multiple (see notes)", "Multiple",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Member of public", "Member public",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Private company (disposal services, etc)", 
                              "Private company",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Member Study Team", "Study team",Data_provider)) %>%
  arrange(., desc(percent))

# Summarize indicators
# Note: here use 
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
  mutate(percent=n/131*100) %>%
  arrange(., desc(percent)) %>%
  mutate(., Indicators_simpl = ifelse(Indicators_simpl == "GI", 
                                      "gastrointestinal",Indicators_simpl))

# Analytical approaches (spatial, temporal, spatio-temporal)
analyt.dat <- dat.analysis2 %>%
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
  mutate(percent=n/131*100) %>%
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
  geom_polygon(aes(long,lat,group=group,fill=Systems), color="white", size = 0.2)+
  scale_fill_viridis(begin = 0.6, direction=, option = "turbo", na.value="light grey") +
  theme(panel.background = element_rect(fill = "lightsteelblue2"))
# Save using ggsave if desired

########################################################################################
# Create sankey plots linking animal sector, data providers,
# and indicators (Figure 4)
########################################################################################
# Use this data set:
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

########################################################################################
# Multiple Factor Analysis
########################################################################################
# Start with:
keep.syst.diff.ind.spp

# Prepare the data for Multiple Factor Analysis
m.dat.pre <- keep.syst.diff.ind.spp %>%
  select(syst_id, Surveillance_Type,
         Geographic_Extent, Country, Region.x, Animal_Type,
         Data_provider, OneHealth, `Income group`, Num_Indicators_System) %>%
  group_by(syst_id) %>%
  mutate(Num_Indicators_System = sum(Num_Indicators_System)) %>%
  distinct() %>%
  ungroup() %>%
  select(-syst_id)

# Further data clean up
m.dat.glob <- m.dat.pre %>%
  select(Surveillance_Type,
         Geographic_Extent, Region.x, Country, Animal_Type,
         Data_provider, OneHealth, `Income group`, Num_Indicators_System) %>%
  mutate(., Animal_Type = ifelse(str_detect(Animal_Type, ","),"Multiple Animal Sector",Animal_Type)) %>%
  mutate(., Surveillance_Type = ifelse(Surveillance_Type=="Prospective/Proof of Concept",
                                       "Proof of Concept",Surveillance_Type)) %>%
  mutate(., Region.x = ifelse(Geographic_Extent=="Global",
                              "Global",Region.x)) %>%
  mutate(., OneHealth = ifelse(OneHealth == "Yes",
                                             "OneHealth","Not OH")) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Multiple (see notes)", "Multiple data providers",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Member of public", "Member public",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Private company (disposal services, etc)", 
                              "Private company",Data_provider)) %>%
  mutate(., Data_provider = ifelse(Data_provider == "Member Study Team", "Study team",Data_provider)) %>%
  mutate(., `Income group` = ifelse(`Income group` == "Low income", "Low/lower middle income",`Income group`)) %>%
  mutate(., `Income group` = ifelse(`Income group` == "Lower middle income", "Low/lower middle income",`Income group`)) %>%
  mutate(., `Income group` = ifelse(Geographic_Extent == "Multi-Country", "High income",`Income group`)) %>%
  mutate(., `Income group` = ifelse(Country == "Global", "World",`Income group`)) %>%
  select(-Country) %>%
  mutate(`Income group` = ordered(factor(`Income group`,
                                         levels=c("High income", "Upper middle income", 
                                                  "Low/lower middle income", "World")))) %>%
  drop_na(Geographic_Extent)

# Perform Multiple Factor Analysis.
# Use all 10 dimensions:
res.mfa <- MFA(m.dat.glob, 
               group = c(1,1,1,1,1,1,1,1), 
               type = c("n", "n","n","n", "n", "n", "n","c"), ncp = 10,
               name.group = c("system type","geographic scale", "continent",
                              "animal sector", "Data_provider","onehealth", "income","number indicators"),
               graph = T)
# Note: Groups representation (Supplementary Figure 4) plots after running this
# code with argument "graph = T"

# Other MFA plots:
# Scree plot (Supplementary Figure 2)
scree <- fviz_screeplot(res.mfa, addlabels = TRUE)

# Contribution of variables to the first dimension
dim1.viz1 <- fviz_contrib(res.mfa, "group", axes = 1,title="")
# Contribution of variables to the second dimension
dim2.viz2 <- fviz_contrib(res.mfa, "group", axes = 2, title="")

# Plot contributions to first and second dimensions in a single figure
# (Supplementary Figure 3)
dims <- ggpubr::ggarrange(dim1.viz1, dim2.viz2, ncol=2, nrow=1, labels = c("a.","b."))

# Visualization -- Plotting first two dimensions, points / confidence intervals
# colored according to different categorical variables:
# Income level (Figure 5)
income.vis.glob <- fviz_mfa_ind(res.mfa, geom = "point",
                                habillage = m.dat.glob$`Income group`,#"Income group", # color by groups 
                                #col.ind = m.dat.glob$`Income group`,
                                palette = c("#0d0887", "#7e03a8", "#cc4778","#f89540"), #,"#f0f921"
                                addEllipses = TRUE, ellipse.type = "confidence", 
                                repel = TRUE # Avoid text overlapping
                                ,title=""
) 
# Save with ggsave if desired

# Visualizations for Supplementary Figure 5:
# Continent
continent.vis.glob <- fviz_mfa_ind(res.mfa, geom = "point",
                                   habillage = as.factor(m.dat.glob$Region.x), 
                                   #col.ind = m.dat.glob$`Income group`,
                                   palette = c("#fdc527","#f89540","#e66c5c","#cc4778",
                                               "#aa2395","#7e03a8","#4c02a1", "#0d0887"), #,"#f0f921"
                                   addEllipses = TRUE, ellipse.type = "confidence", 
                                   repel = TRUE # Avoid text overlapping
                                   ,title="",
                                   labels = NULL
                                   
) 

# Sector
sector.vis <- fviz_mfa_ind(res.mfa, 
                           habillage = "Animal_Type", # color by groups 
                           palette = c("#fcce25","#0d0887", "#7e03a8", "#cc4778","#f89540"),
                           addEllipses = TRUE, ellipse.type = "confidence", 
                           repel = TRUE, # Avoid text overlapping
                           labels = NULL,
                           geom = "point",
                           title=""
) 

# OneHealth
oh.vis <- fviz_mfa_ind(res.mfa, 
                       habillage = "OneHealth", # color by groups 
                       palette = c("#cc4778", "#0d0887"),
                       addEllipses = TRUE, ellipse.type = "confidence", 
                       repel = TRUE, # Avoid text overlapping,
                       labels = NULL,
                       geom = "point", title=""
) 

# Type of system
typ.vis <- fviz_mfa_ind(res.mfa, 
                        habillage = as.factor(m.dat.glob$Surveillance_Type), # color by groups 
                        palette = c("#fcce25","#0d0887", "#7e03a8", "#cc4778","#f89540"),
                        addEllipses = TRUE, ellipse.type = "confidence", 
                        repel = TRUE, # Avoid text overlapping
                        labels = NULL,
                        geom = "point", title=""
) 

# Geographic extent
geo.ext.vis <- fviz_mfa_ind(res.mfa, 
                            habillage = as.factor(m.dat.glob$Geographic_Extent), # color by groups 
                            palette = c("#fdc527","#f89540","#cc4778",
                                        "#aa2395","#7e03a8", "#0d0887"),
                            addEllipses = TRUE, ellipse.type = "confidence", 
                            repel = TRUE, # Avoid text overlapping
                            labels = NULL,
                            geom = "point", title="",
                            legend.partial.title= "Extent"
) 


# Data provider
provider.vis <- fviz_mfa_ind(res.mfa, 
                             habillage = as.factor(m.dat.glob$Data_provider), # color by groups 
                             palette = c("#f0f921","#fcce25","#fca636",
                                         "#f2844b","#e16462", "#cc4778",
                                         "#b12a90","#8f0da4","#6a00a8",
                                         "#41049d","#0d0887"),
                             #addEllipses = TRUE, ellipse.type = "confidence", 
                             repel = TRUE, # Avoid text overlapping
                             labels = NULL,
                             geom = "point", title="",
                             legend.partial.title= "Extent"
) 

# Plot all together (Supplementary Figure 5)
supp.vis <- ggpubr::ggarrange(typ.vis,geo.ext.vis, continent.vis.glob,sector.vis,provider.vis,oh.vis, ncol=2, nrow=3, 
                              labels = c("a.","b.","c.","d.","e.", "f."))

