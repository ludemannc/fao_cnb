#Description of script: ---
##This script uses data from the UN FAO cropland nutrient budget and elsewhere to produce visual material for inclusion in article that describes this database.
#This script was written by Cameron Ludemann in R version 4.1.0 with a 64-bit computer.
 
#Libraries----
library(ggplot2)
library(tidyverse)
library(reshape2)
library(countrycode)
library(ggpubr)
library(readxl)
library(stringr) #For summariser function.
library(rgdal) #For maps
library(sp) #For importing map information as shp file.
library(ggpubr)

#Settings----
#Ensure all values are decimals rather than scientific notation.
options(scipen = 999)

#Read files----
df <- read_csv("data/Environment_Cropland_nutrient_budget_E_All_Data/Environment_Cropland_nutrient_budget_E_All_Data_NOFLAG.csv")

world_N_inputs_Zhang <- read_excel("data/Zhang et al 2021 Nature Food Supp 4_43016_2021_318_MOESM4_ESM.xlsx", 
                                   sheet = "Global N inputs TgNyr")

world_N_harvested_Zhang <- read_excel("data/Zhang et al 2021 Nature Food Supp 4_43016_2021_318_MOESM4_ESM.xlsx", 
                                   sheet = "Global N harvest TgNyr")

world_N_budget_Zhang <- read_excel("data/Zhang et al 2021 Nature Food Supp 4_43016_2021_318_MOESM4_ESM.xlsx", 
                                                                            sheet = "Global N surplus TgNyr")

world_NUE_Zhang <- read_excel("data/Zhang et al 2021 Nature Food Supp 4_43016_2021_318_MOESM4_ESM.xlsx", 
                              sheet = "Global NUE")

df_CR <- as.data.frame(read_csv("data/LudemannDRYAD2023/Combined_crop_data.csv"))

#List UN-sub-region names----
UN_sub_regions <- c("Australia and New Zealand","Central Asia",
                    "Eastern Asia","Eastern Europe","Latin America and the Caribbean",
                    "Northern America","Northern Europe","Southern Asia",
                    "Southern Europe","Africa",
                    "Western Asia","Western Europe", 
                    "Central America", "South America")

#Tidy data frame----
names(df) <- make.names(names(df)) #Remove spaces and special characters in headers.

df <- df %>% dplyr::rename(Area.Code.M49="Area.Code..M49.") %>% #Rename columns
  melt(id=c("Area.Code","Area.Code.M49","Area", "Item.Code","Item","Element.Code","Element","Unit")) %>% 
  mutate(variable=gsub("Y","", variable)) %>% 
  mutate(variable=as.numeric(variable)) %>% 
  dplyr::rename(Year="variable") %>% 
  mutate(Item_order=case_when( #Order Items logically.
    str_detect(Item,regex("Input", ignore_case=TRUE))~"1",
    str_detect(Item,regex("Synthetic Fertilizers", ignore_case=TRUE))~"2",
    str_detect(Item,regex("Manure applied to Soils", ignore_case=TRUE))~"3",
    str_detect(Item,regex("Biological fixation", ignore_case=TRUE))~"4",
    str_detect(Item,regex("Atmospheric Deposition", ignore_case=TRUE))~"5",
    str_detect(Item,regex("Outputs", ignore_case=TRUE))~"6",
    str_detect(Item,regex("Crop removal", ignore_case=TRUE))~"7",
    str_detect(Item,regex("Leaching", ignore_case=TRUE))~"8",
    str_detect(Item,regex("Input", ignore_case=TRUE))~"9",
    str_detect(Item,regex("Volatilisation", ignore_case=TRUE))~"10",
    str_detect(Item,regex("Soil nutrient budget", ignore_case=TRUE))~"11",    
    TRUE~"NA")) %>% 
  mutate_all(~gsub('[^ -~]', '', .)) %>% #Remove any non-UTF-8 characters otherwise countrycode function will not work. 
  mutate(Item_order = as.numeric(Item_order),
         value=as.numeric(value)) %>% 
  mutate(country_iso3 = countrycode(Area, origin='country.name', destination='iso3c'))  #Create column with ranking of total quantity of Synthetic N fertilizer. First only give ranking to country and not other areas.

#Create df with ranking of countries based on total synthetic N applied- in 2020---
df_country_N <- filter(df, !is.na(country_iso3)&Unit=="tonnes"&
                         Year==2020&Item=="Synthetic Fertilizers"&
                         Element=="Cropland nitrogen"&
                         !Area=="China, mainland") %>%   #To avoid double ups in ranking of countries I remove "China, mainland" and assume "China" is the representation of China as per: Tubiello, F. pers comm 29 Mar 2023.
                mutate(value=as.numeric(value), #Change from character to numeric ofr ranking estimate. 
                       N_rank=rank(-value)) %>% #Rank countries based on N Synthetic fertilizer. 
                dplyr::select(Area,N_rank)  #Select only columns of interest for merging.  

#Merge N_ranking df with main df----
df <- merge(df, df_country_N, all.x = TRUE)

#World trends----
#Select data for table
df_world <- df%>% dplyr::filter(Year %in% c(1961,2020)& Area =="World") %>%  #Filter to selected years and World for comparison.
mutate(Year=paste0("Y", Year))

#Pivot wider
df_world <- pivot_wider(df_world, 
                        names_from=Year, values_from = value) %>% 
  mutate( Y2020_Y1961_diff=Y2020-Y1961,
    pc_diff=Y2020_Y1961_diff/Y1961)

#Create functions to turn data into table for reporting----
world_nutrient_diff_table_Mt <- function(df, Element_filter){
  df <- dplyr::filter(df, Element==Element_filter)
  df <- dplyr::select(df, Item, Element,Unit,Y1961, Y2020,Item_order)
  df <- dplyr::arrange(df, Item_order)
  df$Y1961_Mt <- signif(df$Y1961/1000000,digits=2)
  df$Y2020_Mt <- signif(df$Y2020/1000000, digits=2)
  df$diff_Mt <- df$Y2020_Mt-df$Y1961_Mt
  df$pc_diff_from_Mt <- signif((df$diff_Mt/df$Y1961_Mt)*100, digits=2)
  df <- dplyr::select(df, Item,Element,Unit,Y1961_Mt, Y2020_Mt, pc_diff_from_Mt)
  return(df)
}

world_nutrient_diff_table_kg_ha <- function(df, Element_filter){
  df <- dplyr::filter(df, Element==Element_filter)
  df <- dplyr::select(df, Item, Element,Unit,Y1961, Y2020,Item_order)
  df <- arrange(df, Item_order)
  df$Y1961_kg_ha <- signif(df$Y1961, digits=2)
  df$Y2020_kg_ha <- signif(df$Y2020, digits=2)
  df$diff_kg_ha <- df$Y2020_kg_ha-df$Y1961_kg_ha
  df$pc_diff_from_kg_ha <- signif((df$diff_kg_ha/df$Y1961_kg_ha)*100, digits=2)
  df <- dplyr::select(df, Item,Element,Unit,Y1961_kg_ha, Y2020_kg_ha, pc_diff_from_kg_ha)
  return(df)
}

#Apply function to create table for reporting for each nutrient----
df_world_N_Mt <- world_nutrient_diff_table_Mt(df_world, Element_filter = "Cropland nitrogen")
df_world_P_Mt <- world_nutrient_diff_table_Mt(df_world, Element_filter = "Cropland phosphorus")
df_world_K_Mt <- world_nutrient_diff_table_Mt(df_world, Element_filter = "Cropland potassium")

df_world_N_kg_ha <- world_nutrient_diff_table_kg_ha(df_world, Element_filter = "Cropland nitrogen per unit area")
df_world_P_kg_ha <- world_nutrient_diff_table_kg_ha(df_world, Element_filter = "Cropland phosphorus per unit area")
df_world_K_kg_ha <- world_nutrient_diff_table_kg_ha(df_world, Element_filter = "Cropland potassium per unit area")

#Bind columns together for reporting in table with all three nutrients----
world_NPK_Mt_comparison_by_year <- cbind(df_world_N_Mt,df_world_P_Mt,df_world_K_Mt) #For total nutrients
world_NPK_kg_ha_comparison_by_year <- cbind(df_world_N_kg_ha,df_world_P_kg_ha,df_world_K_kg_ha) #For total nutrients

#Save as csv file----
write.csv(world_NPK_Mt_comparison_by_year,"./results/world_NPK_Mt_comparison_by_year.csv",row.names= FALSE)
write.csv(world_NPK_kg_ha_comparison_by_year,"./results/world_NPK_kg_ha_comparison_by_year.csv",row.names= FALSE)

#Show trends in N,P and K balance over time across the world----
df_world <- df%>% dplyr::filter(Area =="World")   #Filter to  World for comparison.

df_world <- df_world %>%mutate(Nutrient=case_when( #Create column with N, P and K information.
  str_detect(Element,regex("nitrogen", ignore_case=TRUE))~"N",
  str_detect(Element,regex("phosphorus", ignore_case=TRUE))~"P",
  str_detect(Element,regex("potassium", ignore_case=TRUE))~"K",    
  TRUE~"NA"), 
  Year=as.numeric(Year)) 

#Create ggplot with trends over time for total and per hectare nutrient budgets----
NPK_world_budget_plot_Mt <- df_world %>% #On a Million tonne basis
  mutate(Nutrient= fct_relevel(Nutrient,
                               "N","P","K")) %>% 
           filter(Element %in% c("Cropland nitrogen","Cropland phosphorus","Cropland potassium")) %>% 
  dplyr::filter(Item=="Soil nutrient budget") %>% 
  ggplot()+
  aes(x=Year, y=value,color=Nutrient,  shape=Nutrient, group=Nutrient)+
  #geom_point(na.rm = TRUE, size=2)+
  geom_point()+geom_line()+
  scale_y_continuous(labels = scales::label_comma(scale = 0.000001))+
  theme_classic()+
  labs(tag="(a)", y= "Nutrient budget in millions of tonnes of nutrient per year")+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=15)) 

NPK_world_budget_plot_kg_ha <- df_world %>% #On a kg/ha basis
  mutate(Nutrient= fct_relevel(Nutrient,
                               "N","P","K")) %>% 
  dplyr::filter(Element %in% c("Cropland nitrogen per unit area","Cropland phosphorus per unit area","Cropland potassium per unit area")) %>% 
  filter(Item=="Soil nutrient budget") %>% 
  ggplot()+
  aes(x=Year, y=value, color=Nutrient, shape=Nutrient, group=Nutrient)+
  #geom_point(na.rm = TRUE, size=2)+
  geom_point()+geom_line()+
  geom_point(na.rm = TRUE, size=2)+
  scale_y_continuous(labels = scales::label_comma(scale = 1))+
  theme_classic()+
  labs(tag="(b)", y= "Nutrient budget in kilograms of nutrient per hectare per year")+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=15))

Plot_world_NPK_budget_Mt_kg_ha <- ggarrange(NPK_world_budget_plot_Mt,NPK_world_budget_plot_kg_ha,
                                      ncol=1,nrow=2, common.legend=TRUE, legend="bottom" )

ggsave("./results/NPK_world_budget_Mt_kg_ha_plot.jpg", width = 50, height = 30, units = "cm")

#Set legend key and text sizes----
legend_title_size <- 16
legend_key_size <- 2

#Regional trends----
#Create histogram of total and kg/ha nutrient budgets for the UN sub-regions. 
#Select data
df_region <- df%>% dplyr::filter(Year %in% c(2020)& Area %in% UN_sub_regions) 

df_region <- df_region %>%mutate(Nutrient=case_when( #Create column with N, P and K information.
  str_detect(Element,regex("nitrogen", ignore_case=TRUE))~"N",
  str_detect(Element,regex("phosphorus", ignore_case=TRUE))~"P",
  str_detect(Element,regex("potassium", ignore_case=TRUE))~"K",    
  TRUE~"NA")) 

#Create ggplot showing differences by UN-sub-region for total and per hectare nutrient budgets----
NPK_region_budget_plot_Mt <- df_region %>% #On a Million tonne basis
  mutate(Nutrient= fct_relevel(Nutrient,
                               "N","P","K")) %>% 
  filter(Element %in% c("Cropland nitrogen","Cropland phosphorus","Cropland potassium")) %>% 
  filter(Item=="Soil nutrient budget") %>% 
ggplot(aes(fill=Nutrient,x=Area,y=value)) +
  geom_col(position = position_dodge(0.75), width = 0.75) + 
 # coord_flip()+ #If you want to flip the axes.

  theme_update(text = element_text(size=10)) +
  scale_y_continuous(labels = scales::label_comma(scale = 0.000001))+
  labs(tag="(a)", x="Region",
       y= "Nutrient budget in Mt of nutrient")+
  theme(axis.line.x = element_line(color="black", size = 0.5),#There was conflict with use of theme_classic and being able to add axis lines so I explicitly made background white.
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        legend.title = element_text(size = legend_title_size), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.text=element_text(size=legend_title_size))  +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.25)

NPK_region_budget_plot_kg_ha <- df_region %>% #On a Million tonne basis
  mutate(Nutrient= fct_relevel(Nutrient,
                               "N","P","K")) %>% 
  dplyr::filter(Element %in% c("Cropland nitrogen per unit area","Cropland phosphorus per unit area","Cropland potassium per unit area")) %>% 
  dplyr::filter(Item=="Soil nutrient budget") %>% 
  ggplot(aes(fill=Nutrient,x=Area,y=value)) +
  geom_col(position = position_dodge(0.75), width = 0.75) + 
  # coord_flip()+ #If you want to flip the axes.
 theme_update(text = element_text(size=10)) +
  scale_y_continuous(labels = scales::label_comma(scale = 1))+
  labs(tag="(b)", x="Region",
       y= "Nutrient budget in kg of nutrient per ha")+
   theme(axis.line.x = element_line(color="black", size = 0.5),
         axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        legend.title = element_text(size = legend_title_size), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.text=element_text(size=legend_title_size))  +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.25)

Plot_region_NPK_budget_Mt_kg_ha <- ggarrange(NPK_region_budget_plot_Mt,NPK_region_budget_plot_kg_ha,
                                      ncol=1,nrow=2, common.legend=TRUE, legend="bottom" )

ggsave("./results/NPK_region_budget_Mt_kg_ha_plot.jpg", Plot_region_NPK_budget_Mt_kg_ha,width = 50, height = 40, units = "cm")

#Country trends----
#Focus on countries with greatest N consumption for 2020.
#Select data 
df_country <- df%>% dplyr::filter(Year %in% c(2020)& N_rank <11) 

df_country <- df_country %>%mutate(Nutrient=case_when( #Create column with N, P and K information.
  str_detect(Element,regex("nitrogen", ignore_case=TRUE))~"N",
  str_detect(Element,regex("phosphorus", ignore_case=TRUE))~"P",
  str_detect(Element,regex("potassium", ignore_case=TRUE))~"K",    
  TRUE~"NA")) 

#Create ggplot showing differences by top countries for total and per hectare nutrient budgets----
NPK_country_budget_plot_Mt <- df_country %>% #On a Million tonne basis
  mutate(Nutrient= fct_relevel(Nutrient,
                               "N","P","K")) %>% 
  dplyr::filter(Element %in% c("Cropland nitrogen","Cropland phosphorus","Cropland potassium")) %>% 
  dplyr::filter(Item=="Soil nutrient budget") %>% 
  ggplot(aes(fill=Nutrient,x=Area,y=value)) +
  geom_col(position = position_dodge(0.75), width = 0.75) + 
  theme_update(text = element_text(size=10)) +
  scale_y_continuous(labels = scales::label_comma(scale = 0.000001))+
  labs(tag="(a)", x="Country",
       y= "Nutrient budget in Mt of nutrient")+
  theme(axis.line.x = element_line(color="black", size = 0.5),#There was conflict with use of theme_classic and being able to add axis lines so I explicitly made background white.
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        legend.title = element_text(size = legend_title_size), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.text=element_text(size=legend_title_size))  +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.25)

NPK_country_budget_plot_kg_ha <- df_country %>% #On a per ha basis.
  mutate(Nutrient= fct_relevel(Nutrient,
                               "N","P","K")) %>% 
  dplyr::filter(Element %in% c("Cropland nitrogen per unit area","Cropland phosphorus per unit area","Cropland potassium per unit area")) %>% 
  dplyr::filter(Item=="Soil nutrient budget") %>% 
  ggplot(aes(fill=Nutrient,x=Area,y=value)) +
  geom_col(position = position_dodge(0.75), width = 0.75) + 
  # coord_flip()+ #If you want to flip the axes.
  theme_update(text = element_text(size=10)) +
  scale_y_continuous(labels = scales::label_comma(scale = 1))+
  labs(tag="(b)", x="Country",
       y= "Nutrient budget in kg of nutrient per ha")+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        legend.title = element_text(size = legend_title_size), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.text=element_text(size=legend_title_size))  +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.25)

Plot_country_NPK_budget_Mt_kg_ha <- ggarrange(NPK_country_budget_plot_Mt,NPK_country_budget_plot_kg_ha,
                                             ncol=1,nrow=2, common.legend=TRUE, legend="bottom" )

ggsave("./results/NPK_country_budget_Mt_kg_ha_plot.jpg", Plot_country_NPK_budget_Mt_kg_ha,width = 50, height = 40, units = "cm")

#Summarise results in tables for ease of use----
#Regions----
NPK_region_budget_Mt <- df_region %>% #On a Million tonne basis
  mutate(Nutrient= fct_relevel(Nutrient,
                               "N","P","K")) %>% 
  dplyr::filter(Element %in% c("Cropland nitrogen","Cropland phosphorus","Cropland potassium")) %>% 
  dplyr::filter(Item=="Soil nutrient budget") %>% 
  dplyr::filter(Area %in% UN_sub_regions) %>% 
  mutate(Value_Mt=value/1000000)
write.csv(NPK_region_budget_Mt,"./results/NPK_region_budget_Mt.csv",row.names= FALSE)

NPK_region_budget_kg_ha <- df_region %>% #On a kg per hectare basis
  mutate(Nutrient= fct_relevel(Nutrient,
                               "N","P","K")) %>% 
  dplyr::filter(Element %in% c("Cropland nitrogen per unit area","Cropland phosphorus per unit area","Cropland potassium per unit area")) %>% 
  dplyr::filter(Item=="Soil nutrient budget") %>% 
  dplyr::filter(Area %in% UN_sub_regions) %>% 
  mutate(Value_kg_ha=value)
write.csv(NPK_region_budget_kg_ha,"./results/NPK_region_budget_kg_ha.csv",row.names= FALSE)

#Top N consuming countries----
NPK_country_budget_Mt <- df_country %>% #On a Million tonne basis
  mutate(Nutrient= fct_relevel(Nutrient,
                               "N","P","K")) %>% 
  dplyr::filter(Element %in% c("Cropland nitrogen","Cropland phosphorus","Cropland potassium")) %>% 
  dplyr::filter(Item=="Soil nutrient budget") %>% 
mutate(Value_Mt=value/1000000)
write.csv(NPK_country_budget_Mt,"./results/NPK_country_budget_Mt.csv",row.names= FALSE)

NPK_country_budget_kg_ha <- df_country %>% #On a kg per hectare basis
  mutate(Nutrient= fct_relevel(Nutrient,
                               "N","P","K")) %>% 
  dplyr::filter(Element %in% c("Cropland nitrogen per unit area","Cropland phosphorus per unit area","Cropland potassium per unit area")) %>% 
  dplyr::filter(Item=="Soil nutrient budget") %>% 
  mutate(Value_kg_ha=value)
write.csv(NPK_country_budget_kg_ha,"./results/NPK_country_budget_kg_ha.csv",row.names= FALSE)

#Create time series plots for UN regions + the world for N, P and K nutrient budget per hectare. 
#Set region names
Regions <- c("Africa","Americas","Asia","Europe","Oceania","World")

#Select data for table
df_regions <- df %>% dplyr::filter(Area %in% Regions & Item=="Soil nutrient budget") #Filter to selected areas/regions

df_regions <- df_regions %>%mutate(Nutrient=case_when( #Create column with N, P and K information.
  str_detect(Element,regex("nitrogen", ignore_case=TRUE))~"N",
  str_detect(Element,regex("phosphorus", ignore_case=TRUE))~"P",
  str_detect(Element,regex("potassium", ignore_case=TRUE))~"K",    
  TRUE~"NA")) 

NPK_regions_budget_plot_kg_ha <- df_regions   %>% mutate(Nutrient= fct_relevel(Nutrient,
                                          "N","P","K")) %>%
  mutate(Year= as.numeric(Year)) %>%
  dplyr::filter(Element %in% c("Cropland nitrogen per unit area","Cropland phosphorus per unit area", "Cropland potassium per unit area" )) %>% #,"Cropland phosphorus per unit area","Cropland potassium per unit area")) %>% 
  filter(Item=="Soil nutrient budget") %>% 
  ggplot(aes(group=Area,x=Year,y=value, color=Area)) +
  geom_point()+geom_line()+
  #geom_smooth(aes(linetype=Area), se=F)+
  scale_color_manual(values=c( "magenta3", "#56B4E9", "#009E73", 
                              "#F0E442", "#0072B2","#000000"))+
  scale_linetype_manual(values=c("twodash", "longdash","dotted", "dotdash", "dashed", "solid")) +
 #geom_smooth(se = F)+
  facet_grid(~Nutrient) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
         axis.line.y = element_line(color="black", size = 0.5),
         axis.text.x=element_text(angle=90),text=element_text(size=20),
         panel.border = element_blank(), panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),  legend.key = element_blank(),
         panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.text=element_text(size=legend_title_size))+
  labs(tag="(a)", y= "Nutrient budget in kg of nutrient per ha") +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.25)

#Create time series plots for top N consuming countries. 
#Filter dataframe to top N consuming countries
df_countries <- df %>% dplyr::filter(N_rank <11 & Item=="Soil nutrient budget") #Filter to selected areas/regions

df_countries <- df_countries %>%mutate(Nutrient=case_when( #Create column with N, P and K information.
  str_detect(Element,regex("nitrogen", ignore_case=TRUE))~"N",
  str_detect(Element,regex("phosphorus", ignore_case=TRUE))~"P",
  str_detect(Element,regex("potassium", ignore_case=TRUE))~"K",    
  TRUE~"NA")) 

NPK_countries_budget_plot_kg_ha <- df_countries   %>% mutate(Nutrient= fct_relevel(Nutrient,
                                              "N","P","K")) %>%
  mutate(Year= as.numeric(Year)) %>%
  filter(Element %in% c("Cropland nitrogen per unit area","Cropland phosphorus per unit area", "Cropland potassium per unit area" )) %>% #,"Cropland phosphorus per unit area","Cropland potassium per unit area")) %>% 
  filter(Item=="Soil nutrient budget") %>% 
  ggplot(aes(group=Area,x=Year,y=value, color=Area)) +
  geom_point()+geom_line()+
  #geom_smooth(aes(linetype=Area), se=F) +
  scale_color_manual(values=c( "#000000", "magenta3", "#56B4E9", "#009E73", 
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
                               "#999999","#999933"))+
  scale_linetype_manual(values=c("twodash", "longdash","dotted", "dotdash", "dashed", "solid", "longdash","dotted", "dotdash", "dashed")) +
  #geom_smooth(se = F)+
  facet_grid(~Nutrient) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.text=element_text(size=legend_title_size))+
  labs(tag="(b)", y= "Nutrient budget in kg of nutrient per ha") +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.25)

Plot_countries_regions_NPK_budget_kg_ha <- ggarrange(NPK_regions_budget_plot_kg_ha,NPK_countries_budget_plot_kg_ha,
                                              ncol=1,nrow=2, common.legend=F, legend="bottom" )

ggsave("./results/Plot_countries_regions_NPK_budget_kg_ha.jpg", Plot_countries_regions_NPK_budget_kg_ha,width = 50, height = 40, units = "cm")

#Create time series plots for UN regions + the world for N, P and K nutrient budget per region or country. 
NPK_regions_budget_plot_Mt <- df_regions   %>% mutate(Nutrient= fct_relevel(Nutrient,
                                                                               "N","P","K")) %>%
  mutate(Year= as.numeric(Year)) %>%
  filter(Element %in% c("Cropland nitrogen","Cropland phosphorus", "Cropland potassium" )) %>% #,"Cropland phosphorus per unit area","Cropland potassium per unit area")) %>% 
  filter(Item=="Soil nutrient budget") %>% 
  ggplot(aes(group=Area,x=Year,y=value, color=Area)) +
  geom_point()+geom_line()+
  #geom_smooth(aes(linetype=Area), se=F)+
  scale_color_manual(values=c( "magenta3", "#56B4E9", "#009E73", 
                               "#F0E442", "#0072B2","#000000"))+
  scale_linetype_manual(values=c("twodash", "longdash","dotted", "dotdash", "dashed", "solid")) +
  #geom_smooth(se = F)+
  facet_grid(~Nutrient) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.text=element_text(size=legend_title_size))+
  labs(tag="(a)", y= "Nutrient budget in Mt per region") + 
  scale_y_continuous(labels = scales::label_comma(scale = 0.000001))+
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.25)

#Create time series plots for top N consuming countries. 
#Filter dataframe to top N consuming countries
NPK_countries_budget_plot_Mt <- df_countries   %>% mutate(Nutrient= fct_relevel(Nutrient,
                                                                                   "N","P","K")) %>%
  mutate(Year= as.numeric(Year)) %>%
  filter(Element %in% c("Cropland nitrogen","Cropland phosphorus", "Cropland potassium" )) %>% #,"Cropland phosphorus per unit area","Cropland potassium per unit area")) %>% 
  filter(Item=="Soil nutrient budget") %>% 
  ggplot(aes(group=Area,x=Year,y=value, color=Area)) +
  geom_point()+geom_line()+
  #geom_smooth(aes(linetype=Area), se=F) +
  scale_color_manual(values=c( "#000000", "magenta3", "#56B4E9", "#009E73", 
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
                               "#999999","#999933"))+
  scale_linetype_manual(values=c("twodash", "longdash","dotted", "dotdash", "dashed", "solid", "longdash","dotted", "dotdash", "dashed")) +
  #geom_smooth(se = F)+
  facet_grid(~Nutrient) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.text=element_text(size=legend_title_size))+
  labs(tag="(b)", y= "Nutrient budget in Mt per country")+ 
scale_y_continuous(labels = scales::label_comma(scale = 0.000001)) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.25)

Plot_countries_regions_NPK_budget_Mt <- ggarrange(NPK_regions_budget_plot_Mt,NPK_countries_budget_plot_Mt,
                                                     ncol=1,nrow=2, common.legend=F, legend="bottom" )

ggsave("./results/Plot_countries_regions_NPK_budget_Mt.jpg", Plot_countries_regions_NPK_budget_Mt,width = 50, height = 40, units = "cm")

#Estimate nutrient use efficiency and plot by region and top N consuming countries----
#Create dataframe with NUE estimations for each nutrient and year. 
df_NUE <- dplyr::filter(df, Item%in% c("Input","Outputs") & Unit=="tonnes") %>% 
  mutate(Nutrient=case_when( #Create column with N, P and K information.
    str_detect(Element,regex("nitrogen", ignore_case=TRUE))~"N",
    str_detect(Element,regex("phosphorus", ignore_case=TRUE))~"P",
    str_detect(Element,regex("potassium", ignore_case=TRUE))~"K",    
    TRUE~"NA")) %>% 
  dplyr::mutate(Item_year=paste0(Item,"_", Nutrient)) %>% 
  dplyr::select(-Item, -Nutrient, -Element, -Item.Code, -Element.Code, -Unit, -Item_order) %>% 
 pivot_wider(names_from=c( Item_year), values_from = value) %>% 
  melt(id=c("Area","Area.Code","Area.Code.M49","country_iso3","N_rank",  "Year")) %>% 
  pivot_wider(names_from=c(variable),values_from = value) %>% 
  mutate(NUE=Outputs_N/Input_N,
         PUE=Outputs_P/Input_P,
         KUE=Outputs_K/Input_K) %>% 
  dplyr::select(Area,N_rank,Year,NUE,PUE,KUE) %>% 
  melt(id=c("Area", "N_rank","Year")) %>% 
  mutate(Nutrient=case_when( #Create column with N, P and K information.
    str_detect(variable,regex("NUE", ignore_case=TRUE))~"N",
    str_detect(variable,regex("PUE", ignore_case=TRUE))~"P",
    str_detect(variable,regex("KUE", ignore_case=TRUE))~"K",    
    TRUE~"NA"))%>% 
  mutate(Nutrient= fct_relevel(Nutrient,
                                                "N","P","K"))

#Plot NUE results at regional level----
#Select regions of interest---
df_regions_NUE <- dplyr::filter(df_NUE, Area %in% Regions)

#Create plot----
NPK_regions_NUE_plot <- df_regions_NUE   %>% 
  mutate(Year= as.numeric(Year)) %>%
   ggplot(aes(group=Area,x=Year,y=value*100, color=Area)) +
  geom_point()+geom_line()+
  #geom_smooth(aes(linetype=Area), se=F)+
  scale_color_manual(values=c( "magenta3", "#56B4E9", "#009E73", 
                               "#F0E442", "#0072B2","#000000"))+
  scale_linetype_manual(values=c("twodash", "longdash","dotted", "dotdash", "dashed", "solid")) +
  #geom_smooth(se = F)+
  facet_grid(~Nutrient) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.text=element_text(size=legend_title_size))+
  labs(tag="(a)", y= "Nutrient use efficiency") 

#Plot NUE results for countries with greatest N consumption----
#Select countries of interest---
df_countries_NUE <- dplyr::filter(df_NUE, N_rank<11)

#Create plot----
NPK_countries_NUE_plot <- df_countries_NUE   %>% 
  mutate(Year= as.numeric(Year)) %>%
  ggplot(aes(group=Area,x=Year,y=value*100, color=Area)) +
  geom_point()+geom_line()+
  #geom_smooth(aes(linetype=Area), se=F)+
  scale_color_manual(values=c( "#000000", "magenta3", "#56B4E9", "#009E73", 
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
                               "#999999","#999933"))+
  scale_linetype_manual(values=c("twodash", "longdash","dotted", "dotdash", "dashed", "solid", "longdash","dotted", "dotdash", "dashed")) +
  #geom_smooth(se = F)+
  facet_grid(~Nutrient) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.text=element_text(size=legend_title_size))+
  labs(tag="(b)", y= "Nutrient use efficiency")

Plot_countries_regions_NPK_NUE <- ggarrange(NPK_regions_NUE_plot,NPK_countries_NUE_plot,
                                                  ncol=1,nrow=2, common.legend=F, legend="bottom" )

ggsave("./results/Plot_countries_regions_NPK_NUE.jpg", Plot_countries_regions_NPK_NUE,width = 50, height = 40, units = "cm")

#Create region and country-centric plots for nutrient budgets and estimates of NUE----
#Create region-centric plots for nutrient budgets and nutrient use efficiency----
Plot_regions_NPK_budget_NUE <- ggarrange(NPK_regions_budget_plot_kg_ha + labs(tag="a", y= "Nutrient budget in kg per hectare") ,
                                         NPK_regions_NUE_plot+labs(tag="(b)", y= "Nutrient use efficiency") ,
                                            ncol=1,nrow=2, common.legend=T, legend="bottom" )

ggsave("./results/Plot_regions_NPK_budget_NUE.jpg", Plot_regions_NPK_budget_NUE,width = 50, height = 40, units = "cm")

#Create country-centric plots for nutrient budgets and nutrient use efficiency----
Plot_countries_NPK_NUE_budget <- ggarrange(NPK_countries_budget_plot_kg_ha+ labs(tag="a", y= "Nutrient budget in kg per hectare"),
                                           NPK_countries_NUE_plot+labs(tag="(b)", y= "Nutrient use efficiency"),
                                            ncol=1,nrow=2, common.legend=T, legend="bottom" )

ggsave("./results/Plot_countries_NPK_NUE_budget.jpg", Plot_countries_NPK_NUE_budget,width = 50, height = 40, units = "cm")

#Save faceted  (by N,P and K) Mt nutrient budget for major world regions----
NPK_regions_budget_plot_Mt <- NPK_regions_budget_plot_Mt  + labs(tag="", y= "Nutrient budget in Mt per region")
ggsave("./results/NPK_regions_budget_plot_Mt.jpg", NPK_regions_budget_plot_Mt,width = 30, height = 20, units = "cm")

#Compare N budget and N use efficiencies against other studies----
#Create table of latest N budget and NUE results by year at world level for comparison with Zhang et al (2021)
world_N_budget <- dplyr::filter(df_world, Unit == "tonnes"& Item==c("Soil nutrient budget" )& Element=="Cropland nitrogen") %>% 
  dplyr::select(Area,Year, Unit,value) %>% 
  mutate(FAO_2022_Mt_N_pa=value/1000000,#convert from tonnes to Mt/Tg 
         Year=as.numeric(Year)) %>% 
  dplyr::select(-value, -Unit)

world_NUE <- dplyr::filter(df,Area=="World" & Element =="Cropland nitrogen use efficiency" ) %>% 
  dplyr::select(Area,Year,Unit,value) %>% 
  mutate(FAO_2022_NUE=value/100,
         Year=as.numeric(Year)) %>%  #convert to proportion, from percentage
dplyr::select(-value, -Unit)

#Create compatible data frames with Zhang N budget and NUE data----
world_N_budget_Zhang <- world_N_budget_Zhang%>%
  pivot_longer(cols=c(-...1),names_to="Year")%>%
  pivot_wider(names_from=c(...1)) %>% 
  as.data.frame()

world_NUE_Zhang <- world_NUE_Zhang%>%
  pivot_longer(cols=c(-...1),names_to="Year")%>%
  pivot_wider(names_from=c(...1))%>% 
  as.data.frame()

#Merge datasets together----
world_N_budget_comparison <- merge(world_N_budget, world_N_budget_Zhang, all.x=TRUE)
world_NUE_comparison <- merge(world_NUE, world_NUE_Zhang, all.x=TRUE)

#Tidy column names remove special characters----
names(world_N_budget_comparison) <- make.names(names(world_N_budget_comparison))
names(world_NUE_comparison) <- make.names(names(world_NUE_comparison))

#Melt data frames for ggplot----
world_N_budget_comparison <- world_N_budget_comparison %>% melt(id=c("Area","Year"))
world_NUE_comparison <- world_NUE_comparison %>% melt(id=c("Area","Year"))

#Tidy up dataframe references----
world_N_budget_comparison <- world_N_budget_comparison %>% 
  mutate(Reference=case_when( #Convert variable names into appropriate reference style
    str_detect(variable,regex("FAO_2022_Mt_N_pa", ignore_case=TRUE))~"FAO (2022) Cropland Nutrient Budget (current study)",
    str_detect(variable,regex("X.Zhang.Reorganized.", ignore_case=TRUE))~"Zhang et al (2015) reorganized",
    str_detect(variable,regex("X.Zhang.2015.", ignore_case=TRUE))~"Zhang et al (2015)",
    str_detect(variable,regex("Lassaletta", ignore_case=TRUE))~"Lassaletta et al (2014;2016)",
    str_detect(variable,regex("X.Lu.and.Tian.", ignore_case=TRUE))~"Lu & Tian (2017)",
    str_detect(variable,regex("X.Nishina.with.double.cropping.", ignore_case=TRUE))~"Nishina et al (2017) with double crop",
    str_detect(variable,regex("X.Nishina.without.double.cropping.", ignore_case=TRUE))~"Nishina et al (2017) no double crop",
    str_detect(variable,regex("X.Conant.and.Dorich.", ignore_case=TRUE))~"Conant et al (2013)",
    str_detect(variable,regex("X.Bodirsky.without.forage.", ignore_case=TRUE))~"Bodirsky et al (2012) no forage",
    str_detect(variable,regex("X.Bodirsky.with.forage.", ignore_case=TRUE))~"Bodirsky (2012) with forage",
    str_detect(variable,regex("X.IMAGE.", ignore_case=TRUE))~"Bouwman et al (2013)",
    str_detect(variable,regex("X.FAO.", ignore_case=TRUE))~"FAO (2021)",
    str_detect(variable,regex("X.Gerber.and.Mueller.", ignore_case=TRUE))~"Gerber & Mueller (2012)",
    str_detect(variable,regex("X.Chang.et.al.", ignore_case=TRUE))~"Chang et al (2014)",
    TRUE~"NA"))  

world_NUE_comparison <- world_NUE_comparison %>% 
  mutate(Reference=case_when( #Convert variable names into appropriate reference style
    str_detect(variable,regex("FAO_2022_NUE", ignore_case=TRUE))~"FAO (2022) Cropland Nutrient Budget (current study)",
    str_detect(variable,regex("X.Zhang.Reorganized.", ignore_case=TRUE))~"Zhang et al (2015) reorganized",
    str_detect(variable,regex("X.Zhang.2015.", ignore_case=TRUE))~"Zhang et al (2015)",
    str_detect(variable,regex("Lassaletta", ignore_case=TRUE))~"Lassaletta et al (2014;2016)",
    str_detect(variable,regex("X.Lu.and.Tian.", ignore_case=TRUE))~"Lu & Tian (2017)",
    str_detect(variable,regex("X.Nishina.with.double.cropping.", ignore_case=TRUE))~"Nishina et al (2017) with double crop",
    str_detect(variable,regex("X.Nishina.without.double.cropping.", ignore_case=TRUE))~"Nishina et al (2017) no double crop",
    str_detect(variable,regex("X.Conant.and.Dorich.", ignore_case=TRUE))~"Conant et al (2013)",
    str_detect(variable,regex("X.Bodirsky.without.forage.", ignore_case=TRUE))~"Bodirsky et al (2012) no forage",
    str_detect(variable,regex("X.Bodirsky.with.forage.", ignore_case=TRUE))~"Bodirsky (2012) with forage",
    str_detect(variable,regex("X.IMAGE.", ignore_case=TRUE))~"Bouwman et al (2013)",
    str_detect(variable,regex("X.FAO.", ignore_case=TRUE))~"FAO (2021)",
    str_detect(variable,regex("X.Gerber.and.Mueller.", ignore_case=TRUE))~"Gerber & Mueller (2012)",
    str_detect(variable,regex("X.Chang.et.al.", ignore_case=TRUE))~"Chang et al (2014)",
    TRUE~"NA")) 

#Set order of References----
world_N_budget_comparison <- world_N_budget_comparison %>% 
  mutate(Reference=fct_relevel(Reference,
                               "FAO (2022) Cropland Nutrient Budget (current study)",
                               "FAO (2021)",
                               "Bouwman et al (2013)",
                               "Bodirsky et al (2012) no forage",
                               "Bodirsky (2012) with forage",
                               "Chang et al (2014)",
                               "Conant et al (2013)",
                               "Gerber & Mueller (2012)",
                               "Lassaletta et al (2014;2016)",
                               "Lu & Tian (2017)",
                               "Nishina et al (2017) no double crop",
                               "Nishina et al (2017) with double crop",
                               "Zhang et al (2015)",
                               "Zhang et al (2015) reorganized"))

world_NUE_comparison <- world_NUE_comparison %>% 
  mutate(Reference=fct_relevel(Reference,
                               "FAO (2022) Cropland Nutrient Budget (current study)",
                               "FAO (2021)",
                               "Bouwman et al (2013)",
                               "Bodirsky et al (2012) no forage",
                               "Bodirsky (2012) with forage",
                               "Chang et al (2014)",
                               "Conant et al (2013)",
                               "Gerber & Mueller (2012)",
                               "Lassaletta et al (2014;2016)",
                               "Lu & Tian (2017)",
                               "Nishina et al (2017) no double crop",
                               "Nishina et al (2017) with double crop",
                               "Zhang et al (2015)",
                               "Zhang et al (2015) reorganized"))

#Plot results----
plot_world_N_budget_comparison  <- world_N_budget_comparison %>%  ggplot(aes(x=Year, y=value, color = Reference)) +
  geom_line(aes(size = Reference)) +
  scale_size_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = 3, 
                               "Zhang et al (2015) reorganized"=1,
                               "Zhang et al (2015)"=1,
                               "Lassaletta et al (2014;2016)"=1,
                               "Lu & Tian (2017)"=1,
                               "Nishina et al (2017) with double crop"=1,
                               "Nishina et al (2017) no double crop"=1,
                               "Conant et al (2013)"=1,
                               "Bodirsky et al (2012) no forage"=1,
                               "Bodirsky (2012) with forage"=1,
                               "Bouwman et al (2013)"=1,
                               "FAO (2021)"=2,
                               "Gerber & Mueller (2012)"=1,
                               "Chang et al (2014)"=1))+
  scale_color_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = "#000000", 
                               "Zhang et al (2015) reorganized"="#56B4E9",
                               "Zhang et al (2015)"="#009E73",
                               "Lassaletta et al (2014;2016)"="magenta3",
                               "Lu & Tian (2017)"="#0072B2",
                               "Nishina et al (2017) with double crop"="#D55E00",
                               "Nishina et al (2017) no double crop"="#CC79A7",
                               "Conant et al (2013)"="#999999",
                               "Bodirsky et al (2012) no forage"="#999933",
                               "Bodirsky (2012) with forage"="brown",
                               "Bouwman et al (2013)"="yellow",
                               "FAO (2021)"="red",
                               "Gerber & Mueller (2012)"="pink",
                               "Chang et al (2014)"="grey")) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size-0.5), 
        legend.key.size = unit(legend_key_size-0.5, 'cm'),
        legend.text=element_text(size=legend_title_size-0.5))+
  labs(tag="(c)", y= "Nitrogen budget (surplus)\n (Mt per year)")

plot_world_NUE_comparison  <- world_NUE_comparison %>%  ggplot(aes(x=Year, y=value*100, color = Reference)) +
  geom_line(aes(size = Reference)) +
  scale_size_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = 3, 
                               "Zhang et al (2015) reorganized"=1,
                               "Zhang et al (2015)"=1,
                               "Lassaletta et al (2014;2016)"=1,
                               "Lu & Tian (2017)"=1,
                               "Nishina et al (2017) with double crop"=1,
                               "Nishina et al (2017) no double crop"=1,
                               "Conant et al (2013)"=1,
                               "Bodirsky et al (2012) no forage"=1,
                               "Bodirsky (2012) with forage"=1,
                               "Bouwman et al (2013)"=1,
                               "FAO (2021)"=2,
                               "Gerber & Mueller (2012)"=1,
                               "Chang et al (2014)"=1))+
  scale_color_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = "#000000", 
                                "Zhang et al (2015) reorganized"="#56B4E9",
                                "Zhang et al (2015)"="#009E73",
                                "Lassaletta et al (2014;2016)"="magenta3",
                                "Lu & Tian (2017)"="#0072B2",
                                "Nishina et al (2017) with double crop"="#D55E00",
                                "Nishina et al (2017) no double crop"="#CC79A7",
                                "Conant et al (2013)"="#999999",
                                "Bodirsky et al (2012) no forage"="#999933",
                                "Bodirsky (2012) with forage"="brown",
                                "Bouwman et al (2013)"="yellow",
                                "FAO (2021)"="red",
                                "Gerber & Mueller (2012)"="pink",
                                "Chang et al (2014)"="grey")) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size-0.5), 
        legend.key.size = unit(legend_key_size-0.5, 'cm'),
        legend.text=element_text(size=legend_title_size-0.5))+
  labs(tag="(d)", y= "Nitrogen use efficiency\n (%)")

plot_world_N_budget_NUE_comparison <- ggarrange(plot_world_N_budget_comparison,
                                                plot_world_NUE_comparison,
                                                  ncol=1,nrow=2, common.legend=T, legend="right" )

ggsave("./results/plot_world_N_budget_NUE_comparison .jpg", plot_world_N_budget_NUE_comparison ,width = 50, height = 40, units = "cm")

#Combine plots for article----
plot_world_region_budget_total_per_ha_use_efficiency <- ggarrange(NPK_regions_budget_plot_Mt+
                                                                    labs(tag="(a)", y="Nutrient budget in Mt per Area"),
                                                  NPK_regions_budget_plot_kg_ha+
                                                    labs(tag="(b)"),
                                                  NPK_regions_NUE_plot+
                                                    labs(tag="(c)", y= "Nutrient use efficiency %"),
                                                ncol=1,nrow=3, common.legend=T, legend="bottom")

plot_world_region_budget_total_per_ha_use_efficiency
ggsave("./results/plot_world_region_budget_total_per_ha_use_efficiency.jpg", plot_world_region_budget_total_per_ha_use_efficiency ,width = 50, height = 40, units = "cm")

plot_countries_budget_per_ha_use_efficiency <- ggarrange(NPK_countries_budget_plot_kg_ha +
                                                                    labs(tag="(a)"),#, y="Nutrient budget in Mt per Area"),
                                                         NPK_countries_NUE_plot +
                                                                    labs(tag="(b)", y= "Nutrient use efficiency %"),
                                                                  ncol=1,nrow=2, common.legend=T, legend="bottom")

plot_countries_budget_per_ha_use_efficiency
ggsave("./results/plot_countries_budget_per_ha_use_efficiency.jpg", plot_countries_budget_per_ha_use_efficiency ,width = 50, height = 40, units = "cm")

#Assess uncertainty in crop removal coefficients for major crops----
df_CR <- as.data.frame(read_csv("data/LudemannDRYAD2023/Combined_crop_data.csv"))
selected_items <- c("Maize","Rice, paddy","Soybeans","Wheat")
selected_variables <- c("N_pc","P_pc","K_pc","DM_pc")

df_CR <- df_CR %>% dplyr::filter(crop_component==c("Crop_products"), 
                                  item %in% selected_items,
                                  variable %in% selected_variables) %>% 
  dplyr::select(item,variable,value) %>% 
  group_by(item, variable) %>% 
  dplyr::summarise(across(
    .cols = where(is.numeric),
    .fns=list(Mean=mean, SD=sd, Max=max, Min=min), na.rm=TRUE,
    .names="{col}_{fn}"))

df_CR <- as.data.frame(df_CR)

df_DM <- df_CR %>% dplyr::filter(variable=="DM_pc") %>% dplyr::rename(mean_DM=value_Mean) %>% dplyr::select(item,mean_DM)
df_CR <- df_CR %>% dplyr::filter(!variable=="DM_pc")

df_CR <- merge(df_CR,df_DM, all.x=TRUE)

df_CR <- df_CR %>% mutate(across(starts_with("value"), ~.*(df_CR$mean_DM)))#Get units in kg per tonne fresh weight by creating Mean DM column.
df_CR$CV_pc <- (df_CR$value_SD/df_CR$value_Mean)*100

df_CR <- df_CR %>% mutate(across(starts_with("value"), ~./10)) #Get units in kg per tonne fresh weight.

df_CR <- df_CR %>% 
  mutate(item=fct_relevel(item, c("Maize","Rice, paddy","Soybeans","Wheat")), variable=fct_relevel(variable,(c("N_pc","P_pc","K_pc")))) %>% 
  arrange(item,variable) %>% 
  mutate(variable=as.character(variable)) %>% 
  mutate(variable=case_when(
    str_detect(variable,regex("N_pc", ignore_case=TRUE))~"N_kg_per_t_fresh_wt", 
    str_detect(variable,regex("P_pc", ignore_case=TRUE))~"P_kg_per_t_fresh_wt", 
    str_detect(variable,regex("K_pc", ignore_case=TRUE))~"K_kg_per_t_fresh_wt", 
    TRUE~"NA"))
#Save as csv file----
write.csv(df_CR,"./results/crop_product_removal_uncertainty_stats.csv",row.names= FALSE)

#Assess ranking of nutrient budgets by country----
# A <- filter(df, Item=="Soil nutrient budget", Element %in% c("Cropland nitrogen per unit area", "Cropland nitrogen use efficiency")) %>% arrange(-as.numeric(Year)) %>% 
#   filter(is.na(country_iso3)) %>% filter(Year=="1986")
# View(A)
# 
# View(as.data.frame(unique(df$Element)))

#Assess why our latest NUE results are much higher than Zhang et al results----
#Create tidy dataframes for N inputs and N harvested
world_N_inputs_Zhang <- world_N_inputs_Zhang%>%
  pivot_longer(cols=c(-...1),names_to="Year")%>%
  pivot_wider(names_from=c(...1)) %>% 
  as.data.frame()

world_N_harvested_Zhang <- world_N_harvested_Zhang%>%
  pivot_longer(cols=c(-...1),names_to="Year")%>%
  pivot_wider(names_from=c(...1)) %>% 
  as.data.frame()

#Create dataframes with latest FAO data for N inputs and N harvested
world_N_inputs <- dplyr::filter(df_world, Unit == "tonnes"& Item==c("Input" )& Element=="Cropland nitrogen") %>% 
  dplyr::select(Area,Year, Unit,value) %>% 
  mutate(FAO_2022_Mt_N_pa=value/1000000,#convert from tonnes to Mt/Tg 
         Year=as.numeric(Year)) %>% 
  dplyr::select(-value, -Unit)

world_N_outputs <- dplyr::filter(df_world, Unit == "tonnes"& Item==c("Outputs" )& Element=="Cropland nitrogen") %>% 
  dplyr::select(Area,Year, Unit,value) %>% 
  mutate(FAO_2022_Mt_N_pa=value/1000000,#convert from tonnes to Mt/Tg 
         Year=as.numeric(Year)) %>% 
  dplyr::select(-value, -Unit)

#Merge datasets together----
world_N_inputs_comparison <- merge(world_N_inputs, world_N_inputs_Zhang, all.x=TRUE)
world_N_outputs_comparison <- merge(world_N_outputs, world_N_harvested_Zhang, all.x=TRUE)

#Tidy column names remove special characters----
names(world_N_inputs_comparison ) <- make.names(names(world_N_inputs_comparison ))
names(world_N_outputs_comparison) <- make.names(names(world_N_outputs_comparison))

#Melt data frames for ggplot----
world_N_inputs_comparison <- world_N_inputs_comparison  %>% melt(id=c("Area","Year"))
world_N_outputs_comparison <- world_N_outputs_comparison %>% melt(id=c("Area","Year"))

#Tidy up dataframe references----
world_N_inputs_comparison <- world_N_inputs_comparison %>% 
  mutate(Reference=case_when( #Convert variable names into appropriate reference style
    str_detect(variable,regex("FAO_2022_Mt_N_pa", ignore_case=TRUE))~"FAO (2022) Cropland Nutrient Budget (current study)",
    str_detect(variable,regex("X.Zhang.Reorganized.", ignore_case=TRUE))~"Zhang et al (2015) reorganized",
    str_detect(variable,regex("X.Zhang.2015.", ignore_case=TRUE))~"Zhang et al (2015)",
    str_detect(variable,regex("Lassaletta", ignore_case=TRUE))~"Lassaletta et al (2014;2016)",
    str_detect(variable,regex("X.Lu.and.Tian.", ignore_case=TRUE))~"Lu & Tian (2017)",
    str_detect(variable,regex("X.Nishina.with.double.cropping.", ignore_case=TRUE))~"Nishina et al (2017) with double crop",
    str_detect(variable,regex("X.Nishina.without.double.cropping.", ignore_case=TRUE))~"Nishina et al (2017) no double crop",
    str_detect(variable,regex("X.Conant.and.Dorich.", ignore_case=TRUE))~"Conant et al (2013)",
    str_detect(variable,regex("X.Bodirsky.without.forage.", ignore_case=TRUE))~"Bodirsky et al (2012) no forage",
    str_detect(variable,regex("X.Bodirsky.with.forage.", ignore_case=TRUE))~"Bodirsky (2012) with forage",
    str_detect(variable,regex("X.IMAGE.", ignore_case=TRUE))~"Bouwman et al (2013)",
    str_detect(variable,regex("X.FAO.", ignore_case=TRUE))~"FAO (2021)",
    str_detect(variable,regex("X.Gerber.and.Mueller.", ignore_case=TRUE))~"Gerber & Mueller (2012)",
    str_detect(variable,regex("X.Chang.et.al.", ignore_case=TRUE))~"Chang et al (2014)",
    TRUE~"NA"))  

world_N_outputs_comparison <- world_N_outputs_comparison %>% 
  mutate(Reference=case_when( #Convert variable names into appropriate reference style
    str_detect(variable,regex("FAO_2022_Mt_N_pa", ignore_case=TRUE))~"FAO (2022) Cropland Nutrient Budget (current study)",
    str_detect(variable,regex("X.Zhang.Reorganized.", ignore_case=TRUE))~"Zhang et al (2015) reorganized",
    str_detect(variable,regex("X.Zhang.2015.", ignore_case=TRUE))~"Zhang et al (2015)",
    str_detect(variable,regex("Lassaletta", ignore_case=TRUE))~"Lassaletta et al (2014;2016)",
    str_detect(variable,regex("X.Lu.and.Tian.", ignore_case=TRUE))~"Lu & Tian (2017)",
    str_detect(variable,regex("X.Nishina.with.double.cropping.", ignore_case=TRUE))~"Nishina et al (2017) with double crop",
    str_detect(variable,regex("X.Nishina.without.double.cropping.", ignore_case=TRUE))~"Nishina et al (2017) no double crop",
    str_detect(variable,regex("X.Conant.and.Dorich.", ignore_case=TRUE))~"Conant et al (2013)",
    str_detect(variable,regex("X.Bodirsky.without.forage.", ignore_case=TRUE))~"Bodirsky et al (2012) no forage",
    str_detect(variable,regex("X.Bodirsky.with.forage.", ignore_case=TRUE))~"Bodirsky (2012) with forage",
    str_detect(variable,regex("X.IMAGE.", ignore_case=TRUE))~"Bouwman et al (2013)",
    str_detect(variable,regex("X.FAO.", ignore_case=TRUE))~"FAO (2021)",
    str_detect(variable,regex("X.Gerber.and.Mueller.", ignore_case=TRUE))~"Gerber & Mueller (2012)",
    str_detect(variable,regex("X.Chang.et.al.", ignore_case=TRUE))~"Chang et al (2014)",
    TRUE~"NA")) 

#Set order of References----
world_N_inputs_comparison <- world_N_inputs_comparison %>% 
  mutate(Reference=fct_relevel(Reference,
                               "FAO (2022) Cropland Nutrient Budget (current study)",
                               "FAO (2021)",
                               "Bouwman et al (2013)",
                               "Bodirsky et al (2012) no forage",
                               "Bodirsky (2012) with forage",
                               "Chang et al (2014)",
                               "Conant et al (2013)",
                               "Gerber & Mueller (2012)",
                               "Lassaletta et al (2014;2016)",
                               "Lu & Tian (2017)",
                               "Nishina et al (2017) no double crop",
                               "Nishina et al (2017) with double crop",
                               "Zhang et al (2015)",
                               "Zhang et al (2015) reorganized"))

world_N_outputs_comparison <- world_N_outputs_comparison %>% 
  mutate(Reference=fct_relevel(Reference,
                               "FAO (2022) Cropland Nutrient Budget (current study)",
                               "FAO (2021)",
                               "Bouwman et al (2013)",
                               "Bodirsky et al (2012) no forage",
                               "Bodirsky (2012) with forage",
                               "Chang et al (2014)",
                               "Conant et al (2013)",
                               "Gerber & Mueller (2012)",
                               "Lassaletta et al (2014;2016)",
                               "Lu & Tian (2017)",
                               "Nishina et al (2017) no double crop",
                               "Nishina et al (2017) with double crop",
                               "Zhang et al (2015)",
                               "Zhang et al (2015) reorganized"))

#Plot results----
world_N_inputs_comparison_plot  <- world_N_inputs_comparison %>%  ggplot(aes(x=Year, y=value, color = Reference)) +
  geom_line(aes(size = Reference)) +
  scale_size_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = 3, 
                               "Zhang et al (2015) reorganized"=1,
                               "Zhang et al (2015)"=1,
                               "Lassaletta et al (2014;2016)"=1,
                               "Lu & Tian (2017)"=1,
                               "Nishina et al (2017) with double crop"=1,
                               "Nishina et al (2017) no double crop"=1,
                               "Conant et al (2013)"=1,
                               "Bodirsky et al (2012) no forage"=1,
                               "Bodirsky (2012) with forage"=1,
                               "Bouwman et al (2013)"=1,
                               "FAO (2021)"=2,
                               "Gerber & Mueller (2012)"=1,
                               "Chang et al (2014)"=1))+
  scale_color_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = "#000000", 
                                "Zhang et al (2015) reorganized"="#56B4E9",
                                "Zhang et al (2015)"="#009E73",
                                "Lassaletta et al (2014;2016)"="magenta3",
                                "Lu & Tian (2017)"="#0072B2",
                                "Nishina et al (2017) with double crop"="#D55E00",
                                "Nishina et al (2017) no double crop"="#CC79A7",
                                "Conant et al (2013)"="#999999",
                                "Bodirsky et al (2012) no forage"="#999933",
                                "Bodirsky (2012) with forage"="brown",
                                "Bouwman et al (2013)"="yellow",
                                "FAO (2021)"="red",
                                "Gerber & Mueller (2012)"="pink",
                                "Chang et al (2014)"="grey")) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size-0.5), 
        legend.key.size = unit(legend_key_size-0.5, 'cm'),
        legend.text=element_text(size=legend_title_size-0.5))+
  labs(tag="(a)", y= "Nitrogen inputs\n (Mt per year)")

world_N_outputs_comparison_plot  <- world_N_outputs_comparison %>%  ggplot(aes(x=Year, y=value, color = Reference)) +
  geom_line(aes(size = Reference)) +
  scale_size_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = 3, 
                               "Zhang et al (2015) reorganized"=1,
                               "Zhang et al (2015)"=1,
                               "Lassaletta et al (2014;2016)"=1,
                               "Lu & Tian (2017)"=1,
                               "Nishina et al (2017) with double crop"=1,
                               "Nishina et al (2017) no double crop"=1,
                               "Conant et al (2013)"=1,
                               "Bodirsky et al (2012) no forage"=1,
                               "Bodirsky (2012) with forage"=1,
                               "Bouwman et al (2013)"=1,
                               "FAO (2021)"=2,
                               "Gerber & Mueller (2012)"=1,
                               "Chang et al (2014)"=1))+
  scale_color_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = "#000000", 
                                "Zhang et al (2015) reorganized"="#56B4E9",
                                "Zhang et al (2015)"="#009E73",
                                "Lassaletta et al (2014;2016)"="magenta3",
                                "Lu & Tian (2017)"="#0072B2",
                                "Nishina et al (2017) with double crop"="#D55E00",
                                "Nishina et al (2017) no double crop"="#CC79A7",
                                "Conant et al (2013)"="#999999",
                                "Bodirsky et al (2012) no forage"="#999933",
                                "Bodirsky (2012) with forage"="brown",
                                "Bouwman et al (2013)"="yellow",
                                "FAO (2021)"="red",
                                "Gerber & Mueller (2012)"="pink",
                                "Chang et al (2014)"="grey")) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size-0.5), 
        legend.key.size = unit(legend_key_size-0.5, 'cm'),
        legend.text=element_text(size=legend_title_size-0.5))+
  labs(tag="(b)", y= "Nitrogen outputs\n (Mt per year)")

plot_world_N_budget_NUE_inputs_outputs_comparison <- ggarrange(
                                                    world_N_inputs_comparison_plot,
                                                    world_N_outputs_comparison_plot,
                                                    plot_world_N_budget_comparison,
                                                    plot_world_NUE_comparison,
                                                ncol=1,nrow=4, common.legend=T, legend="bottom", 
                                                font.label = list(size = 30))                  

ggsave("./results/plot_world_N_budget_NUE_inputs_outputs_comparison.jpg", plot_world_N_budget_NUE_inputs_outputs_comparison,
       width = 60, height = 60, units = "cm")
 
#Map 2020 nutrient use efficiency values by country----
#Filter df areas to actual countries (Area.Code<400 and to )
#Create dataframes with year and element of interest----
df_N_kg_ha <- dplyr::filter(df,Year==2020, Element=="Cropland nitrogen per unit area",  Item=="Soil nutrient budget")
df_P_kg_ha <- dplyr::filter(df,Year==2020, Element=="Cropland phosphorus per unit area", Item=="Soil nutrient budget")
df_K_kg_ha <- dplyr::filter(df,Year==2020, Element=="Cropland potassium per unit area",Item=="Soil nutrient budget")
df_NUE <- dplyr::filter(df,Year==2020, Element=="Cropland nitrogen use efficiency",  Item=="Soil nutrient budget")#Area.Code<400,
df_PUE <- dplyr::filter(df,Year==2020, Element=="Cropland phosphorus use efficiency", Item=="Soil nutrient budget")
df_KUE <- dplyr::filter(df,Year==2020, Element=="Cropland potassium use efficiency", Item=="Soil nutrient budget")

#Create nutrient and category columns----
df_N_kg_ha$Nutrient <- "N"
df_P_kg_ha$Nutrient <- "P"
df_K_kg_ha$Nutrient <-  "K"
df_NUE$Nutrient <- "N"
df_PUE$Nutrient <- "P"
df_KUE$Nutrient <- "K"

df_N_kg_ha$Range <- NA
df_P_kg_ha$Range <- NA
df_K_kg_ha$Range <- NA
df_NUE$Range <- NA
df_PUE$Range <- NA
df_KUE$Range <- NA

#Standardise colours for each category----
colour1 <- "#543005"
colour2 <- "red"
colour3="lightgreen"
colour4="darkgreen"
colour5= "yellow"
colour6="orange"
colour_no_data="grey"

#Get dataframe with map information
nc <- sf::st_read("./data/UNmap_05/BNDA05_CTY.shp", quiet = TRUE)

#Merge map information with data
df_N_kg_ha <- merge(nc, df_N_kg_ha, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))
df_P_kg_ha <- merge(nc, df_P_kg_ha, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))
df_K_kg_ha <- merge(nc, df_K_kg_ha, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))
df_NUE <- merge(nc,df_NUE,  all.x=TRUE,by.x=c("ISO3CD"), by.y=c("country_iso3"))
df_PUE <- merge(nc, df_PUE, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))
df_KUE <- merge(nc, df_KUE, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))

#Assign values to categories----
#N_kg_ha
Quantiles = signif(quantile(df_N_kg_ha$value, c(.20, .40, .60, .80), na.rm=T), 1)
df_N_kg_ha$Range = NA
df_N_kg_ha$Range[df_N_kg_ha$value<round(Quantiles[1],0)[[1]]&!is.na(df_N_kg_ha$value)] = paste0("<= ",as.character(round(Quantiles[1],0)[[1]]))
df_N_kg_ha$Range[df_N_kg_ha$value>=round(Quantiles[1],0)[[1]]&df_N_kg_ha$value<=round(Quantiles[2],0)[[1]]&!is.na(df_N_kg_ha$value)] = paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]]))
df_N_kg_ha$Range[df_N_kg_ha$value>=round(Quantiles[2],0)[[1]]&df_N_kg_ha$value<=round(Quantiles[3],0)[[1]]&!is.na(df_N_kg_ha$value)] = paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]]))
df_N_kg_ha$Range[df_N_kg_ha$value>=round(Quantiles[3],0)[[1]]&df_N_kg_ha$value<=round(Quantiles[4],0)[[1]]&!is.na(df_N_kg_ha$value)] = paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]]))
df_N_kg_ha$Range[df_N_kg_ha$value>=round(Quantiles[4],0)[[1]]] = paste0("> ",as.character(round(Quantiles[4],0)[[1]]))
df_N_kg_ha$Range[is.na(df_N_kg_ha$value)] = "No data"
df_N_kg_ha$Range = as.character(as.factor(df_N_kg_ha$Range))
df_N_kg_ha$Range = as.factor(df_N_kg_ha$Range)
df_N_kg_ha$Range <- factor(df_N_kg_ha$Range, levels = c(paste0("<= ",as.character(round(Quantiles[1],0)[[1]])), 
                                                          paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]])), 
                                                          paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]])),
                                                          paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]])),
                                                          paste0("> ",as.character(round(Quantiles[4],0)[[1]])),
                                                          "No data"))

#P_kg_ha
Quantiles = signif(quantile(df_P_kg_ha$value, c(.20, .40, .60, .80), na.rm=T), 1)
df_P_kg_ha$Range = NA
df_P_kg_ha$Range[df_P_kg_ha$value<round(Quantiles[1],0)[[1]]&!is.na(df_P_kg_ha$value)] = paste0("<= ",as.character(round(Quantiles[1],0)[[1]]))
df_P_kg_ha$Range[df_P_kg_ha$value>=round(Quantiles[1],0)[[1]]&df_P_kg_ha$value<=round(Quantiles[2],0)[[1]]&!is.na(df_P_kg_ha$value)] = paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]]))
df_P_kg_ha$Range[df_P_kg_ha$value>=round(Quantiles[2],0)[[1]]&df_P_kg_ha$value<=round(Quantiles[3],0)[[1]]&!is.na(df_P_kg_ha$value)] = paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]]))
df_P_kg_ha$Range[df_P_kg_ha$value>=round(Quantiles[3],0)[[1]]&df_P_kg_ha$value<=round(Quantiles[4],0)[[1]]&!is.na(df_P_kg_ha$value)] = paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]]))
df_P_kg_ha$Range[df_P_kg_ha$value>=round(Quantiles[4],0)[[1]]] = paste0("> ",as.character(round(Quantiles[4],0)[[1]]))
df_P_kg_ha$Range[is.na(df_P_kg_ha$value)] = "No data"
df_P_kg_ha$Range = as.character(as.factor(df_P_kg_ha$Range))
df_P_kg_ha$Range = as.factor(df_P_kg_ha$Range)
df_P_kg_ha$Range <- factor(df_P_kg_ha$Range, levels = c(paste0("<= ",as.character(round(Quantiles[1],0)[[1]])), 
                                                        paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]])), 
                                                        paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]])),
                                                        paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]])),
                                                        paste0("> ",as.character(round(Quantiles[4],0)[[1]])),
                                                        "No data"))

#K_kg_ha
Quantiles = signif(quantile(df_K_kg_ha$value, c(.20, .40, .60, .80), na.rm=T), 1)
df_K_kg_ha$Range = NA
df_K_kg_ha$Range[df_K_kg_ha$value<round(Quantiles[1],0)[[1]]&!is.na(df_K_kg_ha$value)] = paste0("<= ",as.character(round(Quantiles[1],0)[[1]]))
df_K_kg_ha$Range[df_K_kg_ha$value>=round(Quantiles[1],0)[[1]]&df_K_kg_ha$value<=round(Quantiles[2],0)[[1]]&!is.na(df_K_kg_ha$value)] = paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]]))
df_K_kg_ha$Range[df_K_kg_ha$value>=round(Quantiles[2],0)[[1]]&df_K_kg_ha$value<=round(Quantiles[3],0)[[1]]&!is.na(df_K_kg_ha$value)] = paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]]))
df_K_kg_ha$Range[df_K_kg_ha$value>=round(Quantiles[3],0)[[1]]&df_K_kg_ha$value<=round(Quantiles[4],0)[[1]]&!is.na(df_K_kg_ha$value)] = paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]]))
df_K_kg_ha$Range[df_K_kg_ha$value>=round(Quantiles[4],0)[[1]]] = paste0("> ",as.character(round(Quantiles[4],0)[[1]]))
df_K_kg_ha$Range[is.na(df_K_kg_ha$value)] = "No data"
df_K_kg_ha$Range = as.character(as.factor(df_K_kg_ha$Range))
df_K_kg_ha$Range = as.factor(df_K_kg_ha$Range)
df_K_kg_ha$Range <- factor(df_K_kg_ha$Range, levels = c(paste0("<= ",as.character(round(Quantiles[1],0)[[1]])), 
                                                        paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]])), 
                                                        paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]])),
                                                        paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]])),
                                                        paste0("> ",as.character(round(Quantiles[4],0)[[1]])),
                                                        "No data"))
#NUE
df_NUE$Range[df_NUE$value<0&!is.na(df_NUE$value)] = "<= 0"
df_NUE$Range[df_NUE$value>=0&df_NUE$value<=50&!is.na(df_NUE$value)] = "(0 <= 50)"
df_NUE$Range[df_NUE$value>=50&df_NUE$value<=65&!is.na(df_NUE$value)] = "(50 <= 65)"
df_NUE$Range[df_NUE$value>=65&df_NUE$value<=80&!is.na(df_NUE$value)] = "(65 <= 80)"
df_NUE$Range[df_NUE$value>=80&df_NUE$value<=90&!is.na(df_NUE$value)] = "(80 <= 90)"
df_NUE$Range[df_NUE$value>90&!is.na(df_NUE$value)] = "> 90"
df_NUE$Range[is.na(df_NUE$value)] = "No data"
df_NUE$Range = as.character(as.factor(df_NUE$Range))
df_NUE$Range = as.factor(df_NUE$Range)
df_NUE$Range <- factor(df_NUE$Range, levels = c("<= 0", 
                                                    "(0 <= 50)", 
                                                    "(50 <= 65)",
                                                    "(65 <= 80)",
                                                    "(80 <= 90)",
                                                    "> 90",
                                                    "No data"))

#PUE
df_PUE$Range[df_PUE$value<0&!is.na(df_PUE$value)] = "<= 0"
df_PUE$Range[df_PUE$value>=0&df_PUE$value<=50&!is.na(df_PUE$value)] = "(0 <= 50)"
df_PUE$Range[df_PUE$value>=50&df_PUE$value<=65&!is.na(df_PUE$value)] = "(50 <= 65)"
df_PUE$Range[df_PUE$value>=65&df_PUE$value<=80&!is.na(df_PUE$value)] = "(65 <= 80)"
df_PUE$Range[df_PUE$value>=80&df_PUE$value<=90&!is.na(df_PUE$value)] = "(80 <= 90)"
df_PUE$Range[df_PUE$value>90&!is.na(df_PUE$value)] = "> 90"
df_PUE$Range[is.na(df_PUE$value)] = "No data"
df_PUE$Range = as.character(as.factor(df_PUE$Range))
df_PUE$Range = as.factor(df_PUE$Range)
df_PUE$Range <- factor(df_PUE$Range, levels = c("<= 0", 
                                                "(0 <= 50)", 
                                                "(50 <= 65)",
                                                "(65 <= 80)",
                                                "(80 <= 90)",
                                                "> 90",
                                                "No data"))

#KUE
df_KUE$Range[df_KUE$value<0&!is.na(df_KUE$value)] = "<= 0"
df_KUE$Range[df_KUE$value>=0&df_KUE$value<=50&!is.na(df_KUE$value)] = "(0 <= 50)"
df_KUE$Range[df_KUE$value>=50&df_KUE$value<=65&!is.na(df_KUE$value)] = "(50 <= 65)"
df_KUE$Range[df_KUE$value>=65&df_KUE$value<=80&!is.na(df_KUE$value)] = "(65 <= 80)"
df_KUE$Range[df_KUE$value>=80&df_KUE$value<=90&!is.na(df_KUE$value)] = "(80 <= 90)"
df_KUE$Range[df_KUE$value>90&!is.na(df_KUE$value)] = "> 90"
df_KUE$Range[is.na(df_KUE$value)] = "No data"
df_KUE$Range = as.character(as.factor(df_KUE$Range))
df_KUE$Range = as.factor(df_KUE$Range)
df_KUE$Range <- factor(df_KUE$Range, levels = c("<= 0", 
                                                "(0 <= 50)", 
                                                "(50 <= 65)",
                                                "(65 <= 80)",
                                                "(80 <= 90)",
                                                "> 90",
                                                "No data"))

#Plot results in map----
map_theme <-   theme_classic()

#N kg ha
N_kg_ha_plot <- ggplot(df_N_kg_ha , aes(fill=Range))+
   geom_sf(size=.1) +
   scale_fill_manual(values=c(colour3,
                              colour4,
                              colour5,
                              colour6,
                              colour2,
                              colour_no_data)) +
   labs(tag="(a) N")+
  map_theme

#P kg ha
P_kg_ha_plot <- ggplot(df_P_kg_ha , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c(colour3,
                            colour4,
                             colour5,
                             colour6,
                             colour2,
                             colour_no_data)) +
  labs(tag="(b) P")+
  map_theme

#K kg ha
K_kg_ha_plot <- ggplot(df_K_kg_ha , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c(colour3,
                             colour4,
                             colour5,
                             colour6,
                             colour2,
                             colour_no_data)) +
  labs(tag="(c) K")+
  map_theme

#NUE
NUE_plot <- ggplot(df_NUE , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c("(0 <= 50)"=colour2, 
                             "(50 <= 65)"=colour3,
                             "(65 <= 80)"=colour4,
                             "(80 <= 90)"=colour5,
                             "> 90"=colour6,
                             "No data"=colour_no_data)) +
  labs(tag="(a) N")+
  map_theme

#PUE
PUE_plot <- ggplot(df_PUE , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c("(0 <= 50)"=colour2, 
                             "(50 <= 65)"=colour3,
                             "(65 <= 80)"=colour4,
                             "(80 <= 90)"=colour5,
                             "> 90"=colour6,
                             "No data"=colour_no_data)) +
  labs(tag="(b) P")+
  map_theme

#KUE
KUE_plot <- ggplot(df_KUE , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c("(0 <= 50)"=colour2, 
                             "(50 <= 65)"=colour3,
                             "(65 <= 80)"=colour4,
                             "(80 <= 90)"=colour5,
                             "> 90"=colour6,
                             "No data"=colour_no_data)) +
  labs(tag="(c) K")+
  map_theme

#Join plots into one----
Nu_kg_ha_Plot <-   ggarrange(N_kg_ha_plot,P_kg_ha_plot,K_kg_ha_plot, ncol=1, 
                          font.label = list(size = 30))

NuUE_Plot <-   ggarrange(NUE_plot,PUE_plot,KUE_plot, ncol=1, 
                         common.legend=TRUE, legend="bottom", font.label = list(size = 30)) 

#Save maps into one file----
ggsave("./results/plot_world_N_P_K_kg_ha.jpg", Nu_kg_ha_Plot,
       width = 20, height = 20, units = "cm")
ggsave("./results/plot_world_NUE_PUE_KUE.jpg", NuUE_Plot,
       width = 20, height = 20, units = "cm")


#Further analysis following peer review:
#Compare P results from Zou et al (2022) https://www.nature.com/articles/s41586-022-05220-z
Zou_2022_P <-as.data.frame(read_excel("data/Zou et al 2022 Nature Global trends cropland K.xlsx"))

#Tidy Zou data
Zou_2022_P <- Zou_2022_P %>% pivot_longer(col=c(-Item), 
                                   names_to="year", 
                                   values_to="value") %>% 
  mutate(Source="Zou et al (2022)", 
         Item=gsub( "Zou", "", Item),
         Item=gsub( "kg", "Mt", Item),
         Item=gsub( "%", "pc", Item)) %>% 
  pivot_wider(names_from=c(Item), values_from = value) %>% 
  mutate(Input_MtPperYr=Input_MtPperYr/1000000000, 
         Output_MtPperYr=Output_MtPperYr/1000000000, 
         Surplus_MtPperYr=Surplus_MtPperYr/1000000000) %>% #Ensure columns relate to correct units.
  pivot_longer(col=c(-year, -Source), names_to='Item')

#Get equivalent data from FAO
FAO_2023_P <- dplyr::filter(df_world, 
                            Nutrient=="P",
                            Unit%in%c("%", "tonnes"), 
                            Item.Code%in%c(5079, 5080,5081)) %>% 
  mutate(Element_code=paste(Element.Code, Item, Unit)) %>% 
  mutate(Source="Current study", 
         Item=case_when(
  str_detect(Element_code,regex("7280 Input", ignore_case=TRUE))~"Input_MtPperYr",
  str_detect(Element_code,regex("7280 Outputs", ignore_case=TRUE))~"Output_MtPperYr",
  str_detect(Element_code,regex("7280 Soil nutrient budget tonnes", ignore_case=TRUE))~"Surplus_MtPperYr",
  str_detect(Element_code,regex("7291 Soil nutrient budget %", ignore_case=TRUE))~"PUE_pc",
  TRUE~"NA")) %>% 
  select(Item, Year,value, Source ) %>% 
 pivot_wider(names_from=c(Item), values_from = value) %>% 
  mutate(Input_MtPperYr=Input_MtPperYr/1000000, 
         Output_MtPperYr=Output_MtPperYr/1000000, 
         Surplus_MtPperYr=Surplus_MtPperYr/1000000)%>%  #Ensure column relate to correct units.
  pivot_longer(col=c(-Year, -Source), names_to='Item')

#Align names of dataframes for rbind
names(FAO_2023_P) <- tolower(names(FAO_2023_P)) 
names(Zou_2022_P) <- tolower(names(Zou_2022_P))

FAO_Zou_P <- rbind(FAO_2023_P,
                   Zou_2022_P)
FAO_Zou_P$year <- as.numeric(FAO_Zou_P$year)
FAO_Zou_P$Reference <- FAO_Zou_P$source

FAO_Zou_P <- FAO_Zou_P %>% 
mutate(Reference=case_when(
         str_detect(source,regex("Current study", ignore_case=TRUE))~"FAO (2022) Cropland Nutrient Budget (current study)",
        # str_detect(source,regex("Zou et al \\(2022\\)", ignore_case=TRUE))~"Zou et al (2022)",
         TRUE~Reference))

#Create separate data frames for each component
FAO_Zou_P_Input <- FAO_Zou_P %>% filter(item=="Input_MtPperYr")
FAO_Zou_P_Output <- FAO_Zou_P %>% filter(item=="Output_MtPperYr")
FAO_Zou_P_Surplus <- FAO_Zou_P %>% filter(item=="Surplus_MtPperYr")
FAO_Zou_PUE_pc <- FAO_Zou_P %>% filter(item=="PUE_pc")

#Plot differences between Zou and FAO data
#Inputs
FAO_Zou_P_input_plot  <- FAO_Zou_P_Input %>%  
  ggplot(aes(x=year, y=value, color = Reference)) +
  geom_line(aes(color=Reference, size=Reference)) + 
  scale_size_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = 3,"Zou et al (2022)"=1))+
  scale_color_manual(values=c("FAO (2022) Cropland Nutrient Budget (current study)" = "#000000","Zou et al (2022)"="#56B4E9"))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size-0.5), 
        legend.key.size = unit(legend_key_size-0.5, 'cm'),
        legend.text=element_text(size=legend_title_size-0.5))+
  labs(tag="(a)", y= "Phosphorus inputs\n (Mt per year)")
FAO_Zou_P_input_plot

#Outputs
FAO_Zou_P_output_plot  <- FAO_Zou_P_Output %>%  
  ggplot(aes(x=year, y=value, color = Reference)) +
  geom_line(aes(color=Reference, size=Reference)) + 
  scale_size_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = 3,"Zou et al (2022)"=1))+
  scale_color_manual(values=c("FAO (2022) Cropland Nutrient Budget (current study)" = "#000000","Zou et al (2022)"="#56B4E9"))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size-0.5), 
        legend.key.size = unit(legend_key_size-0.5, 'cm'),
        legend.text=element_text(size=legend_title_size-0.5))+
  labs(tag="(b)", y= "Phosphorus outputs\n (Mt per year)")
FAO_Zou_P_output_plot

#Surplus
FAO_Zou_P_surplus_plot  <- FAO_Zou_P_Surplus %>%  
  ggplot(aes(x=year, y=value, color = Reference)) +
  geom_line(aes(color=Reference, size=Reference)) + 
  scale_size_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = 3,"Zou et al (2022)"=1))+
  scale_color_manual(values=c("FAO (2022) Cropland Nutrient Budget (current study)" = "#000000","Zou et al (2022)"="#56B4E9"))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size-0.5), 
        legend.key.size = unit(legend_key_size-0.5, 'cm'),
        legend.text=element_text(size=legend_title_size-0.5))+
  labs(tag="(c)", y= "Phosphorus surplus\n (Mt per year)")
FAO_Zou_P_surplus_plot

#Surplus
FAO_Zou_PUE_plot  <- FAO_Zou_PUE_pc %>%  
  ggplot(aes(x=year, y=value, color = Reference)) +
  geom_line(aes(color=Reference, size=Reference)) + 
  scale_size_manual(values = c("FAO (2022) Cropland Nutrient Budget (current study)" = 3,"Zou et al (2022)"=1))+
  scale_color_manual(values=c("FAO (2022) Cropland Nutrient Budget (current study)" = "#000000","Zou et al (2022)"="#56B4E9"))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size-0.5), 
        legend.key.size = unit(legend_key_size-0.5, 'cm'),
        legend.text=element_text(size=legend_title_size-0.5))+
  labs(tag="(d)", y= "PUE\n (%)")
FAO_Zou_PUE_plot

plot_world_P_budget_PUE_inputs_outputs_comparison <- ggarrange(
  FAO_Zou_P_input_plot,
  FAO_Zou_P_output_plot,
  FAO_Zou_P_surplus_plot,
  FAO_Zou_PUE_plot,
  ncol=1,nrow=4, common.legend=T, legend="bottom", 
  font.label = list(size = 30))                  

ggsave("./results/plot_world_P_budget_PUE_inputs_outputs_comparison.jpg", 
       plot_world_P_budget_PUE_inputs_outputs_comparison,
       width = 60, height = 60, units = "cm")

#Save csv files
write.csv(FAO_Zou_P,"./results/FAO_Zou_P_comparison_by_year_world.csv",
          row.names= FALSE)

#Compare nutrient concentrations of key crops
Nutrient_concentrations <- read_excel("data/Nutrient_concentrations_across_studies.xlsx")

#Plot results
Nutrient_concentrations_plot  <- Nutrient_concentrations%>%  
  ggplot(aes(x=Crop, y=Concentration, color=Study))+
  geom_point(aes(size=Study, shape=Study)) + 
  scale_size_manual(values = c(2,2,3,2,2,2,2))+
  scale_shape_manual(values=c(1,2,16,4,5,6,7))+ 
  scale_color_manual(values=c("#56B4E9", "#56B4E9","#000000","#56B4E9","#56B4E9","#56B4E9","#56B4E9"))+
  facet_wrap(~Nutrient)+
    theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(angle=90),text=element_text(size=20),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  legend.key = element_blank(),
        panel.background = element_rect(fill="white", color="white"),
        strip.background = element_blank(),
        legend.title = element_text(size = legend_title_size-0.5), 
        legend.key.size = unit(legend_key_size-0.5, 'cm'),
        legend.text=element_text(size=legend_title_size-0.5))+
  labs(y= "Nutrient concentration\n (kg nutrient per tonne product")
Nutrient_concentrations_plot               

#Create maps showing N,P and K Inputs and Outputs in kg/ha by country.
#Create dataframes with year and element of interest----
df_N_kg_ha_in <- dplyr::filter(df,Year==2020, Element=="Cropland nitrogen per unit area",  Item=="Input")
df_P_kg_ha_in <- dplyr::filter(df,Year==2020, Element=="Cropland phosphorus per unit area", Item=="Input")
df_K_kg_ha_in <- dplyr::filter(df,Year==2020, Element=="Cropland potassium per unit area",Item=="Input")
df_N_kg_ha_out <- dplyr::filter(df,Year==2020, Element=="Cropland nitrogen per unit area",  Item=="Outputs")
df_P_kg_ha_out <- dplyr::filter(df,Year==2020, Element=="Cropland phosphorus per unit area", Item=="Outputs")
df_K_kg_ha_out <- dplyr::filter(df,Year==2020, Element=="Cropland potassium per unit area",Item=="Outputs")

#Create nutrient and category columns----
df_N_kg_ha_in$Nutrient <- "N"
df_P_kg_ha_in$Nutrient <- "P"
df_K_kg_ha_in$Nutrient <-  "K"
df_N_kg_ha_out$Nutrient <- "N"
df_P_kg_ha_out$Nutrient <- "P"
df_K_kg_ha_out$Nutrient <-  "K"

df_N_kg_ha_in$Range <- NA
df_P_kg_ha_in$Range <- NA
df_K_kg_ha_in$Range <- NA
df_N_kg_ha_out$Range <- NA
df_P_kg_ha_out$Range <- NA
df_K_kg_ha_out$Range <- NA

#Standardise colours for each category----
colour1 <- "#543005"
colour2 <- "red"
colour3="lightgreen"
colour4="darkgreen"
colour5= "yellow"
colour6="orange"
colour_no_data="grey"

#Get dataframe with map information
#nc <- sf::st_read("./data/UNmap_05/BNDA05_CTY.shp", quiet = TRUE)

#Merge map information with data
df_N_kg_ha_in <- merge(nc, df_N_kg_ha_in, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))
df_P_kg_ha_in <- merge(nc, df_P_kg_ha_in, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))
df_K_kg_ha_in <- merge(nc, df_K_kg_ha_in, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))
df_N_kg_ha_out <- merge(nc, df_N_kg_ha_out, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))
df_P_kg_ha_out <- merge(nc, df_P_kg_ha_out, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))
df_K_kg_ha_out <- merge(nc, df_K_kg_ha_out, all.x=TRUE, by.x=c("ISO3CD"), by.y=c("country_iso3"))

#Assign values to categories----
#N_kg_ha_in
Quantiles = signif(quantile(df_N_kg_ha_in$value, c(.20, .40, .60, .80), na.rm=T), 1)
df_N_kg_ha_in$Range = NA
df_N_kg_ha_in$Range[df_N_kg_ha_in$value<round(Quantiles[1],0)[[1]]&!is.na(df_N_kg_ha_in$value)] = paste0("<= ",as.character(round(Quantiles[1],0)[[1]]))
df_N_kg_ha_in$Range[df_N_kg_ha_in$value>=round(Quantiles[1],0)[[1]]&df_N_kg_ha_in$value<=round(Quantiles[2],0)[[1]]&!is.na(df_N_kg_ha_in$value)] = paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]]))
df_N_kg_ha_in$Range[df_N_kg_ha_in$value>=round(Quantiles[2],0)[[1]]&df_N_kg_ha_in$value<=round(Quantiles[3],0)[[1]]&!is.na(df_N_kg_ha_in$value)] = paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]]))
df_N_kg_ha_in$Range[df_N_kg_ha_in$value>=round(Quantiles[3],0)[[1]]&df_N_kg_ha_in$value<=round(Quantiles[4],0)[[1]]&!is.na(df_N_kg_ha_in$value)] = paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]]))
df_N_kg_ha_in$Range[df_N_kg_ha_in$value>=round(Quantiles[4],0)[[1]]] = paste0("> ",as.character(round(Quantiles[4],0)[[1]]))
df_N_kg_ha_in$Range[is.na(df_N_kg_ha_in$value)] = "No data"
df_N_kg_ha_in$Range = as.character(as.factor(df_N_kg_ha_in$Range))
df_N_kg_ha_in$Range = as.factor(df_N_kg_ha_in$Range)
df_N_kg_ha_in$Range <- factor(df_N_kg_ha_in$Range, levels = c(paste0("<= ",as.character(round(Quantiles[1],0)[[1]])), 
                                                        paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]])), 
                                                        paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]])),
                                                        paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]])),
                                                        paste0("> ",as.character(round(Quantiles[4],0)[[1]])),
                                                        "No data"))

#P_kg_ha_in
Quantiles = signif(quantile(df_P_kg_ha_in$value, c(.20, .40, .60, .80), na.rm=T), 1)
df_P_kg_ha_in$Range = NA
df_P_kg_ha_in$Range[df_P_kg_ha_in$value<round(Quantiles[1],0)[[1]]&!is.na(df_P_kg_ha_in$value)] = paste0("<= ",as.character(round(Quantiles[1],0)[[1]]))
df_P_kg_ha_in$Range[df_P_kg_ha_in$value>=round(Quantiles[1],0)[[1]]&df_P_kg_ha_in$value<=round(Quantiles[2],0)[[1]]&!is.na(df_P_kg_ha_in$value)] = paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]]))
df_P_kg_ha_in$Range[df_P_kg_ha_in$value>=round(Quantiles[2],0)[[1]]&df_P_kg_ha_in$value<=round(Quantiles[3],0)[[1]]&!is.na(df_P_kg_ha_in$value)] = paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]]))
df_P_kg_ha_in$Range[df_P_kg_ha_in$value>=round(Quantiles[3],0)[[1]]&df_P_kg_ha_in$value<=round(Quantiles[4],0)[[1]]&!is.na(df_P_kg_ha_in$value)] = paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]]))
df_P_kg_ha_in$Range[df_P_kg_ha_in$value>=round(Quantiles[4],0)[[1]]] = paste0("> ",as.character(round(Quantiles[4],0)[[1]]))
df_P_kg_ha_in$Range[is.na(df_P_kg_ha_in$value)] = "No data"
df_P_kg_ha_in$Range = as.character(as.factor(df_P_kg_ha_in$Range))
df_P_kg_ha_in$Range = as.factor(df_P_kg_ha_in$Range)
df_P_kg_ha_in$Range <- factor(df_P_kg_ha_in$Range, levels = c(paste0("<= ",as.character(round(Quantiles[1],0)[[1]])), 
                                                        paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]])), 
                                                        paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]])),
                                                        paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]])),
                                                        paste0("> ",as.character(round(Quantiles[4],0)[[1]])),
                                                        "No data"))

#K_kg_ha_in
Quantiles = signif(quantile(df_K_kg_ha_in$value, c(.20, .40, .60, .80), na.rm=T), 1)
df_K_kg_ha_in$Range = NA
df_K_kg_ha_in$Range[df_K_kg_ha_in$value<round(Quantiles[1],0)[[1]]&!is.na(df_K_kg_ha_in$value)] = paste0("<= ",as.character(round(Quantiles[1],0)[[1]]))
df_K_kg_ha_in$Range[df_K_kg_ha_in$value>=round(Quantiles[1],0)[[1]]&df_K_kg_ha_in$value<=round(Quantiles[2],0)[[1]]&!is.na(df_K_kg_ha_in$value)] = paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]]))
df_K_kg_ha_in$Range[df_K_kg_ha_in$value>=round(Quantiles[2],0)[[1]]&df_K_kg_ha_in$value<=round(Quantiles[3],0)[[1]]&!is.na(df_K_kg_ha_in$value)] = paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]]))
df_K_kg_ha_in$Range[df_K_kg_ha_in$value>=round(Quantiles[3],0)[[1]]&df_K_kg_ha_in$value<=round(Quantiles[4],0)[[1]]&!is.na(df_K_kg_ha_in$value)] = paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]]))
df_K_kg_ha_in$Range[df_K_kg_ha_in$value>=round(Quantiles[4],0)[[1]]] = paste0("> ",as.character(round(Quantiles[4],0)[[1]]))
df_K_kg_ha_in$Range[is.na(df_K_kg_ha_in$value)] = "No data"
df_K_kg_ha_in$Range = as.character(as.factor(df_K_kg_ha_in$Range))
df_K_kg_ha_in$Range = as.factor(df_K_kg_ha_in$Range)
df_K_kg_ha_in$Range <- factor(df_K_kg_ha_in$Range, levels = c(paste0("<= ",as.character(round(Quantiles[1],0)[[1]])), 
                                                        paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]])), 
                                                        paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]])),
                                                        paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]])),
                                                        paste0("> ",as.character(round(Quantiles[4],0)[[1]])),
                                                        "No data"))

#N_kg_ha_out
Quantiles = signif(quantile(df_N_kg_ha_out$value, c(.20, .40, .60, .80), na.rm=T), 1)
df_N_kg_ha_out$Range = NA
df_N_kg_ha_out$Range[df_N_kg_ha_out$value<round(Quantiles[1],0)[[1]]&!is.na(df_N_kg_ha_out$value)] = paste0("<= ",as.character(round(Quantiles[1],0)[[1]]))
df_N_kg_ha_out$Range[df_N_kg_ha_out$value>=round(Quantiles[1],0)[[1]]&df_N_kg_ha_out$value<=round(Quantiles[2],0)[[1]]&!is.na(df_N_kg_ha_out$value)] = paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]]))
df_N_kg_ha_out$Range[df_N_kg_ha_out$value>=round(Quantiles[2],0)[[1]]&df_N_kg_ha_out$value<=round(Quantiles[3],0)[[1]]&!is.na(df_N_kg_ha_out$value)] = paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]]))
df_N_kg_ha_out$Range[df_N_kg_ha_out$value>=round(Quantiles[3],0)[[1]]&df_N_kg_ha_out$value<=round(Quantiles[4],0)[[1]]&!is.na(df_N_kg_ha_out$value)] = paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]]))
df_N_kg_ha_out$Range[df_N_kg_ha_out$value>=round(Quantiles[4],0)[[1]]] = paste0("> ",as.character(round(Quantiles[4],0)[[1]]))
df_N_kg_ha_out$Range[is.na(df_N_kg_ha_out$value)] = "No data"
df_N_kg_ha_out$Range = as.character(as.factor(df_N_kg_ha_out$Range))
df_N_kg_ha_out$Range = as.factor(df_N_kg_ha_out$Range)
df_N_kg_ha_out$Range <- factor(df_N_kg_ha_out$Range, levels = c(paste0("<= ",as.character(round(Quantiles[1],0)[[1]])), 
                                                              paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]])), 
                                                              paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]])),
                                                              paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]])),
                                                              paste0("> ",as.character(round(Quantiles[4],0)[[1]])),
                                                              "No data"))

#P_kg_ha_out
Quantiles = signif(quantile(df_P_kg_ha_out$value, c(.20, .40, .60, .80), na.rm=T), 1)
df_P_kg_ha_out$Range = NA
df_P_kg_ha_out$Range[df_P_kg_ha_out$value<round(Quantiles[1],0)[[1]]&!is.na(df_P_kg_ha_out$value)] = paste0("<= ",as.character(round(Quantiles[1],0)[[1]]))
df_P_kg_ha_out$Range[df_P_kg_ha_out$value>=round(Quantiles[1],0)[[1]]&df_P_kg_ha_out$value<=round(Quantiles[2],0)[[1]]&!is.na(df_P_kg_ha_out$value)] = paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]]))
df_P_kg_ha_out$Range[df_P_kg_ha_out$value>=round(Quantiles[2],0)[[1]]&df_P_kg_ha_out$value<=round(Quantiles[3],0)[[1]]&!is.na(df_P_kg_ha_out$value)] = paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]]))
df_P_kg_ha_out$Range[df_P_kg_ha_out$value>=round(Quantiles[3],0)[[1]]&df_P_kg_ha_out$value<=round(Quantiles[4],0)[[1]]&!is.na(df_P_kg_ha_out$value)] = paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]]))
df_P_kg_ha_out$Range[df_P_kg_ha_out$value>=round(Quantiles[4],0)[[1]]] = paste0("> ",as.character(round(Quantiles[4],0)[[1]]))
df_P_kg_ha_out$Range[is.na(df_P_kg_ha_out$value)] = "No data"
df_P_kg_ha_out$Range = as.character(as.factor(df_P_kg_ha_out$Range))
df_P_kg_ha_out$Range = as.factor(df_P_kg_ha_out$Range)
df_P_kg_ha_out$Range <- factor(df_P_kg_ha_out$Range, levels = c(paste0("<= ",as.character(round(Quantiles[1],0)[[1]])), 
                                                              paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]])), 
                                                              paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]])),
                                                              paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]])),
                                                              paste0("> ",as.character(round(Quantiles[4],0)[[1]])),
                                                              "No data"))

#K_kg_ha_out
Quantiles = signif(quantile(df_K_kg_ha_out$value, c(.20, .40, .60, .80), na.rm=T), 1)
df_K_kg_ha_out$Range = NA
df_K_kg_ha_out$Range[df_K_kg_ha_out$value<round(Quantiles[1],0)[[1]]&!is.na(df_K_kg_ha_out$value)] = paste0("<= ",as.character(round(Quantiles[1],0)[[1]]))
df_K_kg_ha_out$Range[df_K_kg_ha_out$value>=round(Quantiles[1],0)[[1]]&df_K_kg_ha_out$value<=round(Quantiles[2],0)[[1]]&!is.na(df_K_kg_ha_out$value)] = paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]]))
df_K_kg_ha_out$Range[df_K_kg_ha_out$value>=round(Quantiles[2],0)[[1]]&df_K_kg_ha_out$value<=round(Quantiles[3],0)[[1]]&!is.na(df_K_kg_ha_out$value)] = paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]]))
df_K_kg_ha_out$Range[df_K_kg_ha_out$value>=round(Quantiles[3],0)[[1]]&df_K_kg_ha_out$value<=round(Quantiles[4],0)[[1]]&!is.na(df_K_kg_ha_out$value)] = paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]]))
df_K_kg_ha_out$Range[df_K_kg_ha_out$value>=round(Quantiles[4],0)[[1]]] = paste0("> ",as.character(round(Quantiles[4],0)[[1]]))
df_K_kg_ha_out$Range[is.na(df_K_kg_ha_out$value)] = "No data"
df_K_kg_ha_out$Range = as.character(as.factor(df_K_kg_ha_out$Range))
df_K_kg_ha_out$Range = as.factor(df_K_kg_ha_out$Range)
df_K_kg_ha_out$Range <- factor(df_K_kg_ha_out$Range, levels = c(paste0("<= ",as.character(round(Quantiles[1],0)[[1]])), 
                                                                    paste0(as.character(round(Quantiles[1],0)[[1]]), " <= ", as.character(round(Quantiles[2],0)[[1]])), 
                                                                    paste0(as.character(round(Quantiles[2],0)[[1]]), " <= ", as.character(round(Quantiles[3],0)[[1]])),
                                                                    paste0(as.character(round(Quantiles[3],0)[[1]]), " <= ", as.character(round(Quantiles[4],0)[[1]])),
                                                                    paste0("> ",as.character(round(Quantiles[4],0)[[1]])),
                                                                    "No data"))

#Plot results in map----
map_theme <-   theme_classic()

#N kg ha inputs
N_kg_ha_in_plot <- ggplot(df_N_kg_ha_in , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c(colour3,
                             colour4,
                             colour5,
                             colour6,
                             colour2,
                             colour_no_data)) +
  labs(tag="(a) N")+
  map_theme

#P kg ha inputs
P_kg_ha_in_plot <- ggplot(df_P_kg_ha_in , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c(colour3,
                             colour4,
                             colour5,
                             colour6,
                             colour2,
                             colour_no_data)) +
  labs(tag="(b) P")+
  map_theme

#K kg ha inputs
K_kg_ha_in_plot <- ggplot(df_K_kg_ha_in , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c(colour3,
                             colour4,
                             colour5,
                             colour6,
                             colour2,
                             colour_no_data)) +
  labs(tag="(c) K")+
  map_theme

#N kg ha outputs
N_kg_ha_out_plot <- ggplot(df_N_kg_ha_out , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c(colour3,
                             colour4,
                             colour5,
                             colour6,
                             colour2,
                             colour_no_data)) +
  labs(tag="(a) N")+
  map_theme

#P kg ha outputs
P_kg_ha_out_plot <- ggplot(df_P_kg_ha_out , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c(colour3,
                             colour4,
                             colour5,
                             colour6,
                             colour2,
                             colour_no_data)) +
  labs(tag="(b) P")+
  map_theme

#K kg ha inputs
K_kg_ha_out_plot <- ggplot(df_K_kg_ha_out , aes(fill=Range))+
  geom_sf(size=.1) +
  scale_fill_manual(values=c(colour3,
                             colour4,
                             colour5,
                             colour6,
                             colour2,
                             colour_no_data)) +
  labs(tag="(c) K")+
  map_theme

#Join plots into one----
Nu_kg_ha_in_Plot <-   ggarrange(N_kg_ha_in_plot,P_kg_ha_in_plot,K_kg_ha_in_plot, ncol=1, 
                             font.label = list(size = 30))

Nu_kg_ha_out_Plot <-   ggarrange(N_kg_ha_out_plot,P_kg_ha_out_plot,K_kg_ha_out_plot, ncol=1, 
                                font.label = list(size = 30))

#Save maps into one file----
ggsave("./results/plot_world_N_P_K_kg_ha_inputs.jpg", Nu_kg_ha_in_Plot,
       width = 20, height = 20, units = "cm")

ggsave("./results/plot_world_N_P_K_kg_ha_outputs.jpg", Nu_kg_ha_out_Plot,
       width = 20, height = 20, units = "cm")