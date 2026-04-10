#Data Cleaning####
##Load Data###
setwd("~/Desktop/Fifth Year/Community and Ecosystems /R data")
library(readr)
b.moss.identification<-read_csv("b.moss.identification.csv")
b.moss.disturbance<-read_csv("b.moss.disturbance.csv")
borealsubregion<-read_csv("borealsubregion_latlong.csv")
#Load Packages###
install.packages("dplyr")
install.packages("rlang")

library(dplyr)
library(rlang)


###Combine Boreal data 

b.moss.identification$site_year<-
  paste(b.moss.identification$`ABMI Site`,b.moss.identification$Year, sep="_")

b.moss.identification<-b.moss.identification %>%
  rename(site=`ABMI Site`,common_name=`Common Name`, sci_name=`Scientific Name`)%>%
  select(site_year,Rotation,site,Quadrant,Year,common_name,sci_name)

b.moss.disturbance$site_year<-
  paste(b.moss.disturbance$`ABMI Site`,b.moss.disturbance$Year,sep="_")

b.moss.disturbance<-b.moss.disturbance %>%
  rename(site=`ABMI Site`,disturbance=`Human Disturbance Type`)%>%
  select(site_year,Rotation,site,Quadrant,Year,disturbance)


borealsubregion$NSRNAME[1] <- "Northern Mixedwood"
borealsubregion$NSRCODE[1]<-"NM"

borealsubregion<-borealsubregion%>%
  rename(site=ABMI.Site)%>%
  select(site,NSRNAME,NSRCODE)

borealsubregion<-borealsubregion%>%
  mutate(site = as.character(site))





# join the two boreal data sets 
boreal.data<-b.moss.identification%>%
  left_join(b.moss.disturbance, by=c("site_year","Rotation","site","Year","Quadrant"))

#turn none into NA
boreal.data<-boreal.data%>%
  mutate(disturbance=na_if(disturbance,"NONE"))

#combining and standardizing distrubances within sites 
boreal.groupingdata<-boreal.data%>%
  group_by(site,Rotation)%>%
  summarise(disturbance_site=if(all(is.na(disturbance)))
    NA_character_ else paste(sort(unique(na.omit(disturbance))), 
                             collapse="+"),.groups="drop")
# join  two boreal data sets 
boreal.data2<-boreal.data%>%
  select(-disturbance,-Quadrant)%>%
  left_join(boreal.groupingdata,by=c("Rotation","site"))


#remove site not with 2 rotations 
species_to_remove <- c("NONE", "PNA", "VNA", "DNC", "SNI","SNR")

boreal.data2 <- boreal.data2 %>% 
  filter(!sci_name %in% species_to_remove)


site.both.rotations<-boreal.data2%>%
  distinct(site,Rotation)%>%
  group_by(site)%>%
  summarise(n_rot=n_distinct(Rotation),.groups="drop")%>%
  filter(n_rot==2)

boreal.data2<-boreal.data2%>%
  filter(site %in% site.both.rotations$site)

#remove duplicate species from same site 

#add region row and B=boreal
boreal.data2<-boreal.data2%>%
  mutate(region=c("B"))

boreal.data3<-boreal.data2%>%
  group_by(site_year)%>%
  distinct(sci_name, .keep_all=TRUE)%>%
  ungroup()

##Boreal and locations data set 


Boreal.fulldata<-boreal.data3%>%
  left_join(borealsubregion,
            by=c("site"))



##remove not use able data 


####export to csv
##write_csv(boreal.data3, "rough_borealdata.csv")
write_csv(Boreal.fulldata,"Boreal.fulldata.csv")







####export to csv
write_csv(full.dataset, "fulldataset2.csv")



#Load Packages####
install.packages("dplyr")
install.packages("rlang")
install.packages("vegan")
install.packages("ggplot2")
install.packages("goeveg")

library(dplyr)
library(rlang)
library(vegan)
library(readr)
library(tidyverse)
library(ggplot2)
library(goeveg) 


##Load Data####
setwd("~/Desktop/Fifth Year/Community and Ecosystems /R data")

##full.dataset<-read_csv("fulldataset2.csv")
Boreal.fulldata<-read_csv("Boreal.fulldata.csv")
Boreal.fulldata<-Boreal.fulldata%>%
  mutate(disturbance_site = na_if(disturbance_site, "NONE"))


head(Boreal.fulldata)
#### Question 1####
#1.	Does disturbance type affect bryophyte community 
#composition in the Alberta Boreal subregions natural regions 
#more than others vs no disturbance?
### Restructure data Matrix####

PAmatrix.data<- Boreal.fulldata%>%
  mutate(presence=1)%>%
  pivot_wider(id_cols=site_year,
              names_from=sci_name,
              values_from = presence,
              values_fill = 0,
              values_fn = max)%>%
  column_to_rownames("site_year")

## Alpha of each site##
# ---- 2) Alpha diversity per site ----
site_N <- rowSums(PAmatrix.data)
S_obs  <- specnumber(PAmatrix.data)               # observed richness


alpha_df <- data.frame(
  site = rownames(PAmatrix.data),
  total_individuals = site_N,
  richness_S = S_obs,
  stringsAsFactors = FALSE)

barplot(alpha_df$richness_S, names.arg = alpha_df$site, las = 2,
        main = "Richness (S) per site", ylab = "S", cex.names = 0.7)

site_count<-Boreal.fulldata%>%
  group_by(NSRCODE)%>%
  summarise(n_distinct((site)))


###alpha for each region#####
result <- Boreal.fulldata %>%
  group_by(NSRCODE, Rotation) %>%
  summarise(
    richness = n_distinct(sci_name),
    .groups = "drop")
    
    result_wide <- result %>%
      pivot_wider(names_from = Rotation, values_from = richness) %>%
      rename(Rot1 = `Rotation 1`, Rot2 = `Rotation 2`)
 
    
##differences <- result_wide$Rot2 - result_wide$Rot1
  

    
    site_richness <- Boreal.fulldata %>%
      group_by(NSRCODE, site, Rotation) %>%
      summarise(richness = n_distinct(sci_name), .groups = "drop")
    site_wide <- site_richness %>%
      pivot_wider(names_from = Rotation, values_from = richness) %>%
      rename(Rot1 = `Rotation 1`, Rot2 = `Rotation 2`) %>%
      filter(!is.na(Rot1) & !is.na(Rot2))
    
    results_by_nsr <- site_wide %>%
      group_by(NSRCODE) %>%
      summarise(
        n_sites    = n(),
        median_R1  = median(Rot1),
        median_R2  = median(Rot2),
        p_value    = wilcox.test(Rot1, Rot2, paired = TRUE)$p.value,
        .groups    = "drop"
      ) %>%
      mutate(p_adj = p.adjust(p_value, method = "bonferroni"),  # correct for multiple NSRCODEs
             significant = p_adj < 0.05)

disturbance_table <- Boreal.fulldata %>%
  filter(!is.na(disturbance_site)) %>%        # keep only disturbed sites
  group_by(NSRCODE, Rotation) %>%
  summarise(
    disturbed_sites = n_distinct(site),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = Rotation,
    values_from = disturbed_sites,
    values_fill = 0                            # fill missing with 0
  )


##PCoA (Jaccard)####
## calculate dissimilarity

jacc <- vegdist(PAmatrix.data, method = "jaccard", binary = TRUE)
pcoa_res <- cmdscale(jacc, k = 2, eig = TRUE)

pcoa_jacc <- cmdscale(jacc, k = 2, eig = TRUE)
jacc_scores <- as.data.frame(pcoa_jacc$points)
colnames(jacc_scores) <- c("PCoA1", "PCoA2")
jacc_scores$site_year <- rownames(jacc_scores)

## join with metadata######
jscore.fulldata<- Boreal.fulldata%>%
  select(site_year,Rotation,site,Year,disturbance_site,region,NSRCODE)%>%
  left_join(jacc_scores,full.dataset, by="site_year")%>%
  distinct()
jscore.fulldata<-jscore.fulldata%>%
  filter(!is.na(NSRCODE))%>%
  mutate(disturbed = ifelse(is.na(disturbance_site), "Undisturbed", "Disturbed"))

group<-factor(jscore.fulldata$disturbed)
levels(group)
table(group)
sum(duplicated(jscore.fulldata$site_year))
all(rownames(PAmatrix.data)%in% jscore.fulldata$site_year)
PAmatrix.data<-PAmatrix.data[rownames(PAmatrix.data)%in% jscore.fulldata$site_year,]
nrow(as.matrix(PAmatrix.data)) == length(group) 

##stacked bar graph ####

head(richness_df)
# ---- 1) Get richness per site_year 
richness_df <- data.frame(
  site_year = rownames(PAmatrix.data),
  richness  = rowSums(PAmatrix.data > 0)
) %>%
  separate(site_year, into = c("site", "Year"), sep = "_", remove = FALSE) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(site) %>%
  filter(n() >= 2) %>%          # only sites with both years
  arrange(Year) %>%
  mutate(time_period = case_when(
    Year == first(Year) ~ "Year 1",
    Year == last(Year)  ~ "Year 2"
  )) %>%
  filter(!is.na(time_period)) %>%   # drop middle years if any
  ungroup()

richness_df <- richness_df %>%
  left_join(
    jscore.fulldata %>% distinct(site_year, NSRCODE),
    by = "site_year"
  )

richness_mean<-richness_df%>%
  group_by(NSRCODE, time_period) %>%
  summarise(
    total_richness = sum(richness),
    mean_richness  = mean(richness),
    .groups = "drop"
  )

####good copy##
head(result)

ggplot(result, aes(x = NSRCODE, y = richness, fill = Rotation )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Rotation 2" = "steelblue", "Rotation 1" = "tomato")) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125,150,175,200,225,250))+
  labs(
       x = "Natural Subregion",
       y = "Species Richness (S)",
       fill = "Rotation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

ggplot(richness_df, aes(x = site, y = richness, fill = time_period)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NSRCODE, scales = "free_x")+
  scale_fill_manual(values = c("Year 2" = "steelblue", "Year 1" = "tomato")) +
  theme_bw() +
  labs(title = "Species Richness per Site by Survey Year",
       x = "Site",
       y = "Species Richness (S)",
       fill = "Time Period") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))


##plotting########

hulls<-jscore.fulldata%>%
  group_by(disturbed)%>%
  slice(chull(PCoA1,PCoA2))

eig_vals <- pcoa_res$eig
pct <- round(eig_vals / sum(eig_vals[eig_vals > 0]) * 100, 1)

  ggplot(jscore.fulldata,aes(x = PCoA1, y = PCoA2,colour = disturbed, shape = NSRCODE)) +
    geom_point(size = 3) +
    geom_polygon(data=hulls, aes(group=disturbed),alpha=0.15, linetype = "dashed")+
    scale_shape_manual(values = c("NM"=0,"CM"=1, "LBH"=2, "UB"=3, "DMW"=4, "PAD"=5, "AP"=9))+
    scale_color_manual(values = c("Disturbed" = "red", "Undisturbed" = "grey50")) +
    theme_bw()
  
  
  ggplot(jscore.fulldata,aes(x = PCoA1, y = PCoA2,colour = disturbed)) +
    geom_point(size = 3, alpha=0.7) +
    stat_ellipse(type = "t", level = 0.95)+
    stat_ellipse(aes(fill = disturbed),
                 geom = "polygon",
                 alpha = 0.2,
                 colour = NA)+
    scale_color_manual(values = c("Disturbed" = "red", "Undisturbed" = "grey50")) +
    scale_fill_manual(values = c("red", "grey"))+
    theme_bw()+
    labs(colour = "Disturbance",
      fill = "Disturbance", 
      x = paste0("PCoA1 (", pct[1], "%)"),
      y = paste0("PCoA2 (", pct[2], "%)"))
  
  ggplot(jscore.fulldata,aes(x = PCoA1, y = PCoA2,colour = Rotation))+
    geom_point(size = 3, alpha=0.7)+
    stat_ellipse(aes(fill = Rotation),
                 geom = "polygon",
                 alpha = 0.2,
                 colour = NA)+
    scale_color_manual(values = c("Rotation 1" = "red", "Rotation 2"= "darkgreen"))+
    scale_fill_manual(values = c("red", "darkgreen"))+
    theme_bw()+
    labs(colour = "Rotation",
         fill = "Rotation", 
         x = paste0("PCoA1 (", pct[1], "%)"),
         y = paste0("PCoA2 (", pct[2], "%)"))
  
  
##Permnova####
levels(group)
  
  if (exists("group")){
    cat("Group levels:", levels(group), "\n\n")
    
    cat("\nPERMANOVA (Jaccard):\n")
    (adonis2(jacc ~ group, permutations = 999))}
  
##Question 2####
  #How does bryophyte community ###
  #composition change over time in the Alberta borealsubregions
  #(two rounds, 5 years apart)###

#NMDS with Jaccard distances####

  # ---- 1) Read data ----
  nmdsdata<- read.csv("Boreal.fulldata.csv", check.names = FALSE)
  
  PAmatrix.data2<-nmdsdata%>%
    mutate(presence=1)%>%
    pivot_wider(id_cols=site_year,
                names_from=sci_name,
                values_from = presence,
                values_fill = 0,
                values_fn = max)%>%
    column_to_rownames("site_year")
PAmatrix.data2<-PAmatrix.data2[rownames(PAmatrix.data2)%in% jscore.fulldata$site_year,]
all(rownames(PAmatrix.data2)%in% jscore.fulldata$site_year)
    
##just cm 
  
  # Metadata columns (edit here if your file differs)
  meta_cols <- c("site_year","site","Year","Rotation", "NSRCODE","disturbed")
  meta <- jscore.fulldata[, meta_cols]
  meta<-meta%>%
    distinct(site_year,.keep_all = TRUE)
  
  
  
  comm <- Boreal.fulldata[, setdiff(names(Boreal.fulldata), meta_cols)]
  
  
  
  
  

  
  # ---- 2) Choose distance and run NMDS ----
  # Jaccard for presence/absence
  set.seed(123)
  nmds <- metaMDS(PAmatrix.data2, distance = "jaccard", k = 2, trymax = 100, autotransform = FALSE)
  plot(nmds)
  
  cat("NMDS stress:", nmds$stress, "\n")
  
  dimcheck_out <- 
    dimcheckMDS(PAmatrix.data2,
                distance = "jaccard",
                k = 6)
  print(dimcheck_out)
  
  # ---- 3) Extract scores ----
  site_scores <- as.data.frame(scores(nmds, display = "sites"))
  site_scores$site_year <- meta$site_year
  site_scores <- merge(site_scores, meta, by = "site_year")
  
  #convex hulls per NSRCODE
  hulls<-site_scores%>%
    group_by(NSRCODE)%>%
    slice(chull(NMDS1,NMDS2))
  
 
  
  
 ### write.csv(site_scores, "nmds_scores_sites.csv", row.names = FALSE)
  
  site_scores %>% arrange(desc(NMDS1)) %>% head(5)
  site_scores %>% filter(NMDS1 > 5)
  rowSums(PAmatrix.data2[outlier_site, ])
  # ---- 5) Plot NMDS (ggplot2) ----
  ggplot(site_scores, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour= NSRCODE), size = 3, alpha = 0.9) +
    geom_polygon(data=hulls,
                 aes(group=NSRCODE,
                     fill=NSRCODE),alpha=0.15,
                 linetype = "dashed",
                 color="black")+
    theme_bw() +
    labs(
         colour= "Boreal Subregion",
         fill = "Boreal Subregion",
         subtitle = paste0("Stress = ", round(nmds$stress, 3)),
         x = "NMDS1", y = "NMDS2")
  
  
  ggplot(site_scores, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour= NSRCODE), size = 3, alpha = 0.9) +
    stat_ellipse(aes(group = NSRCODE, fill = NSRCODE),
                 geom = "polygon",
                 alpha = 0.12,
                 colour = NA) +
    scale_colour_manual(values = c(
      "AP"  = "red2",
      "CM"  = "darkorange",
      "DMW" = "green",
      "LBH" = "deepskyblue",
      "NM"  = "purple",
      "PAD" = "black",
      "UB"  = "deeppink"
    )) +
    scale_fill_manual(values = c(
      "AP"  = "red",
      "CM"  = "orange",
      "DMW" = "darkgreen",
      "LBH" = "deepskyblue",
      "NM"  = "purple",
      "PAD" = "black",
      "UB"  = "green"
    ))+
    theme_bw() +
    labs(title = "NMDS (Jaccard) — community composition",
         subtitle = paste0("Stress = ", round(nmds$stress, 3)),
         x = "NMDS1", y = "NMDS2")

###split into year 1 and two 
  hulls2<-site_scores%>%
    group_by(NSRCODE,Rotation)%>%
    slice(chull(NMDS1,NMDS2))
  
  site_scores2<-site_scores%>%
    filter(!NSRCODE=="PAD")
  
  ggplot(site_scores2, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour= NSRCODE), size = 3, alpha = 0.9) +
    facet_wrap("Rotation")+
    stat_ellipse(aes(colour = NSRCODE),               # ellipse colour matches points
                 type = "t", level = 0.95)+
    stat_ellipse(aes(group = NSRCODE, fill = NSRCODE),
                 geom = "polygon",
                 alpha = 0.12,
                 colour = NA)+
    theme_bw() +
    labs(fill    = "Boreal Subregion",
         colour="Boreal Subregion",
         subtitle = paste0("Stress = ", round(nmds$stress, 3)),
         x = "NMDS1", y = "NMDS2", )
  
  site_scores %>% count(NSRCODE, Rotation)
  
#### split scores into year 1 and year 2 
  
arrows<-site_scores%>%
  separate(site_year, into =c("site","Year"),sep="_", remove=FALSE)%>%
  mutate(Year=as.numeric(Year))%>%
  group_by(site)%>%
  arrange(Year) %>%
  summarise(
    NMDS1_start = first(NMDS1),
    NMDS2_start = first(NMDS2),
    NMDS1_end   = last(NMDS1),
    NMDS2_end   = last(NMDS2),
    Year_start  = first(Year),
    Year_end    = last(Year),
    NSRCODE     = first(NSRCODE),
    disturbed = first(disturbed) )%>%
  ungroup()
  
head(arrows)


ggplot(site_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour= NSRCODE), size = 3, alpha = 0.9)+
  geom_segment(data = arrows,
               aes(x=NMDS1_start,y=NMDS2_start,
                   xend=NMDS1_end,yend=NMDS2_end,
                   colour=NSRCODE),
               arrow= arrow(length=unit(0.3,"cm"),type="closed"),
                            linewidth=0.7,alpha=0.8)+
  theme_bw()+
  labs(title = "NMDS (Jaccard) — community composition",
       subtitle = paste0("Stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2")

##mean direction of arrows ??
arrows %>%
  mutate(
    delta_NMDS1 = NMDS1_end - NMDS1_start,
    delta_NMDS2 = NMDS2_end - NMDS2_start
  ) %>%
  group_by(NSRCODE) %>%
  summarise(
    mean_shift_NMDS1 = mean(delta_NMDS1),
    mean_shift_NMDS2 = mean(delta_NMDS2),
    n_sites = n()
  )
## add polygons#####

year1_scores <- site_scores %>%
  separate(site_year, into = c("site", "Year"), sep = "_", remove = FALSE) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(site) %>%
  filter(n() >= 2) %>%
  arrange(Year) %>%
  slice_head(n = 1) %>%        # keep only first year row
  ungroup()

year2_scores <- site_scores %>%
  separate(site_year, into = c("site", "Year"), sep = "_", remove = FALSE) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(site) %>%
  filter(n() >= 2) %>%
  arrange(Year) %>%
  slice_tail(n = 1) %>%        # keep only first year row
  ungroup()

hulls_year1 <- year1_scores %>%
  slice(chull(NMDS1, NMDS2))

hulls_year2 <- year2_scores %>%
  slice(chull(NMDS1, NMDS2))


ggplot(site_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour= NSRCODE), size = 3, alpha = 0.9)+
  geom_polygon(data = hulls_year1,
               aes(x = NMDS1, y = NMDS2),
               fill = "blue", alpha = 0.15, 
               linetype = "solid", color = "blue") +
  geom_polygon(data = hulls_year2,
               aes(x = NMDS1, y = NMDS2),
               fill = "red", alpha = 0.15, 
               linetype = "dashed", color = "red") +
  geom_segment(data = arrows,
               aes(x=NMDS1_start,y=NMDS2_start,
                   xend=NMDS1_end,yend=NMDS2_end,
                   colour=NSRCODE),
               arrow= arrow(length=unit(0.3,"cm"),type="closed"),
               linewidth=0.7,alpha=0.8)+
  theme_bw() +
  labs(title = "NMDS (Jaccard) — community composition",
       subtitle = paste0("Stress = ", round(nmds$stress, 3)),
       x = "NMDS1", y = "NMDS2")


####boxplot???
# ---- 1) Calculate dissimilarity between year1 and year2 per site ----
dissim_df <- year1_scores %>%
  inner_join(year2_scores, by = "site", suffix = c("_y1", "_y2"))%>%
  rowwise() %>%
  mutate( temporal_dissim = vegdist(rbind(
    PAmatrix.data2[paste0(site, "_", Year_y1), ],
    PAmatrix.data2[paste0(site, "_", Year_y2), ]),
    method = "jaccard", binary = TRUE)[1]) %>%
  ungroup() %>%
  select(site, temporal_dissim, 
         disturbance_site = disturbed_y1,
         NSRCODE = NSRCODE_y1,
         Year1 = Year_y1,
         Year2 = Year_y2)

# Check it looks right
head(dissim_df)


ggplot(dissim_df, aes(x = disturbance_site, 
                      y = temporal_dissim, 
                      fill = disturbance_site)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.5) +  # show individual sites
  facet_wrap(~ NSRCODE) +
  theme_bw() +
  labs(title = "Temporal dissimilarity between first and last survey year",
       subtitle = "Jaccard dissimilarity between Year 1 and Year 2 per site",
       x = "Disturbance",
       y = "Jaccard Dissimilarity",
       fill = "Disturbance") +
  theme(strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"))



ggplot(dissim_df, aes(x = disturbance_site, 
                      y = temporal_dissim, 
                      fill = disturbance_site)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.5) +  # show individual sites+
  theme_bw() +
  labs(title = "Temporal dissimilarity between first and last survey year",
       subtitle = "Jaccard dissimilarity between Year 1 and Year 2 per site",
       x = "Disturbance",
       y = "Jaccard Dissimilarity",
       fill = "Disturbance") +
  theme(strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"))
####linear model
lm_mod <- lm(temporal_dissim ~ disturbance_site * NSRCODE, data = dissim_df)
summary(lm_mod)
anova(lm_mod)

# Permutation test###
install.packages("lmPerm")
library(lmPerm)
perm_mod <- aovp(temporal_dissim ~ disturbance_site * NSRCODE, data = dissim_df)
summary(perm_mod)

head(temporal_dissim)
## variqtion in types of disturbance 

head(Boreal.fulldata)

boreal.disturbance<-Boreal.fulldata%>%
  group_by(Rotation, site)%>%
  distinct(disturbance_site, .keep_all = TRUE)

boreal.disturbance1<-boreal.disturbance%>%
  group_by(NSRCODE, disturbance_site)%>%
  summarise(count=n(),.groups="drop")%>%
  filter(!disturbance_site=="NA")



  
