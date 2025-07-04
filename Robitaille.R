# set working directory
setwd("/Users/mathi/Documents/Maitrise/MathildeRobitaille/Article/Data and script")

#### 0) DATA PREP ####

library(tidyverse) 
library(corrplot) 
library(fastDummies) 
library(vegan) 

### 0.1) HP data ###

photos_csv <- read.csv(file = "Photos.csv", 
                   header = TRUE,
                   sep = ",",
                   stringsAsFactors = TRUE) 

photos <- photos_csv %>%
  mutate(Bloc_nbr = case_when(Bloc == "A" ~ 1, 
                              Bloc == "B" ~ 2, 
                              Bloc == "C" ~ 3)) %>% 
  mutate(PlacetteBloc =
           100 * Placette + Bloc_nbr) %>%   
  dplyr::select(- Placette,
                - Bloc,
                - Bloc_nbr,
                - Micro.plac., 
                - Date_.AA.MM.JJ.) %>% 
  dplyr::select(PlacetteBloc, Trait.:PAI) %>% 
  mutate(PlacetteBloc = factor(PlacetteBloc)) %>% 
  mutate(Trait. = fct_relevel(Trait., "Aucun", "3_ans", "2_ans", "Standard", "Intensif"))

### 0.2) vegetation data ###

Veg_csv <- read.csv(file = "Veg.csv", 
                    header = TRUE,
                    sep = ",",
                    stringsAsFactors = TRUE)

Veg_csv[is.na(Veg_csv)] <- 0

Veg <- Veg_csv %>%
  dplyr::select(- Cladonia_sp., - Cladonia_coniocraea, - Cladonia_rangiferina, 
                - Peltigera_sp.,
                - Bazzania_trilobata, - Dicranum_sp., - Hylocomium_splendens, 
                - Pleuzorium_schreberi, - Polytrichum_sp., - Ptilium_crista.castrensis,
                - Rhytidiadelphus_triquetrus, - Sphagnum_sp.) %>% 
  group_by(Placette, Bloc, Trait.) %>% 
  summarise_at(vars(Abies_balsamea:Viola_sp.), 
               mean) %>%
  mutate(Placette = factor(Placette)) %>% 
  mutate(Trait. = fct_relevel(Trait., "Aucun", "3_ans", "2_ans", "Standard", "Intensif"))

### 0.3) shade tolerance

tolerance <- read.csv(file = "Tolerance.csv", 
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = TRUE)
head(tolerance)
str(tolerance)
tolerance$Tolerance <- as.factor(tolerance$Tolerance)

tol <- tolerance %>% 
  filter(!row_number() %in% c(9, 14, 15, 16, 24, 37, 50, 54, 55, 59, 61, 69))

### 0.4) fruits ###

fruits_csv <- read.csv(file = "Fruits.csv", 
                       header = TRUE,
                       sep = ",",
                       stringsAsFactors = TRUE)

fruits_csv[is.na(fruits_csv)] <- 0

fruits <- fruits_csv %>%
  dplyr::select(- Vaccinium_angustifolium, - Vaccinium_myrtilliodes) %>%  
  group_by(Placette, Bloc, Trait.) %>% 
  summarise_at(vars(Amelanchier_spp.:Rubus_idaeus), 
               sum) %>% 
  mutate(Placette = factor(Placette)) %>% 
  mutate(Trait. = fct_relevel(Trait., "Aucun", "3_ans", "2_ans", "Standard", "Intensif"))

### 0.5) arboreal lichens ###

lichens_csv <- read.csv(file = "Lichens.csv", 
                        header = TRUE,
                        sep = ",",
                        stringsAsFactors = TRUE)

lichens_csv[is.na(lichens_csv)] <- 0

DHP_csv <- read.csv(file = "DHP_2022_parcelles_400m.csv", 
                    header = TRUE,
                    sep = ",",
                    stringsAsFactors = TRUE) 
DHP <- DHP_csv %>% 
  rename(DHP = Diametre_.mm._2022_ou_2021) %>% 
  mutate(DHP = na_if(DHP, "")) %>%
  drop_na(Placette)
DHP$DHP <- as.numeric(as.character(DHP$DHP))

lichens_DHP <- merge(x = lichens_csv, y = DHP)
head(lichens_DHP)

lichens <- lichens_DHP %>%
  dplyr::select(- Date_.AA.MM.JJ., - Arbre, - Localisation, - Secteur) %>% 
  mutate(Placette = factor(Placette)) %>% 
  mutate(Trait. = fct_relevel(Trait., "Aucun", "3_ans", "2_ans", "Standard", "Intensif")) %>% 
  mutate(Biomasse = Evernia_sp. + Usnea_sp. + Bryoria_sp.) 
lichens$DHP <- as.numeric(as.character(lichens$DHP))

lichens <- lichens %>% 
  filter(Biomasse < 20) # retirer ces deux variables abérantes

#### 0.6) environmental variables ####

## 0.6.1) SIG ##
env_csv <- read.csv(file = "DonneesSpatiales.csv", 
                    header = TRUE,
                    sep = ",",
                    stringsAsFactors = TRUE)

env <- env_csv %>% 
  dplyr::select(- EpiType, - EpiDate, 
                - FeuDate, - FeuDist,
                - CoupeType, - CoupeDate, - CoupeDist, 
                - RouteDens, - RouteType, - RouteType, - RouteDist,  
                - Latitude, - Longitude) %>%
  mutate(Placette = factor(Placette)) %>% 
  mutate(Trait. = fct_relevel(Trait., "Aucun", "3_ans", "2_ans", "Standard", "Intensif")) %>% 
  mutate(Depot = dplyr::recode(Depot, 
                               "1AY" = "1A", "1AM" = "1A",
                               "2BD" = "2B", "2BE" = "2B")) 
sig <- env %>% 
  dummy_columns( 
    select_columns = c("Trait.", "Depot")) %>% 
  dplyr::select(- Placette, - Bloc, 
                - Trait., - Depot) %>%  
  cor.mtest(conf.level = 0.95)

env %>% 
  dummy_columns( 
    select_columns = c("Trait.", "Depot")) %>% 
  dplyr::select(- Placette, - Bloc, 
                - Trait., - Depot) %>%  
  cor() %>% 
  corrplot( 
    p.mat = sig$p, 
    sig.level = 0.10) 

## 0.6.2) bioclimatic ##

meteo_csv <- read.csv(file = "DonneesMeteo.csv", 
                      header = TRUE,
                      sep = ",")

meteo <- meteo_csv %>% 
  group_by(Placette, Bloc, Trait.) %>% 
  mutate(Placette = factor(Placette)) %>%
  summarise(
    TMin = min(TMin), 
    MeanTMin = mean(MeanTMin), 
    MeanT = mean(MeanT), 
    MeanTMax = mean(MeanTMax), 
    TMax = max(TMax), 
    MeanPrecipitation = mean(MeanPrecipitation), 
    TotalPrecipitation = mean(TotalPrecipitation), 
    RH = mean(RH), 
    NbPrcp = mean(NbPrcp), 
    MaxConNoPrcp = max(MaxConNoPrcp), 
    DD = mean(DD)) %>% 
  data.frame() 

## select fewer variables with a PCoA ##

siteID <- meteo %>% 
  unite(ID, c(Placette, Bloc), sep = "") %>% 
  pull(ID)
head(siteID)

meteo_mat <- meteo %>% 
  ungroup() %>% 
  dplyr::select(- Placette, - Bloc, - Trait.) %>% 
  as.matrix()

rownames(meteo_mat) <- siteID
meteo_mat 

meteo.pca <- meteo_mat %>% 
  scale(center = TRUE, scale = TRUE) %>% 
  rda() 
pca.summ <- summary(meteo.pca, scaling = 2) 
pca.summ 

## plot pca with ggplot ##

pca.sites <- pca.summ$sites[, 1:2] %>% 
  as_tibble() %>% 
  mutate(site = rownames(pca.summ$sites[, 1:2])) %>% 
  cbind(meteo$Trait.) %>% 
  rename(Trait. = "meteo$Trait.") %>% 
  as_tibble() 
pca.sites

pca.var <- pca.summ$species[, 1:2] %>% 
  as_tibble() %>% 
  mutate(var = rownames(pca.summ$species[, 1:2]))
pca.var

graph.sites <- ggplot(pca.sites) + 
  geom_text(aes(x = PC1,
                y = PC2,
                label = site, 
                colour = Trait.), cex = 6) + 
  xlab('PCA 1 (44.7%)') + 
  ylab('PCA 2 (26.6%)') +
  geom_hline(yintercept = 0, linetype = 'dotted') + 
  geom_vline(xintercept = 0, linetype = 'dotted') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14, color = "black"), 
        axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1.5), 
        axis.ticks = element_line(size = 0.5), 
        legend.text = element_text(size = 16),
        legend.position = "top",
        plot.margin = unit(c(0, 0.2, 0, 0.2), 
                           "inches")) + 
  scale_color_manual(values = c("#F5B041", "#ff5733", "#CF416A", "#900c3f", "#581845"), 
                     labels = c("Control", "Light", "Intermediate" , "Standard", "Intensive"),
                     name = NULL)
graph.sites 

# add environmental variables

graph.meteo <- graph.sites + 
  geom_text(data = pca.var, aes(x = PC1,
                                y = PC2,
                                label = var), cex = 5.5) + 
  geom_segment(data = pca.var, aes(x = 0, y = 0, 
                                   xend = PC1, 
                                   yend = PC2), cex = 0.8)
graph.meteo
ggsave(graph.meteo,
       file = 'env.png', 
       width = 12.5, 
       height = 9, 
       dpi = 400) 

enviro <- env %>% 
  bind_cols(meteo$MeanT, meteo$TotalPrecipitation) %>% 
  rename(MeanT = "...7", TotalPrecipitation = "...8")
head(enviro)

#### 1) PAI analysis ####

library(lme4) 
library(lmerTest) 
library(ggplot2)
library(emmeans) 

### 1.1) with random site factor ###

mod.PAI.1.1 <- with(photos, lmer(log(PAI) ~ Trait. + (1|PlacetteBloc)))
plot(mod.PAI.1.1) 
hist(resid(mod.PAI.1.1))
qqnorm(resid(mod.PAI.1.1))
qqline(resid(mod.PAI.1.1))

anova(mod.PAI.1.1, type = 3)
tukey <- emmeans(mod.PAI.1.1, list(pairwise ~ Trait.), 
                 adjust = "tukey", type = "response")
tukey
means <- multcomp::cld(object = tukey$emmeans,
                       Letters = letters) 

LAI <- ggplot(data = means,
              aes(x = Trait., y = response)) +
  geom_boxplot(data = photos, aes(x = Trait., y = PAI), 
               outlier.size = 0.9, linewidth = 0.6, color = "black") +
  stat_summary(fun.y = mean, geom = "point", 
               shape = 15, size = 2) + 
  geom_text(aes(label = gsub(" ", "", .group)), 
            position = position_nudge(y = 2.5), 
            size = 5.5) + 
  labs(title = "",
       x = "Protection scenario",
       y = expression(Plant~Area~Index~(m^2~m^-2))) +
  theme_classic() + 
  scale_x_discrete(labels = c("Control", "Light", "Inter." , "Stan.", "Intens.")) + 
  theme(axis.text = element_text(size = 11, color = "black"), 
        axis.title.y = element_text(size = 13), 
        axis.line = element_line(size = 0.5), 
        axis.ticks = element_line(size = 0.5),
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 
LAI

### 1.2) with env data ###

mod.PAI.2 <- photos_csv %>% 
  mutate(Placette = factor(Placette)) %>% 
  left_join(x = enviro, y = ., by = c("Placette", "Bloc", "Trait.")) %>%
  lm(data = ., 
     PAI ~ Trait. + Depot + Humidite + 
       MeanT + TotalPrecipitation)
plot(mod.PAI.2)

### 1.3) compare models ###

AIC(mod.PAI.1.1, 
    mod.PAI.2) 

#### 2) Alpha diversity analysis ####

library(lme4) 
library(lmerTest) 
library(ggplot2) 
library(emmeans) 

alpha <- Veg 
alpha$shannon <- diversity(alpha[, 4:70], 
                         index = "shannon") 

alpha$S <- specnumber(alpha[, 4:70])

alpha.PAI <- photos %>% 
  group_by(PlacetteBloc) %>% 
  summarise(PAI_moy = mean(PAI)) %>% 
  cbind(., alpha)

#### 2.2) species richness ####

### 2.1.1) with random site factor ###
mod.richesse.1.1 <- with(alpha, lmer(S ~ Trait. + (1|Placette)))
plot(mod.richesse.1.1) 
hist(resid(mod.richesse.1.1))
qqnorm(resid(mod.richesse.1.1))
qqline(resid(mod.richesse.1.1))

anova(mod.richesse.1.1, type = 3)
tukey <- emmeans(mod.richesse.1.1, list(pairwise ~ Trait.), adjust = "tukey")
tukey
means <- multcomp::cld(object = tukey$emmeans,
                       Letters = letters) 

bplot.S <- boxplot(alpha$S ~ alpha$Trait.)
bplot.S
bplot.S$stats[nrow(bplot.S$stats),] 
bplot.S$out 

plot.S <- ggplot(data = alpha, aes(x = Trait., y = S)) +
  geom_boxplot(outlier.size = 0.9, linewidth = 0.6, color = "black") +
  stat_summary(fun.y = mean, geom = "point", 
               shape = 15, size = 2) + 
  annotate("text", 
           x = c(1, 2, 3, 4, 5),
           y = c(25, 32, 30, 20, 19), 
           label = c("a", "a", "a", "ab", "b"), size = 5.5) +
  labs(title = "",
       x = "Protection scenario",
       y = "Species richness (S)") +
  theme_classic() + 
  scale_x_discrete(labels = c("Control", "Light", "Inter." , "Stan.", "Intens.")) + 
  theme(axis.text = element_text(size = 11, color = "black"), 
        axis.title = element_text(size = 13), 
        axis.line = element_line(size = 0.5), 
        axis.ticks = element_line(size = 0.5), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) 
plot.S

### 2.1.2) with environmental variables ###

mod.richesse.1.2 <- alpha %>% 
  left_join(x = enviro, y = alpha, by = c("Placette", "Bloc", "Trait.")) %>%
  lm(data = ., 
     log(S) ~ Trait. + Depot + Humidite + DistCote + 
       MeanT + TotalPrecipitation) 
plot(mod.richesse.1.2)

car::Anova(mod.richesse.1.2, type = 3)

### 2.1.3) with PAI instead of treatments ###

mod.richesse.2.2 <- alpha.PAI %>% 
  left_join(x = enviro, y = alpha.PAI, by = c("Placette", "Bloc", "Trait.")) %>%
  lm(data = ., S ~ PAI_moy + Depot + Humidite + DistCote + 
       MeanT + TotalPrecipitation)

plot(mod.richesse.2.2) 

### 2.1.4) compare models ###
AIC(mod.richesse.1.1, 
    mod.richesse.1.2, 
    mod.richesse.2.2) 

#### 2.1) shannon diversity index ####

### 2.1.1) with random site factor ###

mod.alpha.1.1 <- with(alpha, lmer(shannon ~ Trait. + (1|Placette)))
shapiro.test(resid(mod.alpha.1.1)) 
plot(mod.alpha.1.1) 
hist(resid(mod.alpha.1.1))
qqnorm(resid(mod.alpha.1.1))
qqline(resid(mod.alpha.1.1))

anova(mod.alpha.1.1, type = 3)
tukey <- emmeans(mod.alpha.1.1, list(pairwise ~ Trait.), adjust = "tukey")
means <- multcomp::cld(object = tukey$emmeans,
                       Letters = letters) 

plot.shannon <- ggplot(data = means,
                       aes(x = Trait., y = emmean)) +
  geom_boxplot(data = alpha, aes(x = Trait., y = shannon),
               outlier.size = 0.9, linewidth = 0.6, color = "black") +
  stat_summary(fun.y = mean, geom = "point", 
               shape = 15, size = 2) + 
  geom_text(aes(label = gsub(" ", "", .group)),
            position = position_nudge(y = 0.8), 
            size = 5.5) + 
  labs(title = "",
       x = "Protection scenarios",
       y = "Shannon diversity index (H')") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
  theme_classic() + # theme plus epure 
  scale_x_discrete(labels = c("Control", "Light", "Inter." , "Stan.", "Intens.")) + 
  theme(axis.text = element_text(size = 11, color = "black"), 
        axis.title = element_text(size = 13), 
        axis.line = element_line(size = 0.5), 
        axis.ticks = element_line(size = 0.5)) 
plot.shannon

### 2.1.2) with environmental variables ###

mod.alpha.1.2 <- alpha %>% 
  left_join(x = enviro, y = alpha, by = c("Placette", "Bloc", "Trait.")) %>%
  lm(data = ., 
     shannon ~ Trait. + Depot + Humidite + DistCote + 
       MeanT + TotalPrecipitation) 
plot(mod.alpha.1.2)

car::Anova(mod.alpha.1.2, type = 3)

### 2.1.3) with PAI instead of treatments ###

mod.alpha.2.2 <- alpha.PAI %>% 
  left_join(x = enviro, y = alpha.PAI, by = c("Placette", "Bloc", "Trait.")) %>%
  lm(data = ., shannon ~ PAI_moy + Depot + Humidite + DistCote + 
       MeanT + TotalPrecipitation)
plot(mod.alpha.2.2) 

car::Anova(mod.alpha.2.2, type = 3) 

### 2.1.4) compare models ###
AIC(mod.alpha.1.1, 
    mod.alpha.1.2, 
    mod.alpha.2.2) 

#### 3) arboreal lichens ####

library(lme4) 
library(lmerTest) 
library(ggplot2)
library(emmeans)

### 3.0) calculate mean lichen biomass per tree ###

lichens.moy <- lichens %>%
  group_by(Placette, Bloc, Trait.) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
head(lichens.moy)

count.vivant <- DHP %>% 
  mutate(Placette = factor(Placette)) %>% 
  group_by(Placette, Bloc, Essence) %>% 
  summarise(N = n(), .groups = "drop") %>% 
  filter(Essence %in% c("EPB", "EPN", "SAB")) %>% # JUSTE VIVANTS
  group_by(Placette, Bloc) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
head(count.vivant)

biomasse.vivant <- left_join(x = lichens.moy, y = count.vivant) %>% 
  mutate(bio_tot = Biomasse * N * 10000 / 400 / 1000)
biomasse.vivant

### 3.1) random model ###

mod.lichens <- lmer(log(bio_tot) ~ Trait. + (1|Placette),
                    data = biomasse.vivant)
plot(mod.lichens)
hist(resid(mod.lichens))
qqnorm(resid(mod.lichens))
qqline(resid(mod.lichens))

anova(mod.lichens, type = 3) 
tukey <- emmeans(mod.lichens, list(pairwise ~ Trait.), 
                 adjust = "tukey", type = "response")
means <- multcomp::cld(object = tukey$emmeans,
                       Letters = letters)

plot.lichens <- ggplot(data = means, aes(x = Trait., y = response)) +
  geom_boxplot(data = biomasse.vivant, aes(x = Trait., y = bio_tot), 
               outlier.size = 0.9, linewidth = 0.6, color = "black") +
  stat_summary(aes(x = Trait.), fun.y = mean, geom = "point", 
               shape = 15, size = 2) + 
  geom_text(aes(label = gsub(" ", "", .group)), 
            position = position_nudge(y = 9.5), 
            size = 5.5) + 
  labs(title = "",
       x = "Protection scenario",
       y = "Lichen biomass per hectare (kg/ha)") +
  theme_classic() + 
  scale_x_discrete(labels = c("Control", "Light", "Inter." , "Stan.", "Intens.")) + 
  theme(axis.text = element_text(size = 11, color = "black"), 
        axis.title = element_text(size = 13), 
        axis.line = element_line(size = 0.5), 
        axis.ticks = element_line(size = 0.5)) 
plot.lichens

library(ggpubr)
labels <- c("a", "b", "c", "d")
fig2 <- ggarrange(LAI, plot.S, plot.lichens, plot.shannon, ncol = 2, nrow = 2, 
                  labels = labels, font.label = list(size = 13))
fig2
ggsave("FIG2.tif",
       fig2,
       device = "tiff",
       height = 190, width = 190,
       units = "mm",
       dpi = 2000)

### 3.2) env model ###

lichens.enviro <- enviro %>% 
  left_join(x = ., y = biomasse.vivant, by = c("Placette", "Bloc", "Trait."))

lichens.2 <- lm(data = lichens.enviro, 
                log(bio_tot) ~ Trait. + Depot + Humidite + 
                  DistCote + MeanT + TotalPrecipitation)
plot(lichens.2) 

summary(lichens.2)
car::Anova(lichens.2, type = 3)

### 3.3) PAI model ###

photos.moy <- photos %>% 
  group_by(PlacetteBloc) %>% 
  summarise(PAI_moy = mean(PAI))

lichens.PAI <- cbind(lichens.enviro, photos.moy$PAI_moy) %>% 
  rename(PAI_moy = "photos.moy$PAI_moy") 

lichens.3 <- 
  lm(data = lichens.PAI, 
     log(bio_tot) ~ PAI_moy + Depot + Humidite + DistCote + MeanT + TotalPrecipitation)
plot(lichens.3) 

car::Anova(lichens.3, type = 3) 

### 3.4) compare models ###

AIC(mod.lichens, lichens.2, lichens.3)

#### 4) Shade tolerance ####

library(lme4) 
library(lmerTest)
library(ggplot2) 
library(emmeans) 
library(car)

## 4.0) create shade tolerance dataframe ##

filter(tol, Tolerance == "1")
intol <- Veg %>% 
  dplyr::select(Trait., Placette, Bloc, Trait., 
                Alnus_incana_subsp._rugosa, Anaphalis_margaritacea, 
                Betula_papyrifera, Chamerion_angustifolium_subsp._angustifolium, 
                Kalmia_angustifolia, Populus_balsamifera, Populus_tremuloides, Prunus_pensylvanica, 
                Rubus_idaeus, Vaccinium_angustifolium) %>% 
  mutate(intolerantes = rowSums(across(where(is.numeric)))) %>% 
  dplyr::select(Placette, Bloc, Trait., intolerantes)

filter(tol, Tolerance == "2")
med <- Veg %>% 
  dplyr::select(Trait., Placette, Bloc,
                Acer_rubrum, Betula_alleghaniensis, Claytosmunda_claytoniana, Clintonia_borealis, 
                Cornus_canadensis, Diervilla_lonicera, Eurybia_macrophylla, 
                Epigaea_repens, Linnaea_borealis, Moneses_uniflora, 
                Pteridium_aquilinum, 
                Ribes_glandulosum, Sambucus_racemosa, Streptopus_amplexifolius, 
                Sorbus_americana, Sorbus_decora, Vaccinium_myrtilloides, Viburnum_edule) %>% 
  mutate(moyennement = rowSums(across(where(is.numeric)))) %>% 
  dplyr::select(Placette, Bloc, Trait., moyennement)

filter(tol, Tolerance == "3")
tol <- Veg %>% 
  dplyr::select(Trait., Placette, Bloc,
                Abies_balsamea, Acer_spicatum, Aralia_nudicaulis, Athyrium_filix.femina, 
                Coptis_trifolia, Cornus_alternifolia, Cypripedium_acaule, 
                Dendrolycopodium_dendroideum, Dryopteris_cristata, Dryopteris_intermedia, 
                Equisetum_sylvaticum, Galium_triflorum, Gaultheria_hispidula, 
                Goodyera_tesselata, Goodyera_repens, Gymnocarpium_dryopteris, 
                Huperzia_lucidula, Lysimachia_borealis, 
                Maianthemum_canadense, Mitella_nuda, Monotropa_uniflora, Nabalus_sp., 
                Neottia_cordata, Oclemena_acuminata, Orthilia_secunda, Oxalis_montana, 
                Phegopteris_connectilis, Picea_glauca, Picea_mariana, 
                Ribes_lacustre, Ribes_triste, Rubus_pubescens, 
                Spinulum_annotinum, Taxus_canadensis, Trillium_undulatum, 
                Viburnum_cassinoides) %>% 
  mutate(tolerantes = rowSums(across(where(is.numeric)))) %>% 
  dplyr::select(Placette, Bloc, Trait., tolerantes)

tout <- intol %>% 
  bind_cols(med$moyennement, tol$tolerantes) %>% 
  rename(med = "...5", tol = "...6", intol = "intolerantes") %>% 
  pivot_longer(intol:tol, names_to = "Tolerance", values_to = "Recouvrement")

prop <- intol %>% 
  bind_cols(med$moyennement, tol$tolerantes) %>% 
  rename(med = "...5", tol = "...6", intol = "intolerantes") %>% 
  mutate(somme = intol + med + tol,
         intolerantes = intol / somme,
         medium = med / somme,
         tolerantes = tol / somme) %>% 
  dplyr::select(Placette, Bloc, Trait., intolerantes, medium, tolerantes) %>% 
  pivot_longer(intolerantes:tolerantes, names_to = "Tolerance", values_to = "Proportion")

prop$Tolerance <- as.factor(prop$Tolerance) 

## 4.1) random model  ##

prop.1 <- with(prop, lmer(Proportion ~ Trait. * Tolerance + (1|Placette)))
plot(prop.1)
hist(resid(prop.1))
qqnorm(resid(prop.1))
qqline(resid(prop.1))

anova(prop.1, type = 3)
tukey <- emmeans(prop.1, list(pairwise ~ Trait. * Tolerance), adjust = "tukey")
tukey
multcomp::cld(object = tukey$emmeans,
                       Letters = letters) 

plot.shade.x <- ggplot(data = prop, aes(x = Tolerance, y = Proportion, 
                                        color = Trait.)) +
  geom_boxplot(outlier.size = 0.9, linewidth = 0.6) + 
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(0.75),
               shape = 15, size = 2) + 
  labs(title = "",
       x = "Shade tolerance class",
       y = "IV per shade tolerance class") +
  theme_classic() + 
  scale_x_discrete(labels = c("Intolerant", "Mid-tolerant", "Tolerant")) + 
  theme(axis.text = element_text(size = 11, color = "black"), 
        axis.title = element_text(size = 13),
        axis.line = element_line(size = 0.5), 
        axis.ticks = element_line(size = 0.5), 
        legend.title =  element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = c(0.26, 0.85),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm")) + 
  scale_color_manual(values = c("#F5B041", "#ff5733", "#CF416A", "#900c3f", "#581845"), 
                     labels = c("Control", "Light", "Intermediate" , "Standard", "Intensive")) +
  guides(color = guide_legend("Protection scenarios")) +
  annotate("text", 
           x = c(0.7, 0.85, 1, 1.15, 1.3),
           y = c(0.42658593, 0.2501546, 0.2654882, 0.23545994, 0.12697044), 
           label = c("a", "a", "a", "a", "a"), size = 5) +
  annotate("text", 
           x = c(1.7, 1.85, 2, 2.15, 2.3),
           y = c(0.6989049, 0.79789916, 0.67332589, 0.83991597, 0.65550459), 
           label = c("e", "e", "e", "e", "e"), size = 5) +
  annotate("text", 
           x = c(2.7, 2.82, 3, 3.15, 3.3),
           y = c(0.66228600, 0.70801653, 0.86546778, 0.82092262, 1.00614035), 
           label = c("x", "xy", "yz", "xy", "z"), size = 5)
plot.shade.x

## 4.2) enviro ##

prop.enviro <- enviro %>% 
  left_join(x = ., y = prop, by = c("Placette", "Bloc", "Trait."))
head(prop.enviro)

prop.2 <- lm(data = prop.enviro, 
             log(Proportion + 1) ~ Trait. * Tolerance + Depot + Humidite + 
               DistCote + MeanT + TotalPrecipitation)
plot(prop.2) 

summary(prop.2)
car::Anova(prop.2, type = 3)
emmeans(prop.2, list(pairwise ~ Trait. * Tolerance), adjust = "tukey")

## 4.3) model with PAI ##
photos.moy <- photos %>% 
  group_by(PlacetteBloc) %>% 
  summarise(PAI_moy = mean(PAI)) %>% 
  slice(rep(1:n(), each = 3)) 

prop.PAI <- cbind(prop.enviro, photos.moy$PAI_moy) %>% 
  rename(PAI_moy = "photos.moy$PAI_moy") 

prop.3 <- 
  lm(data = prop.PAI, 
     Proportion ~ PAI_moy * Tolerance + Depot + Humidite + DistCote + MeanT + TotalPrecipitation)
plot(prop.3) 

## 4.4) compare models ##
AIC(prop.1, prop.2, prop.3)

#### 5) fruits ####

library(tidyverse) 
library(ggplot2) 

fruits_sum <- fruits %>% 
  mutate(Rubus_idaeus = Rubus_idaeus / 12,
         Amelanchier_spp. = Amelanchier_spp. / 12) %>%
  ungroup() %>% 
  group_by(Trait.) %>% 
  summarise_at(vars(Amelanchier_spp.:Rubus_idaeus), sum) %>% 
  pivot_longer(c(Rubus_idaeus, Amelanchier_spp.), 
               names_to = "sp", values_to = "nb_fruits") %>% 
  mutate(sp = factor(sp)) %>% 
  mutate(sp = fct_relevel(sp, "Rubus_idaeus", "Amelanchier_spp.")) # view raspberry before amelanchier

plot.fruits <- ggplot(data = fruits_sum, aes(x = Trait., y = nb_fruits, fill = sp)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(1, 10, 100)) + 
  theme_classic() +
  labs(title = "",
       x = "Protection scenarios",
       y = expression(Fruits~produced~by~species~per~m^2)) +
  scale_x_discrete(labels = c("Control", "Light", "Inter." , "Stan.", "Intens.")) +
  theme(axis.text = element_text(size = 11, color = "black"), 
        axis.title = element_text(size = 13), 
        axis.line = element_line(size = 0.5), 
        axis.ticks = element_line(size = 0.5), 
        legend.title =  element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = c(0.75, 0.9),
        legend.text.align = 0) +
  labs(fill = "Fruit bearing species") +
  scale_fill_manual(values = c(Rubus_idaeus = "#F05039", Amelanchier_spp. = "#1F449C"), 
                    labels = c(parse(text = "italic('Rubus idaeus')"), parse(text = "italic('Amelanchier')~plain(spp.)")))
plot.fruits

library(ggpubr)
labels <- c("a", "b", "c", "d")
fig3 <- ggarrange(plot.shade.x, plot.fruits, ncol = 2, nrow = 1, 
                  labels = labels, font.label = list(size = 13))
fig3
ggsave("FIG3.tif",
       fig3,
       device = "tiff",
       height = 95, width = 190,
       units = "mm",
       dpi = 2000)

#### 6) beta diversity ####

library(vegan) 

sp.pres <- Veg[,4:70]
sp.pres[sp.pres > 0] <- 1
sp.pres

pres.hell <- sp.pres %>% 
  decostand(method = "hellinger") 
pres.dist <- dist(pres.hell) 

gr <- Veg$Trait.
couleurs <- c("#F5B041", "#ff5733", "#CF416A", "#900c3f", "#581845")

dispers.tot.pres <- betadisper(pres.dist,
                          group = gr, 
                          type = "centroid") 
x.p <- dispers.tot.pres
ct.p = x.p$centroids[,1:2] 

png(file = "FIG4.tif", units = "mm",
    width = 90, height = 80, res = 1000)

par(mar = c(2, 2, 1, 1))

plot(dispers.tot.pres, 
     ylim = c(-0.5, 0.5), xlim = c(0.1, 0.3),
     col = couleurs, seg.col = couleurs, 
     lwd = 1, 
     main = "", xlab = "", ylab = "", sub = "", 
     hull = FALSE, ellipse = FALSE, label = FALSE, segments = TRUE, 
     pch = c(16, 16, 16, 16, 16),
     cex.axis = 0.7) 
points(ct.p, pch=19, col = couleurs, cex = 1.5) 
ordiellipse(dispers.tot.pres, gr, conf = 0.40, 
            col = couleurs, lty = 1, lwd = 1.5)
legend("bottomright", legend = c("Control", "Light", "Intermediate" , "Standard", "Intensive"),
       col = couleurs, pch = c(16, 16, 16, 16, 16), bty = "n",
       title = "Protection scenarios", cex = 0.65)

par(fig=c(0.57, 1, 0.5, 1), mgp = c(1, 0.5, 0), new=TRUE)

bplot <- boxplot(dispers.tot.pres, 
                 col = "white", border = couleurs,
                 lwd = 1, 
                 ylim = c(0.45, 0.85), 
                 names = c("", "", "", "", ""),
                 xlab = "", ylab = "",
                 xaxt = "n", 
                 cex.axis = 0.5)
title(ylab = "Distance to centroid", line = 1,
      cex.lab = 0.55) 
text(x = 1:5, y = c(0.7746563, 0.6754093, 0.8361283, 0.7524813, 0.7524813), 
     labels = c("a", "a", "a", "a", "a"),
     cex = 0.7) 
dev.off()

test.dist.pres <- permutest(dispers.tot.pres, permutations = 9999, pairwise = TRUE)
test.dist.pres

dist(ct.p, method = "euclidean")

test.centro.pres <- adonis2(pres.dist ~ x.p$group, method = "bray")
test.centro.pres

#### 7) vegetation groups ####

library(lme4) 
library(lmerTest)
library(ggplot2) 
library(emmeans) 
library(car)

### 7.0) create groups ###

groupes <- Veg_csv %>% 
  group_by(Placette, Bloc, Trait.) %>% 
  summarise_at(vars(Abies_balsamea:Viola_sp.), 
               mean) %>%
  mutate(Placette = factor(Placette)) %>% 
  mutate(Trait. = fct_relevel(Trait., "Aucun", "3_ans", "2_ans", "Standard", "Intensif")) %>% 
  mutate( 
    Forbs = Anaphalis_margaritacea + Aralia_nudicaulis + Chamerion_angustifolium_subsp._angustifolium +
      Clintonia_borealis + Coptis_trifolia + Cornus_canadensis + Cypripedium_acaule +
      Eurybia_macrophylla + Epigaea_repens + 
      Galium_triflorum + Gaultheria_hispidula + Goodyera_tesselata + Goodyera_repens +
      Linnaea_borealis + Lysimachia_borealis + Maianthemum_canadense + Mitella_nuda + Moneses_uniflora +
      Monotropa_uniflora + Nabalus_sp. + Neottia_cordata + Oclemena_acuminata + 
      Orthilia_secunda + Oxalis_montana + Rubus_pubescens + Streptopus_amplexifolius + Trillium_undulatum +
      Viola_sp.,
    Ferns_allies = Athyrium_filix.femina + Claytosmunda_claytoniana + Dryopteris_cristata +
      Dryopteris_intermedia + Gymnocarpium_dryopteris + Phegopteris_connectilis + Pteridium_aquilinum +
      Equisetum_sylvaticum + Dendrolycopodium_dendroideum + Huperzia_lucidula + Spinulum_annotinum,
    Lichens = Cladonia_sp. + Cladonia_coniocraea + Cladonia_rangiferina,
    Bryophytes = Bazzania_trilobata + Dicranum_sp. + Hylocomium_splendens + Pleuzorium_schreberi +
      Polytrichum_sp. + Ptilium_crista.castrensis + Rhytidiadelphus_triquetrus +
      Sphagnum_sp.,
    Coniferous = Abies_balsamea + Picea_glauca + Picea_mariana,
    Decidious = Acer_rubrum +  Betula_alleghaniensis + Betula_papyrifera +
      Populus_balsamifera + Populus_tremuloides,
    Shrubs = Acer_spicatum + Alnus_incana_subsp._rugosa + Amelanchier_sp. + Cornus_alternifolia + 
      Diervilla_lonicera + Kalmia_angustifolia + Prunus_pensylvanica + Ribes_glandulosum +
      Ribes_lacustre + Ribes_triste + Rubus_idaeus + Sambucus_racemosa + 
      Sorbus_americana + Sorbus_decora + Taxus_canadensis + Vaccinium_angustifolium +
      Vaccinium_myrtilloides + Viburnum_edule + Viburnum_cassinoides) %>% 
  dplyr::select(Placette, Bloc, Trait., Forbs, Ferns_allies, Lichens, Bryophytes, Coniferous, Decidious, Shrubs)
groupes

groupes_long <- groupes %>%
  mutate(somme = Forbs + Ferns_allies + Lichens + Bryophytes + Coniferous + Decidious + Shrubs,
         Forbs.p = Forbs / somme, 
         Ferns_allies.p = Ferns_allies/somme,
         Lichens.p = Lichens/somme,
         Bryophytes.p = Bryophytes/somme,
         Coniferous.p = Coniferous/somme,
         Decidious.p = Decidious/somme,
         Shrubs.p = Shrubs/somme) %>% 
  dplyr::select(Placette, Bloc, Trait., Forbs.p, Ferns_allies.p, Lichens.p, Bryophytes.p, 
                Coniferous.p, Decidious.p, Shrubs.p) %>% 
  pivot_longer(Forbs.p:Shrubs.p, names_to = "Group", values_to = "Mean_cover") 

## 7.1) model with random sites ##

group.1 <- with(groupes_long, lmer(asin(sqrt(Mean_cover)) ~ Trait. * Group + (1|Placette)))
plot(group.1)
hist(resid(group.1))
qqnorm(resid(group.1))
qqline(resid(group.1))

anova(group.1, type = 3)

tukey <- emmeans(group.1, list(pairwise ~ Trait. | Group), adjust = "tukey")
tukey
multcomp::cld(object = tukey$emmeans,
              Letters = letters) 

plot.group <- ggplot(data = groupes_long, aes(x = Group, y = Mean_cover, 
                                        color = Trait.)) +
  geom_boxplot(outlier.size = 1, linewidth = 0.7) + 
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(0.75),
               shape = 15, size = 2) + 
  ylim(0,1) +
  labs(title = "",
       x = "Vegetation group",
       y = "IV per vegetation group") +
  theme_classic() + 
  scale_x_discrete(labels = c("Bryophytes", "Coniferous \n trees", "Decidious \n trees", 
                              "Ferns and \n allies", "Forbs", "Terrestrial \n lichens", "Shrubs")) + 
  theme(axis.text = element_text(size = 12, color = "black"), 
        axis.title = element_text(size = 14),
        axis.line = element_line(size = 0.5), 
        axis.ticks = element_line(size = 0.5), 
        legend.title =  element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.position = c(0.85, 0.85),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm")) + 
  scale_color_manual(values = c("#F5B041", "#ff5733", "#CF416A", "#900c3f", "#581845"), 
                     labels = c("Control", "Light", "Intermediate" , "Standard", "Intensive")) +
  guides(color = guide_legend("Protection scenarios")) +
  annotate("text", # bryophytes
           x = c(0.7, 0.85, 1, 1.15, 1.3),
           y = c(0.6600953895, 0.7604784979, 0.7518794556, 0.8838264300, 0.98), 
           label = c("a", "a", "a", "b", "b"), size = 5) +
  annotate("text", # coniferous
           x = c(1.7, 1.85, 2, 2.15, 2.3),
           y = c(0.1154050465, 0.1401109057, 0.1582055906, 0.0977346278, 0.2012609737), 
           label = c("c", "c", "c", "c", "c"), size = 5) +
  annotate("text", # decidious
           x = c(2.7, 2.85, 3, 3.15, 3.3),
           y = c(0.2640221402, 0.0517631502, 0.1431304749, 0.05, 0.0539904230), 
           label = c("d", "d", "d", "d", "d"), size = 5) +
  annotate("text", # fern
           x = c(3.7, 3.85, 4, 4.15, 4.3),
           y = c(0.1105820106, 0.0814047030, 0.1387902331, 0.0705149385, 0.1504159734), 
           label = c("e", "e", "e", "e", "e"), size = 5) +
  annotate("text", # forbs
           x = c(4.7, 4.85, 5.05, 5.20, 5.3),
           y = c(0.5978354978, 0.7172413793, 0.5428538497, 0.3353368483, 0.4632804233), 
           label = c("fg", "f", "gh", "hk", "k"), size = 5) + 
  annotate("text", # lichens
           x = c(5.7, 5.85, 6, 6.15, 6.3),
           y = c(0.0588300221, 0.0521958718, 0.0594542329, 0.05, 0.05), 
           label = c("o", "o", "o", "o", "o"), size = 5) +
  annotate("text", # shrubs
           x = c(6.7, 6.85, 7, 7.20, 7.3),
           y = c(0.3624515128, 0.2382735149, 0.5273917182, 0.1497229917, 0.1004310345), 
           label = c("p", "q", "p", "qs", "s"), size = 5) 
plot.group

ggsave("FIG4.tif",
       plot.group,
       device = "tiff",
       height = 120, width = 190,
       units = "mm",
       dpi = 2000)

## 7.2) model with environmental variables ##

groupes.enviro <- enviro %>% 
  left_join(x = ., y = groupes_long, by = c("Placette", "Bloc", "Trait."))
head(groupes.enviro)

groupes.2 <- lm(data = groupes.enviro, 
             asin(sqrt(Mean_cover)) ~ Trait. * Group + Depot + Humidite + 
               DistCote + MeanT + TotalPrecipitation)
plot(groupes.2) 

summary(groupes.2)
car::Anova(groupes.2, type = 3)
emmeans(groupes.2, list(pairwise ~ Trait. * Group), adjust = "tukey")

## 7.3) model with PAI ##
photos.moy <- photos %>% 
  group_by(PlacetteBloc) %>% 
  summarise(PAI_moy = mean(PAI)) %>% 
  slice(rep(1:n(), each = 7)) 

groupes.PAI <- cbind(groupes.enviro, photos.moy$PAI_moy) %>% 
  rename(PAI_moy = "photos.moy$PAI_moy") 

groupes.3 <- 
  lm(data = groupes.PAI, 
     asin(sqrt(Mean_cover)) ~ PAI_moy * Group + Depot + Humidite + DistCote + MeanT + TotalPrecipitation)
plot(groupes.3) 

## comparer les modèles ##
AIC(group.1, groupes.2, groupes.3)

#### 8) indicator species ####

library(indicspecies)

sp.indic <- Veg_csv %>%
  group_by(Placette, Bloc, Trait.) %>% 
  summarise_at(vars(Abies_balsamea:Viola_sp.), 
               mean) %>%
  ungroup() %>% 
  dplyr::select(- Placette, - Bloc, - Trait.)

groups.indic <- Veg$Trait.

indval <- multipatt(sp.indic, groups.indic, 
                    control = how(nperm=999))

summary(indval, 
        indvalcomp=TRUE, 
        alpha=1) 

indval$sign
