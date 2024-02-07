#### INSTALL PACKAGES ----
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(grid)
library(ggpattern)
library(cowplot)
theme_set(theme_cowplot())
#from fishR blog "LF histo in ggplot2"

#### READ IN DATA ----
setwd("C:/Documents/Pyramid_Lake/RCreations/RProjects/LengthFreq")
data <- read.csv("lfTui_allGill.csv")

#### LENGTH FREQUENCY HISTOGRAMS ----

# LF Gill Tui by Maturity Status
data <- data %>% mutate(sex_names = case_when(sex == "F" ~ "Female",
                                              sex == "M" ~ "Male",
                                              sex == "I" ~ "Non-Fecund",
                                              sex == "U" ~ "Non-Fecund"))
# color
lf_fec <- ggplot() +
  geom_histogram(data, mapping=aes(x=tl, fill=factor(sex_names, levels=c("Female", "Male", "Non-Fecund"))),
                 binwidth=10,boundary=0,closed="left",
                 color="black") +
  theme(plot.title=element_text(hjust=0.5, size=16)) +
  labs(fill="Fecundity Status") +
  scale_y_continuous(name="Number of Fish") +
  scale_x_continuous(name=" ") +
  scale_fill_brewer(palette="Greys")
lf_fec

setwd("C:/Documents/Pyramid_Lake/RCreations/ROutput")
png(filename = "lf_fec_23.png", units = "in", width = 8, height = 6, res=600)
lf_fec
dev.off()

# length frequency by morph

# LF Gill Tui by Gill Status
data <- data %>% mutate(gill_names = case_when(gills == "PEC" | gills == "Pec" ~ "Pectinifer",
                                              gills == "OB" ~ "Obesa",
                                              gills == "HYB" ~ "Potential Intermediate",
                                              gills == "U" ~ "Unknown"))

lf_morph <- ggplot() +
  geom_histogram(data, mapping=aes(x=tl, fill=factor(gill_names, levels=c("Pectinifer", "Obesa", "Potential Intermediate", "Unknown"))),
                 binwidth=10,boundary=0,closed="left",
                 color="black") +
  theme(plot.title=element_text(hjust=0.5, size=16)) +
  labs(fill="Morph Classification") +
  scale_y_continuous(name="Number of Fish") +
  scale_x_continuous(name="Total Length (mm)") +
  scale_fill_brewer(palette="Greys")
lf_morph

setwd("C:/Documents/Pyramid_Lake/RCreations/ROutput")
png(filename = "lf_morph_23.png", units = "in", width = 8, height = 6, res=600)
lf_morph
dev.off()

#### STACKED ----
setwd("C:/Documents/Pyramid_Lake/RCreations/ROutput")
png(filename = "lf_stack_23.png", units = "in", width = 8, height = 6, res=600)
grid.newpage()
grid.draw(rbind(ggplotGrob(lf_fec), ggplotGrob(lf_morph), size = "last"))
dev.off()

#obesa all
lenfreq_obesa <- ggplot(data=obesa_tl,aes(x=TL)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()
lenfreq_obesa + scale_x_continuous(limits = c(0,400))

#pectinifer all
lenfreq_pectinifer <- ggplot(data=pectinifer_tl,aes(x=TL)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()
lenfreq_pectinifer

#female all
lenfreq_female <- ggplot(data=female_tl,aes(x=TL)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()
lenfreq_female

# male all
lenfreq_male <- ggplot(data=male_tl,aes(x=TL)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()
lenfreq_male

#obesa and male
lenfreq_O_M <- ggplot(data=O_M_tl,aes(x=TL)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()
lenfreq_O_M

#obesa and female
lenfreq_O_F <- ggplot(data=O_F_tl,aes(x=TL)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()
lenfreq_O_F

#pectinifer and male
lenfreq_P_M <- ggplot(data=P_M_tl,aes(x=TL)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()
lenfreq_P_M

#pectinifer and female
lenfreq_P_F <- ggplot(data=P_F_tl,aes(x=TL)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()
lenfreq_P_F

#immature all
lenfreq_immature<- ggplot(data=immature_tl,aes(x=TL)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()
lenfreq_immature

# all
lenfreq_all<- ggplot(data=all_tl,aes(x=TL)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()
lenfreq_all

# length frequency by maturity stage
#label for gape limit and juvenile cutoff
ann1 <- data.frame(x=350, y=175,
                  label="dashed= estimated 1 year cutoff")
ann2 <- data.frame(x=350, y=160,
                  label="solid= LCT gape limit")
lenfreq_MvIM <- ggplot() +
  geom_histogram(data=maturity_tl, mapping=aes(x=tl, fill=factor(maturity, levels=c("F", "M", "I"))),
                 binwidth=10,boundary=0,closed="left",
                 color="black") +
  theme(plot.title=element_text(hjust=0.5, size=16)) +
  labs(title="Length Frequency by Maturity Status", fill="Sex/Maturity") +
  scale_y_continuous(name="Number of Fish",expand=expansion(mult=c(0,0.05)), limits =c(0,200)) +
  scale_x_continuous(name="Total Length (mm)", limits=c(0,450)) +
  geom_vline(xintercept=c(150, 200), linetype=c("dashed", "solid"), size=1) +
  geom_text(data=ann1, aes( x=x, y=y, label=label), size=3) +
  geom_text(data=ann2, aes( x=x, y=y, label=label), size=3) +
  scale_fill_brewer(palette="Spectral")
lenfreq_MvIM

# export maturity graphs
setwd("C://Documents//Pyramid_Lake//RCreations//ROutput")

png(filename = "maturity_LF.png", units = "in", width = 8, height = 6, res=300)
lenfreq_MvIM
dev.off()

# length frequency by morph
lenfreq_morph <- ggplot() +
  geom_histogram(data=morph_tl, mapping=aes(x=tl, fill=factor(morph, levels=c("PEC", "OB"))),
                 binwidth=10,boundary=0,closed="left",
                 color="black") +
  theme(plot.title=element_text(hjust=0.5, size=16)) +
  labs(title="Length Frequency by Morph", fill="Morph") +
  scale_y_continuous(name="Number of Fish",expand=expand_scale(mult=c(0,0.05)), limits =c(0,200)) +
  scale_x_continuous(name="Total Length (mm)", limits=c(0,450)) +
  geom_vline(xintercept=c(150, 200), linetype=c("dashed", "solid"), size=1) +
  geom_text(data=ann1, aes( x=x, y=y, label=label), size=3) +
  geom_text(data=ann2, aes( x=x, y=y, label=label), size=3) +
scale_fill_brewer(palette="Spectral")
lenfreq_morph

png(filename = "morph_LF.png", units = "in", width = 8, height = 6, res=300)
lenfreq_morph
dev.off()



  
  
  
  
  
  
  
  
  
  