rm(list=ls())

library(tidyverse)
library(paletteer)
library(extrafont)
library(ggtext)
library(ragg)

data <- read.csv("Data/GBDAlcoholxAge.csv") %>% 
  mutate(age=factor(age, levels=c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
                                  "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
                                  "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                                  "85 to 89", "90 to 94")))

agg_tiff("Outputs/GBDAlcxAgexCauseDeaths.tiff", units="in", width=8, height=8, res=500)
ggplot(data %>% filter(metric=="Number" & measure=="Deaths"), aes(y=age, x=val, fill=cause))+
  geom_col()+
  geom_vline(xintercept=0, colour="Grey50")+
  scale_x_continuous(name="Annual deaths attributable to alcohol")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("rcartocolor::Vivid", name="Cause")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Roboto"))+
  labs(title="Deaths from alcohol are highest around age 70",
       subtitle="Annual alcohol-attributable deaths in the United Kingdom by cause",
       caption="Data from the Global Burden of Disease Study | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/GBDAlcxAgexCauseDALYs.tiff", units="in", width=8, height=8, res=500)
ggplot(data %>% filter(metric=="Number" & measure!="Deaths"), aes(y=age, x=val, fill=cause))+
  geom_col()+
  geom_vline(xintercept=0, colour="Grey50")+
  scale_x_continuous(name="Annual Disability-Adjusted Life Years lost to alcohol")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("rcartocolor::Vivid", name="Cause")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Roboto"))+
  labs(title="The overall health burden of alcohol harm is greatest in ages 60-65",
       subtitle="Annual alcohol-attributable Disability-Adjusted Life Years lost in the United Kingdom by cause",
       caption="Data from the Global Burden of Disease Study | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/GBDAlcxAgexCausexSexDeaths.tiff", units="in", width=14, height=8, res=500)
ggplot(data %>% filter(metric=="Number" & measure=="Deaths"), aes(x=age, y=val, fill=sex))+
  geom_col()+
  geom_hline(yintercept=0, colour="Grey50")+
  scale_y_continuous(name="Annual Deaths attributable to alcohol")+
  scale_x_discrete(name="")+
  scale_fill_manual(values=c("#00cc99", "#6600cc"), name="Cause")+
  facet_wrap(~cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(2)),
        axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        text=element_text(family="Roboto"), plot.subtitle=element_markdown(size=rel(1.4)))+
  labs(title="The age profile of alcohol deaths varies widely by cause",
       subtitle="Annual alcohol-attributable deaths in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> in the United Kingdom",
       caption="Data from the Global Burden of Disease Study | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/GBDAlcxAgexCausexSexDALYs.tiff", units="in", width=14, height=8, res=500)
ggplot(data %>% filter(metric=="Number" & measure!="Deaths"), aes(x=age, y=val, fill=sex))+
  geom_col()+
  geom_hline(yintercept=0, colour="Grey50")+
  scale_y_continuous(name="Annual Disability-Adjusted Life Years lost to alcohol")+
  scale_x_discrete(name="")+
  scale_fill_manual(values=c("#00cc99", "#6600cc"), name="Cause")+
  facet_wrap(~cause)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(2)),
        axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        text=element_text(family="Roboto"), plot.subtitle=element_markdown(size=rel(1.4)))+
  labs(title="The health burden of alcohol falls across all adult age groups",
       subtitle="Annual alcohol-attributable Disability-Adjusted Life Years lost to <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> in the United Kingdom",
       caption="Data from the Global Burden of Disease Study | Plot by @VictimOfMaths")
dev.off()
