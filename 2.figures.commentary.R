library(tidyverse)
library(readxl)
library(ggplot2)
library(stringr)
library(ggnewscale)
library(wesanderson)
library(forcats)
library(scales)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

###########################################################################
###########################################################################
###                                                                     ###
###                         CHARTS AND ANALYSIS                         ###
###                                                                     ###
###########################################################################
###########################################################################

###Read data for figures and other analysis
investors<-read.csv(paste(getwd(),"/output/investors_20220504.csv",sep=""),fileEncoding = "UTF-8")

#apply filters for charts
investments.analysis<-investors %>%
  filter(is.na(investment.type) == FALSE) %>%
  filter(!investor.type.edited %in% c("other","not an investor","accelerator or incubator","university","crowdfunding")) %>%
  mutate(investment.type.revised.basic = ifelse(investment.type=="grant","grant","equity")) %>%
  mutate(investor.type.edited = ifelse(investor.type.edited %in% c("family","angel investor"),"angel or family",as.character(investor.type.edited))) %>%
  mutate(investment.type.revised.detail = str_replace(investment.type, c("series a|series b"), "series ab")) %>%
  mutate(investment.type.revised.detail = (fct_relevel(investment.type.revised.detail, "grant", "seed", "series ab","growth equity"))) %>%
  mutate(investment.type.revised = paste(investor.type.edited, investment.type.revised.basic)) %>%
  filter(investment.year >=2005 & investment.year<=2021) %>%
  mutate(investment.type = fct_relevel(investment.type, "grant", "seed", "series a","series b","growth equity")) %>%
  mutate(investor.type.edited = fct_relevel(investor.type.edited, 
                                            "public or quasi public",
                                            "corporation or corporate venture",
                                            "venture capital", 
                                            "private equity",
                                            "financial sector",
                                            "angel or family",
                                            )) %>%
   select(investor.id,investor.firm.renamed, investment.type.revised,investment.type.revised.basic,investment.type.revised.detail,investor.country.edited,i3.url,investor.type.edited,company,tags,investment.year,
         investment.amount,investment.amount.revised,investment.type,investment.year,investor.category,sbti.status,country) %>%
  unique()
  
#write.csv(investments.analysis,paste(getwd(),"/output/investors_filtered_20211215.csv",sep=""),fileEncoding = "UTF-8")

#generating colors
mypal<-c("black","red","#0868ac","#43a2ca","#7bccc4","#bae4bc")


##-------------------------------------------------------------------------
##                              FIGURE 1                                        
##-------------------------------------------------------------------------

#consolidated data (aggregated by type of investor)

investments.analysis.consolidated<-investments.analysis %>%
  mutate(investor.category = ifelse(is.na(investor.category),as.character(investor.type.edited),investor.category)) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year>=2005) %>%
  mutate(investment.year=as.integer(investment.year)) %>%
  group_by(investment.year,investment.type.revised) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  mutate(investor.deals = length(company)) %>%
  mutate(investor.count = n_distinct(investor.firm.renamed)) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm=FALSE)/10^9) %>%
  ungroup()  %>%
  select(investment.type.revised,investment.type.revised.basic,investment.type.revised.detail,investor.type.edited,investment.year,investment.amount.tot, investor.portco) %>%
  unique() %>%
  pivot_longer(6:7,names_to="data.type")

figure1 <-investments.analysis.consolidated 

figure1.plot <- ggplot(figure1, 
                       aes(x=investment.year, 
                           y=value,
                           color=investor.type.edited)) +
  geom_line(aes(linetype=investment.type.revised.basic))+
  geom_line(data = . %>% filter(investment.type.revised=="corporation or corporate venture equity"), size=1.5) +
  #geom_line(data = . %>% filter(investment.type.revised=="public or quasi public grant"), size=1,linetype="dashed") +
  scale_color_manual(values = mypal) +
  scale_x_continuous(expand = c(0, 0), limits = c(2005,2021),breaks=seq(2006, 2020,2)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank()) +
  facet_wrap(~data.type,scales="free_y")

plot(figure1.plot) 

# ggsave(
#   (paste(getwd(),"/figures/figure1.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 11,
#   height = 5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )


##-------------------------------------------------------------------------
##                          FIGURE SI.1 (portco)                           -
##                   investments are at a later stage                      -
##-------------------------------------------------------------------------

investments.analysis.consolidated<-investments.analysis %>%
  mutate(investor.category = ifelse(is.na(investor.category),as.character(investor.type.edited),investor.category)) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year>=2005) %>%
  mutate(investment.year=as.integer(investment.year)) %>%
  group_by(investment.year,investment.type.revised.detail, investor.type.edited) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  mutate(investor.deals = length(company)) %>%
  mutate(investor.count = n_distinct(investor.firm.renamed)) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm=FALSE)/10^9) %>%
  ungroup()  %>%
  select(investment.type.revised,investment.type.revised.basic,investment.type.revised.detail,investor.type.edited,investment.year,investment.amount.tot, investor.portco,investor.count) %>%
  unique() %>%
  pivot_longer(6:8,names_to="data.type")

figure1.SI <-investments.analysis.consolidated %>%
  filter(data.type == "investor.portco")

figure1SI.plot <- ggplot(figure1.SI, 
                       aes(x=investment.year, 
                           y=value,
                           color=investor.type.edited)) +
  geom_line(aes(linetype=investment.type.revised.basic))+
  geom_line(data = . %>% filter(investment.type.revised=="corporation or corporate venture equity"), size=1) +
  scale_color_manual(values = mypal) +
  scale_x_continuous(expand = c(0, 0), limits = c(2006,2021),breaks=seq(2006, 2021,2)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank()) +
  facet_wrap(~investment.type.revised.detail,scales="fixed")

plot(figure1SI.plot) 

# ggsave(
#   (paste(getwd(),"/figures/figureSI1.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 8,
#   height = 6.5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )

##-------------------------------------------------------------------------
##                          FIGURE SI.1 (amount)                           -
##                   investments are at a later stage                      -
##-------------------------------------------------------------------------

investments.analysis.consolidated<-investments.analysis %>%
  mutate(investor.category = ifelse(is.na(investor.category),as.character(investor.type.edited),investor.category)) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year>=2005) %>%
  mutate(investment.year=as.integer(investment.year)) %>%
  group_by(investment.year,investment.type.revised.detail, investor.type.edited) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  mutate(investor.deals = length(company)) %>%
  mutate(investor.count = n_distinct(investor.firm.renamed)) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm=FALSE)/10^9) %>%
  ungroup()  %>%
  select(investment.type.revised,investment.type.revised.basic,investment.type.revised.detail,investor.type.edited,investment.year,investment.amount.tot, investor.portco,investor.count) %>%
  unique() %>%
  pivot_longer(6:8,names_to="data.type")

figure1.SI <-investments.analysis.consolidated %>%
  filter(data.type == "investment.amount.tot")

figure1SI.plot <- ggplot(figure1.SI, 
                         aes(x=investment.year, 
                             y=value,
                             color=investor.type.edited)) +
  geom_line(aes(linetype=investment.type.revised.basic))+
  geom_line(data = . %>% filter(investment.type.revised=="corporation or corporate venture equity"), size=1) +
  scale_color_manual(values = mypal) +
  scale_x_continuous(expand = c(0, 0), limits = c(2006,2021),breaks=seq(2006, 2021,2)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  theme(legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank()) +
  facet_wrap(~investment.type.revised.detail,scales="free_y")

plot(figure1SI.plot) 

# ggsave(
#   (paste(getwd(),"/figures/figureSI1b.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 8,
#   height = 6.5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )

##-------------------------------------------------------------------------
##                              FIGURE 2                                  -
##                              (dots)                                    -
##-------------------------------------------------------------------------

#dot plot-----

figure2<-investments.analysis %>%
  dplyr::mutate(investor.type.edited = ifelse(investor.type.edited %in% c("public or quasi public","corporation or corporate venture"),as.character(investor.type.edited), "other investors")) %>%
  mutate(investor.category = ifelse(is.na(investor.category),as.character(investor.type.edited),as.character(investor.category))) %>%
  filter(investment.year>=2016) %>%
  group_by(investor.id,investor.category) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  mutate(investor.deals = length(company)) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm=FALSE)) %>%
  ungroup()  %>%
  group_by(investor.id) %>%
  mutate(avg.investment = mean(investment.amount.revised, na.rm=FALSE)) %>%
  ungroup() %>%
  select(investor.id,investor.category,investor.firm.renamed,investor.portco,avg.investment,#investor.deals.avg,investor.amt.avg,
         sbti.status,investment.amount.tot) %>%
  mutate(sbti.status = ifelse(is.na(sbti.status)==TRUE,0,"SBTI")) %>%
  filter(investor.portco>=4) %>%
  #filter(investor.deals>=5) %>%
  filter(investment.amount.tot>=100000) %>%
  unique()

figure2$investor.category <- as.character(figure2$investor.category)
figure2$investor.category <- str_trim(figure2$investor.category)
figure2$investor.category <- as.factor(figure2$investor.category)

figure2 <- figure2  %>%
  mutate(investor.category = fct_relevel(investor.category, "fossil fuels and utilities", "transportation",
                                         "manufacturing, hardware, semiconductors, chemicals","food, beverages & agriculture",
                                         "digital and financial services","clean energy","public or quasi public",
                                         "other investors"))


#write.csv(figure2,paste(getwd(),"/output/investors-analyzed-20211122-dots.csv",sep=""),fileEncoding = "UTF-8")


figure2.plot <- ggplot(figure2,
                       aes(
                           y=investor.portco,
                           x=investment.amount.tot/10^6,
                           color=factor(sbti.status),
                           size = avg.investment,
                           fill = investor.category)) +
  geom_jitter(stroke=0.7, width = 0.02,shape=21,alpha=0.9) +
  #scale_alpha(range = c(0.8,0.6)) +
  scale_radius(range = c(3, 10)) +
  scale_fill_manual(values = (c("#FF0000" ,"#009e73" ,"#e69f00", "#cc79a7", "#0072b2","#56b4e9","grey30","grey70"))) +
  scale_color_manual(values = c("SBTI" = "black", "0"="white")) +
  # scale_shape_manual(values = c(21,22)) +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  guides(fill = guide_legend(override.aes = list(size=5))) +
  theme_bw() +
  theme(legend.position="right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


plot(figure2.plot) 

# ggsave(
#   (paste(getwd(),"/figures/figure2.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 9,
#   height = 5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )


##-------------------------------------------------------------------------
##                              SI Comparison table                       -
##                                                                        -
##-------------------------------------------------------------------------


investments.analysis.consolidated.total<-investments.analysis %>%
  unique() %>%
  mutate(investor.category = ifelse(is.na(investor.category),as.character(investor.type.edited),investor.category)) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year>=2005) %>%
  filter(investment.type.revised.basic=="equity") %>%
  select(-investment.type.revised.detail) %>%
  mutate(investment.year=as.integer(investment.year)) %>%
  group_by(investment.year) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm=FALSE)/10^9) %>%
  ungroup()%>%
  select(investment.year,investment.amount.tot) %>%
  unique()

investments.analysis.consolidated.table<-investments.analysis %>%
  unique() %>%
  mutate(investor.category = ifelse(is.na(investor.category),as.character(investor.type.edited),investor.category)) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year>=2005) %>%
  filter(investment.type.revised.basic=="equity") %>%
  mutate(investment.year=as.integer(investment.year)) %>%
  group_by(investment.year, investor.type.edited ) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm=FALSE)/10^9) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  ungroup()%>%
  select(investment.year,investment.amount.tot, investor.portco,investor.type.edited) %>%
  unique()

#write.csv(investments.analysis.consolidated.table,paste(getwd(),"/output/investors_consolidated_20220504.csv",sep=""),fileEncoding = "UTF-8")

##-------------------------------------------------------------------------
##                             Investor concentratio                      -
##                                                                        -
##-------------------------------------------------------------------------

investors.concentration <-investments.analysis %>%
  unique() %>%
  filter(investor.type.edited %in% c("corporation or corporate venture","venture capital")) %>%
  filter(investment.type.revised.basic=="equity") %>%
  filter(investment.amount.revised>0) %>%
  filter(is.na(investment.year)==FALSE) %>%
  filter((investment.year)>=2016) %>%
  select(investor.firm.renamed,company,investment.type,investment.amount.revised,investor.type.edited) %>%
  unique() %>%
  group_by(investor.firm.renamed) %>%
  mutate(investor.inv = sum(investment.amount.revised, na.rm=FALSE)/10^6) %>%
  mutate(investment.n = as.numeric(n_distinct(company))) %>%
  ungroup() %>%
  select(investor.firm.renamed,investor.inv,investment.n, investor.type.edited) %>%
  unique() %>%
  mutate(investor.annualinv.cut = cut(investor.inv, breaks = c(0, 10,100,1000,5000))) %>%
  mutate(investment.cut = cut(investment.n, breaks = c(0, 10,25,50))) %>%
  group_by(investor.annualinv.cut,investor.type.edited) %>%
  mutate(investor.n = as.numeric(n_distinct(investor.firm.renamed))) %>%
  mutate(investor.inv.tot = sum(investor.inv)) %>%
  ungroup()


investors.concentration.plot<-ggplot(investors.concentration, aes(y=investor.inv, x=investor.annualinv.cut, fill=investment.cut)) + 
  geom_bar(position="stack", stat="identity") +
  #geom_text(aes(label = investor.n, vjust=-0.2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,20000),breaks=seq(0, 20000,5000)) +
  scale_fill_manual(values = (rev(c("#FF0000" ,"#009e73" ,"#e69f00")))) +
  theme_bw()+
  theme(legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank()) +
  facet_wrap(~investor.type.edited)

plot(investors.concentration.plot)  

ggsave(
  (paste(getwd(),"/figures/figureSI2x.pdf",sep="")),
  plot = last_plot(),
  device = NULL,
  path = NULL,
  #scale = 1,
  width = 9,
  height = 5,
  useDingbats = FALSE,
  units = c("in"),
  dpi = 300,
  limitsize = TRUE,
)


