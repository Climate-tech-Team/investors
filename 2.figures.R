library(tidyverse)
library(readxl)
library(ggplot2)
library(stringr)
library(ggnewscale)
library(wesanderson)
library(forcats)
library(scales)
library(ggrepel)


Sys.setlocale("LC_ALL", "en_US.UTF-8")

###########################################################################
###########################################################################
###                                                                     ###
###                         CHARTS AND ANALYSIS                         ###
###                                                                     ###
###########################################################################
###########################################################################


###Read data for figures and other analysis
investors <-
  read.csv(paste(getwd(), "/output/investors_20220805.csv", sep = ""),
           fileEncoding = "UTF-8") %>%
  select(-X) %>%
  mutate(sector = ifelse(is.na(sector), "uncategorized", (as.character(sector))))

#apply filters for charts
investments.analysis <- investors %>%
  filter(
    !is.na(investment.type) &
      !investor.type.edited %in% c(
        "other",
        "not an investor",
        "accelerator or incubator",
        "university",
        "crowdfunding"
      ),
    investment.year >= 2005 & investment.year <= 2021
  ) %>%
  mutate(
    investment.type.revised.basic = ifelse(investment.type == "grant", "grant", "equity"),
    investor.type.edited = ifelse(
      investor.type.edited %in% c("family", "angel investor"),
      "angel or family",
      as.character(investor.type.edited)
    ),
    investment.type.revised.detail = str_replace(investment.type, c("series a|series b"), "series ab"),
    investment.type.revised.detail = fct_relevel(
      investment.type.revised.detail,
      "grant",
      "seed",
      "series ab",
      "growth equity"
    ),
    investment.type.revised = paste(investor.type.edited, investment.type.revised.basic),
    investment.type = fct_relevel(
      investment.type,
      "grant",
      "seed",
      "series a",
      "series b",
      "growth equity"
    ),
    investor.type.edited = fct_relevel(
      investor.type.edited,
      "public or quasi public",
      "corporation or corporate venture",
      "venture capital",
      "private equity",
      "financial sector",
      "angel or family"
    )
  ) %>%
  select(
    investor.id,
    investor.firm.renamed,
    investment.type.revised,
    investment.type.revised.basic,
    investment.type.revised.detail,
    investor.country.edited,
    i3.url,
    investor.type.edited,
    company,
    sector,
    tags,
    short.description,
    industry.group,
    investment.year,
    investment.amount,
    investment.amount.revised,
    investment.type,
    investment.year,
    investor.category,
    sbti.status,
    country
  )


#write.csv(investments.analysis,paste(getwd(),"/output/investors_filtered_20220805.csv",sep=""),fileEncoding = "UTF-8")

#generating colors
mypal <- c("black", "#ff0000", "#123991", "#f46d43", "#1d91c0", "#28cae0")


##-------------------------------------------------------------------------
##                              FIGURE 1a-b
##-------------------------------------------------------------------------

#consolidated data (aggregated by type of investor)

investments.analysis.consolidated <- investments.analysis %>%
  mutate(investor.category = ifelse(
    is.na(investor.category),
    as.character(investor.type.edited),
    investor.category
  )) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year >= 2005) %>%
  mutate(investment.year = as.integer(investment.year)) %>%
  group_by(investment.year, investment.type.revised) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  mutate(investor.deals = length(company)) %>%
  mutate(investor.count = n_distinct(investor.firm.renamed)) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm =
                                       FALSE) / 10 ^ 9) %>%
  ungroup()  %>%
  select(
    investment.type.revised,
    investment.type.revised.basic,
    investment.type.revised.detail,
    investor.type.edited,
    investment.year,
    investment.amount.tot,
    investor.portco
  ) %>%
  unique() %>%
  pivot_longer(6:7, names_to = "data.type")

figure1ab <- investments.analysis.consolidated

figure1ab.plot <- ggplot(figure1ab,
                         aes(x = investment.year,
                             y = value,
                             color = investor.type.edited)) +
  geom_line(aes(linetype = investment.type.revised.basic)) +
  geom_line(
    data = . %>% filter(
      investment.type.revised == "corporation or corporate venture equity"
    ),
    size = 1.5
  ) +
  scale_color_manual(values = mypal) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2005, 2021),
    breaks = seq(2006, 2020, 2)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_blank()
  ) +
  facet_wrap( ~ data.type, scales = "free_y")

plot(figure1ab.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figure1ab-Joule.pdf",sep="")),
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
##                          FIGURE 1c                                    -
##-------------------------------------------------------------------------

investments.analysis.consolidated <- investments.analysis %>%
  mutate(investor.category = ifelse(
    is.na(investor.category),
    as.character(investor.type.edited),
    investor.category
  )) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year >= 2005) %>%
  mutate(investment.year = as.integer(investment.year)) %>%
  group_by(investment.year,
           investment.type.revised.detail,
           investor.type.edited) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  mutate(investor.deals = length(company)) %>%
  mutate(investor.count = n_distinct(investor.firm.renamed)) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm =
                                       FALSE) / 10 ^ 9) %>%
  ungroup()  %>%
  select(
    investment.type.revised,
    investment.type.revised.basic,
    investment.type.revised.detail,
    investor.type.edited,
    investment.year,
    investment.amount.tot,
    investor.portco,
    investor.count
  ) %>%
  unique() %>%
  pivot_longer(6:8, names_to = "data.type")

figure1c <- investments.analysis.consolidated %>%
  filter(data.type == "investor.portco")

figure1c.plot <- ggplot(figure1c,
                        aes(x = investment.year,
                            y = value,
                            color = investor.type.edited)) +
  geom_line(aes(linetype = investment.type.revised.basic)) +
  geom_line(
    data = . %>% filter(
      investment.type.revised == "corporation or corporate venture equity"
    ),
    size = 1
  ) +
  scale_color_manual(values = mypal) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2005, 2021),
    breaks = seq(2006, 2021, 2)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_blank()
  ) +
  facet_wrap( ~ investment.type.revised.detail, scales = "fixed")

plot(figure1c.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figure2b-Joule.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 5.5,
#   height = 4.5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )

##-------------------------------------------------------------------------
##                          FIGURE 1d                                     -
##-------------------------------------------------------------------------

investments.analysis.consolidated <- investments.analysis %>%
  mutate(investor.category = ifelse(
    is.na(investor.category),
    as.character(investor.type.edited),
    investor.category
  )) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year >= 2005) %>%
  mutate(investment.year = as.integer(investment.year)) %>%
  group_by(investment.year,
           investment.type.revised.detail,
           investor.type.edited) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  mutate(investor.deals = length(company)) %>%
  mutate(investor.count = n_distinct(investor.firm.renamed)) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm =
                                       FALSE) / 10 ^ 9) %>%
  ungroup()  %>%
  select(
    investment.type.revised,
    investment.type.revised.basic,
    investment.type.revised.detail,
    investor.type.edited,
    investment.year,
    investment.amount.tot,
    investor.portco,
    investor.count
  ) %>%
  unique() %>%
  pivot_longer(6:8, names_to = "data.type")

figure1d <- investments.analysis.consolidated %>%
  filter(data.type == "investment.amount.tot")

figure1d.plot <- ggplot(figure1d,
                        aes(x = investment.year,
                            y = value,
                            color = investor.type.edited)) +
  geom_line(aes(linetype = investment.type.revised.basic)) +
  geom_line(
    data = . %>% filter(
      investment.type.revised == "corporation or corporate venture equity"
    ),
    size = 1
  ) +
  scale_color_manual(values = mypal) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2005, 2021),
    breaks = seq(2006, 2021, 2)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_blank()
  ) +
  facet_wrap( ~ investment.type.revised.detail, scales = "free_y")

plot(figure1d.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figure2a-Joule.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 5.5,
#   height = 4.5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )

##-------------------------------------------------------------------------
##                              FIGURE 2                                 -
##-------------------------------------------------------------------------

figure2 <- investments.analysis %>%
  mutate(
    investor.type.edited = ifelse(
      investor.type.edited %in% c("public or quasi public", "corporation or corporate venture"),
      as.character(investor.type.edited),
      "other investors"
    )
  ) %>%
  mutate(investor.category = ifelse(
    is.na(investor.category),
    investor.type.edited,
    as.character(investor.category)
  )) %>%
  filter(investment.year >= 2016) %>%
  group_by(investor.id) %>%
  mutate(
    investor.portco = n_distinct(company),
    investor.deals = length(company),
    investment.amount.tot = sum(investment.amount.revised, na.rm = TRUE)
  ) %>%
  mutate(avg.investment = investment.amount.tot / investor.deals) %>%
  ungroup() %>%
  mutate(sbti.status = ifelse(is.na(sbti.status), 0, "SBTI")) %>%
  filter(investor.portco >= 4, investment.amount.tot >= 100000) %>%
  mutate(investor.category = str_trim(investor.category)) %>%
  mutate(investor.category = as.factor(investor.category)) %>%
  mutate(
    investor.category = fct_relevel(
      investor.category,
      "fossil fuels and utilities",
      "transportation",
      "manufacturing, hardware, semiconductors, chemicals",
      "food, beverages & agriculture",
      "digital and financial services",
      "clean energy",
      "public or quasi public",
      "other investors"
    )
  )

figure2 <- figure2 %>%
  select(
    investor.id,
    investor.category,
    investor.firm.renamed,
    investor.portco,
    avg.investment,
    sbti.status,
    investment.amount.tot
  ) %>%
  unique()

# write.csv(figure3,paste(getwd(),"/output/investors-analyzed-20220805-dots.csv",sep=""),fileEncoding = "UTF-8")

#scales
scales <- list(
  scale_radius(range = c(3, 10)),
  scale_fill_manual(
    values = c(
      "#FF0000" ,
      "#009e73" ,
      "#e69f00",
      "#cc79a7",
      "#0072b2",
      "#56b4e9",
      "grey30",
      "grey70"
    )
  ),
  scale_color_manual(values = c("SBTI" = "black", "0" = "white")),
  scale_y_continuous(trans = 'log10'),
  scale_x_continuous(trans = 'log10'),
  guides(fill = guide_legend(override.aes = list(size = 5)))
)

#theme
theme_elements <- list(
  theme_bw(),
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
)

#plot
figure2.plot <-
  ggplot(
    figure2,
    aes(
      y = investor.portco,
      x = investment.amount.tot / 10 ^ 6,
      color = factor(sbti.status),
      size = avg.investment,
      fill = investor.category
    )
  ) +
  geom_jitter(
    stroke = 0.7,
    width = 0.02,
    shape = 21,
    alpha = 0.9
  ) +
  scales +
  theme_elements

plot(figure2.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figure2-Joule.pdf",sep="")),
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
##                              FIGURE 3                                 -
##                        (corporate investors)                 -
##-------------------------------------------------------------------------


figure3 <- investments.analysis %>%
  mutate(
    investor.type.edited = ifelse(
      investor.type.edited %in% c("public or quasi public", "corporation or corporate venture"),
      as.character(investor.type.edited),
      "other investors"
    )
  ) %>%
  mutate(investor.category = ifelse(
    is.na(investor.category),
    investor.type.edited,
    as.character(investor.category)
  )) %>%
  filter(investment.year >= 2016) %>%
  group_by(investor.id) %>%
  mutate(
    investor.portco = n_distinct(company),
    investor.deals = length(company),
    investment.amount.tot = sum(investment.amount.revised, na.rm = TRUE)
  ) %>%
  mutate(avg.investment = investment.amount.tot / investor.deals) %>%
  ungroup() %>%
  mutate(sbti.status = ifelse(is.na(sbti.status), 0, "SBTI")) %>%
  filter(investor.portco >= 4, investment.amount.tot >= 100000) %>%
  mutate(investor.category = str_trim(investor.category)) %>%
  mutate(investor.category = as.factor(investor.category)) %>%
  mutate(
    investor.category = fct_relevel(
      investor.category,
      "fossil fuels and utilities",
      "transportation",
      "manufacturing, hardware, semiconductors, chemicals",
      "food, beverages & agriculture",
      "digital and financial services",
      "clean energy",
      "public or quasi public",
      "other investors"
    )
  )



# ##-------------------------------------------------------------------------
# ##                              FIGURE 3                                 -
# ##                        (different investor priorities)                 -
# ##-------------------------------------------------------------------------

figure3a <- figure3 %>%
  mutate(investment.type.revised = investor.category) %>%
  #by type of investor and type of sector
  group_by(investment.type.revised, sector) %>%
  mutate(
    investorsector.portco = n_distinct(company),
    investorsector.deals = length(company),
    investorsector.amount.tot = sum(investment.amount.revised, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  #by type of sector only
  group_by(sector) %>%
  mutate(sector.portco = n_distinct(company)) %>%
  mutate(sector.deals = length(company)) %>%
  mutate(sector.amount.tot = sum(investment.amount.revised, na.rm = FALSE)) %>%
  ungroup() %>%
  #by type of investor only
  group_by(investment.type.revised) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  mutate(investor.deals = length(company)) %>%
  mutate(investor.amount.tot = sum(investment.amount.revised, na.rm = FALSE)) %>%
  ungroup() %>%
  #total without filters
  mutate(total.portco = n_distinct(company)) %>%
  mutate(total.deals = length(company)) %>%
  mutate(total.amount.tot = sum(investment.amount.revised, na.rm = FALSE)) %>%
  select(
    investment.type.revised,
    sector,
    investorsector.portco,
    investorsector.deals,
    investorsector.amount.tot,
    sector.portco,
    sector.deals,
    sector.amount.tot,
    investor.portco,
    investor.deals,
    investor.amount.tot,
    total.portco,
    total.deals,
    total.amount.tot
  ) %>%
  unique() %>%
  #calculating percentages
  mutate(sector.perc.portco = investorsector.portco / sector.portco) %>%
  mutate(sector.perc.deals = investorsector.deals / sector.deals) %>%
  mutate(sector.perc.amount = investorsector.amount.tot / sector.amount.tot) %>%
  mutate(overall.perc.portco = investor.portco / total.portco) %>%
  mutate(overall.perc.deals = investor.deals / total.deals) %>%
  mutate(overall.perc.amount = investor.amount.tot / total.amount.tot) %>%
  filter(!investment.type.revised %in% c("public or quasi public", "other investors"))


#data for comparing corporate priorities
figureSI2 <- figure3 %>%
  mutate(investment.type.revised = str_remove(investment.type.revised, "grant")) %>%
  mutate(investment.type.revised = str_remove(investment.type.revised, "equity")) %>%
  mutate(investment.type.revised = trimws(investment.type.revised)) %>%
  #by type of investor and type of sector
  group_by(investment.type.revised, sector) %>%
  mutate(
    investorsector.portco = n_distinct(company),
    investorsector.deals = length(company),
    investorsector.amount.tot = sum(investment.amount.revised, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  #by type of sector only
  group_by(sector) %>%
  mutate(sector.portco = n_distinct(company)) %>%
  mutate(sector.deals = length(company)) %>%
  mutate(sector.amount.tot = sum(investment.amount.revised, na.rm = FALSE)) %>%
  ungroup() %>%
  #by type of investor only
  group_by(investment.type.revised) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  mutate(investor.deals = length(company)) %>%
  mutate(investor.amount.tot = sum(investment.amount.revised, na.rm = FALSE)) %>%
  ungroup() %>%
  #total without filters
  mutate(total.portco = n_distinct(company)) %>%
  mutate(total.deals = length(company)) %>%
  mutate(total.amount.tot = sum(investment.amount.revised, na.rm = FALSE)) %>%
  select(
    investment.type.revised,
    sector,
    investorsector.portco,
    investorsector.deals,
    investorsector.amount.tot,
    sector.portco,
    sector.deals,
    sector.amount.tot,
    investor.portco,
    investor.deals,
    investor.amount.tot,
    total.portco,
    total.deals,
    total.amount.tot
  ) %>%
  unique() %>%
  #calculating percentages
  mutate(sector.perc.portco = investorsector.portco / sector.portco) %>%
  mutate(sector.perc.deals = investorsector.deals / sector.deals) %>%
  mutate(sector.perc.amount = investorsector.amount.tot / sector.amount.tot) %>%
  mutate(overall.perc.portco = investor.portco / total.portco) %>%
  mutate(overall.perc.deals = investor.deals / total.deals) %>%
  mutate(overall.perc.amount = investor.amount.tot / total.amount.tot) %>%
  mutate(
    investment.type.revised = fct_relevel(
      investment.type.revised,
      "public or quasi public",
      "angel or family",
      "financial sector",
      "private  equity",
      "venture capital",
      "corporation or corporate venture"
    )
  ) %>%
  mutate(sector = as.factor(sector))

#scales
scales <- list(
  scale_fill_manual(
    values = c("grey40", "#123991", "#f46d43", "#1d91c0", "#28cae0", "#ff0000")
  ),
  scale_x_discrete(expand = c(0, 0)),
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent
  )
)

#theme
theme_elements <- list(
  theme_bw(),
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
)


#SI2a = DEALS ----
#ordering of data
figure3a.order.deals <- figureSI2 %>%
  filter(investment.type.revised == "corporation or corporate venture") %>%
  select(sector, sector.perc.deals) %>%
  unique() %>%
  arrange(sector, desc(sector.perc.deals)) %>%
  mutate(sector = fct_reorder(sector, sector.perc.deals)) %>%
  select(sector) %>%
  mutate(sector.order = as.numeric(sector))

figure3a <- left_join(figure3a, figure3a.order.deals) %>%
  mutate(sector = fct_reorder(sector, as.integer(sector.order)))

intercept.3a.deals <- figureSI2 %>%
  filter(investment.type.revised == "corporation or corporate venture") %>%
  pull(overall.perc.deals) %>%
  unique()

figure3a.plot <-
  ggplot(figure3a, aes(
    y = investorsector.deals,
    x = sector,
    fill = (investment.type.revised)
  )) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("grey40", "#123991", "#f46d43", "#1d91c0", "#28cae0", "#ff0000")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_elements +
  coord_flip()

plot(figure3a.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figure3a-deals-Joule.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 5.5,
#   height = 5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )


#SI2a = AMOUNT ----

#ordering of data
figureSI2a.order.amount <- figureSI2 %>%
  filter(investment.type.revised == "corporation or corporate venture") %>%
  select(sector, sector.perc.amount) %>%
  unique() %>%
  arrange(sector, desc(sector.perc.amount)) %>%
  mutate(sector = fct_reorder(sector, sector.perc.amount)) %>%
  select(sector) %>%
  mutate(sector.order = as.numeric(sector))

figureSI2a <- left_join(figure3a, figureSI2a.order.amount) %>%
  mutate(sector = fct_reorder(sector, as.integer(sector.order)))

intercept.SI2a.amount <- figureSI2 %>%
  filter(investment.type.revised == "corporation or corporate venture") %>%
  pull(overall.perc.amount) %>%
  unique()

figureSI2a.plot <-
  ggplot(figureSI2a,
         aes(
           y = investorsector.amount.tot / 10 ^ 9,
           x = sector,
           fill = (investment.type.revised)
         )) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("grey40", "#123991", "#f46d43", "#1d91c0", "#28cae0", "#ff0000")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_elements +
  coord_flip()

plot(figureSI2a.plot)
#
# ggsave(
#   (paste(getwd(),"/figures/rev1/figureSI@a-amount-Joule.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 5.5,
#   height = 5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )
##-------------------------------------------------------------------------
##                              FIGURE 3b                                 -
##                        (different sectoral priorities)                 -
##-------------------------------------------------------------------------

#data for comparing corporate priorities
figure3b <- figure3 %>%
  mutate(investment.type.revised = investor.category) %>%
  #by type of investor and type of sector
  group_by(investment.type.revised, sector) %>%
  mutate(
    investorsector.portco = n_distinct(company),
    investorsector.deals = length(company),
    investorsector.amount.tot = sum(investment.amount.revised, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  #by type of sector only
  group_by(sector) %>%
  mutate(sector.portco = n_distinct(company)) %>%
  mutate(sector.deals = length(company)) %>%
  mutate(sector.amount.tot = sum(investment.amount.revised, na.rm = FALSE)) %>%
  ungroup() %>%
  #by type of investor only
  group_by(investment.type.revised) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  mutate(investor.deals = length(company)) %>%
  mutate(investor.amount.tot = sum(investment.amount.revised, na.rm = FALSE)) %>%
  ungroup() %>%
  #total without filters
  mutate(total.portco = n_distinct(company)) %>%
  mutate(total.deals = length(company)) %>%
  mutate(total.amount.tot = sum(investment.amount.revised, na.rm = FALSE)) %>%
  select(
    investment.type.revised,
    sector,
    investorsector.portco,
    investorsector.deals,
    investorsector.amount.tot,
    sector.portco,
    sector.deals,
    sector.amount.tot,
    investor.portco,
    investor.deals,
    investor.amount.tot,
    total.portco,
    total.deals,
    total.amount.tot
  ) %>%
  unique() %>%
  #calculating percentages
  mutate(sector.perc.portco = investorsector.portco / sector.portco) %>%
  mutate(sector.perc.deals = investorsector.deals / sector.deals) %>%
  mutate(sector.perc.amount = investorsector.amount.tot / sector.amount.tot) %>%
  mutate(overall.perc.portco = investor.portco / total.portco) %>%
  mutate(overall.perc.deals = investor.deals / total.deals) %>%
  mutate(overall.perc.amount = investor.amount.tot / total.amount.tot) %>%
  filter(!investment.type.revised %in% c("public or quasi public", "other investors"))


#scales
scales <- list(
  scale_fill_manual(
    values = c(
      "#FF0000" ,
      "#009e73" ,
      "#e69f00",
      "#cc79a7",
      "#0072b2",
      "#56b4e9",
      "grey30",
      "grey70"
    )
  ),
  scale_color_manual(
    values = c(
      "#FF0000" ,
      "#009e73" ,
      "#e69f00",
      "#cc79a7",
      "#0072b2",
      "#56b4e9",
      "grey30",
      "grey70"
    )
  ),
  scale_x_discrete(expand = c(0, 0)),
  scale_y_continuous(expand = c(0, 0), labels = scales::percent)
)

#theme
theme_elements <- list(
  theme_bw(),
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
)

###DEALS ----
#ordering of data
figure3b.deals <- left_join(figure3b, figure3a.order.deals) %>%
  mutate(sector = fct_reorder(sector, as.integer(sector.order)))

#plot by deals
figure3b.deals.plot <-
  ggplot(
    figure3b.deals,
    aes(
      y = sector.perc.deals,
      x = sector,
      fill = (investment.type.revised),
      label = sector.perc.deals
    )
  ) +
  geom_col(position = "stack") +
  #geom_text(size = 3, mapping=NULL)  +
  scales +
  theme_elements +
  coord_flip() +
  geom_hline(data = figure3b.deals,
             aes(yintercept = intercept.3a.deals),
             linetype = "dashed")

plot(figure3b.deals.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figure3b-Joule.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 5.5,
#   height = 5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )

###AMOUNT ----
#ordering of data
figure3b.amount <- left_join(figure3b, figureSI2a.order.amount) %>%
  mutate(sector = fct_reorder(sector, as.integer(sector.order)))

#plot by deals
figure3b.amount.plot <-
  ggplot(figure3b.amount, aes(
    y = sector.perc.amount,
    x = sector,
    fill = (investment.type.revised)
  )) +
  geom_col(position = "stack") +
  #geom_text(size = 3, mapping=NULL)  +
  scales +
  theme_elements +
  coord_flip() +
  geom_hline(data = figure3b.amount,
             aes(yintercept = intercept.SI2a.amount),
             linetype = "dashed")

plot(figure3b.amount.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figure3b-SI-Joule.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 5.5,
#   height = 5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )

##-------------------------------------------------------------------------
##                              Figure 3c                                 -
##                                                                        -
##-------------------------------------------------------------------------

figureSI3 <- figure3b %>%
  select(
    investment.type.revised,
    sector,
    investorsector.amount.tot,
    investorsector.deals,
    investor.portco,
    sector.perc.amount,
    sector.perc.deals,
    investor.deals,
    investor.amount.tot
  ) %>%
  unique()

scales <- list(
  scale_fill_manual(
    values = c(
      "#FF0000" ,
      "#009e73" ,
      "#e69f00",
      "#cc79a7",
      "#0072b2",
      "#56b4e9",
      "grey30",
      "grey70"
    )
  ),
  scale_color_manual(
    values = c(
      "#FF0000" ,
      "#009e73" ,
      "#e69f00",
      "#cc79a7",
      "#0072b2",
      "#56b4e9",
      "grey30",
      "grey70"
    )
  ),
  scale_x_discrete(expand = c(0, 0)),
  scale_y_continuous(expand = c(0, 0))
)

###DEALS ----

#ordering of data
figureSI3a.order.deals <- figureSI3 %>%
  select(investment.type.revised, investor.deals) %>%
  unique() %>%
  arrange(investment.type.revised, desc(investor.deals)) %>%
  mutate(investment.type.revised = fct_reorder(investment.type.revised, investor.deals)) %>%
  select(investment.type.revised) %>%
  mutate(investment.type.order = as.numeric(investment.type.revised))

figureSI3a.deals <- left_join(figureSI3, figureSI3a.order.deals) %>%
  mutate(investment.type.revised = fct_reorder(investment.type.revised, as.integer(investment.type.order))) %>%
  select(investment.type.revised, investor.deals) %>%
  unique()

#plot by deals
figureSI3a.deals.plot <-
  ggplot(
    figureSI3a.deals,
    aes(y = investor.deals, x = investment.type.revised, fill = investment.type.revised)
  ) +
  geom_col(position = "stack") +
  #geom_text(size = 3, mapping=NULL)  +
  scales +
  theme_elements +
  coord_flip()

plot(figureSI3a.deals.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figureSI3a-deals-Joule.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width = 7,
#   height = 5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )

###AMOUNT ----

#ordering of data
figureSI3b.order.amount <- figureSI3 %>%
  select(investment.type.revised, investor.amount.tot) %>%
  unique() %>%
  arrange(investment.type.revised, desc(investor.amount.tot)) %>%
  mutate(investment.type.revised = fct_reorder(investment.type.revised, investor.amount.tot)) %>%
  select(investment.type.revised) %>%
  mutate(investment.type.order = as.numeric(investment.type.revised))

figureSI3b.amount <- left_join(figureSI3, figureSI3.order.amount) %>%
  mutate(investment.type.revised = fct_reorder(investment.type.revised, as.integer(investment.type.order))) %>%
  select(investment.type.revised, investor.amount.tot) %>%
  unique()


#plot by amount
figureSI3b.amount.plot <-
  ggplot(
    figureSI3b.amount,
    aes(y = investor.amount.tot / 10 ^ 9, x = investment.type.revised, fill = investment.type.revised)
  ) +
  geom_col(position = "stack") +
  #geom_text(size = 3, mapping=NULL)  +
  scales +
  theme_elements +
  coord_flip()

plot(figureSI3b.amount.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figureSI3b-amount-Joule.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width =7,
#   height = 5,
#   useDingbats = FALSE,
#   units = c("in"),
#   dpi = 300,
#   limitsize = TRUE,
# )


###PORTCO ----

#ordering of data
figureSI3c.order.portco <- figureSI3 %>%
  select(investment.type.revised, investor.portco) %>%
  unique() %>%
  arrange(investment.type.revised, desc(investor.portco)) %>%
  mutate(investment.type.revised = fct_reorder(investment.type.revised, investor.portco)) %>%
  select(investment.type.revised) %>%
  mutate(investment.type.order = as.numeric(investment.type.revised))

figureSI3c.portco <- left_join(figureSI3, figureSI3c.order.portco) %>%
  mutate(investment.type.revised = fct_reorder(investment.type.revised, as.integer(investment.type.order))) %>%
  select(investment.type.revised, investor.portco) %>%
  unique()


#plot by amount
figureSI3c.portco.plot <-
  ggplot(
    figureSI3c.portco,
    aes(y = investor.portco, x = investment.type.revised, fill = investment.type.revised)
  ) +
  geom_col(position = "stack") +
  #geom_text(size = 3, mapping=NULL)  +
  scales +
  theme_elements +
  coord_flip()

plot(figureSI3c.portco.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figureSI3c-portco-Joule.pdf",sep="")),
#   plot = last_plot(),
#   device = NULL,
#   path = NULL,
#   #scale = 1,
#   width =7,
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


investments.analysis.consolidated.total <- investments.analysis %>%
  unique() %>%
  mutate(investor.category = ifelse(
    is.na(investor.category),
    as.character(investor.type.edited),
    investor.category
  )) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year >= 2005) %>%
  filter(investment.type.revised.basic == "equity") %>%
  select(-investment.type.revised.detail) %>%
  mutate(investment.year = as.integer(investment.year)) %>%
  group_by(investment.year) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm =
                                       FALSE) / 10 ^ 9) %>%
  ungroup() %>%
  select(investment.year, investment.amount.tot) %>%
  unique()

investments.analysis.consolidated.table <- investments.analysis %>%
  unique() %>%
  mutate(investor.category = ifelse(
    is.na(investor.category),
    as.character(investor.type.edited),
    investor.category
  )) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year >= 2005) %>%
  #filter(investment.type.revised.basic=="equity") %>%
  mutate(investment.year = as.integer(investment.year)) %>%
  group_by(investment.year,
           investor.type.edited,
           investment.type.revised.basic) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm =
                                       FALSE) / 10 ^ 9) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  ungroup() %>%
  select(
    investment.year,
    investment.amount.tot,
    investor.portco,
    investor.type.edited,
    investment.type.revised.basic
  ) %>%
  unique()

investments.analysis.consolidated.unique <- investments.analysis %>%
  unique() %>%
  mutate(investor.category = ifelse(
    is.na(investor.category),
    as.character(investor.type.edited),
    investor.category
  )) %>%
  filter(!investor.type.edited %in% c("other")) %>%
  filter(investment.year >= 2005) %>%
  #filter(investment.type.revised.basic=="equity") %>%
  mutate(investment.year = as.integer(investment.year)) %>%
  group_by(investment.year) %>%
  mutate(investment.amount.tot = sum(investment.amount.revised, na.rm =
                                       FALSE) / 10 ^ 9) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  ungroup() %>%
  select(investment.year, investment.amount.tot, investor.portco) %>%
  unique()



#write.csv(investments.analysis.consolidated.table,paste(getwd(),"/output/investors_consolidated_20220805.csv",sep=""),fileEncoding = "UTF-8")
#write.csv(investments.analysis.consolidated.unique,paste(getwd(),"/output/investors_consolidatedsum_20220805.csv",sep=""),fileEncoding = "UTF-8")

##-------------------------------------------------------------------------
##                             Investor concentration (SI1)               -
##                                                                        -
##-------------------------------------------------------------------------

investors.concentration <- investments.analysis %>%
  unique() %>%
  filter(investor.type.edited %in% c("corporation or corporate venture", "venture capital")) %>%
  filter(investment.type.revised.basic == "equity") %>%
  filter(investment.amount.revised > 0) %>%
  filter(is.na(investment.year) == FALSE) %>%
  filter((investment.year) >= 2016) %>%
  select(
    investor.firm.renamed,
    company,
    investment.type,
    investment.amount.revised,
    investor.type.edited
  ) %>%
  unique() %>%
  group_by(investor.firm.renamed) %>%
  mutate(investor.inv = sum(investment.amount.revised, na.rm = FALSE) /
           10 ^ 6) %>%
  mutate(investment.n = as.numeric(n_distinct(company))) %>%
  ungroup() %>%
  select(investor.firm.renamed,
         investor.inv,
         investment.n,
         investor.type.edited) %>%
  unique() %>%
  mutate(investor.annualinv.cut = cut(investor.inv, breaks = c(0, 10, 100, 1000, 5000))) %>%
  mutate(investment.cut = cut(investment.n, breaks = c(0, 10, 25, 50))) %>%
  group_by(investor.annualinv.cut, investor.type.edited) %>%
  mutate(investor.n = as.numeric(n_distinct(investor.firm.renamed))) %>%
  mutate(investor.inv.tot = sum(investor.inv)) %>%
  ungroup()


investors.concentration.plot <-
  ggplot(
    investors.concentration,
    aes(y = investor.inv, x = investor.annualinv.cut, fill = investment.cut)
  ) +
  geom_bar(position = "stack", stat = "identity") +
  #geom_text(aes(label = investor.n, vjust=-0.2)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 20000),
    breaks = seq(0, 20000, 5000)
  ) +
  scale_fill_manual(values = (rev(c(
    "#FF0000" , "#009e73" , "#e69f00"
  )))) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_blank()
  ) +
  facet_wrap( ~ investor.type.edited)

plot(investors.concentration.plot)

# ggsave(
#   (paste(getwd(),"/figures/rev1/figureSI1-Joule.pdf",sep="")),
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
#
