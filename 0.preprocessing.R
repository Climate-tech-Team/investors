library(tidyverse)
library(readxl)
library(stringr)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 0:                             ###
###                    DATA INPUT AND PRE-PROCESSING                    ###
###                                                                     ###
###########################################################################
###########################################################################
##-------------------------------------------------------------------------
##            0a.  i3 raw data: investors                                 -
##                  Downloaded 01.19.2022                                 -
##-------------------------------------------------------------------------

#cleaning up, lower case
investors.i3 <-
  read_excel(
    paste(
      getwd(),
      "/input/i3raw/20220119_i3rawdata/investors-export-18921.xls",
      sep = ""
    ),
    na = ""
  ) %>%
  mutate_all(tolower) %>%
  dplyr::rename_all(funs(make.names(.))) %>%
  rename_all(tolower) %>%
  select(investment.firm, investor.type, state, country) %>%
  unique() %>%
  rename ("investor.firm" = "investment.firm") %>%
  rename("investor.state" = "state") %>%
  rename("investor.country" = "country")

##-------------------------------------------------------------------------
##              0b. i3 raw data: investments                              -
##                  Downloaded 01.19.2022                                 -
##-------------------------------------------------------------------------

#cleaning up, selecting relevant information

investments.i3 <-
  read_excel(
    paste(
      getwd(),
      "/input/i3raw/20220119_i3rawdata/investors-export-18920.xls",
      sep = ""
    ),
    sheet = 2,
    na = ""
  ) %>%
  mutate_all(tolower) %>%
  dplyr::rename_all(funs(make.names(.))) %>%
  rename_all(tolower) %>%
  rename ("investor.firm" = "investor") %>%
  rename ("investment.amount" = "investment.amount....") %>%
  rename ("total.paid.in.capital" = "total.paid.in.capital....") %>%
  select(
    investor.firm,
    company,
    i3.url,
    year.founded,
    industry.group,
    sector,
    tags,
    short.description,
    state,
    country,
    region,
    investment.type,
    investment.year,
    investment.date,
    investment.amount,
    total.paid.in.capital,
    year.founded,
    ticker.symbol
  )

##-------------------------------------------------------------------------
##        0c. Verifying founding year and updating missing data           -
## Sources: Kurowski and Doblinger (TU Munich); Authors' team             -                                             -
##-------------------------------------------------------------------------

investments.combined <-
  left_join(investments.i3, investors.i3, by = "investor.firm") %>%
  mutate(year.founded = ifelse(
    year.founded > 2022 |
      is.na(year.founded) == TRUE,
    2100,
    year.founded
  ))


# data verified by collaborators
startup.year1 <-
  read_excel(paste(getwd(), "/input/foundingyr/year_kurowski.xls", sep = ""),
             na = "") %>%
  mutate_all(tolower) %>%
  select(startup, yearfounded) %>%
  unique() %>%
  rename("company" = "startup") %>%
  rename("year.founded.update" = "yearfounded")

# missing data verified by authors' team
startup.year2 <-
  read_excel(paste(getwd(), "/input/foundingyr/year_authors_2022.xlsx", sep =
                     ""),
             na = "") %>%
  mutate_all(tolower) %>%
  select(company, year.founded.update) %>%
  mutate(year.founded.update = as.numeric(year.founded.update)) %>%
  na.omit() %>%
  unique()

startup.year <- rbind(startup.year1, startup.year2) %>%
  filter(year.founded.update != 2100) #2100 is the default when founding year could not be verified

remove(startup.year1, startup.year2)

#this file will be updated and verified for founding years after removing excluded companies in 0d
#write.csv(startups.noyear,paste(getwd(),"/input/foundingyr/year_authors2.csv",sep=""),fileEncoding = "UTF-8")

investments.combined <-
  left_join(investments.combined, startup.year) %>%
  mutate(year.founded = ifelse(
    is.na(year.founded.update) == TRUE,
    year.founded,
    year.founded.update
  )) %>%
  mutate(year.founded = ifelse(is.na(year.founded) == TRUE, 2100, year.founded)) %>%
  select(-year.founded.update)

remove(startup.year)

##-------------------------------------------------------------------------
##            0d.  Removing companies that are not relevant               -                                       -
##-------------------------------------------------------------------------

investments.combined <- investments.combined %>%
  mutate(tags.filter = paste(tags, short.description))


#tags or description to be excluded (or included) if not explicitly related to clean energy or climate change ------
excluded.tags <-
  "ride-hailing|valet service|grocery delivery|delivering restaurant meals|delivery platform|delivery service|autonomous checkout|logistics platform|healthy recipe|consumer food sourcing|restaurant operations|restaurant service|vegetable delivery|fruit delivery|restaurant delivery|restaurant|catering solution|sharing economy|kitchen|hailing app|insurance|autonomous driving|freight logistics|mobile payment service|robotics|e-commerce|e commerce|digital wallet|marketplace|ride sharing|taxi/private|car sharing|agriculture & food|consulting service"
included.tags <-
  "electric vehicle|electric truck|electric scooter|electric bike|precision agriculture|recycling|recycle|waste|energy efficiency|oil & gas|conventional fuels|carbon capture|ccus|photovoltaic|wind|power grid|smart grid|oil and gas|fossil fuel|advanced materials|biofuel|bioenergy|geothermal|hydrogen|energy & power|biomass|green building|efficient construction|direct air capture|pollution|solar|electric mobility|carbon account|carbon removal|negative emission|carbon negative|ethanol|biofuel|renewable|concrete|cement|energy saving|energy storage|battery|fuel cell|recycl|hydrogen|pipeline|solar|aviation|airplane|methane|nuclear|public transit|public transport|sustainable protein|food waste|waste heat|clean meat|fertilizer|crop enhancement|vertical farm|nitrogen oxides|carbon dioxide|gas leak|industrial biotech|solar|wind|no carbon|low carbon|zero carbon|carbon neutral|chp|biomass|biofuel|bioenergy|geothermal|electrolyzer|steel|aluminum|aluminium|NH3|ammonia|ev batteries|ev battery|climate change|carbon offset|cultivated meat|forest|bioreactor|alternative meat|alternative protein|meat substitute|climate"


#companies excluded based on keywords----
investments.exclude <- investments.combined %>%
  filter(str_detect(tags.filter, excluded.tags)) %>%
  select(
    company,
    industry.group,
    sector,
    tags,
    tags.filter,
    short.description,
    year.founded,
    region,
    ticker.symbol
  ) %>%
  unique() %>%
  filter(!str_detect(tags.filter, included.tags))


#competitors of these companies based on keywords----
tmp0 <-
  read_excel(
    paste(
      getwd(),
      "/input/i3raw/20220119_i3rawdata/companies-export-18919.xls",
      sep = ""
    ),
    "Competitors",
    na = ""
  ) %>%
  mutate_all(tolower) %>%
  dplyr::rename_all(funs(make.names(.))) %>%
  rename_all(tolower) %>%
  select(company, competitor) %>%
  rename("c1" = "company") %>%
  rename("c2" = "competitor")

tmp1 <- left_join(investments.exclude, tmp0, by = c("company" = "c1")) %>%
  filter(!is.na(c2)) %>%
  select(c2) %>%
  unique() %>%
  rename("company" = "c2")

tmp2 <- left_join(investments.exclude, tmp0, by = c("company" = "c2")) %>%
  filter(!is.na(c1)) %>%
  select(c1) %>%
  unique() %>%
  rename("company" = "c1")

tmp <- (rbind(tmp1, tmp2)) %>%
  unique()

investments.exclude <- investments.exclude %>%
  select(company) %>%
  rbind(tmp) %>%
  unique() %>%
  left_join(investments.i3) %>%
  mutate(tags.filter = paste(tags, short.description)) %>%
  select(
    company,
    industry.group,
    sector,
    tags,
    tags.filter,
    short.description,
    year.founded,
    region,
    ticker.symbol
  ) %>%
  unique() %>%
  filter(!str_detect(tags.filter, included.tags)) %>%
  mutate(reason = "competitors of companies, not relevant for cleantech")

remove(tmp0, tmp1, tmp2, tmp)

#companies excluded because the sector is "other cleantech" or the industry group is "enabling technologies" or unspecified and not explicitly linked to climate------

investments.other <- investments.combined %>%
  select(
    company,
    industry.group,
    sector,
    tags,
    tags.filter,
    short.description,
    year.founded,
    region,
    ticker.symbol
  ) %>%
  filter(
    sector == "other cleantech" |
      is.na(industry.group) == TRUE |
      industry.group == "enabling technologies"
  ) %>%
  filter(!str_detect(tags.filter, included.tags)) %>%
  unique() %>%
  mutate(reason = "unverified other cleantech")

investments.exclude <- investments.exclude %>%
  rbind(investments.other)

remove(investments.other)

#companies excluded because both year and sector or industry are unspecified------

investments.unspecified <- investments.combined %>%
  select(
    company,
    industry.group,
    sector,
    tags,
    tags.filter,
    short.description,
    year.founded,
    region,
    ticker.symbol
  ) %>%
  filter(year.founded == 2100) %>%
  filter(
    is.na(sector) == TRUE | sector == ("other cleantech") |
      is.na(industry.group) == TRUE |
      industry.group == ("enabling technologies")
  ) %>%
  filter(!str_detect(tags.filter, included.tags)) %>%
  unique() %>%
  mutate(reason = "founding year unspecified - likely too old or not relevant")

investments.exclude <- investments.exclude %>%
  rbind(investments.unspecified)

remove(investments.unspecified)


#companies excluded because they are likely part of a larger group------

investments.group <- investments.combined %>%
  select(
    company,
    industry.group,
    sector,
    tags,
    tags.filter,
    short.description,
    year.founded,
    region,
    ticker.symbol
  ) %>%
  filter(year.founded == 2100) %>%
  filter(str_detect(company, c("group|corporation"))) %>%
  unique() %>%
  mutate(reason = "group or large corporation")

investments.exclude <- investments.exclude %>%
  rbind(investments.group)

remove(investments.group)


#companies excluded because of geographic zone, keeping only north america, europe, israel------

investments.region <- investments.combined %>%
  select(
    company,
    industry.group,
    sector,
    tags,
    tags.filter,
    short.description,
    year.founded,
    region,
    ticker.symbol
  ) %>%
  filter(!region %in% c("north america", "europe & israel")) %>%
  unique() %>%
  mutate(reason = "region out of scope")

investments.exclude <- investments.exclude %>%
  rbind(investments.region)

remove(investments.region)

#companies excluded because they were founded before 2005 or report no year------

investments.old <- investments.combined %>%
  select(
    company,
    industry.group,
    sector,
    tags,
    tags.filter,
    short.description,
    year.founded,
    region,
    ticker.symbol
  ) %>%
  filter(year.founded < 2005 | year.founded == 2100) %>%
  unique() %>%
  mutate(reason = "older firms, pre-Kyoto")

investments.exclude <- investments.exclude %>%
  rbind(investments.old)

remove(investments.old)


#companies excluded because they are universities or research institutes or public agencies------

investments.unis <- investments.combined %>%
  select(
    company,
    industry.group,
    sector,
    tags,
    tags.filter,
    short.description,
    year.founded,
    region,
    ticker.symbol
  ) %>%
  filter(str_detect(
    company,
    c(
      "university|research institute|institute of technology|polytechnique|polytechnic|national laboratory|research laboratory|government-owned|public agency|public utility|research center|research centre|not-for-profit|not for profit|non-profit"
    )
  ) |
    str_detect(
      short.description,
      c(
        "research institute|institute of technology|polytechnique|polytechnic|national laboratory|research laboratory|government-owned|public agency|public utility|research center|research centre|not-for-profit|not for profit|non-profit"
      )
    )) %>%
  unique() %>%
  mutate(reason = "university or research institute or public agency")

investments.exclude <- investments.exclude %>%
  rbind(investments.unis)

remove(investments.unis)


#companies excluded because they are irrelevant -- manual changes-----

manual.edits <-
  "hippo insurance|pecan street|roomorama|scale ai|mapr technologies|top flight technologies|intelligent layer|8 rivers capital|rappaport energy consulting|exxon mobil"
investments.manual <- investments.combined %>%
  filter(str_detect(company, manual.edits)) %>%
  select(
    company,
    industry.group,
    sector,
    tags,
    tags.filter,
    short.description,
    year.founded,
    region,
    ticker.symbol
  ) %>%
  unique() %>%
  mutate(reason = "manual removal by team")

investments.exclude <- investments.exclude %>%
  rbind(investments.manual)

remove(investments.manual, manual.edits)

##-------------------------------------------------------------------------
##             0e.  Export list of companies to exclude                   -
##-------------------------------------------------------------------------

remove(investments.i3, investors.i3)

investments.exclude <- investments.exclude %>%
  #select(-reason) %>%
  unique()

write.csv(
  investments.exclude,
  paste(getwd(), "/exclude/companies-exclude-20220805.csv", sep = ""),
  fileEncoding = "UTF-8"
)
