library(tidyverse)
library(readxl)
library(stringr)


Sys.setlocale("LC_ALL", "en_US.UTF-8")

###########################################################################
###########################################################################
###                                                                     ###
###                             . SECTION 1:                            ###
###                    DATA INPUT AND INITIALIZATION                    ###
###                                                                     ###
###########################################################################
###########################################################################


##-------------------------------------------------------------------------
##            1a.  i3 raw data: investors                                 -
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
##              1b. i3 raw data: investments                              -
##                  Downloaded 01.19.2022                                 -
##        Matching investors with details on their transactions           -
##-------------------------------------------------------------------------

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

#clean up: removing excluded companies-----
investments.exclude <-
  read.csv(paste(getwd(), "/exclude/companies-exclude-20220805.csv", sep =
                   ""),
           fileEncoding = "UTF-8") %>%
  select(-X) %>%
  select(-reason) %>%
  unique() %>%
  select(company, short.description, region) %>%
  unique() %>%
  mutate(exclude.include = "exclude")

investments.combined <-
  left_join(investments.i3, investments.exclude) %>%
  filter(is.na(exclude.include) == TRUE) %>%
  select(-exclude.include) %>%
  unique()

#clean up: update year of remaining companies-----
# data verified by TU Munich team
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

investments.combined <-
  left_join(investments.combined, startup.year) %>%
  mutate(year.founded.new = ifelse(
    is.na(year.founded.update) == TRUE,
    year.founded,
    year.founded.update
  )) %>%
  mutate(year.founded = year.founded.new) %>%
  select(-year.founded.new, -year.founded.update) %>%
  mutate(year.founded = ifelse(is.na(year.founded) == TRUE, 2100, year.founded))

remove(startup.year, investments.exclude)

#updating investment amounts
investments.combined <- investments.combined %>%
  mutate(investment.amount = as.numeric(investment.amount))


#applying filters-----

investments.combined <- investments.combined %>%
  mutate(investment.amount = as.numeric(investment.amount)) %>%
  mutate(investment.year = as.numeric(investment.year)) %>%
  mutate(company = as.factor((company))) %>%
  unique() %>%
  #filters
  filter(investment.type %in% c("series a", "seed", "grant", "series b", "growth equity")) %>%
  add_count(company, investment.year) %>% #number of investments per year
  group_by(company, investment.type, investment.date) %>%
  mutate(investment.amount.revised = investment.amount / n_distinct(investor.firm)) %>%
  ungroup() %>%
  group_by(company) %>%
  mutate(pre2005investments.n = sum(investment.year < 2005, na.rm = TRUE)) %>%
  mutate(post2005investments.n = sum(investment.year >= 2005, na.rm = TRUE)) %>%
  mutate(pre2005investments.amt = sum(investment.amount.revised[investment.year <
                                                                  2005], na.rm = TRUE)) %>%
  mutate(post2005investments.amt = sum(investment.amount.revised[investment.year >=
                                                                   2005], na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pre2005investments.n == 0) %>%
  filter(pre2005investments.amt == 0) %>%
  filter(post2005investments.n >= 1) %>%
  filter(investment.year >= 2005 & investment.year <= 2021) %>%
  left_join(investors.i3)

#for manual cleaning and checking firms and investments
firms.list <- investments.combined %>%
  select(
    company,
    i3.url,
    year.founded,
    state,
    country,
    region,
    sector,
    industry.group,
    tags,
    short.description,
    post2005investments.n,
    post2005investments.amt
  ) %>%
  unique()

investors.list <- investments.combined %>%
  select(investor.firm, investor.type, investor.country) %>%
  unique()

#export list of firms and investors
write.csv(
  investors.list,
  paste(getwd(), "/input/i3manualcheck/investors-20220805.csv", sep = ""),
  fileEncoding = "UTF-8"
)

#export list of startups
write.csv(
  firms.list,
  paste(
    getwd(),
    "/input/i3manualcheck/firms.list-20220805.csv",
    sep = ""
  ),
  fileEncoding = "UTF-8"
)

remove(investments.i3, investors.i3, firms.list, investors.list)

##-------------------------------------------------------------------------
##              1c.  Manual edits on raw investors data                   -
##                    using Maria 09.26.2021                              -
##                   + individual edits                                   -
##-------------------------------------------------------------------------

#importing manually verified, consolidated, and updated list of investors
investors.cleaned <-
  read_excel(
    paste(
      getwd(),
      "/input/i3manualcheck/investors-cleaned-20220504.xlsx",
      sep = ""
    ),
    na = ""
  ) %>%
  select(1:5)

#creating all different possibilities of investor names into one data frame and providing a unique id to each investor
investors.tmp <- investors.cleaned %>%
  mutate(investor.firm = investor.firm.renamed) %>%
  unique() %>%
  rbind(investors.cleaned) %>%
  unique() %>%
  group_by(investor.firm.renamed,
           investor.type.edited,
           investor.country.edited) %>%
  mutate(investor.id = cur_group_id()) %>% #unique id for each investor
  ungroup() %>%
  unique()

#combining with cleaned i3 dataset
investors <-
  left_join(investors.tmp, investments.combined, by = c("investor.firm")) %>%
  unique() %>% select(-investor.type.x, -investor.type.y)

#identify corporations with >3 investments
investors.corp <- investors %>%
  filter(investor.type.edited == "corporation or corporate venture") %>%
  filter(is.na(company) == FALSE) %>%
  group_by(investor.id) %>%
  mutate(investor.portco = n_distinct(company)) %>%
  ungroup() %>%
  filter(investor.portco >= 3) %>%
  select(investor.firm.renamed, investor.portco) %>%
  unique()

remove(investors.cleaned, investors.tmp)

#export list of corporate investors to check type of corporation
#write.csv(investors.corp,paste(getwd(),"/input/i3manualcheck/investors.cpc-20220805.csv",sep=""),fileEncoding = "UTF-8")



##-------------------------------------------------------------------------
##         1c.  merging investors with science based targets data         -
##                 downloaded 25.09.2021                                  -
##-------------------------------------------------------------------------

#cleaning up sbti data
stoplist <-
  c(
    " ltd| plc| inc| llc| co inc| s e| s a b|s a| s p a| holdings| group| corporation| gmbh| sa| management| holding| se| nv| n v| ag| investors| limited| il| plc| spa| incorporated| corporation| corp| a s| company"
  )
targets.sbti <-
  read.csv(paste(getwd(), "/input/sbti/companies-taking-action.csv", sep =
                   "")) %>%
  dplyr::rename_all(funs(make.names(.))) %>%
  rename_all(tolower) %>%
  mutate_all(tolower) %>%
  mutate(company.name = str_replace_all(company.name, "[^[:alnum:]]", " ")) %>%
  mutate(company.name = str_squish(company.name)) %>%
  mutate(company.name = str_remove_all(company.name, stoplist)) %>%
  mutate(company.name = str_squish(company.name)) %>%
  mutate(status = as.factor(status)) %>%
  rename_with(.fn = ~ paste0("sbti.", .x)) %>%
  rename("investor.firm" = "sbti.company.name")

##all combinations of investor names
investors.sbti <- investors %>%
  mutate(investor.firm = investor.firm.renamed) %>%
  rbind(investors) %>%
  select(
    investor.firm,
    investor.country,
    investor.firm.renamed,
    investor.country.edited,
    investor.type.edited,
    investor.id
  ) %>%
  unique()
#filter(investor.type.edited == "corporation or corporate venture")


investors.sbti.matched <- investors.sbti %>%
  left_join(targets.sbti, by = "investor.firm") %>%
  select(-sbti.date.explanation,
         -sbti.target,
         -sbti.target.classification,
         -sbti.sme.) %>%
  group_by(investor.id) %>%
  #for each investor (with different possible names), fill in the SBTI information for all columns
  fill(sbti.isin, .direction = "downup") %>%
  fill(sbti.target.qualification, .direction = "downup") %>%
  fill(sbti.business.ambitions.1.5, .direction = "downup") %>%
  fill(sbti.country, .direction = "downup") %>%
  fill(sbti.region, .direction = "downup") %>%
  fill(sbti.sector, .direction = "downup") %>%
  fill(sbti.status, .direction = "downup") %>%
  fill(sbti.date, .direction = "downup") %>%
  ungroup() %>%
  select(investor.id, sbti.target.qualification, sbti.status)

investors <-
  left_join(investors, investors.sbti.matched, by = "investor.id") %>%
  unique()

remove(stoplist,
       targets.sbti,
       investors.sbti,
       investors.sbti.matched)


##-------------------------------------------------------------------------
##              1d. investor types
##
##-------------------------------------------------------------------------

investor.category <-
  read.csv(paste(getwd(), "/input/investortype/investors-type.csv", sep =
                   "")) %>%
  dplyr::rename_all(funs(make.names(.))) %>%
  rename_all(tolower) %>%
  mutate_all(tolower)

investors <- investors %>%
  left_join(investor.category, by = "investor.firm.renamed") %>%
  group_by(investor.id) %>%
  fill(25, .direction = "downup") %>%
  ungroup() %>%
  filter(is.na(company) == FALSE)

remove(investor.category)

#data for figures and other analysis
write.csv(investors,
          paste(getwd(), "/output/investors_20220805.csv", sep = ""),
          fileEncoding = "UTF-8")
