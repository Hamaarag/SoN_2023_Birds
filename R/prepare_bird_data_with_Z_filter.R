prepare_bird_data <- function (OnlyInteractingSpecies=TRUE, Zthr = 10, min_ct_thr = 60, outlier_plots = FALSE) {
  # prepare bird data for analyses: State of Nature Report, Central Bureau of Statistics Biodiversity Index
  
  # OnlyInteractingSpecies: boolean indicator, whether to keep only species that are likely to be interacting with the habitat of 
  #   the sampling location (TRUE for state of nature report, FALSE for CBS biodiversity index)
  # Zthr and min_ct_thr: thresholds for discretization of outlier observations. Zthr is in standard deviations; min_ct_thr is the minimum count
  #   (per plot per year per species) for discretization.
  #   Z threshold: observations will undergo Z-transformation (center and scale) within a given unit and species, across all years (after completing missing plots
  #   where the species was not observed as counts of zero).
  #   Observations with Z-score higher than Zthr will be set equal to 1. 
  #   A table containing all such filtered observations will be displayed. If no Z-score or minimal count filtering is required, specify NULL.
  #   The rational for discretization (setting equal to 1) of the count, is that we want to keep note that the species was observed
  #   (therefore we do not remove the observation altogether),
  #   however an extremely high count indicates a phenomenon that we probably wish to ignore, e.g. migrants or local movements of wandering, therefore we
  #   set the count to be equal to 1 (rather than setting it to the highest count of the species in the given unit which is less than the threshold, as done in mammals;
  #   in Israeli mammals there is no herd migration)
  # outlier_plots: a boolean indicating whether to print the outlier plots
  
  require(dplyr)
  require(readxl)
  require(data.table)
  require(lubridate)
  require(solartime)
  
  # load data, synonyms and traits, and combine----
  # load observations
  raw_t0_t4 <- read.csv(file = "../data/processed/2023-04-19_Birds_data_T0-T4.csv", fileEncoding = "MS-HEBR") %>% as.data.table()
  # raw_t0_t4 <- read.csv(file = "../data/processed/2023-04-27_Birds_data_T0-T4.csv", fileEncoding = "MS-HEBR") %>% as.data.table()
  t0_t4 <- copy(raw_t0_t4)
  # remove fields with species traits - this is loaded separately from designated table
  t0_t4[,c("resident","order","introduced","status"):=NULL]
  # remove 3 observations without scientific name - this should be fixed in the raw data file
  t0_t4 <- t0_t4[SciName!=""]
  # remove 13 observations without counts - this should be fixed in the raw data file
  t0_t4 <- t0_t4[total_count>0]
  
  # remove english and hebrew names, get them from traits
  t0_t4[,`:=`(species_eng=NULL, species_heb=NULL)]
  
  # Change Columba livia (domesticus) / Columba livia (Feral Pigeon) [recognized as domestic form, not species] to Columba livia
  t0_t4[grepl("Columba livia",SciName),SciName:="Columba livia"]
  
  # change Buteo buteo vulpinus [Steppe buzzard, subspecies] to Buteo buteo [species, Common buzzard]
  t0_t4[grepl("Buteo buteo",SciName),SciName := "Buteo buteo"]
  
  # remove erroneous observations in Sombre tit and Lappet-faced vulture
  t0_t4 <- t0_t4[!grepl("[Poecile|Parus] lugubris",SciName)]
  t0_t4 <- t0_t4[!grepl("Torgos trach",SciName)]
  
  # load synonyms
  syns <- fread("../data/20230428_ObsSpeciesSciNames.csv")
  
  # load trait data
  traits <- read.csv("../data/20231228_BirdSpeciesTraitTable_SingleHeader.csv", fileEncoding = "MS-HEBR") %>% as.data.table()
  # if no data on Resident status then force non-resident
  traits[is.na(Resident),Resident:=0]
  # recode conservation code IL 2018 to numbers (DD and NE=Not Evaluated get NA)
  traits[.(ConservationCodeIL2018=c("LC","NT","VU","EN","CR","RE"), to = 1:6), on = "ConservationCodeIL2018", ConservationCodeIL2018_ordinal := i.to]
  
  # load breeding indication by unit
  IsBreedSheet <- readxl::read_excel(path = "../data/observed species breeding truth table - working copy.xlsx", sheet = "BreedInUnit") %>%
    as.data.table()
  # create table containing only breeding species for each relevant unit
  IsBreed <- IsBreedSheet[,.(unit,subunit,SciName,IsBreeding)][IsBreeding==1][,IsBreeding:=NULL][,un_sp:=paste(unit,subunit,SciName,sep = "_")]
  
  # load indicator for interaction of species with sampling point
  IsInteractSheet <- readxl::read_excel(path = "../data/20230530 species_interacting_with_sampling_point.xlsx") %>% as.data.table()
  IsInteract <- IsInteractSheet[,.(SciName,IsInteractingFinalByRon)]
  
  # fix sci names
  t0_t4_updsci <- merge.data.table(x = t0_t4, y = syns[,.(SciName,SciName_updated,SPECIES_CODE)], by = "SciName", all.x = TRUE)
  if (any(is.na(t0_t4_updsci$SPECIES_CODE))) stop("Could not match all observations to updated scientific names")
  t0_t4_updsci[,SciName:=SciName_updated]
  t0_t4_updsci[,SciName_updated:=NULL]
  
  A <- copy(t0_t4_updsci)
  Araw <- copy(A)
  
  # exclude winter sampling----
  # complete season variable - only missing for Forest and Maquis in 2021
  A[season=="",season:="Spring"]
  A <- A[season=="Spring"]
  cat("- Winter data excluded.\n")
  
  # fix stuff - remove after Tomer fixes this----
  # fix plot assignment
  if (A[grepl("Planted",unit) & year==2015,uniqueN(point_name)]==36) {
    A[grepl("Maquis",unit) & year==2015 & grepl("Kerem Maharal \\d",point_name),`:=`(unit="Planted Conifer Forest", settlements=NA)]
    A[grepl("Maquis",unit) & year==2015 & grepl("Givat Yeshaayahu \\d",point_name),`:=`(unit="Planted Conifer Forest", settlements=NA)]
    A[grepl("Maquis",unit) & year==2015 & grepl("Aderet \\d",point_name),`:=`(unit="Planted Conifer Forest", settlements=NA)]
  }
  A[year==2012 & point_name=="Iftach Far 3" & round(lat, digits = 4)==33.1242 & round(lon, digits = 4)==35.5531,point_name:="Iftach Near 1"]
  A[year==2017 & point_name=="Abirim Near 2" & round(lat, digits = 4)==33.0381 & round(lon, digits = 4)==35.2842,settlements:="Near"]
  A[grepl("Highlands",unit) & year==2012, habitat:="Slope"]
  A[year==2012 & site=="Mitzpe Ramon" & point_name=="Mashabei Sadeh Far 6",point_name:="Mitzpe Ramon Far 6"]
  A[year==2019 & subunit=="Carmel" & site=="Beit Oren" & point_name=="Aderet Far 2", `:=`(subunit="Judean Highlands", site="Aderet")]
  A[year==2014 & unit == "Negev Highlands" & habitat=="Slope" & settlements=="Near" & point_name=="Ezuz Far Slope 31", settlements:="Far"]
  
  # for controlling the assignment of plots
  #A[,.(uq_point=uniqueN(point_name)),keyby=.(unit,year)]
  #A[(grepl("Maquis",unit) | grepl("Planted",unit)) & grepl("Kerem",site),.(date=unique(date)),keyby = .(unit,point_name,plot_coord)][order(date)]
  #A[(grepl("Maquis",unit) | grepl("Planted",unit)) & grepl("Kerem",site),.(Nunits=uniqueN(unit),Ncamps=uniqueN(campaign)), keyby=.(plot_coord,point_name)]
  
  # fix problem with dates - remove this after Tomer fixed the thing
  A[grepl("Inland",unit) & grepl("0?4/0?5/2021",date), date:="05/04/2021 00:00" ]
  A[grepl("Inland",unit) & grepl("0?1/0?4/2021",date), date:="04/01/2021 00:00" ]
  A[grepl("Maquis",unit) & grepl("0?9/0?4/2021",date), date:="04/09/2021 00:00" ]
  A[grepl("Planted",unit) & grepl("0?8/0?5/2021",date), date:="05/08/2021 00:00" ]
  A[grepl("Planted",unit) & grepl("0?9/0?4/2021",date), date:="04/09/2021 00:00" ]
  A[grepl("Dwarf",unit) & grepl("^2015-05-06",date),date:=date %m+% years(1)] #wrongly set in 2015 instead of 2016
  
  # fix pilot field for Maquis: 2012 is not pilot because there was no spring sampling in T0 there
  A[grepl("Maquis",unit) & year==2012, pilot:=FALSE]
  # everything from 2013 and onwards is not pilot
  A[year>2012,pilot:=FALSE]
  
  # fix subunit field - remove dwarf-shrub and herbaceous levels, this is already indicated in the habitat field (and is also reversed in the subunit field)
  A[,subunit:= factor(subunit, levels = c("Judean Highlands", "Carmel", "Galilee"))]
  
  # fix NAs in T4 counts
  A[year>2020 & is.na(rad_0_20),rad_0_20:=0]
  A[year>2020 & is.na(rad_20_100),rad_20_100:=0]
  A[year>2020 & is.na(rad_100_250),rad_100_250:=0]
  A[year>2020 & is.na(rad_over_250),rad_over_250:=0]
  
  # correct dates----
  A[,date:=parse_date_time(date, orders = c("mdY H:M"))]
  # some sanity checks for the above
  if (any(year(A[,date])<2012) | (any(month(A[,date])>7 & year(A$date)>2012)) | any(month(A[,date])<3) | any(is.na(A[,date]))) {
    stop("Problem in date parsing.")
  }
  
  # fix NAs in missing monitors name (in 85 point-year combinations it exists only in one observation, rest are NA)
  A[,pd:=paste(point_name,date,sep = "_")]
  A[pd %in% pd[is.na(monitors_name)], monitors_name:=unique(monitors_name[!is.na(monitors_name)]), by = .(point_name,date)]
  
  # fix NAs in missing precipitation, temperature, clouds (appear only in part of the observations from the same point in 2017)
  A[year==2017 & pd %in% pd[is.na(precipitation)], precipitation:=unique(precipitation[!is.na(precipitation)]), by = .(point_name,date)]
  A[year==2017 & pd %in% pd[is.na(temperature)], temperature:=unique(temperature[!is.na(temperature)]), by = .(point_name,date)]
  A[year==2017 & pd %in% pd[is.na(clouds)], clouds:=unique(clouds[!is.na(clouds)]), by = .(point_name,date)]
  
  # fix time and compute circular variables representing time from sunrise----
  # create datetime object in Israel time zone, accounting for DST (time zone changes accordingly)
  A[,datetime:=NULL]
  A[!is.na(time),datetime_c:=paste(format(date,"%Y-%m-%d"),time)] # character vector
  
  # POSIXct object, time zone changes according to the dates of DST (IST is UTC+2, IDT is UTC+3)
  A[!is.na(time),datetime:=as_datetime(datetime_c, format="%F %T", tz = "Israel")]
  A[,datetime_c:=NULL]
  
  # fix points with multiple times on the same date
  A[point_name=="Iftach Near 1" & year>=2017,datetime:=min(datetime),by=date]
  A[point_name=="Givot Bar Bedouin Agriculture 2" & year==2018,datetime:=min(datetime),by=date]
  
  # change all DST times to UTC+2
  A[!is.na(time),datetime_utc2:=with_tz(datetime, tzone = "Etc/GMT-2")]
  
  #A[!is.na(time),time_c_utc2:=format(datetime_utc2,"%H:%M")] #not needed?
  
  # compute sunrise hour, decimal (hours from midnight)
  A[!is.na(time),sunrise_h:=computeSunriseHour(timestamp = date, latDeg = lat, longDeg = lon, timeZone = 2)]
  
  # time of sunrise in UTC+2
  A[!is.na(time),sunrise_utc2:=force_tz(time = date+3600*sunrise_h, tzone = "Etc/GMT-2")]
  
  # compute hours from sunrise
  A[!is.na(time),h_from_sunrise:=round(difftime(datetime_utc2,sunrise_utc2,units = "hours"),digits = 2)]
  
  # convert to hours from sunrise to circular variable with phase of 24 hours
  A[,hsun_rad:=as.numeric(h_from_sunrise/24*2*pi)]
  A[,`:=`(cos_hsun=cos(hsun_rad),
          sin_hsun=sin(hsun_rad))]
  
  # compute circular variables representing time of year----
  # compute time difference in days from June 21st of the same year.
  A[,timediff_Jun21:=date-floor_date(date, unit = "year")-172]
  # scaled time difference, for optimization purposes when fitting
  A[,td_sc:=scale(timediff_Jun21)]
  # time difference in radians, for cosine and sine IVs
  A[,td_rad:=as.numeric(timediff_Jun21/365*2*pi)]
  A[,`:=`(cos_td_rad=cos(td_rad),
          sin_td_rad=sin(td_rad))]
  
  # add fields----
  # count observations <250m
  A[,count_under_250:=rad_0_20+rad_20_100+rad_100_250]
  
  # add column year_ct: years since 2012
  A[,year_ct := year-2012]
  
  # add plot coordinates field
  A[,plot_coord:=paste(lat,lon,sep="_")]
  
  # convert all character vectors to factors----
  changeCols <- colnames(A)[which(as.data.table(as.vector(A[,lapply(.SD, class)]))[1,]=="character")]
  A[,(changeCols):= lapply(.SD, as.factor), .SDcols = changeCols]
  unit_names <- sort(unique(A$unit))
  
  # convert all weather variables to ordinal variables----
  weather_vars <- c("wind","precipitation","temperature","clouds")
  A[,(weather_vars) := lapply(.SD,\(x) factor(x, ordered = TRUE)),.SDcols = weather_vars]
  
  # keep only breeding species----
  # do this by setting the observed counts to zero, don't filter rows so as not to lose samples that contained only passing / wintering species
  # first check that all species names in the breeding trait table are correct
  tmp <- unique(merge.data.table(x = IsBreedSheet[,.(SciName, PRIMARY_COM_NAME)], y = unique(syns[,.(SciName_updated,SPECIES_CODE)]), by.y = "SciName_updated",
                                 by.x = "SciName", all.x = T))
  if (tmp[,any(is.na(SPECIES_CODE))]) {
    print("The following species could not be matched in the breeding trait table:")
    print(tmp[is.na(SPECIES_CODE)==T,])
    stop()
  }
  rm(tmp)
  
  Abreed_and_non_breed <- copy(A)
  A[,un_sp:=paste(unit,subunit,SciName,sep = "_")]
  A[!(un_sp %in% IsBreed[,un_sp]),count_under_250:=0]
  cat("- The counts of all passing / wintering species (not breeding in the sampled unit) were set to zero. Abreed_and_non_breed contains the non-breeders as well.\n")
  
  # filter species that are less likely to be interacting with the close vicinity of the sampling point - see ReadMe.txt----
  if (OnlyInteractingSpecies) {
    # first check that all species names in the interacting trait table are correct
    tmp <- unique(merge.data.table(x = IsInteractSheet[,.(SciName, IsInteractingFinalByRon)], y = unique(syns[,.(SciName_updated,SPECIES_CODE)]), by.y = "SciName_updated",
                                   by.x = "SciName", all.x = T))
    if (tmp[,any(is.na(SPECIES_CODE))]) {
      print("The following species could not be matched in the interacting trait table:")
      print(tmp[is.na(SPECIES_CODE)==T,])
      stop()
    }
    rm(tmp)
    
    A[SciName %in% IsInteract[IsInteractingFinalByRon==0,SciName],count_under_250:=0]
    cat("- Species less likely to be interacting with sampling location were EXCLUDED\n")
  } else {cat("- Species less likely to be interacting with sampling location were INCLUDED\n")}
  
  # discretize outlier observations which lie >= Zthr standard deviations AND >= minimal count----
  # IMPORTANT: the species counts in plots where the species was not observed will be set to zero.
  cat("- observations with counts greater than or equal to",Zthr,"standard deviations AND greater than or equal to",min_ct_thr,"individuals were set to 1,\nunder the assumption that these are migrating or local wandering phenomena.\n")
  Au_list <- list()
  ct <- 1
  LHS <- c("unit","subunit","site","year","year_ct","settlements","agriculture","habitat","dunes","land_use",
           "point_name","date","datetime","td_sc","td_rad","cos_td_rad","sin_td_rad","timediff_Jun21","monitors_name",
           "wind","precipitation","temperature","clouds","h_from_sunrise","cos_hsun","sin_hsun","pilot")
  for (i in unit_names) {
    Au <- A[unit==i,]
    
    # complete counts of zero where species was not observed
    plots_by_spp <- dcast.data.table(data = Au,formula = as.formula(paste(paste(LHS, collapse = "+"), "~ SciName")),
                                     fun.aggregate = sum, value.var = "count_under_250")
    
    # melt back
    Au <- melt.data.table(data = plots_by_spp, id.vars = LHS, variable.name = "SciName", value.name = "count_under_250")
    
    
    # choose only species that were observed at least once within 250m
    uqSciNameUnit <- Au[,.(tot_abund = sum(count_under_250)), keyby = SciName][tot_abund>0,unique(SciName)] %>% as.character()
    # compute Z-score
    Au[SciName %in% uqSciNameUnit, Z_score := scale(count_under_250), by = SciName]
    
    if (outlier_plots) {
      p_outliers <- ggplot(Au[SciName %in% uqSciNameUnit], aes(x = SciName, y = Z_score, fill = SciName, color = SciName)) +
        geom_boxplot() + coord_flip() + 
        guides(color="none", fill="none") + ggtitle(label = i) +
        scale_color_manual(values=rep(c("darkgrey","darkslategrey"),ceiling(nrow(Au[SciName %in% uqSciNameUnit])/2))) +
        scale_fill_manual(values=rep(c("darkgrey","darkslategrey"),ceiling(nrow(Au[SciName %in% uqSciNameUnit])/2)))
      # add line at Zthr
      if (Au[SciName %in% uqSciNameUnit,any(Z_score>=Zthr)]) {
        p_outliers <- p_outliers + geom_hline(yintercept = Zthr, color = "red", linetype="dashed")
      }
      print(p_outliers)
      
      p_outliers2 <- ggplot(Au[SciName %in% uqSciNameUnit], aes(x = SciName, y = count_under_250, fill = SciName, color = SciName)) +
        geom_boxplot() + coord_flip() + 
        guides(color="none", fill="none") + ggtitle(label = i) +
        scale_color_manual(values=rep(c("darkgrey","darkslategrey"),ceiling(nrow(Au[SciName %in% uqSciNameUnit])/2))) +
        scale_fill_manual(values=rep(c("darkgrey","darkslategrey"),ceiling(nrow(Au[SciName %in% uqSciNameUnit])/2)))
      if (Au[SciName %in% uqSciNameUnit,any(count_under_250>=min_ct_thr)]) {
        p_outliers2 <- p_outliers2 + geom_hline(yintercept = min_ct_thr, color = "red", linetype="dashed")
      }
      print(p_outliers2)
    }
    
    # print list of outliers and set to 1
    num_outliers <- nrow(Au[SciName %in% uqSciNameUnit & Z_score>=Zthr & count_under_250>=min_ct_thr])
    if (num_outliers>0) {
      cat(num_outliers," outliers set equal to 1 in ",i," unit.\n", sep = "")
      Au[SciName %in% uqSciNameUnit & Z_score>=Zthr & count_under_250>=min_ct_thr, .(unit, point_name, year, SciName, count_under_250, Z_score)][order(year, point_name, SciName)] %>% print()
      Au[SciName %in% uqSciNameUnit & Z_score>=Zthr & count_under_250>=min_ct_thr, count_under_250:=1]
    } else {
      cat("No outliers found in unit ",i,".\n", sep = "")
    }
    
    Au_list[[ct]] <- Au
    ct <- ct+1
  }
  
  # keep species code
  spec_name_code <- unique(A[,.(SciName,SPECIES_CODE)])
  
  # combine all units
  A <- rbindlist(Au_list, use.names = TRUE)
  
  # add hebrew and english names
  A <- merge.data.table(x = A, y = spec_name_code, all.x = T, by = "SciName")
  
  # combine traits----
  A <- merge.data.table(x = A, y = traits[,!"SCI_NAME"], by = "SPECIES_CODE", all.x=TRUE)
  if (any(is.na(A$AlienInvasive))) stop("Could not match all observations to trait table species")
  
  # create pivot table for species abundance and incidence----
  abu_by_spp <- dcast.data.table(A,as.formula(paste(paste(LHS, collapse = "+"), "~ SciName")),
                                 fun.aggregate = sum, value.var = "count_under_250")
  inc_by_spp <- dcast.data.table(A,as.formula(paste(paste(LHS, collapse = "+"), "~ SciName")),
                                 fun.aggregate = (\(x) sum(x)>0), value.var = "count_under_250")
  plots_campaigns <- abu_by_spp[,dplyr::select(.SD, all_of(LHS))]
  
  
  # generate no-rare tables----
  
  # cutoff for rare species - minimum number of plots in which the species was observed under 250 meters (as defined in inc_by_spp).
  # The key is two plots per campaign containing 30 plots, rounded to the nearest integer.
  # If there are more than 30 plots per campaign, adjust key accordingly (but not if lower).
  # an additional cutoff is a minimum of 10 individuals (again, under 250 meters, as defined in abu_by_spp).
  # plot numbers are as follows:
  #   Arid South:           30
  #   Herbaceous and dwarf-shrub veg: 60
  #   Inland Sands:         12
  #   Loess:                30
  #   Med-Desert Transition Zone:      30
  #   Maquis:               90
  #   Negev Highlands:      45
  #   Planted Forests:      45
  # include pilot years in the calculation of rare species.
  min_plots_cutoff <- A[,.(campaign_num=uniqueN(year)), keyby=unit][,plots_per_camp:=c(30,60,12,30,30,90,45,45)][,plots_per_camp_factor:=apply(.SD,1,\(x) round(max(x/30,1))),.SDcols="plots_per_camp"][,cutoff:=campaign_num*2*plots_per_camp_factor]
  min_indiv_cutoff <- 10
  
  # filter rare species - by unit
  Au_list <- list()
  ct <- 1
  uqSciName <- sort(unique(A$SciName))
  for (i in unit_names) {
    Au <- A[unit==i,]
    spp_inc_u <- inc_by_spp[unit==i, dplyr::select(.SD, all_of(uqSciName))]
    spp_abu_u <- abu_by_spp[unit==i, dplyr::select(.SD, all_of(uqSciName))]
    tot_plots_spp_u <- colSums(spp_inc_u)
    tot_indiv_spp_u <- colSums(spp_abu_u)
    is_rare <- (tot_indiv_spp_u>=min_indiv_cutoff) & (tot_plots_spp_u>=min_plots_cutoff[unit==i,cutoff])
    non_rare_names_u <- names(is_rare)[is_rare]
    Au_no_rare <- Au[SciName %in% non_rare_names_u,]
    Au_list[[ct]] <- Au_no_rare
    ct <- ct+1
  }
  A_no_rare <- rbindlist(Au_list, use.names = TRUE)
  abu_by_spp_no_rare <- dcast.data.table(A_no_rare,as.formula(paste(paste(LHS, collapse = "+"), "~ SciName")),
                                         fun.aggregate = sum, value.var = "count_under_250")
  
  spec_names <- as.character(uqSciName)
  spec_names_no_rare <- as.character(sort(unique(A_no_rare$SciName)))
  inc_by_spp_no_rare <- copy(abu_by_spp_no_rare)
  inc_by_spp_no_rare[,(spec_names_no_rare):=lapply(.SD,(\(x) x>0)),.SDcols = spec_names_no_rare]
  
  plots_campaigns_no_rare <- abu_by_spp_no_rare[,dplyr::select(.SD, all_of(LHS))]
  
  # add plots_campaigns that were removed by the process of removing observations in rare species
  missing_plots_campaigns <- setdiff(plots_campaigns, plots_campaigns_no_rare)
  K <- as.data.table(matrix(0,nrow(missing_plots_campaigns),
                            ncol(abu_by_spp_no_rare)-ncol(plots_campaigns)))
  names(K) <- names(abu_by_spp_no_rare)[(length(LHS)+1):ncol(abu_by_spp_no_rare)]
  Z <- cbind(missing_plots_campaigns,K)
  abu_by_spp_no_rare <- rbindlist(list(abu_by_spp_no_rare,Z), use.names = T)[
    order(unit,subunit, site, settlements, agriculture, habitat, dunes, point_name)]
  
  # calculate biodiversity indices - by plot----
  P_byplot <- copy(abu_by_spp)
  P_byplot[,`:=`(richness = apply(.SD,1,(\(x) sum(x>0))),
                 abundance = apply(.SD,1,sum),
                 gma=apply(.SD,1,(\(x) exp( sum(log(x[x>0])) / sum(x>0) )))),
           .SDcols = spec_names]
  P_byplot <- P_byplot[,.SD,.SDcols = c(LHS,"richness","abundance","gma")][
    ,`:=`(year=factor(year))][
      order(unit,subunit,year_ct, site, settlements, agriculture, habitat, dunes,land_use,point_name)]
  
  P_byplot_no_rare <- copy(abu_by_spp_no_rare)
  P_byplot_no_rare[,`:=`(richness = apply(.SD,1,(\(x) sum(x>0))),
                         abundance = apply(.SD,1,sum),
                         gma=apply(.SD,1,(\(x) exp( sum(log(x[x>0])) / sum(x>0) )))),
                   .SDcols = spec_names_no_rare]
  P_byplot_no_rare <- P_byplot_no_rare[,.SD,.SDcols = c(LHS,"richness","abundance","gma")][
    ,`:=`(year=factor(year))][
      order(unit,subunit,year_ct, site, settlements, agriculture, habitat, dunes,land_use,point_name)]
  
  # The following line is commented because gma is undefined when no species are found.
  # P_byplot_no_rare[is.na(gma),gma:=0]   # arbitrarily set gma=0 in plots with 0 observations
  
  # set output----
  output <- list(A=A,
                 Araw=Araw,
                 Abreed_and_non_breed=Abreed_and_non_breed,
                 A_no_rare=A_no_rare,
                 P_byplot=P_byplot,
                 P_byplot_no_rare=P_byplot_no_rare,
                 abu_by_spp=abu_by_spp,
                 abu_by_spp_no_rare=abu_by_spp_no_rare,
                 inc_by_spp=inc_by_spp,
                 inc_by_spp_no_rare=inc_by_spp_no_rare,
                 min_plots_cutoff=min_plots_cutoff,
                 min_indiv_cutoff=min_indiv_cutoff,
                 plots_campaigns=plots_campaigns,
                 col_names = LHS)
  return(output)
}