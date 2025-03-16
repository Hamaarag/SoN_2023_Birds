plot_individual_species_national <- function(spec_name, spec_var_name, model, y_cutoff=NULL, at_list=NULL, filter_out_empty_sites=FALSE,
                                             export_plot=TRUE, disregard_p_val=FALSE, SoN_fontname="Almoni ML v5 AAA", output_path="../output/national") {
  
  # model is manyglm model, to extract formula
  model_call <- as.formula(paste(spec_var_name, paste(all.vars(model$formula)[2:4], collapse = " + "), sep = " ~ " ))
  
  plot_alpha_diversity(P = abu_by_spp_no_rare, y_val = spec_name, ylab_val = spec_name)
  d <- cbind(env_data,setnames(data.table(spp_no_rare[,spec_var_name]), old = 1, new = spec_var_name))
  
  if (filter_out_empty_sites==TRUE) {
    # filter out sites with zero observations throughout the entire monitoring period
    nz_site_names <- d[,sum(.SD), keyby = site, .SDcols = spec_var_name][V1>0,site]
    d <- d[site %in% nz_site_names][,`:=`(unit = factor(unit), site=factor(site))]
  }
  
  
  m <- glm.nb(formula = model_call, data = d)
  # m <- glmmTMB(formula = formula(paste(spec_var_name,"year_ct + cos_td_rad + sin_td_rad + (1|unit/site)", sep = "~")), data = d, family = "nbinom1")
  
  cat("\n*****\n\n")
  print(d[,.(num_plots=.N, num_sites=uniqueN(site), num_units=uniqueN(unit))])
  print(d[,.(site=unique(site)), keyby=unit])
  cat("\n\n*****\n\n")
  
  cat("\n\n*** manyglm call ***\n")
  print(model$formula)
  
  cat("\n\n*** manyglm coefficients ***\n")
  print(coef(model)[,spec_var_name])
  
  cat("\n\n*** individual model call ***\n")
  print(model_call)
  
  cat("\n\n*** individual model coefficients ***\n")
  if (class(m)[1]=="glmmTMB") {
    print(fixef(m))
    print(ranef(m))
  } else {
    print(coef(m))
  }
  cat("\n\n")
  
  print(paste("RESULTS FOR",spec_name))
  print("P-values")
  pdt <- setnames(x = as.data.table(anov_m1.uni$uni.p, keep.rownames = T)[,.SD,.SDcols = c("rn",spec_var_name)], old = 1:2, new = c("term","p"))
  print(pdt)
  
  #Temporal trend----
  if (disregard_p_val==TRUE | pdt[term=="year_ct",p<0.1]) {
    plot_time <- effect_plot(data = d, model = m, pred = year_ct, interval = T, plot.points = T, jitter = c(0.1,0), point.size = 3, cat.pred.point.size = 4,
                             partial.residuals = F, colors = "Qual1", main.title = spec_name, at = at_list)
    plot_time
    plot_model_effect(P.anal = d, m = m, eff2plot = "year_ct", export_plot = export_plot, plot_points = F, fontname = SoN_fontname,
                      outpath = output_path, x_breaks=seq(0,9), xticklabs=c("2012","","2014","","2016","","2018","","2020",""), at_list = at_list)
    time_dt <- as.data.table(plot_time$data)[,..spec_var_name]
    print(paste("change in",spec_name,"abundance over monitoring period:",(time_dt[nrow(time_dt)]-time_dt[1])/time_dt[1] * 100,"%"))
  }
}