plot_individual_species_maquis <- function(spec_name, spec_var_name, y_cutoff=NULL, at_list=NULL, SoN_fontname="Almoni ML v5 AAA", output_path="../output/maquis") {
  
  plot_alpha_diversity(P = abu_by_spp.maquis, x_val = "settlements", y_val = spec_name, ylab_val = spec_name)
  d <- cbind(env_data,setnames(data.table(spp_no_rare[,spec_var_name]), old = 1, new = spec_var_name))
  m <- glm.nb(formula = paste(spec_var_name,"settlements + subunit + year_ct + cos_td_rad + sin_td_rad", sep = "~"), data = d)
  print(coef(mva_m1)[,spec_var_name])
  print(coef(m))
  
  print(paste("RESULTS FOR",spec_name))
  print("P-values")
  pdt <- setnames(x = as.data.table(anov_m1.uni$uni.p, keep.rownames = T)[,.SD,.SDcols = c("rn",spec_var_name)], old = 1:2, new = c("term","p"))
  print(pdt)
  
  #Temporal trend----
  if (pdt[term=="year_ct",p<0.1]) {
    plot_time <- effect_plot(data = d, model = m, pred = year_ct, interval = T, plot.points = T, jitter = c(0.1,0), point.size = 3, cat.pred.point.size = 4,
                             partial.residuals = F, colors = "Qual1", main.title = spec_name, at = at_list)
    plot_time
    plot_model_effect(P.anal = d, m = m, eff2plot = "year_ct", export_plot = TRUE, plot_points = F, fontname = SoN_fontname,
                      outpath = output_path, x_breaks=seq(0,9), xticklabs=c("2012","","2014","","2016","","2018","","2020",""), at_list = at_list)
    time_dt <- as.data.table(plot_time$data)[,..spec_var_name]
    print(paste("change in",spec_name,"abundance over monitoring period:",(time_dt[nrow(time_dt)]-time_dt[1])/time_dt[1] * 100,"%"))
  }
  
  # spatial effect - settlements ----
  if (pdt[term=="settlements",p<0.1]) {
    plot_set <- effect_plot(data = d, model = m, pred = settlements, interval = T, plot.points = T, jitter = c(0.1,0), point.size = 3, cat.pred.point.size = 4,
                            partial.residuals = F, colors = "Qual1", main.title = spec_name, at = at_list)
    plot_set
    plot_model_effect(P.anal = d, m = m, eff2plot = "settlements", export_plot = TRUE, plot_points = T, fontname = "Almoni ML v5 AAA",
                      outpath = "../output/maquis/", y_cutoff = y_cutoff, at_list = at_list)
    set_dt <- as.data.table(plot_set$data)[,..spec_var_name]
    print(paste(spec_name,"abundance in high proximity is",(max(set_dt)/min(set_dt) - 1) *100,"% higher than low proximity."))
  }
  
  # spatial effect - subunit----
  if (pdt[term=="subunit",p<0.1]) {
    plot_subunit <- effect_plot(data = d, model = m, pred = subunit, interval = T, plot.points = T, jitter = c(0.1,0), point.size = 3, cat.pred.point.size = 4,
                                partial.residuals = F, colors = "Qual1", main.title = spec_name, at = at_list)
    plot_subunit
    
    plot_model_effect(P.anal = d, m = m, eff2plot = "subunit", export_plot = TRUE, plot_points = T, fontname = "Almoni ML v5 AAA",
                      outpath = "../output/maquis/", y_cutoff = y_cutoff, at_list = at_list)
    
    # post-hoc: test for pairwise differences in subunits if subunit term is significant
    EMM <- emmeans(object = m, ~ subunit)
    print(EMM)
    EMMdt <- as.data.table(EMM)
    
    print("Carmel vs Galilee (% difference)")
    print((exp(EMMdt[subunit %like% 'Carmel',emmean])/exp(EMMdt[subunit %like% 'Galil',emmean]) - 1) * 100)
    print("Carmel vs Judea (% difference)")
    print((exp(EMMdt[subunit %like% 'Carmel',emmean])/exp(EMMdt[subunit %like% 'Judea',emmean]) - 1) * 100)
    print("Galilee vs Judea (% difference)")
    print((exp(EMMdt[subunit %like% 'Galil',emmean])/exp(EMMdt[subunit %like% 'Judea',emmean]) - 1) * 100)
    
    test_results_subunit <- test(pairs(EMM, by=NULL, adjust="fdr"))
    print(test_results_subunit)
  }
}