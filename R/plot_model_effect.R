plot_model_effect <- function (P.anal, m, eff2plot, plot_points=FALSE, plot_residuals=FALSE, export_plot=FALSE, ylabel = NULL, fontname = NULL,
                               fontsize=22, pdf_width=160, outpath = "../output", point_size=3, point_alpha=0.25,
                               horz_jitter=0.1, at_list = NULL, y_cutoff = NULL, x_breaks = NULL, xticklabs = NULL) {
  # this function is for plotting and exporting to pdf of categorical effects or continuous year effect, no interactions. using the function effect_plot from jtools.
  # P.anal is the data table
  # m is the model object, outcome of glm or glmer
  # eff2plot is the factor name for plotting (independent variable)
  # plot_points, plot_residuals: booleans. if plot_residuals is TRUE overrides then residuals will be plotted, regardless of the value of plot_points.
  # export_plot is a boolean indicating whether to export the file to PDF. The file name is created automatically
  # ylabel is an optional character vector specifying the label of the y-axis. If not given then the label will be the response variable in Hebrew
  #   (for richness, gma and abundance) or English (for other variables)
  # fontname is the name of the font for all text in the plot, one of the fonts included in extrafont::fonts()
  # fontsize is the size of all text in the plot
  # pdf_width is the export plot size in mm
  # outpath is the path for saving exported pdf files
  # point_size is the size of the observation points (in case plot_points=TRUE)
  # point_alpha is the opacity of the observation points - 0 is transparent, 1 is opaque (in case plot_points=TRUE)
  # horz_jitter is the degree to which observation points should be jittered (in case plot_points=TRUE)
  # at_list: If you want to manually set the values of other variables in the model, do so by providing a named list where the names are the variables and the list values are vectors of the values. This can be useful especially when you are exploring interactions or other conditional predictions. 
  # y_cutoff is the max limit of the y-axis. Points excluded by this cutoff will be enumerated and indicated in the plot title.
  # x_breaks: to set these manually for year_ct effect, on the scale of year_ct (i.e. 0,1,... which are equivalent to 2012,2013,...)
  # xticklabs: to set these manually. Must match the number of breaks (tick marks) on the x-axis. currently only for year_ct.
  
  # Define a function to reverse Hebrew text in case markdown is knit to pdf
  source("strReverse.R")
  
  pdf_aspect_ratio <- 2/3
  
  require(jtools)
  require(Cairo)
  require(ggplot2)
  require(extrafont)
  
  P.anal <- copy(P.anal)
  # determine what is the response and set the y label----
  response_var <- as.character(formula(m))[2]
  if (is.null(ylabel)) {
    ylabel <- switch(response_var,
                     "richness" = "עושר מינים",
                     "gma" = "שפע ממוצע למין",
                     "abundance" = "שפע כולל",
                     "Alectoris.chukar" = "שפע",
                     "Galerida.cristata" = "שפע",
                     "Garrulus.glandarius" = "שפע",
                     "Spilopelia.senegalensis" = "שפע",
                     "Passer.domesticus" = "שפע",
                     "Acridotheres.tristis" = "שפע",
                     "Alectoris.chukar" = "שפע",
                     "Chloris.chloris" = "שפע",
                     "Cinnyris.osea" = "שפע",
                     "Columba.livia" = "שפע", 
                     "Pycnonotus.xanthopygos" = "שפע",
                     "Psittacula.krameri" = "שפע", 
                     "Prinia.gracilis" = "שפע",
                     "Streptopelia.decaocto" = "שפע",
                     "Streptopelia.turtur" = "שפע", 
                     "Turdus.merula" = "שפע",
                     "Troglodytes.troglodytes" = "שפע",
                     "Curruca.melanocephala" = "שפע",
                     "Corvus.cornix" = "שפע",
                     "Dendrocopos.syriacus" = "שפע",
                     response_var)
  }
  
  # pre-arrange x tick labels----
  # if near vs. far factor, then relevel to get Near on the left hand side
  if (eff2plot %in% c("agriculture","settlements")) {
    P.anal[,c(eff2plot):=factor(get(eff2plot),levels = c("Far","Near"))]
    
    # if year covariate (continuous) then get the data labels to reverse them if needed
  } else if (eff2plot=="year_ct") {
    
    tmp_effplot <- effect_plot(model = m, pred = year_ct, data = P.anal)
    if (!is.null(x_breaks)) {tmp_effplot <- tmp_effplot + scale_x_continuous(breaks = x_breaks)}
    if (is.null(xticklabs)) {xticklabs <- ggplot_build(tmp_effplot)$layout$panel_params[[1]]$x$get_labels()}
    if (is.null(x_breaks)) {x_breaks <- as.numeric(xticklabs)}
  }
  
  # determine what is the plotted effect and set the x axis label----
  xlabel <- switch(eff2plot,
                   "settlements" = "יישוב",
                   "agriculture" = "חקלאות",
                   "dunes" = "סיווג הדיונה",
                   "habitat" = "בית הגידול",
                   "land_use" = "שימוש הקרקע",
                   "is_loess" = "שימוש הקרקע",
                   "subunit" = "תת-יחידה",
                   "unit" = "יחידת ניטור",
                   "year_ct" = "") # revoke x label for temporal trend
  
  # arrange x tick mark labels----
  if (eff2plot %in% c("settlements","agriculture","dunes","habitat","land_use","subunit","is_loess")) {
    
    #  Set the x tick mark labels (labels of levels of the factor plotted)
    # create a table containing English level names and Hebrew translations
    fac_lev <- data.table(fac = c("settlements","settlements","agriculture","agriculture","dunes","dunes","habitat","habitat",
                                  "land_use","land_use","land_use","subunit","subunit","subunit","is_loess","is_loess"),
                          eng_lev = c("Near","Far","Near","Far","Shifting","Semi-shifting","Slope","Wadi",
                                      "Beduoin Agriculture","KKL Plantings","Loess","Judean Highlands","Carmel","Galilee",
                                      "Bedouin Agriculture / KKL Plantings","Loess"),
                          heb_lev = c("קרוב","רחוק","קרוב","רחוק","נודדת","מיוצבת למחצה","מדרון","ערוץ",
                                      "חקלאות מסורתית","נטיעות קקל","לס","הרי יהודה","הכרמל","הגליל",
                                      "חקלאות מסורתית / קציר נגר","לס טבעי"))
    # get the levels of the current factor, in the order in which they appear
    curr_lev <- data.table(eng_lev = levels(P.anal[,get(eff2plot)]))
    # add ordering variable
    curr_lev[,ord:=seq(1,.N)]
    # get the Hebrew translations, in the original order of levels
    hebrew_levels <- curr_lev[fac_lev[fac==eff2plot], on = .(eng_lev)][,.(ord,heb_lev)][order(ord)][,heb_lev]
    predlabels <- hebrew_levels
    
  } else if (eff2plot=="year_ct") {
    if (is.null(xticklabs) || as.numeric(xticklabs)[1]<2012) {
      predlabels <- as.character(as.numeric(xticklabs) + 2012)
    } else {
      predlabels <- xticklabs
    }
  } else {
    predlabels <- "?" # in case somehow levels could not be matched
  }
  
  # set the main title and filename----
  main_title <- paste("Effect of",eff2plot,sep = " ")
  if (length(unique(P.anal$unit))>1) {
    filename <- paste("birds_multiple_units",response_var,eff2plot,sep = "_")
  } else {
    filename <- paste("birds",unique(P.anal$unit),response_var,eff2plot,sep = "_")
  }
  if (plot_points==TRUE) {
    main_title <- paste(main_title,"(points are observations)",sep = " ")
    filename <- paste(filename,"w_observations",sep = "_")
  } else if (plot_residuals==TRUE) {
    main_title <- paste(main_title,"(points are partial residuals)",sep = " ")
    filename <- paste(filename,"w_residuals",sep = "_")
  }
  
  # if markdown is knit to pdf, reverse hebrew text----
  curr_device <- knitr::opts_chunk$get("dev")
  if (export_plot || (!is.null(curr_device) && grepl("pdf",curr_device))) {
    xlabel <- strReverse(xlabel)
    ylabel <- strReverse(ylabel)
    if (eff2plot!="year_ct") {
      predlabels <- strReverse(predlabels)
    }
  }
  
  # evaluate effect plot----
  eval(parse(text=eval(expr = paste0("effplot <- effect_plot(model = m, data=P.anal, pred = ",
                                     eff2plot,
                                     ", interval = T,plot.points = plot_points, jitter = c(horz_jitter,0), point.size = point_size, cat.pred.point.size = 4, partial.residuals = plot_residuals, colors = 'Qual1', main.title = main_title, line.colors = 'black', point.alpha = point_alpha, x.label = xlabel, y.label = ylabel, at = at_list)"))))
  if (eff2plot=="year_ct") {
    effplot <- effplot + scale_x_continuous(breaks = x_breaks, labels = predlabels)
  } else {
    effplot <- effplot + scale_x_discrete(labels = predlabels)
    effplot$layers[[2]]$geom_params$width <- 0.4
  }
  
  # add y_cutoff to figure title----
  if (!is.null(y_cutoff)) {
    if (plot_points) {
      main_title <- paste0(main_title,"\n",paste(P.anal[,sum(.SD>y_cutoff, na.rm = T),.SDcols=response_var],"points ommited above",y_cutoff))
    } else if (plot_residuals) {
      main_title <- paste0(main_title,"\n",paste(as.data.table(effplot$layers[[1]]$data)[,..response_var][,sum(.SD>y_cutoff, na.rm = T),.SDcols=1],"points ommited above",y_cutoff))
    }
    filename <- paste(filename,"y_cutoff",y_cutoff,sep = "_")
    effplot <- effplot + ggtitle(label = main_title)
  }
  
  # add extension to file name----
  filename <- paste(filename,"pdf",sep = ".")
  
  # fix point size----
  # for some reason, when weights are specified (e.g. in a binomial model of proportions), the data point size is computed
  # by the weights variable rather than by the given argument. override this behavior
  if (!all(diff(weights(m))==0)) {
    if (eff2plot=="year_ct") {
      effplot$layers[[1]]$aes_params$size <- point_size
    } else {
      effplot$layers[[3]]$aes_params$size <- point_size
    }
  }      
  
  # if font name specified then apply----
  if (!is.null(fontname)) {
    effplot <- effplot + theme(text=element_text(family = fontname))
    effplot$theme$plot.title$family <- fontname
    effplot$theme$axis.title$family <- fontname
    effplot$theme$axis.text.x$family <- fontname
    effplot$theme$axis.text.y$family <- fontname
  }
  
  #apply default font size----
  effplot$theme$text$size <- fontsize
  effplot$theme$plot.title$size <- fontsize
  effplot$theme$axis.title$size <- fontsize-1
  
  # justify text to right (Hebrew)----
  effplot$theme$plot.title$hjust <- 1
  
  # apply y cutoff----
  if (!is.null(y_cutoff)) {
    effplot <- effplot + ylim(0,y_cutoff)
  }
  
  # output and export plot----
  print(effplot)
  if (export_plot) {
    Cairo(file = file.path(outpath,filename), width = pdf_width, height = pdf_width*pdf_aspect_ratio, type = "PDF", units = "mm")
    print(effplot)
    dev.off()
  }
  
  return(effplot)
}