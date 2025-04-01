plot_coefs_with_traits_for_publication <- function (D,cp,plot_title,plot_xlabel,mark_batha=TRUE, mark_alien=TRUE, show_legend=TRUE,
                                                    export_plot=FALSE, fontname = NULL,fontsize=10, pdf_width=160, outpath = "../output",
                                                    effect_str="", is_NT_endangered = FALSE, filename = NULL) {
  # D is a unit-specific data table where each row is an observation in a given species in a given plot and date. This is only used to get species traits and unit name, not observational data.
  # cp is a 4 column table: scientific name, coefficient and p-value (as extracted from the MVabund model and converted to log2 scale), and total abundance for each species.
  # plot_title is the title of the plot
  # plot_xlabel is the label of the x axis
  # mark_batha is a boolean indicating whether to mark batha specialists with a different marker shape. If there are no batha specialists then mark_batha is set to FALSE, regardless of the value given at function call
  # mark_alien is a boolean indicating whether to mark alien invasive with a different marker shape. If there are no alien invasive species then mark_alien is set to FALSE, regardless of the value given at function call
  # show_legend is a boolean indicating whether to show the plot legend
  # export_plot is a boolean indicating whether to export the file to PDF. The file name is created automatically
  # fontname is the name of the font for all text in the plot, one of the fonts included in extrafont::fonts()
  # fontsize is the size of all text in the plot
  # pdf_width is the export plot size in mm
  # outpath is the path for saving exported pdf files
  # effect_str is a character string of the plotted effect (for use in file name)
  # is_NT_endangered is a boolean indicating whether to include NT in the category of threatened species (default is FALSE, concordant with IUCN)
  # filename is a character string representing the user defined file name, if not specified the file name will be composed automatically
  
  # the function returns a ggplot object to be plotted
  
  # for publication: 
  # marker shape: traits alien invasive, batha specialist
  # font color: synanthrope / invasive (black), other (grey)
  # marker edge color: endangered (red), other (black)
  
  require(ggplot2)
  require(ggnewscale)
  require(Cairo)
  
  # Define a function to reverse Hebrew text in case markdown is knit to pdf
  source("strReverse.R")
  
  pdf_aspect_ratio <- 2/3
  label_text_size <- 3
  marker_size <- 2.5
  stroke_width <- 0.75
  
  trait_font_colors <- data.table(trait = factor(c("other","synanthrope / invasive"), levels = c("synanthrope / invasive","other")),
                                  color = c(colors()[190],"black"))
  setkey(trait_font_colors,trait)
  trait_shapes <- data.table(trait = factor(c("alien invasive","batha specialist","other"), levels = c("alien invasive","batha specialist","other")),
                             shape = c(22,24,21))
  setkey(trait_shapes,trait)
  trait_edge_color <- data.table(trait = factor(c("endangered","other"), levels = c("endangered","other")),
                                 color = c("red","black"))
  setkey(trait_edge_color,trait)
  colnames(cp)[grepl("coef",colnames(cp))] <- "coef"
  colnames(cp)[grepl(".p$",colnames(cp))] <- "pval"
  
  # get column names of vegetation formation
  VegColNames <- grep("VegetationFormation",colnames(D),value = TRUE)
  VegColNames_batha <- grep("low.shrubland|high.shrubland|herbaceous|field.crops",VegColNames, value = TRUE)
  VegColNames_non_batha <- setdiff(VegColNames,VegColNames_batha)
  
  tra <- unique(D[,.SD,.SDcols = c("SciName","HebName","ConservationCodeIL2018_ordinal","AlienInvasive","Synanthrope","NativeInvasive",VegColNames)])
  tra[,SciName:=make.names(SciName)]
  coef_tra <- merge(cp, tra, by = "SciName")
  
  # combine synanthrope and invasive
  coef_tra[,synan_invas:=Synanthrope|NativeInvasive|AlienInvasive]
  coef_tra[is.na(synan_invas),synan_invas:=FALSE]
  
  # define batha species as preferring herbaceous, low shrub or high shrub vegetation formations or agricultural field crops, and no other vegetation formation
  coef_tra[,batha:=apply(.SD,1, \(x) any(x)),.SDcols = VegColNames_batha][,non_batha:=apply(.SD,1, \(x) !any(x)), .SDcols = VegColNames_non_batha][,batha_only:=batha & non_batha]
  coef_tra[is.na(batha_only),batha_only:=FALSE]
  
  # define endangered species
  coef_tra[,endangered:=ConservationCodeIL2018_ordinal>=ifelse (is_NT_endangered, 2, 3)]
  coef_tra[is.na(endangered),endangered:=FALSE]
  
  # create color and shape variables for species traits
  coef_tra[,font_color_trait:=if_else(synan_invas, "synanthrope / invasive","other")]
  coef_tra[,marker_shape_trait:=if_else(AlienInvasive==1,"alien invasive",
                                        if_else(batha_only,"batha specialist","other"))]
  coef_tra[,marker_edge_trait:=if_else(endangered,"endangered","other")]
  
  # plot----
  # test if there are missing traits before plotting
  plot_syn_inv_arrow <- TRUE
  plot_endang_arrow <- TRUE
  plot_batha_arrow <- TRUE
  
  if (show_legend) {
    color_legend_show <- "legend"
    shape_legend_show <- "legend"
    fill_legend_show <- "legend"
    marker_edge_legend_show <- "legend"
  } else {
    color_legend_show <- "none"
    shape_legend_show <- "none"
    fill_legend_show <- "none"
    marker_edge_legend_show <- "none"
  }
  
  annotation_text <- ""
  
  realized_font_color_traits <- trait_font_colors[unique(coef_tra[,.(font_color_trait)])][order(trait)]
  if (nrow(realized_font_color_traits)<2) {
    plot_syn_inv_arrow <- FALSE
    color_legend_show <- "none"
  } else {
    annotation_text <- paste0(annotation_text,"black arrow is median of synanthrope/invasive\n")
  }
  
  realized_shape_traits <- trait_shapes[unique(coef_tra[,.(marker_shape_trait)])][order(trait)]
  if (nrow(realized_shape_traits)<2) {
    shape_legend_show <- "none"
    plot_batha_arrow <- FALSE
  } else {
    if (mark_batha & any(realized_shape_traits[,grepl(pattern = "batha",trait)])) {
      annotation_text <- paste0(annotation_text,"green arrow is median of batha specialists\n")
    } else {
      plot_batha_arrow <- FALSE
      realized_shape_traits <- realized_shape_traits[!grepl("batha",trait)]
    }
    if (!(mark_alien & any(realized_shape_traits[,grepl(pattern = "alien",trait)]))) {
      realized_shape_traits <- realized_shape_traits[!grepl("alien",trait)]
    }
  }
  
  realized_edge_color_traits <- trait_edge_color[unique(coef_tra[,.(marker_edge_trait)])][order(trait)]
  # if there is only one endangered species or none
  if (coef_tra[endangered==TRUE,.N]<=1) {
    plot_endang_arrow <- FALSE
    
    # if there are no endangered species
    if (nrow(realized_edge_color_traits)<2) {
      marker_edge_legend_show <- "none"
    }
    
  } else {
    annotation_text <- paste0(annotation_text,"red arrow is median of endangered\n")
  }
  
  # set x axis range
  rang <- range(coef_tra$coef) %>% diff
  marg.x <- rang*0.15
  
  # if markdown is knit to pdf, reverse hebrew text
  curr_device <- knitr::opts_chunk$get("dev")
  if (export_plot || (!is.null(curr_device) && grepl("pdf",curr_device))) {
    coef_tra[,HebName:=factor(strReverse(as.character(HebName)))]
  }
  
  p <- ggplot(coef_tra[order(coef)], aes(coef,as.numeric(rownames(coef_tra)))) +
    theme_bw() +
    geom_vline(xintercept = 0, color = "grey", size=1.5, linetype = "dashed")
  if (plot_syn_inv_arrow) p <- p + geom_segment(x = median(coef_tra[synan_invas==TRUE,coef]), y = 3, xend = median(coef_tra[synan_invas==TRUE,coef]), yend = 0.1,lineend = "round",
                                                linejoin = "round", size = 1.5, arrow = arrow(length = unit(0.3, "cm")),colour = "black")
  if (plot_endang_arrow) p <- p + geom_segment(x = median(coef_tra[endangered==TRUE,coef]), y = 3, xend = median(coef_tra[endangered==TRUE,coef]), yend = 0.1,lineend = "round",
                                               linejoin = "round", size = 1.5, arrow = arrow(length = unit(0.3, "cm")),colour = "red")
  
  if (plot_batha_arrow) {
    batha_arrow_y_offset <- 0
    # check if batha arrow overlaps endangered arrow; if yes then plot batha arrow above endangered arrow
    if (plot_endang_arrow) {
      if (median(coef_tra[endangered==TRUE,coef]) == median(coef_tra[batha_only==TRUE,coef])) {
        batha_arrow_y_offset <- 3
      }
    }
    p <- p  + geom_segment(x = median(coef_tra[batha_only==TRUE,coef]), y = 3+batha_arrow_y_offset,
                           xend = median(coef_tra[batha_only==TRUE,coef]), yend = 0.1+batha_arrow_y_offset,
                           lineend = "round",linejoin = "round", size = 1.5,
                           arrow = arrow(length = unit(0.3, "cm")),colour = "lightgreen")
  }
  
  p <- p +
    theme(axis.ticks.y=element_blank(), axis.text.y = element_blank(),
          panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_line(color = "lightgrey", size = 0.2),
          panel.grid.major.x = element_line(color = "lightgrey", size = 0.2)) +
    xlim (min(coef_tra$coef)-marg.x*2, max(coef_tra$coef)+marg.x) +
    
    geom_text(aes(label = paste0("(",species_abundance,") ",HebName), color=font_color_trait), size=label_text_size, hjust=1, nudge_x = -rang*0.02, family = fontname) +
    scale_color_manual(values = realized_font_color_traits$color) +
    guides(color = color_legend_show)
  
  # call ggnewscale::new_scale_color to use a different color scale on points
  p <- p + new_scale_color() +
    geom_point(aes(fill=factor(pval<0.1), shape=marker_shape_trait, color=marker_edge_trait), size=marker_size, stroke=stroke_width) +
    scale_color_manual(values = realized_edge_color_traits$color) +
    scale_shape_manual(values = realized_shape_traits$shape) +
    scale_fill_discrete(breaks = factor(TRUE), labels = "P<0.1", type = c("white","black")) +
    guides(shape = shape_legend_show, color = marker_edge_legend_show, fill = fill_legend_show,
           colour_new=color_legend_show) +  # colour_new is the new name of the old property "color" once new_scale_color is called
    xlab(plot_xlabel) +
    ylab("") +
    ggtitle(plot_title)
  
  # remove legend labels
  # p <- p + labs(fill = "", shape = "", color = "")
  
  print(paste0(annotation_text,"\n"))
  
  # if font name specified then apply
  if (!is.null(fontname)) {
    p <- p + theme(text=element_text(family = fontname))
  }
  
  #apply default font size
  p$theme$text$size <- fontsize
  
  if (is.null(filename)) {
    if (length(unique(D$unit))>1) {
      filename <- paste("birds_multiple_units_species_composition",effect_str,sep = "_")
    } else {
      filename <- paste("birds",unique(D$unit),"species_composition",effect_str,sep = "_")
    }
  }
  if (export_plot) {
    Cairo(file = file.path(outpath,filename), width = pdf_width, height = pdf_width*pdf_aspect_ratio, type = "PDF", units = "mm")
    print(p)
    dev.off()
  }
  
  return(p)
}