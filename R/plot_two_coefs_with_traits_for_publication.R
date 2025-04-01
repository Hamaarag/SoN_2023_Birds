plot_two_coefs_with_traits_for_publication <- function (D,cp,plot_title,plot_xlabel,plot_ylabel,mark_batha=TRUE,
                                                        mark_alien=TRUE,
                                                        show_legend=TRUE,
                                                        export_plot=FALSE, fontname = "Almoni ML v5 AAA",
                                                        fontsize=10, pdf_width=160, 
                                                        outpath = "../output", effect_str=""){
  # D is a unit-specific data table where each row is an observation in a given species in a given plot and date.
  # cp is a 5 column table: scientific name, coefficients of the two levels (named coef.x and coef.y) and the p-value of the effect (as extracted from the MVabund model and converted to log2 scale), and total abundance for each species.
  # plot_title is the title of the plot
  # plot_xlabel is the label of the x axis, plot_ylabel of y axis
  # mark_batha is a boolean indicating whether to mark batha specialists with a different marker shape. If there are no batha specialists then mark_batha is set to FALSE, regardless of the value given at function call
  
  # Define a function to reverse Hebrew text in case markdown is knit to pdf
  source("strReverse.R")
  
  pdf_aspect_ratio <- 2/3
  label_text_size <- 3
  marker_size <- 2
  
  # the function returns ggplot objects to be plotted
  
  require(ggrepel)
  require(ggplot2)
  
  label_text_size <- 3
  trait_colors <- data.table(trait = factor(c("endangered","other","synanthrope / invasive"), levels = c("endangered","other","synanthrope / invasive")),
                             color = c("red",colors()[190],"black"))
  setkey(trait_colors,trait)
  trait_shapes <- data.table(trait = factor(c("alien invasive","batha specialist","other"), levels = c("alien invasive","batha specialist","other")),
                             shape = c(22,24,21))
  setkey(trait_shapes,trait)
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
  # regardless of the value given at function call, if there are no batha specialists then set mark_batha to FALSE
  if(!any(coef_tra$batha_only)) mark_batha <- FALSE
  
  # define endangered species
  coef_tra[,endangered:=ConservationCodeIL2018_ordinal>=3]
  coef_tra[is.na(endangered),endangered:=FALSE]
  
  # combine batha and endangered
  coef_tra[,batha_endang:=batha_only | endangered]
  
  # create color and shape variables for species traits
  coef_tra[,color_trait:=if_else(synan_invas & !batha_endang, "synanthrope / invasive",
                                   if_else(endangered & !synan_invas, "endangered","other"))]
  coef_tra[,marker_trait:=if_else(AlienInvasive==1,"alien invasive",
                                  if_else(batha_only,"batha specialist","other"))]
  
  # plot----
  # test if there are missing traits before plotting
  plot_syn_inv_arrow <- TRUE
  plot_endang_arrow <- TRUE
  annotation_text <- "arrows are median of\nsynanthrope / invasive in black\nendangered in red"
  if (show_legend) {
    color_legend_show <- "legend"
    shape_legend_show <- "legend"
    fill_legend_show <- "legend"
  } else {
    color_legend_show <- "none"
    shape_legend_show <- "none"
    fill_legend_show <- "none"
  }
  
  realized_color_traits <- trait_colors[unique(coef_tra[,.(color_trait)])][order(trait)]
  if (nrow(realized_color_traits)<3) {
    if (!("endangered" %in% realized_color_traits$trait) & !("synanthrope / invasive" %in% realized_color_traits$trait)) {
      annotation_text <- ""
      plot_syn_inv_arrow <- FALSE
      plot_endang_arrow <- FALSE
      color_legend_show <- "none"
    } else if (!("endangered" %in% realized_color_traits$trait)) {
      annotation_text <- "arrow is median of\nsynanthrope / invasive in black"
      plot_endang_arrow <- FALSE
    } else if (!("synanthrope / invasive" %in% realized_color_traits$trait)) {
      annotation_text <- "arrow is median of\nendangered in red"
      plot_syn_inv_arrow <- FALSE
    }
  }
  
  realized_shape_traits <- trait_shapes[unique(coef_tra[,.(marker_trait)])][order(trait)]
  if (mark_batha & any(realized_shape_traits[,grepl(pattern = "batha",trait)])) {
    annotation_text <- paste0(annotation_text,"\nbatha in green")
  } else {
    realized_shape_traits <- realized_shape_traits[!grepl("batha",trait)]
  }
  if (!(mark_alien & any(realized_shape_traits[,grepl(pattern = "alien",trait)]))) {
    realized_shape_traits <- realized_shape_traits[!grepl("alien",trait)]
  }



# if markdown is knit to pdf, reverse hebrew text
curr_device <- knitr::opts_chunk$get("dev")
if (export_plot || (!is.null(curr_device) && grepl("pdf",curr_device))) {
  coef_tra[,HebName:=factor(strReverse(as.character(HebName)))]
}

p <- ggplot(coef_tra, aes(coef.x,coef.y)) +
  geom_hline(yintercept = 0, color = "grey", size=1.5, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", size=1.5, linetype = "dashed") +
  geom_point(aes(fill=factor(pval<0.1),shape=marker_trait), color = "black", size=marker_size, alpha=0.7) + 
  scale_shape_manual(values = realized_shape_traits$shape) +
  guides(shape = shape_legend_show, color = color_legend_show, fill = fill_legend_show) +
  scale_fill_discrete(breaks = factor(TRUE), labels = "P<0.1", type = c("white","black")) +
  geom_text_repel(aes(label = paste0("(",species_abundance,") ",HebName), color=color_trait), size=label_text_size, force_pull = 0, max.overlaps = 50,
                  family = fontname,  max.iter = 1e5, box.padding = 0.7, min.segment.length = 0) +
  scale_color_manual(values = realized_color_traits$color) +
  labs(fill = "", shape = "", color = "") +
  xlab(plot_xlabel) +
  ylab(plot_ylabel) +
  ggtitle(plot_title)

p <- p + 
  theme(panel.background = element_rect(fill = "white"),
        #panel.border = element_rect(fill = "transparent", 
        #       color = "white"), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_line(color = "lightgrey", size = 0.2),
        panel.grid.major.x = element_line(color = "lightgrey", size = 0.2))

print(paste0(annotation_text,"\n"))

# if font name specified then apply
if (!is.null(fontname)) {
  p <- p + theme(text=element_text(family = fontname))
}

#apply default font size
p$theme$text$size <- fontsize

filename <- paste("birds",unique(D$unit),"species_composition",effect_str,sep = "_")
if (export_plot) {
  Cairo(file = file.path(outpath,filename), width = pdf_width, height = pdf_width*pdf_aspect_ratio, type = "PDF", units = "mm")
  print(p)
  dev.off()
}

return(p)
}
  