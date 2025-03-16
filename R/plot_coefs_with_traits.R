plot_coefs_with_traits <- function (D,cp,plot_title,plot_xlabel,mark_batha=TRUE, is_NT_endangered = FALSE) {
  # D is a unit-specific data table where each row is an observation in a given species in a given plot and date.
  # cp is a 4 column table: scientific name, coefficient and p-value (as extracted from the MVabund model and converted to log2 scale), and total abundance for each species.
  # plot_title is the title of the plot
  # plot_xlabel is the label of the x axis
  # mark_batha is a boolean indicating whether to mark batha specialists with a different marker shape. If there are no batha specialists then mark_batha is set to FALSE, regardless of the value given at function call
  # is_NT_endangered is a boolean indicating whether to include NT in the category of threatened species (default is FALSE, concordant with IUCN)
  
  # the function returns ggplot objects to be plotted
  
  label_text_size <- 3
  trait_colors <- data.table(trait = factor(c("endangered","other","synanthrope / invasive"), levels = c("endangered","other","synanthrope / invasive")),
                             color = c("red","aquamarine4","black"))
  setkey(trait_colors,trait)
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
  # regardless of the value given at function call, if there are no batha specialists then set mark_batha to FALSE
  if(!any(coef_tra$batha_only)) mark_batha <- FALSE
  
  # define endangered species
  coef_tra[,endangered:=ConservationCodeIL2018_ordinal>=ifelse (is_NT_endangered, 2, 3)]
  coef_tra[is.na(endangered),endangered:=FALSE]
  
  # combine batha and endangered
  coef_tra[,batha_endang:=batha_only | endangered]
  
  # create color and shape variables for species traits
  coef_tra[,trait_general:=if_else(synan_invas & !batha_endang, "synanthrope / invasive",
                                   if_else(endangered & !synan_invas, "endangered","other"))]
  coef_tra[,marker_shape:=if_else(batha_only,23,21)]
  #coef_tra[,face:=if_else(batha_only,"italic","plain")]
  # coef_tra[,data_labels:=if_else(batha_only,paste0("*",HebName),HebName)]
  
  # plot----
  # test if there are missing traits before plotting
  plot_syn_inv_arrow <- TRUE
  plot_endang_arrow <- TRUE
  annotation_text <- "arrows are median of\nsynanthrope / invasive in black\nendangered in red"
  color_legend_show <- "legend"
  realized_traits <- trait_colors[unique(coef_tra[,.(trait_general)])][order(trait)]
  if (nrow(realized_traits)<3) {
    if (!("endangered" %in% realized_traits$trait) & !("synanthrope / invasive" %in% realized_traits$trait)) {
      annotation_text <- ""
      plot_syn_inv_arrow <- FALSE
      plot_endang_arrow <- FALSE
      color_legend_show <- "none"
    } else if (!("endangered" %in% realized_traits$trait)) {
      annotation_text <- "arrow is median of\nsynanthrope / invasive in black"
      plot_endang_arrow <- FALSE
    } else if (!("synanthrope / invasive" %in% realized_traits$trait)) {
      annotation_text <- "arrow is median of\nendangered in red"
      plot_syn_inv_arrow <- FALSE
    }
  }
  
  if (mark_batha) {
    marker_scale_val <- c(21,23)
    marker_legend_show <- "legend"
    annotation_text <- paste0(annotation_text,"\nbatha in blue")
  } else {
    marker_scale_val <- c(21,21)
    marker_legend_show <- "none"
  }
  
  rang <- range(coef_tra$coef) %>% diff
  marg.x <- rang*0.1
  # only coefs
  p1 <- ggplot(coef_tra[order(coef)], aes(coef,as.numeric(rownames(coef_tra)))) +
    geom_vline(xintercept = 0, color = "grey", size=1.5, linetype = "dashed") + 
    theme(axis.ticks.y=element_blank(), axis.text.y = element_blank()) +
    xlim (min(coef_tra$coef)-marg.x*2, max(coef_tra$coef)+marg.x) +
    geom_point(aes(fill=factor(pval<0.1)),shape = 21, color = "black", size=3) + 
    scale_fill_discrete(breaks = factor(TRUE), labels = "P<0.1", type = c("grey","black")) +
    geom_text(aes(label = paste0("(",species_abundance,") ",HebName)), size=label_text_size, hjust=1, nudge_x = -rang*0.02, color="black") +
    labs(fill = "", shape = "", color = "") +
    xlab(plot_xlabel) +
    ylab("") +
    ggtitle(plot_title)
  
  # add traits
  p2 <- ggplot(coef_tra[order(coef)], aes(coef,as.numeric(rownames(coef_tra)))) +
    geom_vline(xintercept = 0, color = "grey", size=1.5, linetype = "dashed") + 
    theme(axis.ticks.y=element_blank(), axis.text.y = element_blank()) +
    xlim (min(coef_tra$coef)-marg.x*2, max(coef_tra$coef)+marg.x) +
    geom_point(aes(fill=factor(pval<0.1), shape=batha_only),color = "black", size=3) +
    scale_shape_manual(values = marker_scale_val, labels = c("other","batha specialist")) +
    scale_fill_discrete(breaks = factor(TRUE), labels = "P<0.1", type = c("grey","black")) +
    geom_text(aes(label = paste0("(",species_abundance,") ",HebName), color=trait_general), size=label_text_size, hjust=1, nudge_x = -rang*0.02) +
    scale_color_manual(values = realized_traits$color) + 
    guides(shape = marker_legend_show, color = color_legend_show) +
    labs(fill = "", shape = "", color = "") +
    xlab(plot_xlabel) +
    ylab("") +
    ggtitle(plot_title)
  
  # complete plot
  p3 <- ggplot(coef_tra[order(coef)], aes(coef,as.numeric(rownames(coef_tra)))) +
    geom_vline(xintercept = 0, color = "grey", size=1.5, linetype = "dashed")
  if (plot_syn_inv_arrow) p3 <- p3 + geom_segment(x = median(coef_tra[synan_invas==TRUE,coef]), y = 3, xend = median(coef_tra[synan_invas==TRUE,coef]), yend = 0,lineend = "round",
                                                  linejoin = "round", size = 1.5, arrow = arrow(length = unit(0.3, "cm")),colour = "black")
  if (plot_endang_arrow) p3 <- p3 + geom_segment(x = median(coef_tra[endangered==TRUE,coef]), y = 3, xend = median(coef_tra[endangered==TRUE,coef]), yend = 0,lineend = "round",
                                                 linejoin = "round", size = 1.5, arrow = arrow(length = unit(0.3, "cm")),colour = "red")
  
  if (mark_batha) p3 <- p3  + geom_segment(x = median(coef_tra[batha_only==TRUE,coef]), y = 3, xend = median(coef_tra[batha_only==TRUE,coef]), yend = 0,lineend = "round",
                                           linejoin = "round", size = 1.5, arrow = arrow(length = unit(0.3, "cm")),colour = "blue")
  p3 <- p3 +
    theme(axis.ticks.y=element_blank(), axis.text.y = element_blank()) +
    xlim (min(coef_tra$coef)-marg.x*2, max(coef_tra$coef)+marg.x) +
    geom_point(aes(fill=factor(pval<0.1), shape=batha_only),color = "black", size=3) +
    scale_shape_manual(values = marker_scale_val, labels = c("other","batha specialist")) +
    guides(shape = marker_legend_show, color = color_legend_show) +
    scale_fill_discrete(breaks = factor(TRUE), labels = "P<0.1", type = c("grey","black")) +
    geom_text(aes(label = paste0("(",species_abundance,") ",HebName), color=trait_general), size=label_text_size, hjust=1, nudge_x = -rang*0.02) +
    scale_color_manual(values = realized_traits$color) + 
    labs(fill = "", shape = "", color = "") +
    xlab(plot_xlabel) +
    ylab("") +
    ggtitle(plot_title) +
    annotate("text", x = max(coef_tra$coef)*0.4, y = 5, label=annotation_text, hjust=0, size=3.5)
  
  coef_plots <- list(p1,p2,p3)
}