plot_two_coefs_with_traits <- function (D,cp,plot_title,plot_xlabel,plot_ylabel,mark_batha=TRUE) {
  # D is a unit-specific data table where each row is an observation in a given species in a given plot and date.
  # cp is a 5 column table: scientific name, coefficients of the two levels (named coef.x and coef.y) and the p-value of the effect (as extracted from the MVabund model and converted to log2 scale), and total abundance for each species.
  # plot_title is the title of the plot
  # plot_xlabel is the label of the x axis, plot_ylabel of y axis
  # mark_batha is a boolean indicating whether to mark batha specialists with a different marker shape. If there are no batha specialists then mark_batha is set to FALSE, regardless of the value given at function call
  
  # the function returns ggplot objects to be plotted
  
  require(ggrepel)
  require(ggplot2)
  
  label_text_size <- 3
  trait_colors <- data.table(trait = factor(c("endangered","other","synanthrope / invasive"), levels = c("endangered","other","synanthrope / invasive")),
                             color = c("red","aquamarine4","black"))
  setkey(trait_colors,trait)
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
  coef_tra[,trait_general:=if_else(synan_invas & !batha_endang, "synanthrope / invasive",
                                   if_else(endangered & !synan_invas, "endangered","other"))]
  
  # plot----
  # test if there are missing traits before plotting
  color_legend_show <- "legend"
  realized_traits <- trait_colors[unique(coef_tra[,.(trait_general)])][order(trait)]
  if (nrow(realized_traits)<3 & !("endangered" %in% realized_traits$trait) & !("synanthrope / invasive" %in% realized_traits$trait)) {
      color_legend_show <- "none"
  }
  
  if (mark_batha) {
    marker_scale_val <- c(21,23)
    marker_legend_show <- "legend"
  } else {
    marker_scale_val <- c(21,21)
    marker_legend_show <- "none"
  }
  
  # only coefs
  p1 <- ggplot(coef_tra, aes(coef.x,coef.y)) +
    geom_hline(yintercept = 0, color = "grey", size=1.5, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "grey", size=1.5, linetype = "dashed") +
    geom_point(aes(fill=factor(pval<0.1)),shape = 21, color = "black", size=3, alpha=0.7) + 
    scale_fill_discrete(breaks = factor(TRUE), labels = "P<0.1", type = c("grey","black")) +
    geom_text_repel(aes(label = paste0("(",species_abundance,") ",HebName)), size=label_text_size, color="black", force_pull = 0, max.overlaps = 50,
                    max.iter = 1e5, box.padding = 0.7, min.segment.length = 0) +
    labs(fill = "", shape = "", color = "") +
    xlab(plot_xlabel) +
    ylab(plot_ylabel) +
    ggtitle(plot_title)
  
  # with traits
  p2 <- ggplot(coef_tra, aes(coef.x,coef.y)) +
    geom_hline(yintercept = 0, color = "grey", size=1.5, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "grey", size=1.5, linetype = "dashed") +
    geom_point(aes(fill=factor(pval<0.1), shape=batha_only), color = "black", size=3, alpha=0.7) +
    scale_shape_manual(values = marker_scale_val, labels = c("other","batha specialist")) +
    scale_fill_discrete(breaks = factor(TRUE), labels = "P<0.1", type = c("grey","black")) +
    geom_text_repel(aes(label = paste0("(",species_abundance,") ",HebName), color = trait_general), size=label_text_size, force_pull = 0, max.overlaps = 50,
                    max.iter = 1e5, box.padding = 0.7, min.segment.length = 0) +
    scale_color_manual(values = realized_traits$color) + 
    guides(shape = marker_legend_show, color = color_legend_show) +
    labs(fill = "", shape = "", color = "") +
    xlab(plot_xlabel) +
    ylab(plot_ylabel) +
    ggtitle(plot_title)
  
  # with x=y diagonal
  p3 <- ggplot(coef_tra, aes(coef.x,coef.y)) +
    geom_hline(yintercept = 0, color = "grey", size=1.5, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "grey", size=1.5, linetype = "dashed") +
    geom_point(aes(fill=factor(pval<0.1), shape=batha_only), color = "black", size=3, alpha=0.7) +
    scale_shape_manual(values = marker_scale_val, labels = c("other","batha specialist")) +
    scale_fill_discrete(breaks = factor(TRUE), labels = "P<0.1", type = c("grey","black")) +
    geom_text_repel(aes(label = paste0("(",species_abundance,") ",HebName), color = trait_general), size=label_text_size, force_pull = 0, max.overlaps = 50,
                    max.iter = 1e5, box.padding = 0.7, min.segment.length = 0) +
    scale_color_manual(values = realized_traits$color) + 
    guides(shape = marker_legend_show, color = color_legend_show) +
    labs(fill = "", shape = "", color = "") +
    xlab(plot_xlabel) +
    ylab(plot_ylabel) +
    ggtitle(plot_title) +
    geom_abline(color = "grey", size=1.5, linetype="dashed")
  
  coef_plots <- list(p1,p2,p3)
}