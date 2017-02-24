library(gridExtra)

NACount <- function(col){
  return(table(is.na(col)))
}

plot_box_and_histograms <- function(DT){
  histogram_plots <- list()
  box_plots <- list()
 character_cols = sapply(DT,is.character)
 factor_cols = sapply(DT, is.factor)
 final_cols = character_cols
 final_cols[factor_cols == TRUE] <- TRUE
 character_factor_subset = as.data.frame(subset(DT, select = final_cols))
 for(index in 1:(ncol(character_factor_subset))){
   current_subset = data.frame(X = character_factor_subset[,index])
   current_subset = cbind(current_subset, DT$SalePrice)
   print(colnames(current_subset))
   current_subset_name = colnames(character_factor_subset)[index]
   histogram_plots[[index]] <- ggplot(data=current_subset, aes(x=X)) + xlab(current_subset_name) + ylab("Frequency") + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
   box_plots[[index]] <- ggplot(data=current_subset, aes(x=as.factor(X), y=`DT$SalePrice`)) + xlab(current_subset_name) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
 }
 ggsave("plots/Histogram.pdf", marrangeGrob(grobs = histogram_plots, nrow = 1, ncol = 1))
 ggsave("plots/BoxPlots.pdf", marrangeGrob(grobs = box_plots, nrow = 1, ncol = 1))
}

rename_column <- function(DT, old_name, new_name){
  names(DT)[names(DT) == old_name] <- new_name
  return(DT)
}