library(here)
library(tidyverse)
library(conflicted)
library(ggthemes)

myfiles <- list.files(path = here("huPCLS_qPCR_plots", "data"), full.names = T, recursive = F)
data_list <- list()
for (i in 1:length(myfiles)) {
  data_list[[i]] <- read_csv(file = myfiles[i])
  colnames(data_list[[i]]) <-  c("target", "sample", "Cq", NA, NA, "dCq", "ddCq", "lfc") 
  data_list[[i]] <- data_list[[i]] %>% dplyr::select(c("target", "sample", "Cq", "dCq", "ddCq", "lfc"))
  data_list[[i]] <- data_list[[i]] %>% separate(col = sample, into = c("cocktail", "tx"), sep = "_")
  data_list[[i]]$Cq <- as.numeric(data_list[[i]]$Cq)
  data_list[[i]]$dCq <- as.numeric(data_list[[i]]$dCq)
  data_list[[i]]$ddCq <- as.numeric(data_list[[i]]$ddCq)
  data_list[[i]]$lfc <- as.numeric(data_list[[i]]$lfc)
}

ggtitles <- c("PCLS10", "PCLS13", "PCLS15", "PCLS16")
yaxes <- colnames(data_list[[1]])[c(4,5,7)]
plot_list_nested <- list()
# Question: what is wrong with the below for loop?
# My goal is to create a list, plot_list_nested, containing three plot_grid objects, each plot_grid object containing four plots. 
# Each ggplot should have target on the x-axis and one of Cq, dCq, or ddCq on the y-axis. 
# plot_list_nested[[1]] should have Cq, plot_list_nested[[2]] should have dCq, and plot_list_nested[[3]] should have ddCq on the y-axes.
# Each plot_grid object should have four plots: one whose data comes from data_list[[1]], one from data_list[[2]], and so on.

for (x in 1:length(yaxes)) { # x iterates for each of Cq, dCq, ddCq
  for (i in 1:length(data_list)) { # i iterates for each sample in data_list
    plot_list_nested[[x]] <- cowplot::plot_grid(
        data_list[[i]] %>%
        ggplot(aes(x = target, y = yaxes[x], color = cocktail, shape = tx)) +
        geom_point() +
        geom_jitter(width = .4, size = 2) + 
        theme_few() + 
        theme(axis.text.x = element_text(angle = 60, hjust=1)) +
        ggtitle(ggtitles[i])
    )
  }
}

# My output plot_list_nested is a list of three individual plots, apparently made from the data in data_list[[4]], which have incorrect y-axes.