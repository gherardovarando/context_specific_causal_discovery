library(ggplot2)
library(data.table)
library(plyr)

ps <- c(2, 5) ## num variables
Ns <- c(100, 250, 500, 1000, 2000, 3000, 5000, 10000) ## sample size
ks <- c(2, 3, 4) ## complexity (stages per stratum or prob_edges = k / (p-1))
ls <- c(4) ## maximum number of levels per variable 

sampling_methods_names <- c("random_sevt")
dtall <- NULL
for (nm in sampling_methods_names){
  for (p in ps){
    for (N in Ns){
      for (k in ks){
        for (l in ls){
          fnam <- paste0("results/",  nm, "/","p", p, "N", N, "k", k, 'l', l, "results.rds")
          if (file.exists(fnam)){
            results <- readRDS(fnam)
            bic.df <- as.data.frame(t(sapply(results, getElement, 'bic')))
            dist.df <- as.data.frame(t(sapply(results, getElement, 'distance')))
            cid.df <- as.data.frame(t(sapply(results, getElement, 'cid')))
            time.df <- as.data.frame(t(sapply(results, getElement, 'time')))
            dist.dt <- melt(as.data.table(dist.df), measure.vars = colnames(dist.df), variable.name = "algorithm", value.name = "distance")
            cid.dt <- melt(as.data.table(cid.df), measure.vars = colnames(intdist.df), variable.name = "algorithm", value.name = "cid")
            bic.dt <- melt(as.data.table(bic.df), measure.vars = colnames(bic.df), variable.name = "algorithm", value.name = 'bic')
            time.dt <- melt(as.data.table(time.df), measure.vars = colnames(time.df), variable.name = "algorithm", value.name = 'time')
            dt <- cbind(dist.dt, cid = cid.dt$cid, bic = bic.dt$bic, time = time.dt$time, p = p, N = N, k = paste0("k=",k), l = paste0("l=",l), nm = nm)
            if (is.null(dtall)){
              dtall <- dt
            }else{
              dtall <- rbind(dtall, dt)
            }  
          }
        }
      }
    }
  }
  
}

selected <- c("best_bhc", "best_kmeans", "bn_tabu", "bn_mmhc")
path <- "plot"
dir.create(path)
dt1 <- data.table(ddply(dtall, .(p, N, k, l, nm, algorithm), summarise, 
	     time = mean(time,na.rm = TRUE),
             KD = mean(distance, na.rm = TRUE), 
             CID = mean(cid, na.rm = TRUE)))
dt1 <- dt1[algorithm %in% selected] 


ggplot(dt1[N==3000], aes(x=p, y=time, 
				    group = algorithm, color = algorithm, fill = algorithm)) +
   scale_x_log10() + 
   geom_point() + facet_grid(cols = vars(l), rows = vars(k))+ theme_bw() + 
   ggsave(
        file = "plot_time_N3000.pdf",
        path = path,
        width = 5.5,
        height = 4,
        units = "in"
      )



dt2 <- melt(dt1, measure.vars = c("time", "CID", "KD")) 

ggplot(dt2[nm == "random_sevt" & p == 5 & l == "l=4" & variable %in% c("CID", "KD")], 
       aes(x=N, y=value, group = algorithm, color = algorithm)) + 
       geom_line() + geom_point() + 
       facte_grid(rows = vars(variable), cols = vars(k)) + 
       theme_bw() + 
       scale_x_log10(breaks = c(100,250,500,1000,3000, 10000)) + 
       theme(legend.position = "bottom", axis.text.x = element_text(angle=45)) + 
       ggsave(file = "plot_both_N_p5_l4_random_sevt.pdf", path = path, 
	      width = 5.5, height = 4, units = "in")



