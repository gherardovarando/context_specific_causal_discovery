library(SID)
library(stagedtrees)
library(ggplot2)
source("interventional_distance.R")
p <- 5
l <- 2
k <- 3

vs <- paste0("X", seq(p))
values <- sapply(vs, function(v){
  nl <- 1 + sample(l -1, size = 1)
  as.character(1:nl)
} , USE.NAMES = TRUE, simplify = FALSE)



res <- replicate(1000, {

  bn1 <- bnlearn::random.graph(vs, 
                               method = "melancon")
  model1 <- as_sevt(bn1, values = values)
  A1 <- bnlearn::as.graphAM(bn1)@adjMat
  bn2 <- bnlearn::random.graph(vs, 
                               method = "melancon")
  model2 <- as_sevt(bn2, values = values)
  A2 <- bnlearn::as.graphAM(bn2)@adjMat
  
  return(c(sid = SID::structIntervDist(A1, A2)$sid,
  intdist = int_dist(model1, model2)$dist))
})

plot(t(res))
D <- as.data.frame(t(res)) 
colnames(D) <- c("SID", "CID")

ggplot(D) + geom_density_2d(aes(x = SID, y = CID)) + theme_bw()

ggplot(D,aes(x=SID,y=CID))+
  ylim(c(-1, 5)) + xlim(-1, 21) +
  stat_density2d(aes(alpha=..level..), geom="polygon", fill = "black") +
  scale_alpha_continuous(limits=c(0,0.5),breaks=seq(0,0.5,by=0.05), guide = FALSE)+
  geom_point(colour="red", alpha=0.02)+
  theme_bw() + ggsave(filename = "density.pdf", path = "plot/", width = 3.5, height = 2)
