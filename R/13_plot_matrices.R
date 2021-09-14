library(ggplot2)
library(data.table)

#################################################################
# Plot heatmap of Z (country x country)
#################################################################
rm(list = ls()); gc()
agg <- function(x) { 
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x)) 
  return(x) }

Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")

Z <- Z[["2017"]]
colnames(Z) <- sprintf("%03d", rep(1:222, each=23))
Z <- agg(Z)
Z <- t(Z)
colnames(Z) <- sprintf("%03d", rep(1:222, each=23))
Z <- agg(Z)
Z <- t(Z)
gc()

Z <- data.table(Z)

# Add rownames to the data frame as a column
Z <- cbind(data.table(rnames=sprintf("%03d", 1:222)), Z)

# melt data for ggplot to draw graphs
Z <- data.table::melt(Z)

Z$value[Z$value<1] <- 0

Z$rnames <- factor(Z$rnames, labels = 222:1, levels = Z$rnames[222:1])
levels(Z$variable) <- 1:222

p <- ggplot(Z, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c(1,(1:22)*10), position = "top") + 
  scale_y_discrete(breaks = c(1,(1:22)*10))

ggsave(filename = "./output/heatmap_z.png", plot = p, dpi = 640)


#################################################################
# Plot heatmap of Z (product x product)
#################################################################
rm(list = ls()); gc()
agg <- function(x) { 
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x)) 
  return(x) }

Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")

Z <- Z[["2017"]]
colnames(Z) <- sprintf("%03d", rep(1:23,222))
Z <- agg(Z)
Z <- t(Z)
colnames(Z) <- sprintf("%03d", rep(1:23,222))
Z <- agg(Z)
Z <- t(Z)
gc()

Z <- data.table(Z)

# Add rownames to the data frame as a column
Z <- cbind(data.table(rnames=sprintf("%03d", 1:23)),Z)

# melt data for ggplot to draw graphs
Z <- data.table::melt(Z)

Z$value[Z$value<1] <- 0

Z$rnames <- factor(Z$rnames, labels = 23:1, levels = Z$rnames[23:1])
levels(Z$variable) <- 1:23

p <- ggplot(Z, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c(1,(1:23)), position = "top") + 
  scale_y_discrete(breaks = c(1,(1:23)))

ggsave(filename = "./output/heatmap_z_pxp.png", plot = p, dpi = 640)


#################################################################
# Plot heatmap of Y (country x country)
#################################################################
rm(list = ls()); gc()
agg <- function(x) { 
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x) }

Y <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Y.rds")

Y <- Y[["2017"]]
colnames(Y) <- rep(1:222,each=2)
Y <- agg(Y)
Y <- t(Y)
colnames(Y) <- sprintf("%03d", rep(1:222,each=23))
Y <- agg(Y)
Y <- t(Y)
gc()

Y <- data.table(Y)

# Add rownames to the data frame as a column
Y <- cbind(data.table(rnames=sprintf("%03d", 1:222)),Y)

# melt data for ggplot to draw graphs
Y <- data.table::melt(Y)

Y$value[Y$value<1] <- 0

Y$rnames <- factor(Y$rnames, labels = 222:1, levels = Y$rnames[222:1])
levels(Y$variable) <- 1:222

p <- ggplot(Y, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Y)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c(1,(1:22)*10), position = "top") + 
  scale_y_discrete(breaks = c(1,(1:22)*10))

ggsave(filename = "./output/heatmap_y.png", plot = p, dpi = 640)

#################################################################
# Plot heatmap of Z for Austria (product x product)
#################################################################
rm(list = ls()); gc()
agg <- function(x) {
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x) }

Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")
Z <- Z[["2017"]]
Y <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Y.rds")
Y <- Y[["2017"]]

regions <- read_csv("inst/regions.csv")
# Austria is No 9 in the regions list
reg <- 9
Zdom <- cbind(Z[(23*(reg-1)+1):(23*reg),(23*(reg-1)+1):(23*reg)], Y[(23*(reg-1)+1):(23*reg),(2*(reg-1)+1):(2*reg)])
Zimp <- cbind(Z[-((23*(reg-1)+1):(23*reg)),(23*(reg-1)+1):(23*reg)], Y[-((23*(reg-1)+1):(23*reg)),(2*(reg-1)+1):(2*reg)])
Zexp <- cbind(Z[(23*(reg-1)+1):(23*reg),-((23*(reg-1)+1):(23*reg))], Y[(23*(reg-1)+1):(23*reg),-((2*(reg-1)+1):(2*reg))])

colnames(Zexp) <- c(sprintf("%03d", rep(1:23,221)),paste0("f_", rep(1:2,221)))
Zexp <- agg(Zexp)

Zimp <- t(Zimp)
colnames(Zimp) <- sprintf("%03d", rep(1:23,221))
Zimp <- agg(Zimp)
Zimp <- t(Zimp)
gc()

Zdom <- data.table(as.matrix(Zdom))
Zimp <- data.table(as.matrix(Zimp))
Zexp <- data.table(as.matrix(Zexp))

# Add rownames to the data frame as a column
Zdom <- cbind(data.table(rnames=sprintf("%03d", 1:23)),Zdom)
Zimp <- cbind(data.table(rnames=sprintf("%03d", 1:23)),Zimp)
Zexp <- cbind(data.table(rnames=sprintf("%03d", 1:23)),Zexp)

# melt data for ggplot to draw graphs
Zdom <- data.table::melt(Zdom)
Zimp <- data.table::melt(Zimp)
Zexp <- data.table::melt(Zexp)

Zdom$value[Zdom$value<1] <- 0
Zimp$value[Zimp$value<1] <- 0
Zexp$value[Zexp$value<1] <- 0

Zdom$rnames <- factor(Zdom$rnames, labels = 23:1, levels = Zdom$rnames[23:1])
levels(Zdom$variable) <- 1:25
Zdom$log <- log(Zdom$value)
Zdom$log[!is.finite(Zdom$log)] <- 0
Zimp$rnames <- factor(Zimp$rnames, labels = 23:1, levels = Zimp$rnames[23:1])
levels(Zimp$variable) <- 1:25
Zimp$log <- log(Zimp$value)
Zimp$log[!is.finite(Zimp$log)] <- 0
Zexp$rnames <- factor(Zexp$rnames, labels = 23:1, levels = Zexp$rnames[23:1])
levels(Zexp$variable) <- 1:25
Zexp$log <- log(Zexp$value)
Zexp$log[!is.finite(Zexp$log)] <- 0


p <- ggplot(Zdom, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_dom)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_aut_dom.png", plot = p, dpi = 640)

p <- ggplot(Zimp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_imp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_aut_imp.png", plot = p, dpi = 640)

p <- ggplot(Zexp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_exp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_aut_exp.png", plot = p, dpi = 640)

