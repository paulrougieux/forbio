library(ggplot2)
library(data.table)
library("Matrix")

agg <- function(x) {
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x) }

# Standard heatmaps ---------------------------------------------------------------------

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

Y <- Y[["1997"]]
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

# Heatmaps single countries --------------------------------------------------------------------

#################################################################
# Plot heatmap of Z for Austria (product x product)
#################################################################

rm(list = ls()); gc()

Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")
Z <- Z[["2017"]]
Y <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Y.rds")
Y <- Y[["2017"]]

regions <- fread("inst/regions.csv")
regions <- regions[baci==TRUE]
regions <- regions[order(area_code)]

# colnames(Z) <- rownames(Z) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=23)))
# colnames(Y) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=2)))
# rownames(Y) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=23)))

# Austria's area_code is 11
# country <- 9
# Austria position in list is
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

# Heatmap domestic use
p <- ggplot(Zdom, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_dom)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_aut_dom.png", plot = p, dpi = 640)

# Heatmap import use
p <- ggplot(Zimp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_imp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_aut_imp.png", plot = p, dpi = 640)

# Heatmap export use
p <- ggplot(Zexp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_exp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_aut_exp.png", plot = p, dpi = 640)


#################################################################
# Plot heatmap of Z for USA (product x product)
#################################################################
rm(list = ls()); gc()

Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")
Z <- Z[["2017"]]
Y <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Y.rds")
Y <- Y[["2017"]]

regions <- fread("inst/regions.csv")
regions <- regions[baci==TRUE]
regions <- regions[order(area_code)]

# USA's area_code is 231
reg <- 231

# Here it stops working
# Warning message:
# Error in intI(i, n = d[1], dn[[1]], give.dn = FALSE) : 
# index larger than maximal 5106

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

ggsave(filename = "./output/heatmap_z_usa_dom.png", plot = p, dpi = 640)

p <- ggplot(Zimp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_imp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_usa_imp.png", plot = p, dpi = 640)

p <- ggplot(Zexp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_exp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_usa_exp.png", plot = p, dpi = 640)


#################################################################
# Plot heatmap of Z for China (product x product)
#################################################################
rm(list = ls()); gc()
agg <- function(x) {
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x) }

regions <- fread("inst/regions.csv")
regions <- regions[baci==TRUE]
regions <- regions[order(area_code)]

Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")
Z <- as.matrix(Z[["2017"]])
Y <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Y.rds")
Y <- as.matrix(Y[["2017"]])

# colnames(Z) <- rownames(Z) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=23)))
# colnames(Y) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=2)))
# rownames(Y) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=23)))

# China's area_code is 41
country <- 41

colnames(Z)[colnames(Z) != paste0("r",sprintf("%03d", country))] <- "r999"
rownames(Z)[rownames(Z) != paste0("r",sprintf("%03d", country))] <- "r999"
colnames(Z) <- paste0(colnames(Z), "_", sprintf("%02d", rep(1:23, 222)))
rownames(Z) <- paste0(rownames(Z), "_", sprintf("%02d", rep(1:23, 222)))
Z <- t(agg(t(agg(Z[order(rownames(Z)), order(colnames(Z))]))))

colnames(Y)[colnames(Y) != paste0("r",sprintf("%03d", country))] <- "r999"
rownames(Y)[rownames(Y) != paste0("r",sprintf("%03d", country))] <- "r999"
colnames(Y) <- paste0(colnames(Y), "_", rep(1:2, 222))
rownames(Y) <- paste0(rownames(Y), "_", sprintf("%02d", rep(1:23, 222)))
Y <- t(agg(t(agg(Y[order(rownames(Y)), order(colnames(Y))]))))

Z[24:46, 24:46] <- 0
Y[24:46, 3:4] <- 0

X <- cbind(Z,Y)

X <- data.table(X)
# Add rownames to the data frame as a column
X <- cbind(data.table(rnames=sprintf("%03d", 1:46)),X)
# melt data for ggplot to draw graphs
X <- data.table::melt(X)

X$value <- round(X$value)

X$rnames <- factor(X$rnames, labels = 46:1, levels = X$rnames[46:1])
levels(X$variable) <- 1:50
X$log <- log(X$value)
X$log[!is.finite(X$log)] <- 0


p <- ggplot(X, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(X)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:50)), position = "top") + 
  scale_y_discrete(breaks = c((1:46)))

# p <- ggplot(X, aes(variable, rnames)) + geom_tile(aes(fill = value), colour = "white") + 
#   viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="X") +
#   theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:50)), position = "top") + 
#   scale_y_discrete(breaks = c((1:46)))

ggsave(filename = "./output/heatmap_z_chn_dom.png", plot = p, dpi = 640)

p <- ggplot(Zimp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_imp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_chn_imp.png", plot = p, dpi = 640)

p <- ggplot(Zexp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_exp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_chn_exp.png", plot = p, dpi = 640)

#################################################################
# Plot heatmap of Z for India (product x product)
#################################################################

rm(list = ls()); gc()
agg <- function(x) {
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x) }

Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")
Z <- Z[["2017"]]
Y <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Y.rds")
Y <- Y[["2017"]]

regions <- fread("inst/regions.csv")
regions <- regions[baci==TRUE]
regions <- regions[order(area_code)]

# India is No 81 in the regions list
# reg <- 81
# but position 85 in the list
reg <- 85
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

ggsave(filename = "./output/heatmap_z_ind_dom.png", plot = p, dpi = 640)

p <- ggplot(Zimp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_imp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_ind_imp.png", plot = p, dpi = 640)

p <- ggplot(Zexp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_exp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_ind_exp.png", plot = p, dpi = 640)

#################################################################
# Plot heatmap of Z for Chile (product x product)
#################################################################

# This graph works as well

rm(list = ls()); gc()
agg <- function(x) {
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x) }

Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")
Z <- Z[["2017"]]
Y <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Y.rds")
Y <- Y[["2017"]]

regions <- fread("inst/regions.csv")
regions <- regions[baci==TRUE]
regions <- regions[order(area_code)]

# Chile is No 32 in the regions list
# reg <- 32
# But Chile has position #34 in the list
reg <- 34

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

ggsave(filename = "./output/heatmap_z_chl_dom.png", plot = p, dpi = 640)

p <- ggplot(Zimp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_imp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_chl_imp.png", plot = p, dpi = 640)

p <- ggplot(Zexp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_exp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:25)), position = "top") + 
  scale_y_discrete(breaks = c((1:23)))

ggsave(filename = "./output/heatmap_z_chl_exp.png", plot = p, dpi = 640)


# Heatmaps Country and RoW ---------------------------------------------------------------------

#################################################################
# Plot heatmap of Z and Y for China and RoW (product x product)
#################################################################
rm(list = ls()); gc()
agg <- function(x) {
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x) }

regions <- fread("inst/regions.csv")
regions <- regions[baci==TRUE]
regions <- regions[order(area_code)]

Z <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Z.rds")
Z <- Z[["1997"]]
Y <- readRDS("/mnt/nfs_fineprint/tmp/forbio/Y.rds")
Y <- Y[["1997"]]

colnames(Z) <- rownames(Z) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=23)))
colnames(Y) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=2)))
rownames(Y) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=23)))

# China's area_code is 41, but no 35 in the list
country <- 41
#country <- 35

colnames(Z)[colnames(Z) != paste0("r",sprintf("%03d", country))] <- "r999"
rownames(Z)[rownames(Z) != paste0("r",sprintf("%03d", country))] <- "r999"
colnames(Z) <- paste0(colnames(Z), "_", sprintf("%02d", rep(1:23, 222)))
rownames(Z) <- paste0(rownames(Z), "_", sprintf("%02d", rep(1:23, 222)))
Z <- t(agg(t(agg(Z[order(rownames(Z)), order(colnames(Z))]))))

colnames(Y)[colnames(Y) != paste0("r",sprintf("%03d", country))] <- "r999"
rownames(Y)[rownames(Y) != paste0("r",sprintf("%03d", country))] <- "r999"
colnames(Y) <- paste0(colnames(Y), "_", rep(1:2, 222))
rownames(Y) <- paste0(rownames(Y), "_", sprintf("%02d", rep(1:23, 222)))
Y <- t(agg(t(agg(Y[order(rownames(Y)), order(colnames(Y))]))))

Z[24:46, 24:46] <- 0
Y[24:46, 3:4] <- 0

X <- cbind(Z,Y)

X <- data.table(X)
# Add rownames to the data frame as a column
X <- cbind(data.table(rnames=sprintf("%03d", 1:46)),X)
# melt data for ggplot to draw graphs
X <- data.table::melt(X)

X$value <- round(X$value)

X$rnames <- factor(X$rnames, labels = 46:1, levels = X$rnames[46:1])
levels(X$variable) <- 1:50
X$log <- log(X$value)
X$log[!is.finite(X$log)] <- 0


p <- ggplot(X, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(X)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c((1:50)), position = "top") + 
  scale_y_discrete(breaks = c((1:46)))

ggsave(filename = "./output/heatmap_z_y_chn_row.png", plot = p, dpi = 640)
