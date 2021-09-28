#################################################################
# Plot heatmap of Z for Austria (product x product)
#################################################################

rm(list = ls()); gc()
agg <- function(x) {
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x) }

Z <- readRDS("data/Z.rds")
Z <- Z[["2017"]]
Y <- readRDS("data/Y.rds")
Y <- Y[["2017"]]

regions <- fread("inst/regions.csv")
regions <- regions[baci==TRUE]
regions <- regions[order(area_code)]

colnames(Z) <- rownames(Z) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=23)))
colnames(Y) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=2)))
rownames(Y) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=23)))

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


#################################################################
# Plot heatmap of Z for USA (product x product)
#################################################################
rm(list = ls()); gc()
agg <- function(x) {
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x) }

Z <- readRDS("data/Z.rds")
Z <- Z[["2017"]]
Y <- readRDS("data/Y.rds")
Y <- Y[["2017"]]

regions <- fread("inst/regions.csv")
regions <- regions[baci==TRUE]
regions <- regions[order(area_code)]

# USA's area_code is 231
reg <- 231
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

regions <- read_csv("inst/regions.csv")
regions <- regions[baci==TRUE]
regions <- regions[order(area_code)]

Z <- readRDS("data/Z.rds")
Z <- as.matrix(Z[["2017"]])
Y <- readRDS("data/Y.rds")
Y <- as.matrix(Y[["2017"]])
colnames(Z) <- rownames(Z) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=23)))
colnames(Y) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=2)))
rownames(Y) <- paste0("r",sprintf("%03d", rep(regions$area_code, each=23)))

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

Z <- readRDS("data/Z.rds")
Z <- Z[["2017"]]
Y <- readRDS("data/Y.rds")
Y <- Y[["2017"]]

regions <- read_csv("inst/regions.csv")
# India is No 81 in the regions list
reg <- 81
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
rm(list = ls()); gc()
agg <- function(x) {
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x) }

Z <- readRDS("data/Z.rds")
Z <- Z[["2017"]]
Y <- readRDS("data/Y.rds")
Y <- Y[["2017"]]

regions <- read_csv("inst/regions.csv")
# Chile is No 32 in the regions list
reg <- 32
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
