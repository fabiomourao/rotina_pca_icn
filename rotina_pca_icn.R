library(devtools)

library(data.table)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("tibble")
library(tibble)
#devtools::install_github("hadley/tidyverse")
library(tidyverse)

library(plyr)
library(ggplot2)
## install_github("vqv/ggbiplot")
library(ggbiplot)

### setwd('~/Dropbox/tese_fabio/dados/')

library(readxl)
div_meso_emp_06 <- read_excel("div_meso_emp_06.xls")
View(div_meso_emp_06)
str(div_meso_emp_06)

div_meso_06 <- as.data.frame(div_meso_emp_06)
attributes(div_meso_06)

head(div_meso_06)
View(div_meso_06)
rownames(div_meso_06) <- div_meso_06[,1]
attributes(div_meso_06)
str(div_meso_06)

excluir <- c("CNAE 2.0 Div", "{ñ class}", drop = FALSE)
div_meso_06 <- div_meso_06[,!(names(div_meso_06)%in% excluir)]
#div_meso_06 <- div_meso_06[-88, ]

rownames(div_meso_06)
colnames(div_meso_06)

# índice de especialização: quociente locacional (QL)

quociente_loc1 <- (div_meso_06[ , ] / div_meso_06[ ,'Total'])
quociente_loc1t <- t(quociente_loc1)
quociente_loc1t <- as.data.frame(quociente_loc1t)
quociente_loc2 <- (div_meso_06['Total', ] / div_meso_06[88,138])
quociente_loc2t <- t(quociente_loc2)
quociente_loc2t <- as.data.frame(quociente_loc2t)
quociente_loc <- ((quociente_loc1t[ , ]) / quociente_loc2t[ , ])
quociente_loc
quociente_loc <- t(quociente_loc)
quociente_loc <- as.data.frame(quociente_loc)

# índice de Hirschman-Herfindahl modificado (HHm)

hhm1 <- (div_meso_06[ , ] / div_meso_06[ ,'Total'])
hhm1t <- t(hhm1)
hhm1 <- as.data.frame(hhm1)
hhm2 <- (div_meso_06['Total', ] / div_meso_06[88,138])
hhm2t <- t(hhm2)
hhm2t <- as.data.frame(hhm2t)
hhm <- ((hhm1t[ , ]) - hhm2t[ , ])
hhm
hhm <- t(hhm)
hhm <- as.data.frame(hhm)

# índice de participação relativa do emprego (PR)

pr <- ((div_meso_06[ , ]) / div_meso_06[ ,'Total'])
pr

# análise multivariada de componentes principais

icn <- rbind(quociente_loc['26', ], hhm['26', ], pr['26', ])
icn
pca_icn <- t(icn)
pca_icn <- as.data.frame(pca_icn)
pca_icn <- pca_icn[-138, ]
#pca_icn <- pca_icn %>% slice(137:n())
pca_icn

icn_pca <- prcomp(pca_icn, center = TRUE, scale. = TRUE)
icn_pca
summary(icn_pca)

ggbiplot(icn_pca)

### Salvando o Plot

dev.copy(png,'Figures/icn_pca.png')
dev.off()

str(icn_pca)

pca_icn2 <- princomp(pca_icn, scores=TRUE, cor=TRUE)
pca_icn2
summary(pca_icn2)

# Loadings of principal components
loadings(pca_icn2)
pca_icn2$loadings

# Scree plot of eigenvalues
plot(pca_icn2)
dev.copy(png,'Figures/pca_icn2.png')
dev.off()



screeplot(pca_icn2, type="line", main="Scree Plot")
dev.copy(png,'Figures/scrplt_pca_icn2.png')
dev.off()

# Biplot of score variables
biplot(pca_icn2)
dev.copy(png,'Figures/bplt_pca_icn2.png')
dev.off()



# Scores of the components
pca_icn2$scores[1:137, ]

