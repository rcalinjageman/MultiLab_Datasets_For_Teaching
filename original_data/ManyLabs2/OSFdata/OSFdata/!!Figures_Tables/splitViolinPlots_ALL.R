## (somewhat-split) VIOLIN PLOTS - MANYLABS 2
#
# Corresponding coder: Fred Hasselman (https://osf.io/ujgs6/)
#
# This script mainly shows how to reorganise the data.
# If you want to just plot the figure use: figure2_JustPlotItAlready.R

# SETUP -----------------------------------------------------------------------------------------------------------
# NOTE: Don't run init() if you do not want to load, and possiblt install packages we need to run this script!!
library(devtools)
devtools::source_url("https://raw.githubusercontent.com/ManyLabsOpenScience/manylabRs/master/R/manylabRs_SOURCE.R")
init()

devtools::source_url("https://raw.githubusercontent.com/FredHasselman/invctr/master/R/invictor.R")

library(plyr)
library(tidyverse)
library(tidyverse)

ML2.key    <- get.GoogleSheet(data='ML2masteRkey')$df
ML2.key    <- ML2.key[ML2.key$study.name!="",]
# Set to the directory where OSFdata is located
dir.in     <- "~"

# oriEffects <- get.oriESCI()$oriFULL
# oriEffects <- oriEffects[oriEffects$study.analysis%in%ML2.key$study.analysis[ML2.key$ori.study.figure2.include==1],]
# oriEffects$study.analysis.ori <- uniqueID.StudyName.WEIRD/NONWEIRD

oriEffects <- rio::import(paste0(dir.in,"/OSFdata/!!RawData/ML2_OriginalEffects.csv"))

outlist.tmp <- readRDS(paste0(dir.in,"/OSFdata/!!RawData/ML2_results_primary_all.rds"))
outlist.tmp <- outlist.tmp$aggregated
outlist1 <-ldply(outlist.tmp)
rm(outlist.tmp)

outlist.tmp <- readRDS(paste0(dir.in,"/OSFdata/!!RawData/ML2_results_secondary_all.rds"))
outlist.tmp <- outlist.tmp$aggregated
outlist2 <- ldply(outlist.tmp)
rm(outlist.tmp)

outlistAll <- rbind(outlist1[,which(colnames(outlist1)%in%colnames(outlist2))],outlist2[,which(colnames(outlist2)%in%colnames(outlist1))])

outlist.tmp <- readRDS(paste0(dir.in,"/OSFdata/!!RawData/ML2_results_global_all.rds"))
outlist.tmp <- outlist.tmp$aggregated
outlistG <- ldply(outlist.tmp)

outlistAll$GlobalES <- NA
outlistAll$splitWEIRD <- NA

for(l in unique(outlistAll$analysis.name)){
  id  <- outlistG$analysis.name%in%l
  id2 <- ML2.key$study.analysis%in%l
  if(any(id)){
    outlistAll$GlobalES.r[outlistAll$analysis.name%in%l]    <- outlistG$ESCI.r[id]
    outlistAll$GlobalESlo.r[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.l.r[id]
    outlistAll$GlobalEShi.r[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.u.r[id]
    outlistAll$GlobalES.d[outlistAll$analysis.name%in%l]    <- outlistG$ESCI.d[id]
    outlistAll$GlobalESlo.d[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.l.d[id]
    outlistAll$GlobalEShi.d[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.u.d[id]
    outlistAll$GlobalES.q[outlistAll$analysis.name%in%l]    <- outlistG$ESCI.cohensQ[id]
    outlistAll$GlobalESlo.q[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.cohensQ.l[id]
    outlistAll$GlobalEShi.q[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.cohensQ.u[id]
    outlistAll$GlobalN[outlistAll$analysis.name%in%l]       <- outlistG$stat.N[id]
    outlistAll$GlobalConsole[outlistAll$analysis.name%in%l] <- paste(outlistG$test.ConsoleOutput[id])
    outlistAll$splitWEIRD[outlistAll$analysis.name%in%l]    <- ML2.key$study.table1.weird.split[id2]
  }
}

outlistFigure <- outlistAll[outlistAll$analysis.name%in%ML2.key$study.analysis[ML2.key$study.figure2.include==1],]

cnames <- paste0("ori.",colnames(oriEffects))
outlistFigure2 <- cbind.data.frame(outlistFigure, matrix(NA,nrow=NROW(outlistFigure), ncol= length(cnames), dimnames = list(NULL,cnames)))

cID <- 1:NCOL(oriEffects)

for(o in unique(oriEffects$study.analysis.ori)){
  ID1 <- outlistFigure2$study.id%in%strsplit(o,"[.]")[[1]][1]
  ID2 <- outlistFigure2$study.name%in%strsplit(o,"[.]")[[1]][2]
  ID3 <- outlistFigure2$source.Weird==ifelse(strsplit(o,"[.]")[[1]][3]=="WEIRD",1,0)
 outlistFigure2[ID1&ID2&ID3,239:NCOL(outlistFigure2)] <- oriEffects[oriEffects$study.analysis.ori%in%o,cID]
 #outlistFigure2$ori.ESCI.r[outlistFigure2$ori.study.analysis.ori%in%o]
}
outlistFigure2$ori.source.Weird <- outlistFigure2$ori.study.analysis.ori

outlistFigure2$study.description <- ""
outlistFigure2$cohensQ <- 0
for(o in unique(ML2.key$study.id)){
  outlistFigure2$study.description[outlistFigure2$study.id%in%o] <- ML2.key$study.description[ML2.key$study.id%in%o][1]
  outlistFigure2$cohensQ[outlistFigure2$study.id%in%o] <- ifelse(!is.na(ML2.key$cohensQ[ML2.key$study.id%in%o][1]),1,0)
}

  # rio::export(outlistFigure2, "~/OSFdata/!!RawData/Data_Figure_NOweird.rds")
  # rio::export(outlistFigure2, "~/OSFdata/!!RawData/Data_Figure_NOweird.csv")


# make plot data frame ----
#df <- rio::import(paste0(,dir.in,"/OSFdata/!!RawData/Data_Figure_NOweird.csv")

df <- outlistFigure2
df$labels <- factor(as.character(df$analysis.name))
df$study.labels <- factor(as.character(df$study.description))
df <- df %>% group_by(labels) %>%
  mutate(meanES   = mean(ESCI.r, na.rm=TRUE),
         medianES = median(ESCI.r, na.rm=TRUE),
         oriES = first(ori.ESCI.r)) %>%
  ungroup()


# Arrange by MEAN
df <- df[!is.na(df$ESCI.r),]
df <- dplyr::arrange(df, labels, cohensQ, meanES)
df$study.labels <- factor(df$study.labels,ordered = TRUE)
df$study.labels <- reorder(df$study.labels, df$cohensQ+df$meanES, order=TRUE)
df$study.labels <- reorder(df$study.labels, df$cohensQ+df$meanES, order=TRUE)
studOrder <- attributes(df$study.labels)$scores
offsets   <- names(studOrder) %>% {setNames(0:(length(.) - 1), .)}
all.equal(names(studOrder),names(offsets))

Qlabs <- paste(unique(df$study.labels[df$cohensQ==1]))
ID <- df$study.labels%in%Qlabs
df$ESCI.r[ID] <- df$ESCI.cohensQ[ID]
df$ori.ESCI.r[ID] <- df$ori.ESCI.cohensQ[ID]

oriEffects$ESCI.cohensQ

okN <- df$stat.N>=30
okCond <- (df$stat.n1>=15)|(df$stat.n2>=15)

df <- df[okN|okCond, ]

df$sigf     <- "p > .05"
df$sigf[df$test.p.value<.05] <- "p < .05"
df$sigf.f <- factor(df$sigf)
df$sigf.f <- relevel(df$sigf.f, ref = "p > .05")
df$source.WEIRD.f <- factor(df$source.Weird,
                             levels = c(0,1),
                             labels = c("less WEIRD","WEIRD"))
 df$splitv <- df$source.WEIRD.f
 df$splitv <- relevel(df$splitv, ref = "less WEIRD")


# Calculate and scale group densities
pdat <- df %>%
  group_by(study.labels) %>%
  do(tidy(density(.[['ESCI.r']]))) %>%
  dplyr::rename(loc = x, dens = y) %>%
  mutate(dens = 0.45 * dens / max(dens)) %>%
  ungroup()

# Connect the extrema of the split distrubutions
for(sl in levels(pdat$study.labels)){
  ID <- pdat$study.labels%in%sl
  tmp <- pdat[ID,]
  mxID <- which.max(tmp$loc)
  mnID <- which.min(tmp$loc)
  pdat <- add_row(pdat,
                  study.labels = rep(sl,2),
                  #splitv      =  c(mxLab,mnLab),
                  loc         =  c(max(tmp$loc),min(tmp$loc)),
                  dens        = c(0,0)
                  )
  rm(tmp)
}


# Calculate summary statistics in a separate dataframe
sums <- df %>%
  group_by(study.labels, source.Source) %>%
  summarise(sample_loc = first(ESCI.r)) %>%
  ungroup() %>%
  gather(segment, loc_sum, sample_loc)

means <- df %>%
  group_by(study.labels) %>%
  summarise(mean_loc   = mean(ESCI.r, na.rm = TRUE)) %>% #median_loc = median(ESCI.r, na.rm = TRUE),
  ungroup() %>%
  gather(segment, loc_sum, mean_loc)

# Calculate the corresponding points on each group's density curve
sums <- left_join(pdat, sums, by=c('study.labels')) %>%
  group_by(study.labels, source.Source) %>%
  do(data.frame(loc     = unique(.$loc_sum),
                dens = approx(.$loc, .$dens, unique(.$loc_sum))[['y']])) %>%
  ungroup()

means <- left_join(pdat, means, by=c('study.labels')) %>%
  group_by(study.labels) %>%
  do(data.frame(loc     = unique(.$loc_sum),
                dens = approx(.$loc, .$dens, unique(.$loc_sum))[['y']])) %>%
  ungroup()

# Offset the densities and summary statistics
pdat <- arrange(pdat, study.labels,loc)
pdat <- pdat %>%
  mutate(offset_dens = offsets[.[['study.labels']]] + dens)

means <- means %>%
  mutate(offset = offsets[.[['study.labels']]],
         offset_dens = offset + dens,
         offset_fix  = offset + .45)
means<-means[complete.cases(means),]

sums = sums %>%
  mutate(offset      = offsets[.[['study.labels']]],
         offset_dens = offset +  dens,
         offset_fix  = offset +  .2)

df$ESCI.N.bin <- cut(df$ESCI.N.total,c(0,80,200,900))
sums$offset_densN <- NA
for(smpl in unique(df$source.Source)){
  tblN <- df[df$source.Source%in%smpl,c('study.labels','ESCI.N.bin')]
  for(l in unique(tblN$study.labels)){
    sums$offset_densN[sums$source.Source%in%smpl&sums$study.labels%in%l] <- tblN$ESCI.N.bin[tblN$study.labels == l]
  }
}

sums$tsize   <- sums$offset +  2
sums$relSize <- sums$offset + ifelse(as.numeric(sums$offset_densN)==1, 1/6, ifelse(as.numeric(sums$offset_densN)==2,2/6,3/6))


# PLOT settings -----

# Colour blind safe
myCols <- brewer_pal(palette="RdYlBu")(11)

#Colorblindsafe colors
cwhite = "#f7f7f7"
ccream = "#2166ac"
cblank = "#d1e5f0"
corange = "#f4a582"
cblue  = myCols[11]  #"#2166ac"
cblueL = myCols[10]  #"#d1e5f0"
cred   = myCols[1] #"#d6604d"
credL  = myCols[2]  #"#f7f7f7"
cpurp  = "#b2abd2"

mypalette <- c(cwhite,cblueL)

outdir <- "~" #/Dropbox/Manylabs2/Figures"

cols <- c("less WEIRD" = cwhite, "WEIRD" = cblueL, "Mixed"="#5aae61")

pdat$study.labels <- ordered(as.character(pdat$study.labels), levels = names(studOrder[order(studOrder)]))

means <- means %>%
  mutate( offset_fix  = offset +  .2)

oriEffects$oriWEIRD <-NA
oriEffects$oriWEIRD[grepl(".WEIRD",oriEffects$study.analysis.ori,fixed = TRUE)] <- "WEIRD"
oriEffects$oriWEIRD[grepl(".NONWEIRD",oriEffects$study.analysis.ori,fixed = TRUE)] <- "less WEIRD"

Sbreaks <- c(24,24,23,21)
Slabels <- c("less WEIRD","WEIRD","Mixed",".")
OriShapes <- list()

pdat <- na.omit(pdat)

for(s in unique(pdat$study.labels)){

  IDs <- pdat$study.labels%in%s

  locmn <- min(pdat$loc[IDs])
  locmx <- max(pdat$loc[IDs])

  pdat$loc[IDs][IDminSV1]         <- pdat$loc[IDs][IDminSV2]         <- locmn
  pdat$offset_dens[IDs][IDminSV1] <- pdat$offset_dens[IDs][IDminSV2] <- offsets[names(offsets)%in%s]

  pdat$loc[IDs][IDmaxSV1]         <- pdat$loc[IDs][IDmaxSV2]         <- locmx
  pdat$offset_dens[IDs][IDmaxSV1] <- pdat$offset_dens[IDs][IDmaxSV2] <- offsets[names(offsets)%in%s]
}

IDq <- pdat$study.labels%in%Qlabs[1]
pdat$offset_dens[IDq] <- pdat$offset_dens[IDq]+2
IDq <- means$study.labels%in%Qlabs[1]
means$offset[IDq] <- means$offset[IDq]+2
means$offset_fix[IDq] <- means$offset_fix[IDq]+2
means$offset_dens[IDq] <- means$offset_dens[IDq]+2

IDq <- sums$study.labels%in%Qlabs[1]
sums$offset[IDq] <- sums$offset[IDq]+2
sums$offset_fix[IDq] <- sums$offset_fix[IDq]+2

IDq <- pdat$study.labels%in%Qlabs[2]
pdat$offset_dens[IDq] <- pdat$offset_dens[IDq]
IDq <- means$study.labels%in%Qlabs[2]
means$offset[IDq] <- means$offset[IDq]
means$offset_fix[IDq] <- means$offset_fix[IDq]
means$offset_dens[IDq] <- means$offset_dens[IDq]

IDq <- sums$study.labels%in%Qlabs[2]
sums$offset[IDq] <- sums$offset[IDq]
sums$offset_fix[IDq] <- sums$offset_fix[IDq]


oriEffects$offset      <- NA
oriEffects$offset_dens <- NA
oriEffects$offset_fix  <- NA
oriEffects$study.labels[oriEffects$study.analysis%in%ML2.key$study.analysis] <- ML2.key$study.description[ML2.key$study.analysis%in%oriEffects$study.analysis]

Slabels <- c("less WEIRD","WEIRD","Mixed",".")
labs <- levels(means$study.labels)
oriEffects$usethisES <- NA

for(n in seq_along(labs)){
  esID    <- oriEffects$study.labels%in%labs[n]
  meansID <- means$study.labels%in%labs[n]

  if(any(meansID,esID)){
    oriEffects$offset[esID]      <-   means$offset[meansID]
    oriEffects$offset_fix[esID]  <- means$offset_fix[meansID]
    oriEffects$offset_dens[esID] <- means$offset_dens[meansID]
    if(any(oriEffects$study.analysis[esID]%in%c("Inbar.1a","Schwarz.1a"))){
      oriEffects$usethisES[esID]   <- oriEffects$ESCI.cohensQ[esID]
    } else {
        oriEffects$usethisES[esID]   <- oriEffects$ESCI.r[esID]
    }
    }
}


for(s in seq_along(Sbreaks)){
  oriEffects$oriWEIRD.n[oriEffects$oriWEIRD%in%Slabels[s]] <- Sbreaks[s]
  OriShapes[[s]] <- Sbreaks[s]
}

oriEffects$oriWEIRD.n <- factor(oriEffects$oriWEIRD.n,Sbreaks,Sbreaks)
names(OriShapes) <- paste(Sbreaks)
OriShapes<-unlist(OriShapes)
oriEffects$offset_fix <- oriEffects$offset_fix-.4

xbreaks <- c(unname(offsets), 28,29)
xlabels <- c(names(offsets)[1:26],"",names(offsets)[c(28,27)],"")

oriEffectsNW <- oriEffects # %>% filter(oriWEIRD=="WEIRD")

pdat$loc[(pdat$loc%][%c(-1,1))&!(pdat$study.labels%in%c("Assimilation & Contrast (Schwarz et al., 1991)","Disgust & Homophobia (Inbar et al., 2009)"))] <- NA

# export(pdat,"~/OSFdata/!!RawData/Data_Figure_NOweird_long.csv")
# export(means,"~/OSFdata/!!RawData/Data_Figure_NOweird_means.csv")
# export(sums,"~/OSFdata/!!RawData/Data_Figure_NOweird_sums.csv")
# export(oriEffectsNW,"~/OSFdata/!!RawData/Data_Figure_NOweird_oriEffects.csv")



xx <- c(-.9999,-.999,-.99, -.9, -.5,-.1,-.01,-.001,0,.001,.01,.1,.5,.9,.99,.999,.9999)
reg <- lm(atanh(xx)~xx +xx^2 + xx^3+xx^4)

#predict(reg)

# PLOT it -------

g2 <- ggplot(pdat, aes(offset_dens, loc, group = study.labels)) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_vline(xintercept = xbreaks[-c(27,30)], color = "grey70") +
  geom_vline(xintercept = 26, color = "grey70", linetype=2) +
  geom_path(size=1, alpha = .5, color = "grey70")+
  geom_path() +
  geom_segment(data=sums, aes(x = offset,
                              y = loc,
                              xend = offset_fix,
                              yend = loc),
               inherit.aes=FALSE, alpha=.7, size= .2) +
  geom_segment(data=means, aes(x = offset, y = loc, xend = offset_dens, yend = loc), inherit.aes=FALSE, size=.8, alpha=.8) +
  geom_point(data=oriEffectsNW, aes(x = offset_fix, y = usethisES, shape=oriWEIRD.n, fill=oriWEIRD), colour = "black", inherit.aes = FALSE, size=1.2, alpha=.8) +
  scale_colour_manual('Sample', values = cols) +
   scale_x_continuous(name = '', breaks = xbreaks, labels = xlabels, limits = c(-0.5,max(xbreaks)+0.5), expand = c(0,0)) +
   scale_y_continuous("Effect Size r", expand = c(0,0),limits = c(-1,1), sec.axis = sec_axis(trans = ~reg$coefficients[1]+.*reg$coefficients[2], name = "Cohen's q")) +
   scale_fill_manual('',values = cols, guide=FALSE) +
   scale_shape_manual('Original Effect Size',values=OriShapes, breaks = Sbreaks,labels = c("","","","")) +
   guides(colour = guide_legend(order = 1, ncol=1,title.vjust=.81, title.theme = element_text(size = 9,angle=0,face="bold")), shape = guide_legend(ncol =1,title.position = "right",title.vjust=.81, title.theme = element_text(size = 9,angle=0,face="bold"))) +
  # facet_wrap(~q,ncol = 1, ) +
  theme_minimal() +
  theme(legend.position = "top",
        # legend.text = element_text(size=rel(.8)),
        # legend.box.background  =element_blank(),
        # legend.box.just = "left",
        # legend.spacing.x = unit(1,"lines"),
        legend.margin = margin(0,0,0,0),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(20,20,20,10)) +
  coord_flip()
g2

# SAVE ----

figname <- paste0("ML2_SplitViolin_MEANsort_QonTop_2ndAxis_with_line",as.Date(now()))
ggsave(plot = g2, filename = paste0(outdir,"/",figname,".eps"),width=10, units = "in",dpi=600)
ggsave(plot = g2, filename = paste0(outdir,"/",figname,".pdf"),width=10, units = "in",dpi=600)
ggsave(plot = g2, filename = paste0(outdir,"/",figname,".png"),width=10, units = "in",dpi=600)
ggsave(plot = g2, filename = paste0(outdir,"/",figname,".tiff"),width=10, units = "in",dpi=600)
