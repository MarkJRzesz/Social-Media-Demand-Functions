# # libraries ----
library(beezdemand)
library(tidyverse)
library(data.table)
library(nls.multstart)
library(nlme)
library(emmeans)
library(psych)

# Data import ----
importFile <- fread("socialMedia.csv")

nrow(importFile[-c(1,2),])
# data cleaning stuff---
demandFile <- importFile[-c(1,2), ]

demandFile[, ID := paste0("a", ID)]

demandFile[Q5 == "20 years old", Q5 := 20]

table(demandFile$Q5)

demandFile[as.numeric(Q5) < 100 ,] |> nrow()

demandFile <- demandFile[Q_RelevantIDFraudScore <.5 & Q_RelevantIDDuplicate !=  TRUE & as.numeric(Q5) < 100]
na.omit(demandFile) |> nrow()

demandFile[, .(attn_TikTokFaceBook, attn_VeryRarely, attn_55, attn_AlmostAlways, attn_5)]
demandFile[, attn_5clean := (gsub("\\$", "", attn_5))]
demandFile[, cbind(attn_5, attn_5clean)]
demandFile[, `:=`(att1 = attn_TikTokFaceBook == "TikTok,Facebook",
                  att2 = attn_VeryRarely == "Very Rarely",
                  att3 = attn_AlmostAlways == "Almost Always",
                  att4 = attn_55 == "55",
                  att5 = attn_5clean == 5)
]

demandFile[, colSums(data.frame(attn_TikTokFaceBook == "TikTok,Facebook",
                                attn_VeryRarely == "Very Rarely",
                                attn_AlmostAlways == "Almost Always",
                                attn_55 == "55",
                                attn_5clean == 5))
]
demandFile[, attnPass := rowSums(data.frame(attn_TikTokFaceBook == "TikTok,Facebook",
                                            attn_VeryRarely == "Very Rarely",
                                            attn_AlmostAlways == "Almost Always",
                                            attn_55 == "55",
                                            attn_5clean == 5))
]

pastYearRaw <- demandFile[, lapply(.SD, function(x) fcase(x == "No", 0,
                                                          x == "Yes", 1
)), 
.SDcols = names(demandFile)[startsWith(names(demandFile), "pastYear")]]
SMDSRawScores <- rowSums(pastYearRaw)

alpha(pastYearRaw)

lastYearRaw <- demandFile[, lapply(.SD, function(x) fcase(x == "Very Rarely", 1,
                                                          x == "Rarely", 2,
                                                          x == "Sometimes", 3,
                                                          x == "Often", 4,
                                                          x == "Very Often", 5
)), .SDcols = names(demandFile)[startsWith(names(demandFile), "lastYear")]]
BSMASRawScores <- rowSums(lastYearRaw)

alpha(lastYearRaw)

pastWeekRaw <- demandFile[, lapply(.SD, function(x) fcase(x == "Never", 0,
                                                          x == "One Time", 1,
                                                          x == "Two Times", 2,
                                                          x == "Three Times", 3,
                                                          x == "Four Times", 4,
                                                          x == "Five Times", 5,
                                                          x == "Six Times", 6,
                                                          x == "Seven Times", 7
)), .SDcols = names(demandFile)[startsWith(names(demandFile), "pastWeek")]]
SMEQRawScores <- rowSums(pastWeekRaw)

alpha(pastWeekRaw)
demandFile[, `:=`(SMEQ = SMEQRawScores, BSMAS = BSMASRawScores, SMDS = SMDSRawScores)]

demandFile[, minutes := as.numeric(`Duration (in seconds)`)/60]
# SMPT organizing ----
demandSMPT <- demandFile[, as.data.frame(.SD), .SDcols = c(names(demandFile)[startsWith(names(demandFile), "smpt")],
                                                           c("SMEQ", "SMDS", "BSMAS", "attnPass", "minutes",
                                                             "att1", "att2", "att3", "att4", "att5")), "ID"]


demandSMPT <- melt(demandSMPT, id.vars = c("ID", "SMEQ", "SMDS", "BSMAS", "attnPass", "minutes",
                                           "att1", "att2", "att3", "att4", "att5"),
                   measure.vars = c(names(demandFile)[startsWith(names(demandFile), "smpt")]), value = "y")[order(ID)]

demandSMPT[, c("condition", "x") := tstrsplit(as.character(variable), "_") ]
demandSMPT[, condition := factor(fcase(
    condition == "smptA", "Escape",
    condition == "smptB", "Tangible",
    condition == "smptC", "Attention",
    condition == "smptD", "Automatic"), levels = c("Attention",
                                                   "Automatic",
                                                   "Escape",
                                                   "Tangible")
)]


demandSMPT[, `:=`(y = as.numeric(y), x = as.numeric(x), ID = as.factor(ID), condition = as.factor(condition))]
unique(demandSMPT[which(demandSMPT$y >1440), ]$ID)
nrow(demandSMPT)
smptDataClean <- na.omit(demandSMPT[!ID %in% unique(demandSMPT[which(demandSMPT$y >1440), ]$ID), ])
smptDataClean <- smptDataClean[!ID %in% smptDataClean[, length(x), by = ID][V1 < 59, ID]]
smptDataClean[, length(unique(x))]

smptDataClean[x == .01 & condition == "Attention", colSums(data.frame(att1, att2, att3, att4, att5))/.N*100] |> round(1)
smptDataClean[x == .01 & condition == "Attention", table(attnPass)]
smptDataClean[x == .01 & condition == "Attention", sum(attnPass >= 3)/.N]


# Unsystematic ----
smptDataClean[, id := ID]
AttentionSystematic <- CheckUnsystematic(na.omit(smptDataClean[condition == "Attention"]))
automaticSystematic <- CheckUnsystematic(na.omit(smptDataClean[condition == "Automatic"]))
escapeSystematic <- CheckUnsystematic(smptDataClean[condition == "Escape"])
tangibleSystematic <- CheckUnsystematic(na.omit(smptDataClean[condition == "Tangible"]))

sapply(AttentionSystematic[, c("DeltaQPass", "BouncePass", "ReversalsPass")], table)/nrow(AttentionSystematic)
sapply(automaticSystematic[, c("DeltaQPass", "BouncePass", "ReversalsPass")], table)/nrow(automaticSystematic)
sapply(escapeSystematic[, c("DeltaQPass", "BouncePass", "ReversalsPass")], table)/nrow(escapeSystematic)
sapply(tangibleSystematic[, c("DeltaQPass", "BouncePass", "ReversalsPass")], table)/nrow(tangibleSystematic)

length(smptDataClean$ID |> unique())
smptDataClean[x == .01, sum(y == 0), by = .(condition)]
smptDataClean[, sum(all(y == 0)), by = .(ID,condition)][, sum(V1), by = condition]
smptDataClean[, sum(all(y == 0)), by = .(ID,condition)][, round(sum(V1)/328*100,1), by = condition]

51.5-17.4
51.5-7

177/328
37/328
smptDataClean$x |> unique()

merge(data.table(AttentionSystematic), demandFile[, .(id = ID, attnPass)], by = "id")[, table(DeltaQPass, attnPass)]
merge(data.table(automaticSystematic), demandFile[, .(id = ID, attnPass)], by = "id")[, table(DeltaQPass, attnPass)]
merge(data.table(escapeSystematic), demandFile[, .(id = ID, attnPass)], by = "id")[, table(DeltaQPass, attnPass)]
merge(data.table(tangibleSystematic), demandFile[, .(id = ID, attnPass)], by = "id")[, table(DeltaQPass, attnPass)]

merge(data.table(AttentionSystematic), demandFile[, .(id = ID, attnPass)], by = "id")[, table(BouncePass, attnPass)]
merge(data.table(automaticSystematic), demandFile[, .(id = ID, attnPass)], by = "id")[, table(BouncePass, attnPass)]
merge(data.table(escapeSystematic), demandFile[, .(id = ID, attnPass)], by = "id")[, table(BouncePass, attnPass)]
merge(data.table(tangibleSystematic), demandFile[, .(id = ID, attnPass)], by = "id")[, table(BouncePass, attnPass)]



# Demand Analysis ----
set.seed(09264)
smptSNDNLS <- nls_multstart(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                            data = smptDataClean,
                            iter = c(8, 8),
                            start_lower = c(q0 = .1, alpha = -4),
                            start_upper = c(q0 = 2, alpha = 4))


smptSNDGNLS <- gnls(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                    data = smptDataClean,
                    params = list(q0 ~ condition,  
                                  alpha ~ condition
                    ),
                    start = list(fixed = c(coef(smptSNDNLS)[1], 0, 0, 0, coef(smptSNDNLS)[2], 0, 0, 0)),
                    verbose = 2,
                    control = list(msMaxIter = 50000,
                                   niterEM = 5000,
                                   maxIter = 5000,
                                   pnlsTol = .01,
                                   tolerance = .01,
                                   apVar = T,
                                   minScale = .0000001,
                                   opt = "optim"), na.action = na.omit)



smptSNDGNLSSM <- gnls(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                      data = smptDataClean,
                      params = list(q0 ~ condition + BSMAS + SMEQ + SMDS,  
                                    alpha ~ condition + BSMAS + SMEQ + SMDS
                      ),
                      start = list(fixed = c(coef(smptSNDNLS)[1], 0, 0, 0, 0, 0, 0,
                                             coef(smptSNDNLS)[2], 0, 0, 0, 0, 0, 0)),
                      verbose = 2,
                      control = list(msMaxIter = 50000,
                                     niterEM = 5000,
                                     maxIter = 5000,
                                     pnlsTol = .01,
                                     tolerance = .01,
                                     apVar = T,
                                     minScale = .0000001,
                                     opt = "optim"), na.action = na.omit)


smptSNDGNLSInt <- gnls(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                      data = smptDataClean,
                      params = list(q0 ~ condition * BSMAS + condition * SMEQ + condition * SMDS,  
                                    alpha ~ condition * BSMAS + condition * SMEQ + condition * SMDS
                      ),
                      start = list(fixed = c(coef(smptSNDNLS)[1], rep(0, 15),
                                             coef(smptSNDNLS)[2], rep(0, 15))),
                      verbose = 2,
                      control = list(msMaxIter = 50000,
                                     niterEM = 5000,
                                     maxIter = 5000,
                                     pnlsTol = .01,
                                     tolerance = .01,
                                     apVar = T,
                                     minScale = .0000001,
                                     opt = "optim"), na.action = na.omit)


smptSNDMLM <- nlme(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                   data = smptDataClean,
                   fixed = list(q0 ~ condition,  
                                alpha ~ condition),
                   random = pdBlocked(list(pdSymm(q0 + alpha ~ 1),
                                           pdDiag(q0 + alpha ~ condition - 1))),
                   start = list(fixed = coef(smptSNDGNLS)),
                   groups = ~ID,
                   method = "ML",
                   verbose = 2,
                   control = list(msMaxIter = 50000,
                                  niterEM = 5000,
                                  maxIter = 5000,
                                  pnlsTol = .01,
                                  tolerance = .01,
                                  apVar = T,
                                  minScale = .0000001,
                                  opt = "optim"), na.action = na.omit)

smptSNDMLMSM <- nlme(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                     data = smptDataClean,
                     fixed = list(q0 ~ condition + BSMAS + SMEQ + SMDS,  
                                  alpha ~ condition + BSMAS + SMEQ + SMDS),
                     random = pdBlocked(list(pdSymm(q0 + alpha ~ 1),
                                             pdDiag(q0 + alpha ~ condition - 1))),
                     start = list(fixed = coef(smptSNDGNLSSM)),
                     groups = ~ID,
                     method = "ML",
                     verbose = 2,
                     control = list(msMaxIter = 50000,
                                    niterEM = 5000,
                                    maxIter = 5000,
                                    pnlsTol = .01,
                                    tolerance = .01,
                                    apVar = T,
                                    minScale = .0000001,
                                    opt = "optim"), na.action = na.omit)

smptSNDMLMInt <- nlme(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                     data = smptDataClean,
                     fixed = list(q0 ~ condition * BSMAS + condition * SMEQ + condition * SMDS,  
                                  alpha ~ condition * BSMAS + condition * SMEQ + condition * SMDS),
                     random = pdBlocked(list(pdSymm(q0 + alpha ~ 1),
                                             pdDiag(q0 + alpha ~ condition - 1))),
                     start = list(fixed = coef(smptSNDGNLSInt)),
                     groups = ~ID,
                     method = "ML",
                     verbose = 2,
                     control = list(msMaxIter = 50000,
                                    niterEM = 5000,
                                    maxIter = 5000,
                                    pnlsTol = .01,
                                    tolerance = .01,
                                    apVar = T,
                                    minScale = .0000001,
                                    opt = "optim"), na.action = na.omit)

#  Attention Checks Demand Sensitivity ----
set.seed(09264)
attnsmptSNDNLS <- nls_multstart(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                            data = smptDataClean[attnPass >= 3],
                            iter = c(8, 8),
                            start_lower = c(q0 = .1, alpha = -4),
                            start_upper = c(q0 = 2, alpha = 4))


attnsmptSNDGNLS <- gnls(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                    data = smptDataClean[attnPass >= 3],
                    params = list(q0 ~ condition,  
                                  alpha ~ condition
                    ),
                    start = list(fixed = c(coef(attnsmptSNDNLS)[1], 0, 0, 0, coef(attnsmptSNDNLS)[2], 0, 0, 0)),
                    verbose = 2,
                    control = list(msMaxIter = 50000,
                                   niterEM = 5000,
                                   maxIter = 5000,
                                   pnlsTol = .01,
                                   tolerance = .01,
                                   apVar = T,
                                   minScale = .0000001,
                                   opt = "optim"), na.action = na.omit)

attnsmptSNDMLM <- nlme(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                   data = smptDataClean[attnPass >= 3],
                   fixed = list(q0 ~ condition,  
                                alpha ~ condition),
                   random = pdBlocked(list(pdSymm(q0 + alpha ~ 1),
                                           pdDiag(q0 + alpha ~ condition - 1))),
                   start = list(fixed = coef(attnsmptSNDGNLS)),
                   groups = ~ID,
                   method = "ML",
                   verbose = 2,
                   control = list(msMaxIter = 50000,
                                  niterEM = 5000,
                                  maxIter = 5000,
                                  pnlsTol = .01,
                                  tolerance = .01,
                                  apVar = T,
                                  minScale = .0000001,
                                  opt = "optim"), na.action = na.omit)

attnsmptSNDGNLSInt <- gnls(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                       data = smptDataClean[attnPass >= 3],
                       params = list(q0 ~ condition * BSMAS + condition * SMEQ + condition * SMDS,  
                                     alpha ~ condition * BSMAS + condition * SMEQ + condition * SMDS
                       ),
                       start = list(fixed = c(coef(attnsmptSNDNLS)[1], rep(0, 15),
                                              coef(attnsmptSNDNLS)[2], rep(0, 15))),
                       verbose = 2,
                       control = list(msMaxIter = 50000,
                                      niterEM = 5000,
                                      maxIter = 5000,
                                      pnlsTol = .01,
                                      tolerance = .01,
                                      apVar = T,
                                      minScale = .0000001,
                                      opt = "optim"), na.action = na.omit)

attnsmptSNDMLMInt <- nlme(y ~ 10^(q0) *exp(-10^(alpha)*10^(q0)*x),
                      data = smptDataClean[attnPass >= 3],
                      fixed = list(q0 ~ condition * BSMAS + condition * SMEQ + condition * SMDS,  
                                   alpha ~ condition * BSMAS + condition * SMEQ + condition * SMDS),
                      random = pdBlocked(list(pdSymm(q0 + alpha ~ 1),
                                              pdDiag(q0 + alpha ~ condition - 1))),
                      start = list(fixed = coef(attnsmptSNDGNLSInt)),
                      groups = ~ID,
                      method = "ML",
                      verbose = 2,
                      control = list(msMaxIter = 50000,
                                     niterEM = 5000,
                                     maxIter = 5000,
                                     pnlsTol = .01,
                                     tolerance = .01,
                                     apVar = T,
                                     minScale = .0000001,
                                     opt = "optim"), na.action = na.omit)



anova(attnsmptSNDMLM)
summary(attnsmptSNDMLM)$tTable |> round(3)

emmeans(smptSNDMLM, param =  "q0", specs = pairwise ~ condition, data = smptDataClean[attnPass >= 3])
emmeans(attnsmptSNDMLM, param =  "q0", specs = pairwise ~ condition, data = smptDataClean[attnPass >= 3])

emmeans(smptSNDMLM, param =  "alpha", specs = pairwise ~ condition, data = smptDataClean[attnPass >= 3])
emmeans(attnsmptSNDMLM, param =  "alpha", specs = pairwise ~ condition, data = smptDataClean[attnPass >= 3])

# EXPD for Posterity ----
set.seed(09264)
GetK(smptDataClean)
# 2.206312
smptEXPDNLS <- nls_multstart(y ~ 10^(q0) * 10^(2.206312*(exp(-10^(alpha)*10^(q0)*x)-1)),
                            data = smptDataClean,
                            iter = c(8, 8),
                            start_lower = c(q0 = .1, alpha = -4),
                            start_upper = c(q0 = 2, alpha = 4))


smptEXPDGNLS <- gnls(y ~ 10^(q0) * 10^(2.206312*(exp(-10^(alpha)*10^(q0)*x)-1)),
                    data = smptDataClean,
                    params = list(q0 ~ condition,  
                                  alpha ~ condition
                    ),
                    start = list(fixed = c(coef(smptEXPDNLS)[1], 0, 0, 0, coef(smptEXPDNLS)[2], 0, 0, 0)),
                    verbose = 2,
                    control = list(msMaxIter = 50000,
                                   niterEM = 5000,
                                   maxIter = 5000,
                                   pnlsTol = .01,
                                   tolerance = .01,
                                   apVar = T,
                                   minScale = .0000001,
                                   opt = "optim"), na.action = na.omit)

smptEXPDMLM <- nlme(y ~ 10^(q0) * 10^(2.206312*(exp(-10^(alpha)*10^(q0)*x)-1)),
                   data = smptDataClean,
                   fixed = list(q0 ~ condition,  
                                alpha ~ condition),
                   random = pdBlocked(list(pdSymm(q0 + alpha ~ 1),
                                           pdDiag(q0 + alpha ~ condition - 1))),
                   start = list(fixed = coef(smptEXPDGNLS)),
                   groups = ~ID,
                   method = "ML",
                   verbose = 2,
                   control = list(msMaxIter = 50000,
                                  niterEM = 5000,
                                  maxIter = 5000,
                                  pnlsTol = .01,
                                  tolerance = .01,
                                  apVar = T,
                                  minScale = .0000001,
                                  opt = "optim"), na.action = na.omit)
# Used in Markdown----
individualSNDR2 <- smptDataClean[, .(R2 = 1 - sum((predict(smptSNDMLM, data.frame(.SD)) - (y))^2)/
                sum((mean((y)) - (y))^2)), .SDcols = c("ID", "y", "x", "condition"), 
              by = .(ID, condition)]

individualSNDMAE <- smptDataClean[, .(MAE = mean(abs(predict(smptSNDMLM, data.frame(.SD)) - (y)))),
              .SDcols = c("ID", "y", "x", "condition"), 
              by = .(ID, condition)]

individualEXPDR2 <- smptDataClean[, .(R2 = 1 - sum((predict(smptEXPDMLM, data.frame(.SD)) - (y))^2)/
                                       sum((mean((y)) - (y))^2)), .SDcols = c("ID", "y", "x", "condition"), 
                                 by = .(ID, condition)]

individualEXPDMAE <- smptDataClean[, .(MAE = mean(abs(predict(smptEXPDMLM, data.frame(.SD)) - (y)))),
                                  .SDcols = c("ID", "y", "x", "condition"), 
                                  by = .(ID, condition)]


1 - sum((predict(smptSNDMLM) - (smptDataClean$y))^2)/
  sum((mean((smptDataClean$y)) - (smptDataClean$y))^2)
individualSNDR2[!is.infinite(R2), median(R2, na.rm = TRUE)] |> round(3)
individualSNDR2[!is.infinite(R2), median(R2, na.rm = TRUE), by = .(condition)]
individualSNDR2[, range(R2, na.rm = TRUE), by = .(condition)]

mean(abs(predict(smptSNDMLM) - (smptDataClean$y))) |> round(3)
individualSNDMAE[, median(MAE, na.rm = TRUE)] |> round(3)
individualSNDMAE[, median(MAE, na.rm = TRUE), by = .(condition)]
individualSNDMAE[, range(MAE, na.rm = TRUE), by = .(condition)]

1 - sum((predict(smptEXPDMLM) - (smptDataClean$y))^2)/
  sum((mean((smptDataClean$y)) - (smptDataClean$y))^2)
individualEXPDR2[!is.infinite(R2), median(R2, na.rm = TRUE), by = .(condition)]
individualEXPDR2[, range(R2, na.rm = TRUE), by = .(condition)]

mean(abs(predict(smptEXPDMLM) - (smptDataClean$y)))
individualEXPDMAE[!is.infinite(MAE), median(MAE, na.rm = TRUE), by = .(condition)]
individualEXPDMAE[, range(MAE, na.rm = TRUE), by = .(condition)]



pairs.panels(data.frame(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAttention"],
                        coef(smptEXPDMLM)["q0.(Intercept)"] + coef(smptEXPDMLM)["q0.conditionAttention"]), 
             main = expression(bold("SND vs EXPD"~Q[0]~"Attention")), 
             labels = c(expression(Q[0]~"SND"),expression(Q[0]~"EXPD")))
pairs.panels(data.frame(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAutomatic"],
                        coef(smptEXPDMLM)["q0.(Intercept)"] + coef(smptEXPDMLM)["q0.conditionAutomatic"]), 
             main = expression(bold("SND vs EXPD"~Q[0]~"Automatic")), 
             labels = c(expression(Q[0]~"SND"),expression(Q[0]~"EXPD")))
pairs.panels(data.frame(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionEscape"],
                        coef(smptEXPDMLM)["q0.(Intercept)"] + coef(smptEXPDMLM)["q0.conditionEscape"]), 
             main = expression(bold("SND vs EXPD"~Q[0]~"Escape")), 
             labels = c(expression(Q[0]~"SND"),expression(Q[0]~"EXPD")))
pairs.panels(data.frame(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionTangible"],
                        coef(smptEXPDMLM)["q0.(Intercept)"] + coef(smptEXPDMLM)["q0.conditionTangible"]), 
             main = expression(bold("SND vs EXPD"~Q[0]~"Tangible")), 
             labels = c(expression(Q[0]~"SND"),expression(Q[0]~"EXPD")))

pairs.panels(data.frame(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionAttention"],
                        coef(smptEXPDMLM)["alpha.(Intercept)"] + coef(smptEXPDMLM)["alpha.conditionAttention"]), 
             main = expression(bold("SND vs EXPD"~alpha~"Attention")), 
             labels = c(expression(alpha~"SND"),expression(alpha~"EXPD")))
pairs.panels(data.frame(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionAutomatic"],
                        coef(smptEXPDMLM)["alpha.(Intercept)"] + coef(smptEXPDMLM)["alpha.conditionAutomatic"]), 
             main = expression(bold("SND vs EXPD"~alpha~"Automatic")), 
             labels = c(expression(alpha~"SND"),expression(alpha~"EXPD")))
pairs.panels(data.frame(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionEscape"],
                        coef(smptEXPDMLM)["alpha.(Intercept)"] + coef(smptEXPDMLM)["alpha.conditionEscape"]), 
             main = expression(bold("SND vs EXPD"~alpha~"Escape")), 
             labels = c(expression(alpha~"SND"),expression(alpha~"EXPD")))
pairs.panels(data.frame(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionTangible"],
                        coef(smptEXPDMLM)["alpha.(Intercept)"] + coef(smptEXPDMLM)["alpha.conditionTangible"]), 
             main = expression(bold("SND vs EXPD"~alpha~"Tangible")), 
             labels = c(expression(alpha~"SND"),expression(alpha~"EXPD")),digits = 3)


mean(abs(predict(smptEXPDMLM) - (smptDataClean$y)))

anova(smptSNDMLM, smptSNDMLMSM, smptSNDMLMInt)
anova(smptSNDMLM, smptSNDMLMInt)

anova(smptSNDMLM, type = "marginal")
anova(smptSNDMLMSM, type = "marginal") |> round(3)
anova(smptSNDMLMInt, type = "marginal") |> round(3)
summary(smptSNDMLM)
summary(smptSNDMLMSM)$tTable |> round(3)
summary(smptSNDMLMInt)$tTable |> round(3)
emmeans(smptSNDMLMInt, param = "alpha", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "q0", var = "BSMAS", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "q0", var = "SMDS", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "q0", var = "SMEQ", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "alpha", var = "BSMAS", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "alpha", var = "SMDS", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "alpha", var = "SMEQ", pairwise ~ condition)

emtrends(smptSNDMLMInt, param = "q0", var = "BSMAS", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "q0", var = "SMDS", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "q0", var = "SMEQ", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "alpha", var = "BSMAS", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "alpha", var = "SMDS", pairwise ~ condition)
emtrends(smptSNDMLMInt, param = "alpha", var = "SMEQ", pairwise ~ condition)


# Demand Metrics Correlations ----
q0Attention <- coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAttention"] 
q0Automatic <- coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAutomatic"] 
q0Escape <- coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionEscape"]
q0Tangible <- coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionTangible"] 
alphaAttention <- coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionAttention"] 
alphaAutomatic <- coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionAutomatic"] 
alphaEscape <- coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionEscape"]
alphaTangible <- coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionTangible"] 

# ----
svg("SMDemandEMmeans.svg", 11, 5.25) 
par(mfrow = c(1,2), pty = "m")
smq0s <- data.frame(emmeans(smptSNDMLM, param = "q0", ~condition))
emmeans(smptSNDMLM, param = "q0", specs = pairwise ~ condition)
plot(seq(1, 4, 1), smq0s$emmean, frame = FALSE, axes = FALSE, ylim = c(1, 2.2), xlim = c(.25, 4.5),
     xlab = NA, ylab = NA, pch = 21:24, bg = c("orange","grey","black", "blue"), cex = 2.5)
arrows(x0 = seq(1, 4, 1), x1 = seq(1, 4, 1), y0 = smq0s$emmean - smq0s$SE,
       y1 = smq0s$emmean + smq0s$SE, code = 3, length = .05, angle = 90)
text(x = 1:4, y = smq0s$emmean, c("Au.,E,T", "At.", "At.", "At."), pos = 2)
axis(1, at = c(1:4), labels = c("Attention", "Automatic", "Escape", "Tangible"), las = 1)
axis(2, at = seq(1, 2.2, by = .2), labels = seq(1, 2.2, by = .2), line = -1)
title(ylab = expression(atop("log"[10](Q[0])~"Estimates",""%<-%"Less Value"~~~"|"~~~"More Value"%->%"")), line = 1.25)
title(main = expression(bold("Social Media"~Q[0]~"Across Conditions")))

smAlphas <- data.frame(emmeans(smptSNDMLM, param = "alpha", ~condition))
emmeans(smptSNDMLM, param = "alpha", specs = pairwise ~ condition)
plot(seq(1, 4, 1), smAlphas$emmean, frame = FALSE, axes = FALSE, ylim = c(-1.5, -.9), xlim = c(.25, 4.5),
     xlab = NA, ylab = NA, pch = 21:24, bg = c("orange","grey","black", "blue"), cex = 2.5)
arrows(x0 = seq(1, 4, 1), x1 = seq(1, 4, 1), y0 = smAlphas$emmean - smAlphas$SE,
       y1 = smAlphas$emmean + smAlphas$SE, code = 3, length = .05, angle = 90)
text(x = 1:4, y = smAlphas$emmean, c("Au.,T", "At.,E", "Au", "At"), pos = 2)

axis(1, at = c(1:4), labels = c("Attention", "Automatic", "Escape", "Tangible"), las = 1)
axis(2, at = seq(-1.5, -.9, by = .2), labels = seq(-1.5, -.9, by = .2), line = -1)
title(ylab = expression(atop("log"[10](alpha)~"Estimates",""%<-%"More Value"~~~"|"~~~"Less Value"%->%"")), line = 1.25)
title(main = expression(bold("Social Media"~alpha~"Across Conditions")))
dev.off()
# All Individual Fits ----
forPlotting <- data.frame(predict(smptSNDMLM, level = 0:1), na.omit(smptDataClean[, .(x, y, condition)]))


pdf("individualSMDemandfits.pdf", 40, 40)
forPlotting %>%
    mutate(x = ifelse(x < .005, .005, x)) %>%
    ggplot(aes(x = x, y = y, fill = condition)) +
    facet_wrap(~ID) +
    geom_point(aes(y = y, shape = condition), size = 2) +
    geom_line(aes(y = predict.ID, color = condition)) +
    scale_x_log10(labels = c(0, .01, .1, 1, 10, 100, 1000), breaks = c(.001, .01, .1, 1, 10, 100, 1000)) +
    scale_color_manual(values = c("orange","grey","black", "blue")) +
    scale_fill_manual(values = c("orange","grey","black", "blue")) +
    scale_shape_manual(values = c(21, 22, 23, 24)) +
    coord_cartesian(xlim = c(.005, 1300), clip = "off", expand = FALSE)+
    scale_y_continuous(limits = c(0, 200), expand = c(0,0), oob = scales::squish) +
    theme_classic() +
    theme(plot.title = element_text(hjust = .5))
dev.off()

# ----
tickFunc <- function(from = .0001, to = .001, jumps = 6){
    tickHolder <- vector("numeric")
    for(a in 1:jumps){
        ticks <- seq(from*(10^a), to*(10^a), by = to*(10^a)/10)
        tickHolder<- append(tickHolder, ticks)
    }
    return(tickHolder)
}


# Overall Demand ----
svg("fixedeffects.svg", 6,6)

fixedCurveQ0 <- data.frame(emmeans(smptSNDMLM, param = "q0", ~ condition))
fixedCurvealpha <- data.frame(emmeans(smptSNDMLM, param = "alpha", ~ condition))
curve(10^fixedCurveQ0$emmean[1] * 
         exp(-10^fixedCurvealpha$emmean[1] * 
                10^fixedCurveQ0$emmean[1] * x),
      axes = FALSE, col = "orange", xlim = c(.01, 1400), ylim = c(0,150), log = "x",
      main = "Fixed Effects Estimates", cex.main = 2, ylab = "Minutes Purchased", xlab = "Cost per Minute ($)", lwd = 2)
curve(10^fixedCurveQ0$emmean[2] * 
        exp(-10^fixedCurvealpha$emmean[2] * 
              10^fixedCurveQ0$emmean[2] * x),
      add = TRUE, col = "grey", lwd = 2)
curve(10^fixedCurveQ0$emmean[3] * 
        exp(-10^fixedCurvealpha$emmean[3] * 
              10^fixedCurveQ0$emmean[3] * x),
      add = TRUE, lwd = 2)
curve(10^fixedCurveQ0$emmean[4] * 
        exp(-10^fixedCurvealpha$emmean[4] * 
              10^fixedCurveQ0$emmean[4] * x),
      add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("Attention", "Automatic", "Escape", "Tangible"),
       bty = "n", lty = 1, col = c("orange","grey","black", "blue"), lwd = 2)
axis(2, at = seq(0, 250, 50))
axis(1, at = c(.001, .01, .1, 1, 10, 100, 1000), labels = c(.001, .01, .1, 1, 10, 100, 1000))
axis(1, at = tickFunc(jumps = 8), tcl = -.25, labels = NA)

dev.off()

# Expected Demand Plots ----
svg("expected.svg", 7, 8)
parts <- c("a102",
           "a253",
           "a280",
           "a46")
par(mfrow = c(2,2), mar = c(4.5, 4, 3, 0), pty = "s")
for(a in parts){
    plot(smptDataClean[ID == a & condition == "Attention", .(x, y)], log = "x", pch = 21,
         axes = FALSE, bg = "orange", cex = 1.25,
         xlim = c(.01, 1400), ylim = c(0,200), main = a, cex.main = 1.25, y = "Minutes Purchased", "Cost per Minute ($)")
    points(smptDataClean[ID == a & condition == "Automatic", .(x, y)], pch = 22, bg = "grey",
           xlim = c(.01, 200), ylim = c(0,100), cex.main = 1.25, cex = 1.25)
    points(smptDataClean[ID == a & condition == "Escape", .(x, y)], pch = 23, bg = "black",
           xlim = c(.01, 200), ylim = c(0,100), cex.main = 1.25, cex = 1.25)
    points(smptDataClean[ID == a & condition == "Tangible", .(x, y)], pch = 24, bg = "blue", 
           xlim = c(.01, 200), ylim = c(0,100), cex.main = 1.25, cex = 1.25)
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAttention"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionAttention"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAttention"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE, col = "orange")
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAutomatic"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionAutomatic"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAutomatic"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE, col = "grey")
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionEscape"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionEscape"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionEscape"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE)
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionTangible"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionTangible"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionTangible"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE, col = "blue")
    legend("topright", legend = c("Attention", "Automatic", "Escape", "Tangible"),
           bty = "n", pch = c(21:24), pt.cex = 1.5, pt.bg = c("orange","grey","black", "blue"))
    axis(2, at = c(0, 100, 200 ,300 ,400), labels = c(0,100, 200, 300, 400))
    axis(1, at = c(.01, .1, 1, 10, 100, 1000), labels = c(.01, .1, 1, 10, 100, 1000))
    axis(1, at = tickFunc(jumps = 8), tcl = -.25, labels = NA)
}
dev.off()


# Quick Drop Off Plots ----
svg("quickdrop.svg", 7, 8)
parts <- c("a168",
           "a171",
           "a415",
           "a441")
par(mfrow = c(2,2), mar = c(4.5, 4, 3, 0), pty = "s")
for(a in parts){
    plot(smptDataClean[ID == a & condition == "Attention", .(x, y)], log = "x", pch = 21,
         axes = FALSE, bg = "orange", cex = 1.25,
         xlim = c(.01, 1400), ylim = c(0,200), main = a, cex.main = 1.25, y = "Minutes Purchased", "Cost per Minute ($)")
    points(smptDataClean[ID == a & condition == "Automatic", .(x, y)], pch = 22, bg = "grey",
           xlim = c(.01, 200), ylim = c(0,100), cex.main = 1.25, cex = 1.25)
    points(smptDataClean[ID == a & condition == "Escape", .(x, y)], pch = 23, bg = "black",
           xlim = c(.01, 200), ylim = c(0,100), cex.main = 1.25, cex = 1.25)
    points(smptDataClean[ID == a & condition == "Tangible", .(x, y)], pch = 24, bg = "blue", 
           xlim = c(.01, 200), ylim = c(0,100), cex.main = 1.25, cex = 1.25)
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAttention"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionAttention"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAttention"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE, col = "orange")
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAutomatic"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionAutomatic"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAutomatic"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE, col = "grey")
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionEscape"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionEscape"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionEscape"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE)
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionTangible"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionTangible"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionTangible"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE, col = "blue")
    legend("topright", legend = c("Attention", "Automatic", "Escape", "Tangible"),
           bty = "n", pch = c(21:24), pt.cex = 1.5, pt.bg = c("orange","grey","black", "blue"))
    axis(2, at = c(0, 100, 200 ,300 ,400), labels = c(0,100, 200, 300, 400))
    axis(1, at = c(.01, .1, 1, 10, 100, 1000), labels = c(.01, .1, 1, 10, 100, 1000))
    axis(1, at = tickFunc(jumps = 8), tcl = -.25, labels = NA)
}
dev.off()
# Zero Demand Plots ----
svg("nodemand.svg", 7, 8)
parts <- c("a105",
           "a6",
           "a335",
           "a416"
)
par(mfrow = c(2,2), mar = c(4.5, 4, 3, 0), pty = "s")
for(a in parts){
    plot(smptDataClean[ID == a & condition == "Attention", .(x, y)], log = "x", pch = 21,
         axes = FALSE, bg = "orange", cex = 1.25,
         xlim = c(.01, 1400), ylim = c(0,200), main = a, cex.main = 1.25, y = "Minutes Purchased", "Cost per Minute ($)")
    points(smptDataClean[ID == a & condition == "Automatic", .(x, y)], pch = 22, bg = "grey",
           xlim = c(.01, 200), ylim = c(0,100), cex.main = 1.25, cex = 1.25)
    points(smptDataClean[ID == a & condition == "Escape", .(x, y)], pch = 23, bg = "black",
           xlim = c(.01, 200), ylim = c(0,100), cex.main = 1.25, cex = 1.25)
    points(smptDataClean[ID == a & condition == "Tangible", .(x, y)], pch = 24, bg = "blue", 
           xlim = c(.01, 200), ylim = c(0,100), cex.main = 1.25, cex = 1.25)
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAttention"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionAttention"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAttention"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE, col = "orange")
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAutomatic"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionAutomatic"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionAutomatic"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE, col = "grey")
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionEscape"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionEscape"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionEscape"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE)
    curve((10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionTangible"])[which(rownames(coef(smptSNDMLM))== a)] * 
               (exp(-10^unlist(coef(smptSNDMLM)["alpha.(Intercept)"] + coef(smptSNDMLM)["alpha.conditionTangible"])[which(rownames(coef(smptSNDMLM))== a)] * 
                        10^unlist(coef(smptSNDMLM)["q0.(Intercept)"] + coef(smptSNDMLM)["q0.conditionTangible"])[which(rownames(coef(smptSNDMLM))== a)] * x))),
          add = TRUE, col = "blue")
    legend("topright", legend = c("Attention", "Automatic", "Escape", "Tangible"),
           bty = "n", pch = c(21:24), pt.cex = 1.5, pt.bg = c("orange","grey","black", "blue"))
    axis(2, at = c(0, 100, 200 ,300 ,400), labels = c(0,100, 200, 300, 400))
    axis(1, at = c(.01, .1, 1, 10, 100, 1000), labels = c(.01, .1, 1, 10, 100, 1000))
    axis(1, at = tickFunc(jumps = 8), tcl = -.25, labels = NA)
}
dev.off()
