library(pROC)
load("toolProbs.RData")

lncPro_ori_roc     <- pROC::roc(prob_lncPro_ori$label, prob_lncPro_ori$finalScore,
                                levels = c("Non.Interact", "Interact"), direction = "<")
lncPro_ML_roc      <- pROC::roc(prob_lncPro_ML$label, prob_lncPro_ML$Interact,
                                levels = c("Non.Interact", "Interact"), direction = "<")

rpiCool_train_roc  <- pROC::roc(prob_rpiCool_train$label, prob_rpiCool_train$Interact,
                                levels = c("Non.Interact", "Interact"), direction = "<")

RPISeq_SVM_roc     <- pROC::roc(prob_RPISeq_web$label, prob_RPISeq_web$SVM_prob,
                                levels = c("Non.Interact", "Interact"), direction = "<")
RPISeq_RF_roc      <- pROC::roc(prob_RPISeq_web$label, prob_RPISeq_web$RF_prob,
                                levels = c("Non.Interact", "Interact"), direction = "<")
RPISeq_train_roc   <- pROC::roc(prob_RPISeq_train$label, prob_RPISeq_train$Interact,
                                levels = c("Non.Interact", "Interact"), direction = "<")

LncADeep_ori_roc   <- pROC::roc(prob_LncADeep_ori$label, prob_LncADeep_ori$Score,
                                levels = c("Non.Interact", "Interact"), direction = "<")
LncADeep_train_roc <- pROC::roc(prob_LncADeep_train$label, prob_LncADeep_train$X0,
                                levels = c("Non.Interact", "Interact"), direction = "<")

IPMiner_ori_roc    <- pROC::roc(prob_IPMiner_ori$label,   prob_IPMiner_ori$X1,
                                levels = c("Non.Interact", "Interact"), direction = "<")
IPMiner_train_roc  <- pROC::roc(prob_IPMiner_train$label, prob_IPMiner_train$X1,
                                levels = c("Non.Interact", "Interact"), direction = "<")

LION_roc           <- pROC::roc(prob_LION$label, prob_LION$Interact,
                                levels = c("Non.Interact", "Interact"), direction = "<")

val_1 = paste0("LION (AUC = ", round(pROC::auc(prob_LION$label, prob_LION$Interact,
                                               levels = c("Non.Interact", "Interact"),
                                               direction = "<"), 4), ")")

val_2 = paste0("RPISeq_SVM (AUC = ", round(pROC::auc(prob_RPISeq_web$label, prob_RPISeq_web$SVM_prob,
                                                     levels=c("Non.Interact", "Interact"),
                                                     direction = "<"), 4), ")")
val_3 = paste0("RPISeq_RF (AUC = ", round(pROC::auc(prob_RPISeq_web$label, prob_RPISeq_web$RF_prob,
                                                    levels=c("Non.Interact", "Interact"),
                                                    direction = "<"), 4), ")")
val_4 = paste0("RPISeq_retrain (AUC = ", round(pROC::auc(prob_RPISeq_train$label, prob_RPISeq_train$Interact,
                                                         levels=c("Non.Interact", "Interact"),
                                                         direction = "<"), 4), ")")

val_5 = paste0("lncPro (AUC = ", round(pROC::auc(prob_lncPro_ori$label, prob_lncPro_ori$finalScore,
                                                 levels=c("Non.Interact", "Interact"),
                                                 direction = "<"), 4), ")")
val_6 = paste0("lncPro_RF (AUC = ", round(pROC::auc(prob_lncPro_ML$label, prob_lncPro_ML$Interact,
                                                    levels=c("Non.Interact", "Interact"),
                                                    direction = "<"), 4), "0)")

val_7 = paste0("rpiCOOL_retrain (AUC = ", round(pROC::auc(prob_rpiCool_train$label, prob_rpiCool_train$Interact,
                                                          levels=c("Non.Interact", "Interact"),
                                                          direction = "<"), 4), ")")

val_8 = paste0("IPMiner (AUC = ", round(pROC::auc(prob_IPMiner_ori$label,   prob_IPMiner_ori$X1,
                                                  levels=c("Non.Interact", "Interact"),
                                                  direction = "<"), 4), ")")
val_9 = paste0("IPMiner_retrain (AUC = ", round(pROC::auc(prob_IPMiner_train$label, prob_IPMiner_train$X1,
                                                          levels=c("Non.Interact", "Interact"),
                                                          direction = "<"), 4), ")")

val_10 = paste0("LncADeep (AUC = ", round(pROC::auc(prob_LncADeep_ori$label, prob_LncADeep_ori$Score,
                                                    levels=c("Non.Interact", "Interact"),
                                                    direction = "<"), 4), ")")
val_11 = paste0("LncADeep_retrain (AUC = ", round(pROC::auc(prob_LncADeep_train$label, prob_LncADeep_train$X0,
                                                            levels=c("Non.Interact", "Interact"),
                                                            direction = "<"), 4), ")")


line_size = 2
pROC::plot.roc(LION_roc, print.auc = F, legacy.axes = T, grid = c(0.1, 0.1),
               grid.col = c("green", "red"), print.thres = F, col = "red", lwd = line_size)

pROC::plot.roc(RPISeq_SVM_roc, print.auc = F, add = T, legacy.axes = T,
               print.thres = F, col = "darkorange4", lty = 5, lwd = line_size)
pROC::plot.roc(RPISeq_RF_roc, print.auc = F, add = T, legacy.axes = T,
               print.thres = F, col = "darkorange4", lty = 3, lwd = line_size)
pROC::plot.roc(RPISeq_train_roc, print.auc = F, add = T, legacy.axes = T,
               print.thres = F, col = "darkorange4", lwd = line_size)

pROC::plot.roc(lncPro_ori_roc, print.auc = F, add = T, legacy.axes = T,
               print.thres = F, col = "darkcyan", lty = 5, lwd = line_size)
pROC::plot.roc(lncPro_ML_roc, print.auc = F, add = T, legacy.axes = T,
               print.thres = F, col = "darkcyan", lwd = line_size)

pROC::plot.roc(rpiCool_train_roc, print.auc = F, add = T, legacy.axes = T,
               print.thres = F, col = "green", lwd = line_size)

pROC::plot.roc(IPMiner_ori_roc, print.auc = F, add = T, legacy.axes = T,
               print.thres = F, col = "chocolate2", lty = 5, lwd = line_size)
pROC::plot.roc(IPMiner_train_roc, print.auc = F, add = T, legacy.axes = T,
               print.thres = F, col = "chocolate2", lwd = line_size)

pROC::plot.roc(LncADeep_ori_roc, print.auc = F, add = T, legacy.axes = T,
               print.thres = F, col = "maroon2", lty = 5, lwd = line_size)
pROC::plot.roc(LncADeep_train_roc, print.auc = F, add = T, legacy.axes = T,
               print.thres = F, col = "maroon2", lwd = line_size)

legend("bottomright", legend =
           c(val_1, val_2, val_3, val_4, val_5, val_6, val_7, val_8, val_9, val_10, val_11),
       col = c("red", "darkorange4", "darkorange4", "darkorange4", "darkcyan", "darkcyan",
               "green", "chocolate2", "chocolate2", "maroon2", "maroon2"),
       lty = c(1, 5, 3, 1, 5, 1, 1, 5, 1, 5, 1), lwd = 2, cex = 0.9, x.intersp = 0.11,
       y.intersp = 0.35)


# Or use ggplot2:
#
# library(ggplot2)
#
# pROC::ggroc(list(LION = LION_roc, RPISeq_SVM = RPISeq_SVM_roc,
#                  RPISeq_RF = RPISeq_RF_roc, RPISeq_retrain = RPISeq_train_roc,
#                  lncPro = lncPro_ori_roc, lncPro_RF = lncPro_ML_roc,
#                  rpiCool_retrain = rpiCool_train_roc, IPMiner = IPMiner_ori_roc,
#                  IPMiner_retrain = IPMiner_train_roc, LncADeep = LncADeep_ori_roc,
#                  LncADeep_retrain = LncADeep_train_roc), legacy.axes = TRUE, aes = c("linetype", "color")) +
#     geom_line(size = 1) + aes(alpha = I(0.5)) +
#     scale_linetype_manual(values = c("dashed", "solid",
#                                      "dashed", "solid",
#                                      "dashed", "solid",
#                                      "solid",
#                                      "solid",
#                                      "solid", "dashed", "dotted")) +
#     scale_color_manual(values = c("#666600", "#666600",
#                                   "#66CC00", "#66CC00",
#                                   "#6600CC", "#6600CC",
#                                   "#FF0000",
#                                   "#CC9900",
#                                   "#003366", "#003366", "#003366"))
