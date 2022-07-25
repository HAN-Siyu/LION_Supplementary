library(ggplot2)

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
        library(grid)

        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)

        numPlots = length(plots)

        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }

        if (numPlots==1) {
                print(plots[[1]])

        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

generate_data <- function(path) {
        human.perf <- read.csv(path, header = F)
        names(human.perf) <- c("Tool", "Performance", "Metric")
        acc.rank <- human.perf[human.perf$Metric == "Accuracy",]
        acc.rank <- as.character(acc.rank$Tool[sort(acc.rank$Performance, decreasing = T, index.return = T)$ix])
        human.perf$Tool <- factor(human.perf$Tool, levels = acc.rank)
        human.perf$Metric <- factor(human.perf$Metric, levels = c("Accuracy", "Sensitivity", "Specificity", "F-Measure", "Kappa"))
        names(human.perf) <- c("Tool", "Performance", "Metrics")
        human.perf$Performance <- ifelse(human.perf$Performance > 0, human.perf$Performance, 0)
        human.perf
}

data_1807 <- generate_data("1807.csv")
data_2241 <- generate_data("2241.csv")
data_488 <- generate_data("488.csv")
data_NPI <- generate_data("NPI.csv")

generate_img <- function(input_data, caption = NULL) {
        img_NPI <- ggplot2::ggplot(data = input_data, aes(x = Metrics, y = Performance, fill = Tool)) +
                geom_bar(stat = "identity", position = position_dodge(), colour = "black", alpha = 0.70, size = 0.5) +
                scale_y_continuous(breaks = seq(0.1, 1, 0.1)) +
                coord_cartesian(ylim = c(0, 1)) +
                guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
                labs(caption = caption) +
                theme(legend.position = "bottom",
                      axis.title.x = element_blank(),
                      axis.text.x  = element_text(size = 12),
                      axis.title.y = element_blank(),
                      axis.text.y  = element_text(size = 12),
                      legend.title = element_blank(),
                      legend.text  = element_text(size = 12),
                      plot.caption = element_text(hjust = 0.5, size = rel(1.2), vjust = -0.8, face = "bold")) +
                # scale_fill_brewer(palette = "Accent")
                scale_fill_discrete(h = c(100, 410), l = 70, c = 85, direction = -1) # Output: 3.8 * 12.5 in
        img_NPI
}

img_1807 <- generate_img(data_1807, caption = "(a) Performances of different tools on dataset RPI1807")
img_2241 <- generate_img(data_2241, caption = "(b) Performances of different tools on dataset RPI2241")
img_488 <- generate_img(data_488, caption = "(c) Performances of different tools on dataset RPI488")
img_NPI <- generate_img(data_NPI, caption = "(d) Performances of different tools on dataset NPInter")

multiplot(img_1807, img_488, img_2241, img_NPI, cols = 2) # Output: 7 * 19 in

generate_img <- function(input_data, caption = NULL) {
        img_NPI <- ggplot2::ggplot(data = input_data, aes(x = Metrics, y = Performance, fill = Tool)) +
                geom_bar(stat = "identity", position = position_dodge(), colour = "black", alpha = 0.70, size = 0.5) +
                scale_y_continuous(breaks = seq(0.1, 1, 0.1)) +
                coord_cartesian(ylim = c(0, 1)) +
                guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
                labs(caption = caption) +
                theme(legend.position = "bottom",
                      axis.title.x = element_blank(),
                      axis.text.x  = element_text(size = 12),
                      axis.title.y = element_blank(),
                      axis.text.y  = element_text(size = 12),
                      legend.title = element_blank(),
                      legend.text  = element_text(size = 12),
                      plot.caption = element_text(hjust = 0.5, size = rel(1.2), vjust = -0.8, face = "bold")) +
                # scale_fill_brewer(palette = "Accent")
                scale_fill_discrete(h = c(100, 460), l = 70, c = 85, direction = -1) # Output: 3.8 * 12.5 in
        img_NPI
}


######

img_1807 <- ggplot2::ggplot(data = data_1807, aes(x = Metrics, y = Performance, fill = Tool)) +
        geom_bar(stat = "identity", position = position_dodge(), colour = "black", alpha = 0.70) +
        scale_y_continuous(breaks = seq(0.1, 1, 0.1)) +
        coord_cartesian(ylim = c(0, 1)) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
        labs(caption = "(a) Performances of different tools on dataset RPI1807") +
        theme(legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.text.x  = element_text(size = 12),
              axis.title.y = element_blank(),
              axis.text.y  = element_text(size = 12),
              legend.title = element_blank(),
              legend.text  = element_text(size = 12),
              plot.caption = element_text(hjust = 0.5, size = rel(1.2), vjust = -0.8, face = "bold")) +
        # scale_fill_brewer(palette = "Accent")
        scale_fill_discrete(h = c(100, 410), l = 65, c = 70, direction = -1) # Output: 3.8 * 12.5 in

img_2241 <- ggplot2::ggplot(data = data_2241, aes(x = Metrics, y = Performance, fill = Tool)) +
        geom_bar(stat = "identity", position = position_dodge(), colour = "black", alpha = 0.70) +
        scale_y_continuous(breaks = seq(0.1, 1, 0.1)) +
        coord_cartesian(ylim = c(0, 1)) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
        labs(caption = "(b) Performances of different tools on dataset RPI2241") +
        theme(legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.text.x  = element_text(size = 12),
              axis.title.y = element_blank(),
              axis.text.y  = element_text(size = 12),
              legend.title = element_blank(),
              legend.text  = element_text(size = 12),
              plot.caption = element_text(hjust = 0.5, size = rel(1.2), vjust = -0.8, face = "bold")) +
        # scale_fill_brewer(palette = "Accent")
        scale_fill_discrete(h = c(100, 410), l = 65, c = 70, direction = -1) # Output: 3.8 * 12.5 in

img_488 <- ggplot2::ggplot(data = data_488, aes(x = Metrics, y = Performance, fill = Tool)) +
        geom_bar(stat = "identity", position = position_dodge(), colour = "black", alpha = 0.70) +
        scale_y_continuous(breaks = seq(0.1, 1, 0.1)) +
        coord_cartesian(ylim = c(0, 1)) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
        labs(caption = "(c) Performances of different tools on dataset RPI488") +
        theme(legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.text.x  = element_text(size = 12),
              axis.title.y = element_blank(),
              axis.text.y  = element_text(size = 12),
              legend.title = element_blank(),
              legend.text  = element_text(size = 12),
              plot.caption = element_text(hjust = 0.5, size = rel(1.2), vjust = -0.8, face = "bold")) +
        # scale_fill_brewer(palette = "Accent")
        scale_fill_discrete(h = c(100, 410), l = 65, c = 70, direction = -1) # Output: 3.8 * 12.5 in

img_NPI <- ggplot2::ggplot(data = data_NPI, aes(x = Metrics, y = Performance, fill = Tool)) +
        geom_bar(stat = "identity", position = position_dodge(), colour = "black", alpha = 0.70, size = 1) +
        scale_y_continuous(breaks = seq(0.1, 1, 0.1)) +
        coord_cartesian(ylim = c(0, 1)) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
        labs(caption = "(d) Performances of different tools on dataset NPInter") +
        theme(legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.text.x  = element_text(size = 12),
              axis.title.y = element_blank(),
              axis.text.y  = element_text(size = 12),
              legend.title = element_blank(),
              legend.text  = element_text(size = 12),
              plot.caption = element_text(hjust = 0.5, size = rel(1.2), vjust = -0.8, face = "bold")) +
        # scale_fill_brewer(palette = "Accent")
        scale_fill_discrete(h = c(100, 410), l = 70, c = 85, direction = -1) # Output: 3.8 * 12.5 in
