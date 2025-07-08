# This script takes the proteinGroups.txt file from PRIDE project PXD018588 
# and merges technical replicates. For description of the processes see 
# the original description in the PolySTest paper (10.1074/mcp.RA119.001777)
# However, here we do not perform normalization of log-transformation
#
# This pre-analysis has the purpose to provide a simple file for testing the 
# OmicsQ, and not as example for how to process the data before running the app.
library(matrixStats)


prots <- read.csv(unz("data/MaxQuant_output.zip", "proteinGroups.txt"), sep="\t")

prots <- prots[, c(1,5:7, grep("LFQ.intensity", colnames(prots)))]  # keep only protein IDs, gene names and LFQ intensities

# Remove proteins with less than 2 unique peptides
prots <- prots[sapply(prots$Peptide.counts..unique., function(x) max(as.numeric(unlist(strsplit(unlist(x), ";"))))) >= 2, ]

# remove contaminants and reverse proteins
prots <- prots[!grepl("CON__|REV__", prots$Protein.IDs), ]

# sum technical replicates
lfq_names <- grep("LFQ.intensity", colnames(prots), value = TRUE)
for (i in 40:57) {
    new_name <- paste0("LFQ.intensity_Day", floor((i-1)/3)-14, 
                       "_Rep", ((i+2) %% 3) + 1)  # create new name for the summed intensity
    print(i)
    print(new_name)
    prots[[new_name]] <- rowSums(prots[, grep(paste0("LFQ.intensity.", i), lfq_names, value = TRUE)], na.rm = TRUE)
}

prots <- prots[, c("Protein.IDs", "Gene.names", grep("Day", colnames(prots), value = TRUE))]  # keep only relevant columns

# Turn zeroes into NAs
prots[prots == 0] <- NA

# Remove proteins with less than 6 of the 18 LFQ intensities
prots <- prots[rowSums(!is.na(prots[, grep("Day", colnames(prots))])) >= 6, ]

boxplot(log2(prots[, grep("Day", colnames(prots))]), 
        main = "Boxplot of LFQ intensities per day",
        xlab = "Days", ylab = "LFQ Intensity",
        las = 2, outline = FALSE, col = "#6cbabf", border = "#333333")
# Save the processed data to a CSV file
write.csv(prots, "data/Myo.csv", row.names = FALSE)
