avian <- read.csv("avianHabitat.csv")
# coverage_vars <- names(avian)[-(1:4)][c(T,F)]
coverage_vars <- names(avian)[str_detect(names(avian), "^P")]

avian$total_coverage <- rowSums(avian[, coverage_vars])
avian$Observer <- as.factor(avian$Observer)

  
# avian2 <- read.csv2("avianHabitat2.csv", skip = 5, header = T, comment.char = "%", na.strings = "Don't remember", sep = ";")
# avian2$VOR <- as.numeric(as.character(avian2$VOR))
# avian2$DBHt <- as.numeric(as.character(avian2$DBHt))
# avian2$WHt <- as.numeric(as.character(avian2$WHt))
# avian2$EHt <- as.numeric(as.character(avian2$EHt))
# avian2$HHt <- as.numeric(as.character(avian2$HHt))
# avian2$LHt <- as.numeric(as.character(avian2$LHt))
# avian2 <- add_column(avian2, Observer = "KT", .after = avian2$Site)
# coverage_vars2 <- names(avian2)[-(1:4)][c(T,F)]
# avian2$total_coverage <- rowSums(avian2[, coverage_vars])

# avian3 <- merge(avian, avian2, by="Subpoint")
# 
# summary(avian3)
# 
# colsP <- names(avian3)[grepl("P", names(avian))]
# 
# meanP <- apply(avian3[, colsP], 2, mean)
# meanP <- round(sort(meanP, decreasing = T), digits = 2)
# meanP
# 
# diff_between_df <- function(df1, df2, type = "P", m = "max"){
#   cols <- names(df1)[grepl(type, names(df1))]
#   need1 <- apply(df1[, cols], 2, m)
#   need1 <- sort(need1, decreasing = T)
#   need2 <- apply(df2[, cols], 2, m)
#   need2 <- sort(need2, decreasing = T)
#   need3 <- round(abs(need1 - need2), digits = 4)
#   
#   
#   print(paste(m,"different", "for", type, "is"))
#   need3
# }

check_percent_range <- function(x) {
  any(x < 0 | x > 100)
}


# Create site names factor
sapply(coverage_vars, function(name) check_percent_range(avian[[name]]))
avian$site_name <- str_replace(avian$Site, "[:digit:]+", "")
avian$site_name <- factor(avian$site_name)


# Search investigators top by Ht
ht_vars <- names(avian) [str_detect(names(avian), "Ht$")]
sapply(ht_vars, function(name) tapply(avian[[name]], avian$Observer, max))


sapply(ht_vars, function(x) names(which.max(tapply(avian[[x]], avian$Observer, max))))

