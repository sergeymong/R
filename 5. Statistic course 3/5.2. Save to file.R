x <- read.table("input.txt")
res <- as.numeric(x[1] + x[2])
out <- write.table(res, file = "output.txt", col.names = F, row.names = F)
