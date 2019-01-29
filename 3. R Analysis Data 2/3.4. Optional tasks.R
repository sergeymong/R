# домашняя работа 1 -- убрать пробелы из датафрейма числового
home_opt_1 <- read.csv("https://stepik.org/media/attachments/course/724/test_data_01.csv", 
                  stringsAsFactors = FALSE)
library(stringr)
fix_data <- function(d){
  t <- as.data.frame(sapply(d, function(x) grepl("[[:alpha:]]", x)))
  cn <- names(t[lapply(t, sum) == 0])
  d[cn] <- as.numeric(sapply(d[cn], function(x) gsub("[[:blank:]]", "", x)))
  d
}
str(fix_data(home_opt_1))


# домашяя работа 2 -- медицинское исследование
str(all_data)
head(all_data)
library(data.table)
get_id <- function(data_list){
  data <- rbindlist(lapply(data_list, as.data.frame.list))
  data[order(id), .(mean_temp = mean(temp[length(temp) == 7])), by = id][mean_temp > 0]
}


# домашняя работа 3. Поиск гетероскедастичных переменных
library(lmtest)
get_strange_var <- function(d){
  #formulas <- sapply(names(d), function(x) paste(x, "~ ."))
  formulas <- c("x ~ t", "x ~ z", "t ~ z", "t ~ x", "z ~ x", "z ~ t")
  models <- lapply(formulas, function(x) lm(as.formula(x), d))
  residds <- sapply(formulas, function(x) lm(as.formula(x), d)$residuals)
  p.values <- apply(residds, 2, function(x) shapiro.test(x)$p.value)
  names_1 <- names(which(lapply(models, function(x) bptest(x)$p.value) > 0.05))
  names_2 <- names(which((p.values < 0.05) & p.values == min(p.values)))
  if(length(names_2[names_1 == names_2]) == 0) "There is no strange variable in the data" else names_2[names_1 == names_2]
}

get_strange_var(d)





