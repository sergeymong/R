fun_list <- c(mean, max)
sapply(fun_list, function(x) x(1:100))

apply_f <- function(f, x) f(x)
apply_f 
sapply(c(min), apply_f, x = 1:100)

square <- function() function(x) x^2

f <- function(x) {
  g <- function(y) if (y>0) 1 else if (y<0) -1 else 0
  sapply(x, g)
}
f(-100:100)


f <- function(arg1, arg2, remove_na = TRUE, ..., optional_arg)  {print(arg1, arg2)}

f(arg1 = 5, arg2 = (arg1 + 10))


decorate_string <- function(pattern, ...) { 
  paste0(pattern, paste(...), paste(sapply(strsplit(pattern, NULL), rev), collapse= ""))
}

decorate_string(pattern = "123", "abc")            # "123abc321"
decorate_string(pattern = "123", "abc", "def")     # "123abc def321"
decorate_string(pattern = "123", c("abc", "def"))  # "123abc321" "123def321" (вектор длины 2)

decorate_string(pattern = "123", "abc", "def", sep = "+")    # "123abc+def321"
decorate_string(pattern = "!", c("x", "x"), collapse = "_")  # "!x_x!"
decorate_string(pattern = ".:", 1:2, 3:4, 5:6, sep = "&")    # ".:1&3&5:." ".:2&4&6:." (вектор длины 2)

