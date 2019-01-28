values <- c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Clubs", "Diamonds", "Hearts", "Spades")
card_deck <- outer(values, suits, paste, sep = " of ")

cat_temper <- c("задиристый", "игривый", "спокойный", "ленивый")
cat_color <- c("белый", "серый", "чёрный", "рыжий")
cat_age <- c("кот", "котёнок")
cat_trait <- c("с умными глазами", "с острыми когтями", "с длинными усами")

cat_catalog <- sort(c(outer(cat_temper, c(outer(cat_color, c(outer(cat_age, cat_trait, paste)), paste)), paste)))


cat_catalog2 <- mapply(outer())
cat_catalog[42]


generator <- function(set) function(n) sample(set, n, replace = T)

card_generator <- generator(card_deck)
coin_generator <- generator(c("Head", "Tail"))

coin_generator(10)


generator <- function(set, prob = rep(1/length(set), length(set))) { 
  function(n) sample()
} 

generator <- function(set) function(n) sample(set, n, replace = T)
roulette_values <- c("Zero!", 1:36)
fair_roulette <- generator(roulette_values)
rigged_roulette <- generator(c("Zero!",roulette_values))

fair_roulette(5)
rigged_roulette(5)

"%+%" <- function(x, y) {
  min_l <- min(length(x), length(y))
  (v3 <- c(x[1:min_l] + y[1:min_l],rep(NA, abs(length(x) - length(y)))))
}



df <-expand.grid(cat_temper,cat_color,cat_age,cat_trait)
df
all_cats <- sort(apply(df, 1, paste, collapse=" "))
all_cats
all_cats[42]
