library(data.table)
products <- fread("./Datasets/ecommerce/products.csv")
products[1:10,]

products[price > 10000 & brand %in% c("Epson", "Apple")]

products[, list(name = name,
                price.1k = price/1000)]

products[order(products$price, decreasing = T)]

products[, .(mean.price = mean(price)), by = brand]

products[order(-price), .(name = head(name, 3),
                          price = head(price, 3)), by = brand]


# домашнее задание 1
filter.expensive.available <- function(products, brands) {
  products[price >= 500000 &
             available & 
             brand %in% brands]
}
# проверка дз1
brands <- c("a", "c", "d")
sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))

filter.expensive.available(sample.products, c("a", "c", "d"))

# домашнее задание 2
ordered.short.purchase.data <- function(purchases) {
  purchases[quantity > 0][order(-price), .(ordernumber, product_id)]
}
# проверка дз 2
sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
ordered.short.purchase.data(sample.purchases)

# домашнне задание 3
purchases.median.order.price <- function(purchases) {
  as.numeric(purchases[quantity > 0][,.(median.order = sum(price*quantity)), by = ordernumber][,.(median(median.order))])
}


#чуть упрощённый вариант
purchases.median.order.price <- function(purchases) {    
  median(purchases[quantity >= 0][, list(w = sum(price * quantity)), by=list(ordernumber)]$w)
}

# проверка дз 3
sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

purchases.median.order.price(sample.purchases)


# продолжение классной работы
products[price<1000, name.with.price := paste0(name, " ( ", price, " руб.)")]
products[order(-price)]

products[ , price := price/max(price), by=brand]


purchases <- fread("./Datasets/ecommerce/purchases.csv")

setkey(purchases, product_id, externalsessionid)
setkey(products, product_id, brand)

key(purchases)
key(products)


table3 <- merge(purchases, products, by = "product_id", all.x = F, all.y = F)


products[J(c(158, 208, 10001, 826355, 958238))]
products[data.table(c(158, 208, 10001, 826355, 958238))]
products[.(c(158, 208, 10001, 826355, 958238))]
products[list(c(158, 208, 10001, 826355, 958238))]


print(SJ(c(158, 208, 10001, 826355, 958238)))

# берём топ 20 брендов
purchases.with.brands <- merge(
  purchases,
  products[, .(product_id, brand)],
  by="product_id"
)

pop.20.brands <- head(
  purchases.with.brands[,
                        .(total.brand.users = length(unique(externalsessionid))),
                        by = brand][order(-total.brand.users)], 20)

users <- purchases.with.brands[, .(unique.brands = length(ungique(brand)),
                                   items = .N,
                                   brand = brandp[1]),
                               by=externaluserid]

# домашняя работа 4
get.category.ratings <- function(purchases, product.category) {
  setkey(purchases)
  setkey(product.category)
  table3 <- merge(purchases, product.category, by = "product_id")
  #table3[, .(totalcents = sum(totalcents), quantity = sum(quantity)), by=category_id] #с нижней строкой смыслы одинаковые
  table3[, lapply(.SD, sum), by=category_id][,-2]
}

# проверка дз4
product.category <- data.table(product_id = c(1,1,2,2,3),
                               category_id = c(1,2,1,3,3))
purchases <- data.table(product_id = c(1, 2, 3),
                        totalcents = c(100, 200, 300),
                        quantity = c(1, 1, 3))
get.category.ratings(purchases, product.category)


# домашняя работа 5
mark.position.portion <- function(purchases) {
  purchases[quantity > 0][, price.portion := as.character(formatC((price*quantity)/sum(price*quantity)*100, format = 'f', digits = 2)), 
                           by=ordernumber]

}

sample.purchases <- data.table(price = c(100, 300, 50, 700, 30),
                               ordernumber = c(1,1,1,2,3),
                               quantity = c(1,1,2,1,-1),
                               product_id = 1:5)


       