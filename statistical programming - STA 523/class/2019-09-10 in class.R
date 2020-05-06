df <- data.frame(coin = c("BTC", "ETH", "XRP"),
                 price = c(10417.04, 172.52, .26),
                 vol = c(21.29, 8.07, 1.23),
                 stringsAsFactors = FALSE)

df[1]
df[c(1,3)]
df[1:2,3]
df[,"price"]
df[[1]]
df[["vol"]]
df[[c(1,3)]]
df[[1,3]]
data("longley")

#Exercise1
longley$ratio <- longley$Employed/longley$Population
longley[longley$ratio==max(longley$ratio),"Year"]

which.max(longley$Employed/longley$Population)
longley[which.max(longley$Employed/longley$Population),"Year",drop=F]
#Exercise2
longley[longley$Year %in% 1950:1953,]

#Exercise3
longley[longley$Armed.Forces>longley$Unemployed,"Year"]

summary
