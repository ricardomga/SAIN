d <- read.table("card.csv", header = TRUE, sep = ";")

# nomes mais explicitos
names(d) <- c("ID", "X1_LIMIT_BAL", "X2_SEX", "X3_EDUCATION", "X4_MARRIAGE", "X5_AGE", "X6_PAY_Set", "X7_PAY_Ago", "X8_PAY_Jul", "X9_PAY_Jun", "X10_PAY_Mai", "X11_PAY_Abr", "X12_BILL_Set", "X13_BILL_Ago", "X14_BILL_Jul", "X15_BILL_Jun", "X16_BILL_Mai", "X17_BILL_Abr", "X18_PAY_Set", "X19_PAY_Ago", "X20_PAY_Jul", "X21_PAY_Jun", "X22_PAY_Mai", "X23_PAY_Abr", "Y")

d$ID <- NULL

d$X2_SEX <- factor(d$X2_SEX)
d$X4_MARRIAGE <- factor(d$X4_MARRIAGE)
d$Y <- factor(d$Y)

d$X5_AGE <- cut(
    d$X5_AGE,
    c(21, 31, 41, 75),
    c("21-30", "31-40", "41-75"),
    include.lowest = TRUE)

d$X6_PAY_Set <- cut(
    d$X6_PAY_Set,
    c(-2, 0, 4, 9),
    c(0, 1, 2),
    include.lowest = TRUE)

d$X7_PAY_Ago <- cut(
    d$X7_PAY_Ago,
    c(-2, 0, 4, 9),
    c(0, 1, 2),
    include.lowest = TRUE)

d$X8_PAY_Jul <- cut(
    d$X8_PAY_Jul,
    c(-2, 0, 4, 9),
    c(0, 1, 2),
    include.lowest = TRUE)

d$X9_PAY_Jun <- cut(
    d$X9_PAY_Jun,
    c(-2, 0, 4, 9),
    c(0, 1, 2),
    include.lowest = TRUE)

d$X10_PAY_Mai <- cut(
    d$X10_PAY_Mai,
    c(-2, 0, 4, 9),
    c(0, 1, 2),
    include.lowest = TRUE)

d$X11_PAY_Abr <- cut(
    d$X11_PAY_Abr,
    c(-2, 0, 4, 9),
    c(0, 1, 2),
    include.lowest = TRUE)

d$X3_EDUCATION <- factor(d$X3_EDUCATION)
