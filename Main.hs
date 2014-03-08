import S.Model

main = do
    m0 <- model0 7 200
    m1 <- build m0
    print $ base m1
    print $ trans m1
