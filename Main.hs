import S.Model

main = do
    m <- build $ model0 5 1000
    print m
