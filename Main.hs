import S.Model

main = do
    m <- build $ model0 5 2000
    print m
