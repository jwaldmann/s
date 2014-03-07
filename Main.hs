import S.Model

main = do
    m <- build $ model0 5 200
    print m
