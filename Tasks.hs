import PageRank

-- Pages from the net, links with calculated weights
p1 = Page "TV" [(1/3, p2), (1/3, p3), (1/3, p4)]
p2 = Page "President" [(1/2, p3), (1/2, p4)]
p3 = Page "University" [(1.0, p1)]
p4 = Page "Oil" [(1/2, p1), (1/2, p3)]

moritzNet :: Web
moritzNet = [p1, p2, p3, p4]

task1g :: IO ()
task1g = do
    h1 <- surfHistory 10 p1
    putStrLn "Person 1"
    putStrLn $ show (p1:h1)
    putStrLn "Person 2"
    h2 <- surfHistory 10 p1
    putStrLn $ show (p1:h2)
    putStrLn "Person 3"
    h3 <- surfHistory 10 p1
    putStrLn $ show (p1:h3)

task1h :: IO ()
task1h = do
    let v2 = surfN 2 moritzNet [0.25, 0.25, 0.25, 0.25] 0.0
    putStrLn $ show v2
    let v10 = surfN 10 moritzNet [0.25, 0.25, 0.25, 0.25] 0.0
    putStrLn $ show v10

task1i :: IO ()
task1i = do
    let v = getRanks 0.0 moritzNet
    putStrLn $ show v

task3a :: Double -> IO ()
task3a lambda = do
    let v = getRanks lambda moritzNet
    putStrLn $ show v
    putStrLn $ show $ (abs $ sum v - 1.0) < 0.00001

task3b :: Double -> [[Int]] -> IO ()
task3b lambda adj = do
    let pageNames = generateNames (length $ head adj)
    let web = getAdjWeb pageNames adj
    let v = getRanks lambda web
    putStrLn $ show v
    putStrLn $ show $ (abs $ sum v - 1.0) < 0.00001