type Time = String
type Jogo = (Time, Int, Int, Time)

-- exemplo: ("Brasil" , 1, "Haiti" 0) —--> significa Brasil 1 x 0 Haiti

jogos1 :: [Jogo]
jogos1 = [("Haiti",    1, 3, "Escocia"), ("Brasil", 2, 0, "Marrocos"),
          ("Catar",    0, 2, "Suica"),  ("Canada",  0, 0, "Bosnia"), 
          ("Suica",    0, 1, "Bosnia"), ("Canada",  0, 0, "Catar"), 
          ("Brasil",   1, 0, "Haiti"),  ("Escocia", 0, 1, "Marrocos"),
          ("Suica",    1, 0, "Canada"), ("Bosnia",  1, 1, "Catar"),
          ("Marrocos", 0, 0, "Haiti"),  ("Escocia", 1, 2, "Brasil")]

gols :: Time -> [Jogo] -> Int
gols equipe [] = 0
gols equipe ((time1, gols1, gols2, time2) : as) | time1==equipe = gols1 + gols equipe as
                                                | time2==equipe = gols2 + gols equipe as
                                                | otherwise = gols equipe as 

gols2 :: Time -> [Jogo] -> Int
gols2 equipe jogos = foldr (+) 0 ([g1 | (t1, g1, g2, t2) <- jogos, t1==equipe] ++ [g2 | (t1, g1, g2, t2) <- jogos, t2==equipe])

-- exemplo: gols "Brasil" jogos1 —--> resposta: 5 (2+1+2)

saldo :: Time -> [Jogo] -> Int
saldo equipe [] = 0
saldo equipe ((time1, gols1, gols2, time2) : as) | time1==equipe = gols1 - gols2 + saldo equipe as
                                                 | time2==equipe = gols2 - gols1 + saldo equipe as
                                                 | otherwise = saldo equipe as 
saldo2 :: Time -> [Jogo] -> Int
saldo2 equipe jogos = foldr1 (+) ([g1-g2 | (t1, g1, g2, t2) <- jogos, t1==equipe] ++ [g2-g1 | (t1, g1, g2, t2) <- jogos, t2==equipe]) 

saldo3 :: Time -> [Jogo] -> Int
saldo3 equipe jogos = foldr1 (+) ([abs(g1-g2) | (t1, g1, g2, t2) <- jogos, t1==equipe || t2==equipe]) 
 
 

-- exemplo: saldo "Brasil" jogos1 —---> resposta: 4 (2-0+1-0+2-1)

pontos2 :: Time -> [Jogo] -> Int
pontos2 equipe jogos = foldr1 (+) (
                                    [1 | (t1, g1, g2, t2) <- jogos, (t1==equipe || t2==equipe) && g2==g1] ++
                                    [3 | (t1, g1, g2, t2) <- jogos, (t1==equipe && g1>g2) || (t2==equipe && g2>g1)]
                                   )

pontos :: Time -> [Jogo] -> Int
pontos equipe [] = 0
pontos equipe ((time1, gols1, gols2, time2) : as) | (time1==equipe && gols1>gols2) = 3 + pontos equipe as
                                                  | (time2==equipe && gols2>gols1) = 3 + pontos equipe as
                                                  | (time1==equipe || time2==equipe) && gols2==gols1 = 1 + pontos equipe as
                                                  | otherwise = pontos equipe as


-- exemplo: pontos "Marrocos" jogos1 —---> resposta: 4 (0+3+1)

melhorTime :: [Time] -> [Jogo] -> Time -> Time
melhorTime [] jogos timeAux = timeAux
melhorTime (a:as) jogos timeAux | pontos a jogos > pontos timeAux jogos = melhorTime as jogos a
                                | otherwise = melhorTime as jogos timeAux  

getSemMelhor :: [Time] -> Time -> [Time]
getSemMelhor times equipe = [time | time <- times, time/=equipe]

classificados :: [Time] -> [Jogo] -> [Time]
classificados (a:as) jogos = [melhorTime as jogos a] ++ [melhorTime (getSemMelhor (a:as) (melhorTime as jogos a)) jogos ""]

--classificados :: [Time] -> [Jogo] -> [Time]
--classificados (a : b : c : d) jogos = 


-- exemplo: classificados ["Haiti", "Escocia", "Brasil", "Marrocos"] jogos1 
--                         resposta: —---> [Brasil, Marrocos]

{-
main = do
    -- print (gols Brasil jogos1)
    -- print (pontos Marrocos jogos1)
    -- print (saldo Brasil jogos1)
    -- print (classificados [Haiti, Brasil, Escocia, Marrocos] jogos1)
    done
-}

