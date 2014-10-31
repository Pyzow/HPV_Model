module NewModel6 where 

import Data.List
import System.Random
import Data.Array

-------------------------------
-- DATA AND TYPE DEFINITIONS --
-------------------------------

-- 0 is Susceptible, 1 is Infected, 2 :: Vaccinated 
type InfectionStatus = Int
type WeeksLeftOfInfection = Int

--this is an ID number for an individual
type PersonNumber = Int
type WeeksLeftOfRelationship = Int
 
--defining an individual
data Man = Man PersonNumber{-Individual's ID-} InfectionStatus{-Individual's Status-} WeeksLeftOfInfection PersonNumber{-Partner's ID-} InfectionStatus{-Partner's Infection Status-} WeeksLeftOfRelationship
    deriving (Ord, Eq)

--a list of individuals that compose one group (all the individuals in a simulation) 
type Group = [Man]

----------------
-- PARAMETERS --
----------------

par_PREVALENCE = 0.5
par_PARTNERSHIP_RATE = 0.1
par_PARTNERSHIP_LENGTH = 8
par_TRANSMISSION_RATE = 0.05
par_POPULATION_SIZE = 100
par_PERCENT_VACCINATED = 0.8
par_ENTRY_EXIT_RATE_PER_WEEK = 0.0006
par_WEEKS = 4000
par_DURATION_OF_INFECTION = 104
par_ENTRY_EXIT_NUMBER_PER_WEEK =  (fromIntegral par_POPULATION_SIZE) * par_ENTRY_EXIT_RATE_PER_WEEK 
par_NUM_OF_RUNS = 5 :: Int

{-take all the men and with frequency f, have them select a person and become partners with them.  otherwise they will have sex but not become partners.  So go through each person and have them have sex with someone in the pool, and remove them both from the pool and then keep going down the list. -}


--this generates a number of simulations according to the number of simulations specified in the parameters.  This function will almost always be given an empty list of final populations to start with.   
runManyPrograms :: RandomGen g => ([Group], g) -> ([Group], g)
runManyPrograms = iterateNTimes par_NUM_OF_RUNS runprogram
    --this runs one simulation, and appends the 
    --final population onto the list of final populations it was given
    where 
    runprogram :: RandomGen g => ([Group],g) -> ([Group], g) 
    runprogram (worlds, gen) = (newWorld:worlds, gen')  
        where (newWorld, gen')  = (iterateNTimes par_WEEKS goThrough) (guys, gen)
              guys = generateMen par_POPULATION_SIZE par_PREVALENCE
    --uses the parameters that specify population and initial HPV prevalence 
    --to generate a starting population of men with the given prevalence    
    generateMen :: Int -> Double -> Group
    generateMen population prevalence = giveIDs (allMen, length allMen)   
        where   allMen = infectedMen ++ susceptibleMen 
                susceptibleMen = replicate susceptibleNumber (Man 0 0 0 0 0 0)
                infectedMen = replicate infectedNumber (Man 0 1 par_DURATION_OF_INFECTION 0 0 0)
                susceptibleNumber = population - infectedNumber 
                infectedNumber = floor $ (fromIntegral population) * prevalence
                --assigns each man in the population a distinct ID number starting at 1
                giveIDs :: (Group, Int) -> Group
                giveIDs ([], 0) = []
                giveIDs (m:man, count) = (changeID count m) : (giveIDs (man, count-1)  )
 
--this takes a population and simulates the passing of one time-step (a week) and returns the population one week into the future. It's easiest read starting from the bottom of the function.
goThrough :: RandomGen g => (Group,g) -> (Group,g)
goThrough (guys, gen) =  enterAndExit guysAfterWeek
    where guysAfterWeek = (map decreaseWeek postsexGuys, gen'''')
          (enterAndExit, gen'''') = iterateNRTimes par_ENTRY_EXIT_NUMBER_PER_WEEK deathAndEntry gen'''
          (postsexGuys, gen''') = coupleSex couplesAndSingles [] gen''
          couplesAndSingles = notSingleGuys ++ pairsOfShuffledSingleGuys
          (pairsOfShuffledSingleGuys, gen'') = paired shuffledSingleGuys [] gen'
          (shuffledSingleGuys, gen') = smartShuffle (singleGuys, gen)
          numSinGuys    =  length singleGuys
          singleGuys    =  [ (Man a b c d e weeks) | (Man a b c d e weeks) <- guys, weeks<=0]
          notSingleGuys =  [ (Man a b c d e weeks) | (Man a b c d e weeks) <- guys, weeks >0] 

iterateNRTimes n f gen
    | randomNumber < remainder = (iterateNTimes (ceiling n) f, gen')
    | otherwise                = (iterateNTimes (floor n)   f, gen')
    where (randomNumber, gen') = randomR (0::Float, 1::Float) gen
          remainder = n - (fromIntegral (floor n))

smartShuffle :: RandomGen g => (Group, g) -> (Group, g)
smartShuffle (men, gen) = (stripped, gen')
    where stripped = strip $ sort zipped 
          zipped = zip list men 
          (a, list, gen') = generateList (length men, [], gen)

          generateList :: RandomGen g => (Int, [Float], g) -> (Int, [Float], g)
          generateList (0,   doubles, gen) = (0, doubles, gen)
          generateList (int, doubles, gen) = generateList ((int-1), (rand:doubles), gen')
              where (rand, gen') = randomR (0::Float, 1::Float) gen

          strip :: [(Float, Man)] -> [Man]
          strip [] = []
          strip ((_,man):rest) = man:(strip rest)
 
coupleSex :: RandomGen g => Group -> Group -> g -> (Group,g)
coupleSex [] finalMen gen = (finalMen,gen)
coupleSex (man:rest) processedMen gen = case man of
    (Man _ 0 _ _ 1 _) -> coupleSex rest (newMan:processedMen) gen'
    _                   -> coupleSex rest (man:processedMen) gen
    where (newMan, gen') = potentialInfection man gen
 
potentialInfection :: RandomGen g => Man -> g -> (Man,g)
potentialInfection man gen
    |  randomNumber < par_TRANSMISSION_RATE = (infectMan man, gen')
    |  otherwise = (man, gen')  
    where (randomNumber, gen') = randomR (0::Double,1::Double) gen

paired :: RandomGen g => Group -> Group -> g -> (Group, g)
paired [] finalPaired gen = (finalPaired, gen) 
paired [man] finalPaired gen = (man:finalPaired, gen)
paired (man1: (man2 :rest)) processedMen gen 
    | randomNumber < par_PARTNERSHIP_RATE = paired rest (seriousTwosome ++ processedMen) gen'
    | otherwise                                 = paired rest (casualTwosome  ++ processedMen) gen'
    where seriousTwosome = map makeItSerious casualTwosome 
          casualTwosome = pairUp man1 man2 
          (randomNumber, gen') = randomR (0::Double,1::Double) gen    

pairUp :: Man -> Man -> Group
pairUp (Man name1 status1 age1 _ _ _) (Man name2 status2 age2 _ _ _) = [(Man name1 status1 age1 name2 status2 1), (Man name2 status2 age2 name1 status1 1)]

deathAndEntry :: RandomGen g => (Group, g) -> (Group, g)
deathAndEntry (men, gen) = ((group1 ++ (newguy: (tail group2))), gen'')
    where (newguy, gen'') = genNewGuy oldguy gen'
          oldguy = head group2
          (group1, group2) = splitAt randomNumber men
          (randomNumber, gen') = randomR (0::Int, (l-1)::Int) gen
          l = length men 
     
          genNewGuy :: RandomGen g => Man -> g -> (Man, g)
          genNewGuy (Man id _ _ _ _ _) gen 
              | randomNumber < par_PERCENT_VACCINATED = ((Man newID 2 0 0 0 0), gen') 
              | otherwise                                   = ((Man newID 0 0 0 0 0), gen')
              where (randomNumber, gen') = randomR (0::Double, 1::Double) gen
                    newID = id + par_POPULATION_SIZE
     
--this function marks the passing of a week.  Each relationship and infection comes one week closer to ending.  The first case is when there is no infection, the second is where an infection disappears, and the third is where an infection stays for another week.
decreaseWeek :: Man -> Man
decreaseWeek (Man a b 0 d e week) = Man a b 0 d e (week-1)
decreaseWeek (Man a 1 1 d e week) = Man a 0 0 d e (week-1) 
decreaseWeek (Man a b c d e week) = Man a b (c-1) d e (week-1)

increaseWeek :: Man -> Man 
increaseWeek (Man a b c d e week) = Man a b c d e (week+1)

makeItSerious :: Man -> Man
makeItSerious (Man a b c d e _) = Man a b c d e par_PARTNERSHIP_LENGTH

changeID :: Int -> Man -> Man 
changeID 0 a = a
changeID int (Man _ b c d e f) = Man int b c d e f

infectMan :: Man -> Man 
infectMan (Man a _ _ d e f) = Man a 1 par_DURATION_OF_INFECTION d e f  

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

avrgNumMen :: InfectionStatus -> [Group] -> Double
avrgNumMen status worlds = average $ map length ((map (pullMenByStatus status) worlds))

pullMenByStatus :: InfectionStatus -> Group -> Group
pullMenByStatus status men = [(Man a b c d e w)| (Man a b c d e w) <- men, b==status]

average :: [Int] -> Double
average list = (foldr (+) 0 (map fromIntegral list)) / (fromIntegral (length list))

--iterateNTimes :: Int -> (a -> b) -> (a -> b)    
iterateNTimes n f = foldr (.) id (replicate n f)   

-------------

instance Show Man where
  show (Man a b c d e f) = "\nMan " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++  (show e) ++ " " ++  (show f)  



{-
singleSex :: Group -> Group
singleSex [] = []
singleSex (man1:(man2:rest))
    | man1 == Man _ 0 _ _ _ _ && man2 == Man _ 1 _ _ _ _ = [potentialInfection man1, man2] ++ (singleSex rest)
    | man1 == Man _ 1 _ _ _ _ && man2 == Man _ 0 _ _ _ _ = [man1, potentialInfection man2] ++ (singleSex rest)
    | otherwise = [man1,man2] ++ (singleSex rest)


smartMix3 :: Group -> Integer -> Group
smartMix3 men randomNumber = x!randomNumber 
    where x = listArray (0::Integer,(l-1)::Integer) p
          l = (factorial  (fromIntegral (length men)  :: Integer)) ::Integer
          p = permutations men



smartMix2 :: Group -> Integer -> Group
smartMix2 men randomNumber = x!randomNumber 
    where x = array (1,l) p
          l = factorial $ fromIntegral $ length men
          p = permutations men 

mixEmUp :: RandomGen g => [a] -> [a] -> g -> ([a], g)
mixEmUp input processed gen 
    | length input > 2 = mixEmUp (drop 3 input) (mixedThree ++ processed) gen' 
    | otherwise        = (input++processed, gen) 
    where mixedThree = (permutations (take 3 input)) !! randomNumber
          (randomNumber, gen') = randomR (0::Int,5::Int) gen 


-}

