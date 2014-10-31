--Newmodel3IO.hs 

module Main where

import NewModel6
--this library allows us to generate seeds from enviroment variables and generate pseudo-random numbers from those seeds 
import System.Random 

main :: IO () 
main = do 
    gen <- getStdGen 
    mainQ gen

mainQ :: RandomGen g => g -> IO()
mainQ gen = do
    
    --this runs the number of simulations specified in the parameters and creates a list of the final population of each simulation
    let (allSimulations, gen') = runManyPrograms ([], gen)
    
    --this averages the number of susceptible, infected, and vaccinated men from the list of final populations.
    let susceptibleMen  = avrgNumMen 0 allSimulations
    let infectedMen  = avrgNumMen 1 allSimulations
    let vaccinatedMen  = avrgNumMen 2 allSimulations
    putStrLn $ "Number of Simulations:    " ++ (show par_NUM_OF_RUNS)
    putStrLn $ "Average # of Infected:    " ++ (show infectedMen) 
    putStrLn $ "Average # of Susceptible: " ++ (show susceptibleMen) 
    putStrLn $ "Average # of Vaccinated:  " ++ (show vaccinatedMen)
   



