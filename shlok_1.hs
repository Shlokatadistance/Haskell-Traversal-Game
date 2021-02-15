import System.IO  
import Control.Monad
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import System.TimeIt
import System.Console.Byline
data Color

green :: Color
red :: Color
blue :: Color
magenta :: Color
-- The first extra function.
-- This is basically like a wipeout. It deleted the files and then checks whether the file exists or not
-- If the file does not exist, then the last statement is given out.

-- this is a standard function and declaration, cannot be changed
-- if changed gives me an error statement

timeItT :: IO a -> IO (Double, a)Source

killSwitch = do y <- getLine
                x <- doesFileExist y
                putStrLn ("Do you really wish to sacrific the board?")
                z <- getLine
                if z == "Yes" && x == True then removeFile y
                else
                    putStrLn ("Better make your mind fast buddy!")

-- this is the loadpart of the game. This essentially ensures that the game is being loaded properly and this initiates the game for the player
-- This takes help of the IO part, simply becuase this game takes inspiration from the TicTacToe game and how
-- the IO was used there.
-- The timeit function is given right along with it
load :: IO ()
load =  do  
            putStr "Welcome! Enter the name of your file"
            dat <- getLine
            map <- loadMap dat
            putStrLn ("Whats your name? Lets Play!")
            w <- getLine
            putStrLn ("Hello there!" ++ w ++ "!")
-- We have also created a user and then this user can start playing the game 
            outputMap map
            putStr "Start by entering your commands: \n" <> fg red
            commandents <- getInputs
            putStr "\nYellow?: " <>fg yellow
            y <- colorMate 
            putStr "\nOrange?: "
            o <- colorMate 
            putStr "\nPurple?: " <fg> magenta
            p <- colorMate

            let playing = mainMoverTwo map commandents [p, y, o]

            outputMap $ fst playing
            putStr "\nCongratulation! YOur Score and Time Taken is:  "
            print $ snd playing
            timeIt $ putStrLn ("Result: " ++ show primesSum) <fg> green

-- This function loads the map, essentially vey trivial.
-- The map converts the input into an IO list. 
loadMap :: FilePath -> IO [[String]]
loadMap name =  do

                    dat <- readFile name
                    let contents = convert dat
                    return ([words z | z <- contents])

convert x = lines x
--The function which helps in printing the map out

outputMap :: [[String]] -> IO ()
outputMap []     =   putChar '\n'
outputMap (x:xs) =   do
                        outputArr x
                        outputMap xs
-- Because the input is being read like a list of list, each line counts as one array
-- Hence this function is used for printing out the individual arrays. 
outputArr :: [String] -> IO ()
outputArr []     =   putChar '\n'
outputArr (x:xs) =   do 
                        putStr x
                        putChar ' '
                        outputArr xs
-- Straight from the lecture sheets, this is used to get the input from the terminal. 
getLn :: IO String 
getLn = do  x <- getChar
            if x == '\n' || x == ' ' then
                return []
            else
                do  xs <- getLn
                    return (x:xs)


equalityTest :: Char -> [Char] -> Bool 
equalityTest _ [] = False 
equalityTest x (m:ms) = if x == m then  True 
                            else
                              equalityTest x ms
-- Because the inputs can also be taken in the form of loops, it is essential to check whether the 
-- input is being taken in correctly through the loop
-- this is also becuase the function will simply quit with an error and the whole program comes crashing down.
noCheck :: String -> Bool 
noCheck [] = True 
noCheck (x:xs) = if equalityTest x ['1','2','3','4','5','6','7'] then True && (noCheck xs)
                 else 
                  False 

-- Basically takes in the integer from the terminal and then checks it
-- this is what is the core of the program being used in this program.
getInt :: IO Int 
getInt = do x <- getLn
            if noCheck x then
                return (read x :: Int)
            else
                getInt

-- Applying the concept of parser and the problems in assignment 2, 
-- the input can be varied, depinging on the preference of the user.
-- thus it is crucial that the input is sufficiently capable of handling these user inputs and 
-- can effectively work in the program

diviDer :: Int -> [String] -> [String]
diviDer 0 dir = []
diviDer x dir = dir ++ diviDer (x-1) dir

-- this is the function which gives the looping strategy, by using the parser it can effectively make out how the loop
-- is to be used.
reDer :: Int -> IO [String]
reDer 0 = return []
reDer steps = do    putStr "Command Please!: "
                    x <- getLn
                    if x == "" then
                        return []
                        -- the empty list is returned in this case becuase the user has supplied nothing
                    else
                        if x == "Function" then
                            do  xs <- reDer 3
                                lm <- reDer (steps - 1)
                                return (xs ++ lm)
                        else
                            if x == "Loop" then
                                do  putStr "Lets Loop! You choose!: \n"
                                    p <- getInt
                                    ds <- reDer 3
                                    let k = diviDer p ds
                                    xs <- reDer (steps - 1)
                                    return (k ++ xs)
                            else
                                if checkInst [x] then
                                    do  xs <- reDer (steps - 1)
                                        return (x:xs)
                                        -- the list of  command is retuned in this case
                                else
                                    do  putStr "\n\nNo invalids please!: "
                                        reDer steps


-- the color balls are scattered throughout the board
-- it is thus necessary to create the functions to understand how the user
-- can navigate through them
colorMate :: IO String
colorMate = do  x <- getLn
                if checkInst [x] then
                    return x
                else
                    return ""
data Stylized = <> | scones | sconesone

-- standard definition from the haskage packer
(<>) :: Stylized -> Stylized -> Stylized

scones :: NonEmpty Stylized -> Stylized

sconesone :: Integral b => b -> Stylized -> Stylized

foreGroundcolor :: Color -> Stylized

--This function takes in the input for the number of steps to solve the probkem in 

getInputs :: IO [String]
getInputs = do  putStr "steps:"
                count <- getInt
                x <- reDer count
                if checkInst x then
                    return x
                else
                    return []

-- the functions are given as poyb and hence the check is performed that way.

spot:: [String]
spot = ["o","p","y","b"]

backGroundColor :: Color -> Stylized

checkSpot :: String -> Bool
checkSpot s = testSafe spot s

testSafe :: [String] -> String -> Bool
testSafe [] _       = False
testSafe (x:xs) s = if s == x then True
                    else
                        False || testSafe xs s
       





positions :: [String]
positions = ["Down","Up","Right","Left"]

testerOne :: [String] -> [String] -> Bool
testerOne [] (d:dir) = True
testerOne _ [] = False 
testerOne (x:xs) (p:pos) = if x == p then True && testerOne xs positions
                            else
                              testerOne (x:xs) pos

-- checks the instructions  and then tells the user whether they can be counted as valid or not.
checkInst :: [String] -> Bool 
checkInst dir = testerOne dir positions




alongx :: [[String]] -> String -> [Int] -> Int -> [[String]]
alongx (a:ab) p (x:y) count     | count == x    = (alongy a p (x:y) 0):ab
                                | count < x     = a:(alongx ab p (x:y) (count+1))

alongy :: [String] -> String -> [Int] -> Int -> [String]
alongy (a:ab) p (x:y) count     | count == head y   = (a:ab)
                                | count < head y    = a:(alongy ab p (x:y) (count + 1))


makeMove :: [[String]] -> [Int] -> [Int] -> [[String]]
makeMove ab xy zy =  if ((ab !! head xy) !! head (tail xy)) == "@" then
                        alongx (alongx ab "-" xy 0) "@" zy 0
                    else
                        ab



hanDler :: ([[String]], Int) -> [Int] -> String -> [String] -> ([[String]], Int)
hanDler (mp, score) xy dir col = do     let bound = [length mp, (length (head mp))]
                                        
                                        if dir == "p" then
                                            mainMover (mp, score) [head col] xy bound col
                                        else
                                            if dir == "y" then
                                                mainMover (mp, score) [col !! 1] xy bound col
                                            else
                                                if dir == "o" then
                                                    mainMover (mp, score) [col !! 2] xy bound col
                                                else
                                                    (mp, -1)
                        
-- the logic in these below functions is essentially
-- As the ball keeps moving, and the integer positions are being supplied,
-- simultaneously the balls position in the map must also keep getting updated
-- this will ensure that 

-- Note : The inspiration for this method came from the website of maze solving.
-- This method was implemented along the same lines, excep that the the corner cases were enlarged

righTer :: ([[String]], Int) -> [Int] -> [Int] -> [String] -> (([[String]], Int), [Int])
righTer (mp, score) xy bound c = do     let xone = head xy
                                        let yone = head (tail xy) + 1
                                        if yone < ((tail bound)!!0) && ((mp !! xone) !! yone) == "-" then
                                            do  let newmp = makeMove mp xy (xone:[yone])
                                                righTer (newmp, score) (xone:[yone]) bound c
                                        else
                                            if yone < ((tail bound)!!0) && ((mp !! xone) !! yone) == "b" then
                                            do  let newmp = makeMove mp xy (xone:[yone])
                                                let newscore = score + 1
                                                righTer (newmp, newscore) (xone:[yone]) bound c
                                            else
                                                if yone < ((tail bound)!!0) && checkSpot((mp !! xone) !! yone) then
                                                    do  let newmp = makeMove mp xy (xone:[yone])
                                                        ((hanDler (newmp, score) (xone:[yone]) ((mp !! xone) !! yone)) c, (xone:[yone]))
                                                else
                                                    if yone < ((tail bound)!!0) && ((mp !! xone) !! yone) == "t" then
                                                        do  let newmp = makeMove mp xy (xone:[yone])
                                                            ((newmp, score), xone:[yone])
                                                    else
                                                        ((mp, score), xy)
-- Same logic as that of the above function, except that now it
-- is being used to solve the problem for the upwards directions
-- the movable area is only when the - is encountered
-- the ball cannot move in the restricted area hence it is important that the case is handled


upPer :: ([[String]], Int) -> [Int] -> [Int] -> [String] -> (([[String]], Int), [Int])
upPer (mp, score) xy bound c = do   let xone = (head xy) - 1
                                    let yone = head (tail xy)
                                    if xone >= 0 && ((mp !! xone) !! yone) == "-" then
                                        do  let newmp = makeMove mp xy (xone:[yone])
                                            upPer(newmp, score) (xone:[yone]) bound c
                                    else
                                        if xone >= 0 && ((mp !! xone) !! yone) == "b" then
                                           do   let newmp = makeMove mp xy (xone:[yone])
                                                let newscore = score + 1
                                                upPer (newmp, newscore) (xone:[yone]) bound c
                                        else
                                            if xone >= 0 && checkSpot((mp !! xone) !! yone) then
                                                do  let newmp = makeMove mp xy (xone:[yone])
                                                    ((hanDler (newmp, score) (xone:[yone]) ((mp !! xone) !! yone)) c, (xone:[yone]))
                                            else
                                                if xone >= 0 && ((mp !! xone) !! yone) == "t" then
                                                    do  let newmp = makeMove mp xy (xone:[yone])
                                                        ((newmp, score), xone:[yone])
                                                else
                                                    ((mp, score), xy)

-- Implementing the same function is the leftwards directions

lefTer :: ([[String]], Int) -> [Int] -> [Int] -> [String] -> (([[String]], Int), [Int])
lefTer (mp, score) xy bound c = do  let xone = head xy
                                    let yone = head (tail xy) - 1
                                    if yone >= 0 && ((mp !! xone) !! yone) == "-" then
                                        do  let newmp = makeMove mp xy (xone:[yone])
                                            lefTer (newmp, score) (xone:[yone]) bound c
                                    else
                                          -- This command essentially keeps checking which spot to fill
                                        if yone >= 0 && ((mp !! xone) !! yone) == "b" then
                                           do   let newmp = makeMove mp xy (xone:[yone])
                                                let newscore = score + 1
                                                lefTer (newmp, newscore) (xone:[yone]) bound c
                                        else
                                            if yone >= 0 && checkSpot((mp !! xone) !! yone) then
                                                do  let newmp = makeMove mp xy (xone:[yone])
                                                    ((hanDler (newmp, score) (xone:[yone]) ((mp !! xone) !! yone)) c, (xone:[yone]))
                                            else
                                                if yone >= 0 && ((mp !! xone) !! yone) == "t" then
                                                    do  let newmp = makeMove mp xy (xone:[yone])
                                                        ((newmp, score), xone:[yone])
                                                else
                                                    ((mp, score), xy)                                         


dowNer :: ([[String]], Int) -> [Int] -> [Int] -> [String] -> (([[String]], Int), [Int])
dowNer (mp, score) xy bound c = do      let xone = (head xy) + 1
                                        let yone = head (tail xy)
                                        if xone < head bound && ((mp !! xone) !! yone) == "-" then
                                            do  let newmp = makeMove mp xy (xone:[yone])
                                                dowNer (newmp, score) (xone:[yone]) bound c
                                        else
                                            if xone < head bound && ((mp !! xone) !! yone) == "b" then
                                            do  let newmp = makeMove mp xy (xone:[yone])
                                                let newscore = score + 1
                                                dowNer (newmp, newscore) (xone:[yone]) bound c
                                                -- depending on what color the ball encounters, so should the ball turn.
                                            else
                                                if xone < head bound && checkSpot((mp !! xone) !! yone) then
                                                    do  let newmp = makeMove mp xy (xone:[yone])
                                                        ((hanDler (newmp, score) (xone:[yone]) ((mp !! xone) !! yone)) c, (xone:[yone]))
                                                else
                                                    if xone < head bound && ((mp !! xone) !! yone) == "t" then
                                                        do  let newmp = makeMove mp xy (xone:[yone])
                                                            ((newmp, score), xone:[yone])
                                                    else
                                                        ((mp, score), xy) 


-- this function is the main mover because it keeps checking the directions
-- and accordingly moves the map and its components. 

mainMover :: ([[String]], Int) -> [String] -> [Int] -> [Int] -> [String] -> ([[String]], Int)
mainMover (mp, score) [] xy bound col = (mp, score)
mainMover (mp, score) (p:pos) xy bound col  | p == "Up" =   do  let nr = upPer (mp, score) xy bound col
                                                                mainMover (fst nr) pos (snd nr) bound col
                                            | p == "Down" = do  let nr = dowNer (mp, score) xy bound col
                                                                mainMover (fst nr) pos (snd nr) bound col
                                            | p == "Left" = do  let nr = lefTer (mp, score) xy bound col
                                                                mainMover (fst nr) pos (snd nr) bound col
                                            | p == "Right"= do  let nr = righTer (mp, score) xy bound col
                                                                mainMover (fst nr) pos (snd nr) bound col
                                            | otherwise =       ([[""]], -1)



mainMoverTwo :: [[String]] -> [String] -> [String] -> ([[String]], Int)
mainMoverTwo mp dir col = do let bound = [length mp, (length (head mp))]
                             let xy = getStartingPoint mp
                             if checkInst dir then
                              mainMover (mp, 0) dir xy bound col
                             else
                              (mp, 0)



-- this function is helpful in getting the position of the ball out of the game
getStartingPoint :: [[String]] -> [Int]
getStartingPoint [] = [-1, -1]
getStartingPoint mx = do let a = checkStartingPoint mx 0
                         if fst a then
                              [snd a, 0]
                         else
                              [-1, -1]


-- While we know that logically the map should have the @ which is the ball, this function still checks whether the
-- the ball is present or not and that we can extract the information of the ball out                                
checkStartingPoint :: [[String]] -> Int -> (Bool, Int)
checkStartingPoint [] _ = (False, -1)
checkStartingPoint (m:mx) n = if head m == "@" then (True,n)
                              else
                                    checkStartingPoint mx (n+1)
-- these functions are then again the helper functions, 
-- they help in the movement of the ball in the map
-- you can see they take in the 
moveRight   :: [[String]] -> [Int] -> [Int] -> [Int]
moveRight mp xy bound = do  let xone = head xy
                            let yone = (((tail xy)!!0) + 1)
                            if yone < ((tail bound)!!0) && checkSpot ((mp !! xone) !! yone) then
                             
                                [xone,yone]
                            else
                                if yone < ((tail bound)!!0) && (((mp !! xone) !! yone) == "-" || ((mp !! xone) !! yone) == "b") then
                                   moveRight mp [xone,yone] bound
                                else
                                    xy
moveDown   :: [[String]] -> [Int] -> [Int] -> [Int]
moveDown mp xy bound = do   let xone = (head xy) + 1
                            let yone = head (tail xy)
                            if (xone < head bound) && checkSpot ((mp !! xone) !! yone) then
                                [xone,yone]
                            else
                                if (xone < head bound) && (((mp !! xone) !! yone) == "-" || ((mp !! xone) !! yone) == "b") then
                                   moveDown mp [xone,yone] bound
                                else
                                    xy

moveLeft   :: [[String]] -> [Int] -> [Int] -> [Int]
moveLeft mp xy bound = do   let xone = head xy
                            let yone = (head (tail xy)) - 1
                            if yone >= 0 && checkSpot ((mp !! xone) !! yone) then

                                [xone,yone]
                            else
                                if (yone >= 0) && (((mp !! xone) !! yone) == "-" || ((mp !! xone) !! yone) == "b") then
                                   moveLeft mp [xone,yone] bound
                                else
                                    xy

moveUp   :: [[String]] -> [Int] -> [Int] -> [Int]
moveUp mp xy bound = do     let xone = (head xy) - 1
                            let yone = head (tail xy)
                            if (xone >= 0) && checkSpot ((mp !! xone) !! yone) then
                                [xone,yone]
                            else
                                if (xone >= 0) && (((mp !! xone) !! yone) == "-" || ((mp !! xone) !! yone) == "b") then
                                   moveUp mp [xone,yone] bound
                                else
                                    xy



eQd :: Eq a => [a] -> [a] -> Bool
eQd (a:b) (c:d) = not ((a == c) && (head b == head d))


-- these functions help handle the different cases of the movement along the map
-- these functions are also an inspiration taken from the maze solving algoirthms

hanDleone :: [[String]] -> [Int] -> Bool
hanDleone [] _ = False
hanDleone xs ab = do let bod = [length xs, (length (head xs))]
                     finale xs (moveUp xs ab bod) ab 1 || finale xs (moveLeft xs ab bod) ab 3 || finale xs (moveRight xs ab bod) ab 4

hanDlethree :: [[String]] -> [Int] -> Bool
hanDlethree [] _ = False
hanDlethree ab xy = do  let bod = [length ab, (length (head ab))]
                        finale ab (moveUp ab xy bod) xy 1 || finale ab (moveDown ab xy bod) xy 2 || finale ab (moveLeft ab xy bod) xy 3

hanDlefour :: [[String]] -> [Int] -> Bool
hanDlefour [] _ = False
hanDlefour ab xy = do   let bod = [length ab, (length (head ab))]
                        finale ab (moveUp ab xy bod) xy 1 || finale ab (moveDown ab xy bod) xy 2 || finale ab (moveRight ab xy bod) xy 4



hanDletwo :: [[String]] -> [Int] -> Bool
hanDletwo [] _ = False
hanDletwo ab xy = do    let bod = [length ab, (length (head ab))]
                        finale ab (moveDown ab xy bod) xy 2 || finale ab (moveLeft ab xy bod) xy 3 || finale ab (moveRight ab xy bod) xy 4


-- this is the final stage of the game, where the positions are being checked.
finale :: [[String]] -> [Int] -> [Int] -> Int -> Bool
finale ab xy zy pos     | ((ab !! (head xy)) !! ((tail xy)!!0)) == "t" = True
                        | not (eQd xy zy) = False
                        | head zy == -1 || head (tail zy) == -1 || pos == 0 = hanDlefour ab xy
                        | pos == 1  = hanDleone ab xy 
                        | pos == 2  = hanDletwo ab xy 
                        | pos == 3  = hanDlethree ab xy 
                        | pos == 4  = hanDlefour ab xy 
                        | otherwise = False

