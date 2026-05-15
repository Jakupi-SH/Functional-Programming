import Data.Maybe
import Data.List

type EId = String

data WorkPermit = Permit { number :: String, expiryDate :: Date }
    deriving (Show, Eq)

data Employee = Emp
    {
empId :: EId,
joinedOn :: Date,
permit :: Maybe WorkPermit,
leftOn :: Maybe Date
}
    deriving (Show, Eq)

data Date = Date Int Int Int
    deriving (Show, Eq, Ord)

d1 = Date 2025 1 1
d2 = Date 2025 6 1
d3 = Date 2025 3 1
d4 = Date 2025 12 1
d5 = Date 2026 1 1

p1 = Permit "P1" d2
p2 = Permit "P2" d4
p3 = Permit "P3" d5

e1 = Emp "E1" d1 (Just p1) Nothing
e2 = Emp "E2" d3 (Just p2) Nothing
e3 = Emp "E3" d5 (Just p3) Nothing
e4 = Emp "E4" d1 (Just p1) (Just (Date 2028 1 1))
e5 = Emp "E5" d3 (Just p2) (Just (Date 2028 1 1))
e6 = Emp "E6" d1 (Just p3) (Just (Date 2030 1 1))

-- Exercise 1
-- Example prompt: 
-- employeesWithOverlappingPermits[e1,e2,e3]
-- [("E1","E2")]


permitsOverlap :: Employee -> Employee -> Bool
permitsOverlap e1 e2 =
    case (permit e1, permit e2) of
        (Just p1, Just p2) ->
            joinedOn e1 <= expiryDate p2 &&
            expiryDate p1 >= joinedOn e2
        _ -> False


employeesWithOverlappingPermits :: [Employee] -> [(EId, EId)]
employeesWithOverlappingPermits [] = []
employeesWithOverlappingPermits (x:xs) =
    overlappingPairs x xs ++ employeesWithOverlappingPermits xs

overlappingPairs :: Employee -> [Employee] -> [(EId, EId)]
overlappingPairs _ [] = []
overlappingPairs e1 (e2:xs)
    | permitsOverlap e1 e2 =
        (empId e1, empId e2) : overlappingPairs e1 xs
    | otherwise =
        overlappingPairs e1 xs

-- Exercise 2
-- Example Prompt: 
-- [(3,[Emp {empId = "E4", joinedOn = Date 2025 1 1, permit = Just (Permit {number = "P1", expiryDate = Date 2025 6 1}), leftOn = Just (Date 2028 1 1)},Emp {empId = "E5", joinedOn = Date 2025 3 1, permit = Just (Permit {number = "P2", expiryDate = Date 2025 12 1}), leftOn = Just (Date 2028 1 1)}]),(5,[Emp {empId = "E6", joinedOn = Date 2025 1 1, permit = Just (Permit {number = "P3", expiryDate = Date 2026 1 1}), leftOn = Just (Date 2030 1 1)}])]


tenure :: Employee -> Int
tenure e =
    case leftOn e of
        Just (Date leftYear _ _) ->
            let (Date joinYear _ _) = joinedOn e
            in leftYear - joinYear

        Nothing ->
            0

-- It returns employees grouped by their tenure year, the employee object includes all of their attributes. 

employeesByTenure :: [Employee] -> [(Int, [Employee])]
employeesByTenure [] = []

employeesByTenure (x:xs) =
    (tenure x, employeesWithSameTenure x (x:xs))
    : employeesByTenure (removeSameTenure x xs)

employeesWithSameTenure :: Employee -> [Employee] -> [Employee]
employeesWithSameTenure _ [] = []

employeesWithSameTenure e (x:xs)
    | tenure e == tenure x =
        x : employeesWithSameTenure e xs
    | otherwise =
        employeesWithSameTenure e xs


removeSameTenure :: Employee -> [Employee] -> [Employee]
removeSameTenure _ [] = []

removeSameTenure e (x:xs)
    | tenure e == tenure x =
        removeSameTenure e xs
    | otherwise =
        x : removeSameTenure e xs




-- Exercise 3
-- Example prompt:
-- longestWorkingEmployee[e1,e2,e3,e4,e5,e6]
-- Just (Emp {empId = "E6", joinedOn = Date 2025 1 1, permit = Just (Permit {number = "P3", expiryDate = Date 2026 1 1}), leftOn = Just (Date 2030 1 1)})


longestWorkingEmployee :: [Employee] -> Maybe Employee
longestWorkingEmployee [] = Nothing
longestWorkingEmployee [x] = Just x
longestWorkingEmployee (x:xs) =
    Just (longestHelper x xs)


longestHelper :: Employee -> [Employee] -> Employee
longestHelper current [] = current
longestHelper current (x:xs)
    | tenure x > tenure current = longestHelper x xs
    | otherwise                 = longestHelper current xs


-- Exercise 4
-- Example prompt:
-- withExpiredPermit[e1,e2,e3,e4,e5,e6] today
-- ["E1","E2","E4","E5"]


isExpired :: Employee -> Date -> Bool
isExpired e today =
    case permit e of
        Just p  -> expiryDate p < today
        Nothing -> False

today = Date 2025 12 2 
--example date to show which employees have expired permits

withExpiredPermit :: [Employee] -> Date -> [EId]
withExpiredPermit [] _ = []
withExpiredPermit (x:xs) today
    | isExpired x today = empId x : withExpiredPermit xs today
    | otherwise         = withExpiredPermit xs today

-- Exercise 5
-- Example prompt:
-- avgYearsWorked[e1,e2,e3,e4,e5,e6]
-- 3.6666666666666665

hasLeft :: Employee -> Bool
hasLeft e =
    case leftOn e of
        Just _  -> True
        Nothing -> False


avgYearsWorked :: [Employee] -> Double
avgYearsWorked xs =
    let (total, count) = sumAndCount xs
    in if count == 0
       then 0
       else fromIntegral total / fromIntegral count


sumAndCount :: [Employee] -> (Int, Int)
sumAndCount [] = (0, 0)

sumAndCount (x:xs)
    | hasLeft x =
        let (s, c) = sumAndCount xs
        in (tenure x + s, c + 1)

    | otherwise =
        sumAndCount xs    



-- Based on this definition, you are tasked to write functions that will support certain queries on the employee
-- database. Implement the following functionalities, you can define as many separate helper functions as you
-- want.

-- 1. A function employeesWithOverlappingPermits :: [Employee] -&gt; [(EId, EId)] that returns a list of
-- unique pairs of employee IDs whose permits overlap. Two permits overlap if: the start date of one
-- permit is before or on the end date of the other, and the end date of one permit is after or on the start
-- date of the other. (4 pts)

-- 2. A function employeesByTenure :: [Employee] -&gt; [(Int, [Employee])] that returns a list of tuples
-- where the first element is the tenure (measured in years) and the second is the list of employees. It
-- essentially groups the employees by their tenure. (4 pts)

-- 3. A function longestWorkingEmployee :: [Employee] -&gt; Maybe Employee that returns the employee
-- that has worked the most amount of time. (4 pts)

-- 4. A function withExpiredPermit :: [Employee] -&gt; Date -&gt; [EId] that given the current date returns the
-- ids of employees with an expired permit. . (4 pts)

-- 5. A function avgYearsWorked :: [Employee] -&gt; Double that returns the average years an employee
-- worked in the company. Consider only employees who have left. (6 pts)


