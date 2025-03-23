             module A1E1_32146983
             (quadruple, triplesum, collatz, oddStringAppl, eliminationSort, triangleNumber)
             where
           
-- 1. Function takes a number and quadruples it to give the output

quadruple :: Num n => n -> n                  -- function declaration
quadruple = (*4)                              -- function definition

{- The type declared 'Num' ensures that the function only accepts numerical inputs.
   The function 'quadruple' takes a number as input and returns four times the number.
   The approach for a partially applied function was taken to make code more concise & reusable -}



-- 2. Function to sum three numbers

triplesum :: Num s => s -> s -> s -> s        
triplesum nr1 nr2 nr3 = nr1 + nr2 + nr3       -- nr1, nr2, nr3 are input variables.

{- Type declaration 'Num' is given, as the function only accepts numerical inputs.
   The function 'triplesum' takes three numbers as input & sums it up to give the output. -}



-- 3. Function takes an integer and returns the number of steps required to reach 1 as output.

collatz :: Integral c => c -> c 
collatz 1 = 0                                               -- Base case: 1 makes 0 steps
collatz z
    | z <= 0    = error "Input must be a positive non-zero integer"  -- Check if z - a positive integer
    | even z    = 1 + collatz (z `div` 2)                   -- If even, divide by 2
    | otherwise = 1 + collatz (3 * z + 1)                   -- If odd, apply 3n + 1

{- * The base case checks if the input = 1, in which case, 
     there's no application of the Collatz conjecture and hence gives 0 as output.
   * When z is less than or equal to 0, then the function cannot be applied, 
     therefore throws a custom error message stating the input must be a positive integer.
 
   * If input is even, division by 2 is done until 1 is reached. 
   * In case the input is odd, the input is multiplied by 3, then added with 1. If result is even, 
     it'll be divided by 2, else, 3n+1 again. This process is repeated until 1 is reached. -}



-- 4. Function to apply a function to all lists of odd length within a list of lists

oddStringAppl :: ([o] -> [o]) -> [[o]] -> [[o]]
oddStringAppl _ []  = []                                          -- Base case: empty list
oddStringAppl f lss =  map (\xs -> if odd (length xs) then f xs   
                else xs) lss

{- * The function oddStringAppl takes 2 arguments - a function and a list of lists 
     represented by 'f' and 'lss' respectively.

   * It checks if the list values in 'lss' have odd length, then applies function 'f' to each
     sublist values inside the list 'lss' using the Prelude 'map' function, to make it more aligned
     with functional programming paradigms as compared to list comprehension. 
   * If value is of even length it remains unchanged.

   * The type declaration = ([o] -> [o]) is the function f that takes a list of type o and applies 
     itself to it to return a list of the same type (o). [[o]] is the argument lss which takes 
     a list of sublists of type o and then returns a list of the same type [[o]].  -}



-- 5. Function receives list as argument & returns list of elements not in ascending order removed.

eliminationSort :: Ord e => [e] -> [e]  
eliminationSort []  = []              -- Base case I:  empty list
eliminationSort [e] = [e]             -- Base case II: singleton list
eliminationSort sortL = foldr (\c res -> c : filter (>= c) res) [] sortL

{- * In the function declaration, Ord e is used as the function compares a list with operators like (>, =).
     [e] -> [e] shows function takes a list of elements of type e (i.e., [e]) as input and 
     returns a list of same type ([e]), though not of same quantity.

   * Base Case 1: If list is empty, no elements are to be sorted, so function returns an empty list.
   * Base Case 2: If list has a single element, nothing to compare & sort, so function returns list as it is.

   * The function accepts a list (held by sortL) & with foldr applies function to elements from right to left.
     Foldr takes two  arguments: rightmost element (c) from list and the accumulated result (res) so far.
     It adds c to a new list created using the cons operator (:) which adds an element to the start of list. 
     The c is compared using filter with the values in 'res'and keeps elements greater than or equal to c. 
     'res' will finally hold the sorted list values. 
   * Foldr and filter was used in this function to implement higher-order functions, and lazy evaluation 
     of expressions complementing the purely functional nature of Haskell. -}



-- 6. A recursive function that gives the sum of all positive integers less than or equal to the given number.

triangleNumber :: Int -> Int 
triangleNumber maxNum                                            -- maxNum = input variable 
  | maxNum < 0    =  error "Input must be a positive integer"    -- Base case I:  If maxNum is a negative no.
  | maxNum == 0   =  0                                           -- Base case II: sum of numbers up to 0 is 0
  | otherwise     =  maxNum + triangleNumber (maxNum - 1)                    

{- * The function takes input of type Int, and provides a result of the same type.
   * Base Case I:  If maxNum is a negative no. (i.e, <0), it throws a custom error.
   * Base Case II: If maxNum is 0, it doesn't calculate anything and returns 0. 
   * otherwise:    Recursively add all numbers > 0 and upto input number including the input.-}
