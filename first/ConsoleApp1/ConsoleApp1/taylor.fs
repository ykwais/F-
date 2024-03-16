module ConsoleApp1.taylor

open System
// Print a table of a given function f, computed by taylor series

// function to compute
let f x = (1./4.) * (x * x - Math.PI * Math.PI/3.)

let a = Math.PI/5.
let b = Math.PI
let h = 100

let EPS = 0.000001

// Define a function to compute f using naive taylor series method

let unit (x,  n) = Math.Pow(-1., n) * Math.Cos(n*x)/Math.Pow(n, 2)
let first x = -1. * Math.Cos(x)
let rec taylor_naive x  = 
    
    let rec wh value count summ =
        if (Math.Abs(float value) < EPS && count >= 1 ) then summ, count
        else
            let new_value = unit (x, float (count + 1))
            wh new_value (count + 1) (summ + new_value)
    wh 0.0 0 0.0 
        
    


// Define a function to do the same in a more efficient way
let rec fact n =
    if n = 0. then float 1.
    else float  n * float (fact (n - 1.))
    
let second_unit (delim, count, x, n) = Math.Pow(-1., n) * Math.Pow(delim, n) * Math.Pow(x, (2. * n)) / Math.Cos(count*x) / float (fact (n * 2.0) )

let rec summ_row (x, delim, count_bot)  = 
    
    let rec wh value count summ =
        if (Math.Abs(float value) < EPS && count >= 1 ) then summ
        else
            let new_value = second_unit (delim, count_bot, x, count)
            wh new_value (count + 1) (summ + new_value)
    wh 0.0 0 0.0 
        
let previous x = -1. * Math.Cos(x)
let taylor x =
    let rec wh (digit, count, prev, summ) =
        let next = prev * (-1.) * count / Math.Pow((count + 1.),2) * summ_row (x, Math.Pow((count + 1.),2), count)
        if Math.Abs(float next) < EPS then summ, int(count)
        else
            wh (Math.Pow((count + 1.), 2.), (count + 1.), next, (summ + next))
    
    
  
    wh (1, 1, (previous x), (previous x))
    

let main =
   printfn "%10s  %10s %10s %10s  %10s  %10s" "x" "Built-in""Naive" "# terms" "Smart" "# terms"
   for i=0 to h do
     
     let x = a+(float i)/(float h)*(b-a)
     let summ, iter = taylor_naive x
     let summ2, iter2 = taylor x
     printfn "%10.2f  %10.6f  %10.6f  %10d  %10.6f %10d" x (f x) summ iter summ2 iter2
// make sure to improve this table to include the required number of iterations
// for each of the methods

main
