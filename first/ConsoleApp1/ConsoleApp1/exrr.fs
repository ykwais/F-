module ConsoleApp1.exrr

let EPS = 0.000001
let f1 x = exp(x) - exp(-x) - 2.
let f2 x = sin(log(x)) - cos(log(x)) + 2.* log(x)
let f3 x = x - 2. + sin(1./x)

let f1' x = (exp(2.*x) + 1.)/exp(x)
let f2' x = ( cos(log(x)) + sin(log(x)) + 2.)/x
let f3' x = 1. - cos(1./x)/x/x

let p1 x = log(exp(-x) + 2.)

let p2 x = exp((cos(log(x)) - sin(log(x)))/2.)
let p3 x = 2. - sin(1./x)


let rec dichotomy f l r =
    let m = (l + r) / 2.0
    if abs(l - r) < EPS then m
    elif f l * f m < 0.0 then dichotomy f l m
    else dichotomy f m r
    
let rec newton f f' x0 =
    let x1 = x0 - (f x0 / f' x0)
    if abs(x1 - x0) < EPS then x1
    else newton f f' x1
    
let iter f l r =
    let x0 = (r + l)/2.0
    let rec iter_m f x0 =
        let x1 = f x0
        if abs(x1 - x0) < EPS then x1
        else iter_m f x1
    iter_m f x0
    
    

let main = 
    printfn "| Method     | Equation 1 | Equation 2 | Equation 3 |"
    printfn "|------------|------------|------------|------------|"
    printfn "| Dichotomy  | %10.5f | %10.5f | %10.5f |" (dichotomy f1 0. 1.) (dichotomy f2 1. 3.) (dichotomy f3 1.2 2.)
    printfn "| Iterations | %10.5f | %10.5f | %10.5f |" (iter p1 0. 1.) (iter p2 1. 3.) (iter p3 1.2 2.)
    printfn "| Newton     | %10.5f | %10.5f | %10.5f |" (newton f1 f1' 0.) (newton f2 f2' 1.) (newton f3 f3' 1.2)

main
