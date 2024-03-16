module ConsoleApp1.test
printfn "Hello from F#"
let twice x = x * 2

printfn "%d" (twice 7)
let res = twice( twice 3 )
printfn "%d" res

let x = 5 |> twice |> twice
printfn "%d" x

let solve (a, b, c) =
    let d = b * b - 4. * a * c
    let x1 = (-b - sqrt d) / (2. * a)
    let x2 = (-b + sqrt d) / (2. * a)
    (x1, x2)

let a,b =  (1., 2., 1.) |> solve
printfn "%.3f %.3f" a b
let c,d = solve <| (1., 2., -3.)
printfn "%.3f %.3f" c d