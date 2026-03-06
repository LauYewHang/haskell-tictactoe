import Fudgets

main :: IO()
main = fudlogue (shellF "Button uwu" counterF)

counterF = intDispF >==< mapstateF count 0 >==< buttonsF

data Button = Up | Down | Reset deriving (Eq)

buttonsF = listF [
    (Up, buttonF "Up"),
    (Down, buttonF "Down"),
    (Reset, buttonF "Reset")]

count n (Up, Click) = (n+1, [n+1])
count n (Down, Click) = (n-1, [n-1])
count n (Reset, Click) = (0, [0])