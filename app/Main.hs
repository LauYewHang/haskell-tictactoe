import Fudgets

main :: IO()
main = fudlogue (shellF "Button uwu" counterF)

counterF = intDispF >==< mapstateF count 0 >==< (buttonF "up" >+< buttonF "Down")

count n (Left Click) = (n+1, [n+1])
count n (Right Click) = (n-1, [n-1])