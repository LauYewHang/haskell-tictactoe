import Fudgets

main :: IO()
main = fudlogue (shellF "Button uwu" counterF)

counterF = intDispF >==< mapstateF count 0 >==< buttonF "up"

count n Click = (n+1, [n+1])