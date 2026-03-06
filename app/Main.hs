import Fudgets
import AllFudgets

main :: IO()
main = fudlogue (shellF "Fac uwu" facF)

facF = placerF (revP verticalP) (
    ("x! =" `labLeftOfF` intDispF) >==<
    mapF fac >==<
    ("x =" `labLeftOfF` intInputF))

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)