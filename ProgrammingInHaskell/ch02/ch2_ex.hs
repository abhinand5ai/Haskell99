

n = a `div` length xs
 where
  a = 10
  xs = [1,2,3,4,5]

last' = head . reverse

init' = reverse . tail . reverse