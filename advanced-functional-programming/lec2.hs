type Grid = Matrix Value

-- parameterized type
type Matrix a = [Row a]

-- parameterized type
type Row a = [a]

type Value = Char

-- Example

easy :: Grid
easy =
  [ "2....1.38",
    "........5",
    ".7...6...",
    ".......13",
    ".981..257",
    "31....8..",
    "9..8...2.",
    ".5..69784",
    "4..25...."
  ]


blank :: Grid
blank = replicat