factorial 0 = 1
factorial n = n * factorial (n-1)

perm (x:[]) _ = [x]
perm nums n   = let fac = factorial ((length nums) - 1) in
                let digit = nums !! (div n fac) in
		digit : perm (filter (/= digit) nums) (mod n fac)

main = print $ perm [0..9] 999999
