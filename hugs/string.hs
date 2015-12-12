test x
 | x==1= "We will use the following string {string}: {" ++ string ++ "}\n"
 | x== -1||x==10="0123456789012345"
 | x== -2||x==20=string
 | x==2= "Length of the string: " ++ show(length string)
 | x==3= "Head of the string:   " ++ show(head string)
 | x==4= "Last of the string:   " ++ show(last string)
 | x==5= "Tail of the string: {" ++ tail    string ++ "}"
 | x==6= "Init of the string: {" ++ init    string ++ "}"
 | x==7= "Reverse the string: {" ++ reverse string ++ "}"
 | otherwise = "Enter 1,2,3...,or,6, or 10, or 20 as input to test"
    where string = "Hello Evren :-)"
