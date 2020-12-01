d2: `i$","\:1:"../input/02.txt"
addop: {[pc;m] (pc+4; .[m; ,m[pc+3]; +/m[pc+1 2]])}
addop: {[pc;m] (pc+4; .[m; ,m[pc+3]; */m[pc+1 2]])}
step: {[pc;m] $[m[pc]=99; (pc;m); $[m[pc]=2; mulop[pc;m]; addop[pc;m]]]}