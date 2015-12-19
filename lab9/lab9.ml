#require "pa_ounit"
#directory "broken"
#load "bstDict.d.cmo"
#load "listDict.d.cmo"
#load "sortedListDict.d.cmo"

let x = BstDict.(insert empty 0 'x')
let y = ListDict.(insert empty 1 'y')
let z = SortedListDict.(insert empty 2 'z')
