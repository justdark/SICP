(car  ''abcdeaffs)
(car '(quote abcdeaffs))
;
;根据 97 页的注释 100 ,符号 ' 在求值时会被替换成 quote 特殊形式,因此,求值:
;(car '(quote sdasd))
;