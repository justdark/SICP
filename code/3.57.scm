;a) n-1 times,because: fib(n)=fib(n-1)+fib(n-2)
;                    fib(n-1)=fib(n-2)+fib(n-3)
;                    fib(n-2)=fib(n-3)+fib(n-4)
;                     .
;                     .
;                    fib(1)=1
;thanks to the memory,n times
;b) if no memo-proc: f(2)=f(0)+f(1)     1 time
;                    f(3)=f(2)+f(1)=f(1)+f(0)+f(0)  1*2 times
;                    f(4)=f(3)+f(2)=f(1)+f(0)+f(0)+f(0)+f(1) 1*1*2*2 times
;                    f(5)=f(4)+f(3)=f(1)+f(0)+f(0)+f(0)+f(1)+f(1)+f(0)+f(0)  1*2*2*2 times...
;                      .
;                      .
;                      .
;                   2^(n-2) times 

