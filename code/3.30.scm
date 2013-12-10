;set the delay of
;       and-gate-delay:x     
;       or-gate-delay :y
;  inverter-gate-delay:z
;according to the structure or adder:
;so a half adder's delay: 2*x + y +z
;so a full adder's delay: 2*(ha)+y=2*(2*x + y +z)+y=4*x+3*y+2*z
;so a n-bits adder's delay: (4n*x+3n*y+2n*z)