
; Recursive Mandelbrot point generation
; Each time a set of points is recursively plotted and calculated,
; the resulting set (z c) is added to a file, which is read by a python graphing script

; Understanding the Mandelbrot Set
; The Mandelbrot Set works on complex numbers (a number consisting of two parts)
; Whether a point on the graph is inside the fractal or not depends on:
;  -If the complex number reaches towards infinity, it is likely outside the fractal
;  -If the complex number reaches towards 0 or oscilates between numbers, it is inside the fractal
;
; The Mandlebrot Set used the idea above along with the formula:
; z(n+1) = z(n)^2 + c, where:
; z is the orbit of the point c, bounded to 0 < z < 10,000 where z0 = 0,
; n is the iteration, bounded to 0 <= n <= 500, and
; c is the point on the graph we want to text, can be +/-
;
; During calculation, the result of z recurses and is recalculated for the next iteration
; until the upper bound is hit, where it is decided whether the point is in or out of the set
;
; Example z calculation where z0 = 1, c = 1:
; formula: z = z^2 + c
; z0 = 1
; z1 = 1^2 + 1 = 2
; z2 = 2^2 + 1 = 5
; z3 = 5^2 + 1 = 26
; z4 = 26^2 + 1 = 677
; ...
; ...
; ...
; zn > upper bound, therefore 1 is outside of the set
;
;
; Considering the Mandelbrot Set to be a cartesian graph, 
; the x-axis would be the scale of the real numbers and
; the y-axis would be the scale of the imaginary numbers
;
; For graphing: 
; c is a complex number which corresponds to a point on the graph as x + yi, where:
; x is a real number in the graph, corresponding to the position on the x-axis
; y is an imaginary number in the graph, corresponding to the position on the y-axis 
; i denotes an imaginary number
; 
; Example set calculation where c = 0.4 + 0.1i:
; formula: z = z^2 + c
; z0 = 0^2 + (0.4 + 0.1i)	       = 0.4  + 0.1i
; z1 = (0.4 + 0.1i)^2 + 0.4 + 0.1i     = 0.55 + 0.18i
; z2 = (0.55 + 0.18i)^2 + 0.4 + 0.1i   = 0.67 + 0.3i
; z3 = (0.67 + 0.30i)^2 + 0.4 + 0.1i   = 0.76 + 0.5i
; ...
; ...
; ...
; zn > upper bound, therefore c = 0.4 + 0.1i is outside of the set
; The results on the right-hand side would be the points to plot on the graph (0.40x and 0.10y)

(define start-mandelbrot
  (mandelbrot-graph -2.00 0-2.00i)
)

(define mandelbrot-graph
   ; Start at the bottom left corner, so -2 whatever points when graphing
   (lambda(x y)
      ; x and y will be processed over within the bounds of -2 < x|y < 2
      ; Since x and y both start at -2.00, only bound we have to check is upper bound

      (if (mandelbrot 0 x y 0 0) ; if the points are in the set (true)
	 (begin
	    (display "(")
	    (display (+ x y))
	    (display ") ")
	    (display "is part of the mandelbrot set\n")
	 )
	 (display "")
      )

      (if (!= x 2.01)
	 ; if x != +2.00: x moves forwards towards +2.00
	 (mandelbrot-graph (+ x 0.01) y)

	 ; else:
	 (if (= y (+ x 0+2.01i))
	    ; if y == +0+2.00i: graphing is finished
	    (display "Graphing is finished")
	    (begin
	       ; else: move x back to the first column (-2.00) and move y down a row
	       (set! x -2.00)
	       (mandelbrot-graph x (+ y 0+0.01i))
	    )
	    ; Graphing is done (graphed: -2.00-2.00i -> 2.00+2.00i)
	 )
      )

   )
)

; Will be returning true or false if the number is part of the set or not
(define mandelbrot
   (lambda (z cx cy acc count) 
      ; At the end of 500 iterations of formula z = z^2 + c if z <= 2
      ; #t if zn <= 2

      ; cx = n
      ; cy = 0+ni

      ()
   )
)



