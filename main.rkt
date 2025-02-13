; Recursive Mandelbrot point generation
; Each time a set of points is recursively plotted and calculated,
; the resulting set (z c) is added to a file, which is read by a python or graphing script or HTDP
;
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

      (if (mandelbrot 0+0i x y 0) ; if the points are in the set (true)
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

	    ; else: 
	    (begin
	       ; move x back to the first column (-2.00) and move y up a row
	       (set! x -2.00)
	       (mandelbrot-graph x (+ y 0+0.01i))
	    )
	 )
      )
   )
)

; Will be returning true or false if the number is part of the set or not
(define mandelbrot
   (lambda (z cx cy counter) 
      ; At the end of 500 iterations of formula z = z^2 + c if z <= 2
      ; #t if zn <= 2

      ; cx = n
      ; cy = 0+ni

      ; doing: if (cx > 10000 or cx < -10000) or (cy > cx+10000i or cy < cx-10000i) the hard way 
      (if (> cx 10000)
	 ; if cx is out of positive bounds
	 #f
	 (display "") 
      )
      (if (< cx -10000)
	 ; if cx is out of negative bounds
	 #f
	 (display "") 
      )
      (if (> cy (+ cx 0+10000i))
	 ; if cy is out of positive bounds
	 #f
	 (display "") 
      )
      (if (< cy (+ cx 0-10000i))
	 ; if cy is out of negative bounds
	 #f
	 (display "") 
      )
	 
      ; Now that the checks are over, we can start calculating for z. Remember:
      ; z0 =		0^2 + (0.4+0.1i) = 0.40+0.10i
      ; z1 = (0.40+0.10i)^2 + (0.4+0.1i) = 0.55+0.18i
      ; z2 = (0.55+0.18i)^2 + (0.4+0.1i) = 0.67+0.30i
      ; z3 = (0.67+0.30i)^2 + (0.4+0.1i) = 0.76+0.50i
      ; ...
      ; ...
      ; ...
      ; zn > upper bound, therefore c = 0.4 + 0.1i is outside of the set
       
      ; if the count is at the final step and hasn't returned false yet
      (if (= counter 500)
	 #t
	 ; Get the new z for the next iteration, increase counter, and recurse
	 (begin
	    (set! z (+ (expt z 2) (+ cx cy)))
	    ; Rounding Complex Numbers: Revised Report Pg. 24 (imag-num real-num)
	    (+ counter 1)
	    (mandelbrot z cx cy counter)
	 )
      )
   )
)



