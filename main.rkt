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
(define out-file (open-output-file "points.txt"))

(define output-to-file
  (lambda (out)
    (let ((formatted-out (string-append (number->string (real-part out)) (if (>= (imag-part out) 0) "+" "") (number->string (imag-part out)) "i")))
      (write
	formatted-out
	out-file
      )
      (newline out-file)
    )
  )
)

(define start-graph 
   ; Start at the bottom left corner, so -2 whatever points when graphing
   (lambda(x y)
      ; x and y will be processed over within the bounds of -2 < x|y < 2
      ; Since x and y both start at -2.00, only bound we have to check is upper bound
      ;   (mandelbrot z  c	x y counter)
      ; (set! y (make-rectangular 0+0i (/ (round (* (imag-part y) 100)) 100)))
      (set! x (/ (round (* x 100)) 100))
      (set! y (make-rectangular 0+0i (/ (round (* (imag-part y) 100)) 100)))
      (if (mandelbrot 0 (+ x y) 0 0 0) ; if the points are in the set (true)
	 (begin
	    ;(display (+ x y))
	    ;(display "\tis part of the mandelbrot set\n")
	    (output-to-file (+ x y))
	 )
	 (display "")
	 #| (begin |#
	 #|    (display (+ x y)) |#
	 #|    (display "is NOT part of the mandelbrot set\n") |#
	 #| ) |#
      )

      ; Advances the recursion
      (if (< x 2.00)
	 ; if x < 2.00: x moves forwards towards +2.00
	 (start-graph (+ x 0.01) y)

	 ; else:
	 (if (>= (imag-part y) 2.00)
	    ; if y == 0+2.00i: graphing is finished since x and y are both at +2.00: exit
	    (begin
	       (display "Graphing is finished")
	       (close-output-port out-file)
	    )
	    ; else: 
	    (begin
	       ; move x back to the first column (-2.00) and move y up a row
	       (set! x -2.00)
	       (start-graph x (+ y 0+0.01i))
	    )
	 )
      )
   )
)

; Will be returning true or false if the number is part of the set or not
(define mandelbrot
   (lambda (z c zx zy counter) 
      (set! zx (real-part z)) 
      (set! zy (imag-part z))

      #| (display counter) |#
      #| (display ": z=") |#
      #| (display z) |#
      #| (display "\t\t\tzx=") |#
      #| (display zx) |#
      #| (display "\tzy=") |#
      #| (display zy) |#
      #| (display "\n") |#

      ; At the end of 500 iterations of formula z = z^2 + c if z <= 2
      ; #t if zn <= 2
      ; cx = n
      ; cy = 0+ni
      ; z  = cx+cyi

      ; doing: if ((cx > 500 or cx < -500) or (cy > cx+500i or cy < cx-500i)) )
      (if (or (or (> zx 500) (< zx -500))
	      (or (> zy 500) (< zy -500))) 
	#f
	#| (begin |#
	#|   (display "Failed bounds check\n") |#
	#|   #f |#
	#| ) |#
	   
	; Now that the checks are over, we can start calculating for z. Remember:
	; z0 =		(0)^2 + (0.4+0.1i) = 0.40+0.10i
	; z1 = (0.40+0.10i)^2 + (0.4+0.1i) = 0.55+0.18i
	; z2 = (0.55+0.18i)^2 + (0.4+0.1i) = 0.67+0.30i
	; z3 = (0.67+0.30i)^2 + (0.4+0.1i) = 0.76+0.50i
	; ...
	; ...
	; ...
	; zn > upper bound, therefore c = 0.4 + 0.1i is outside of the set
	 
	; Translating the formula above to the logic below:
	; z(n+1) = (zx + zy)^2 + (c-real + c-imag)
	; if the count is at the final step and hasn't returned false yet
	; (if (= counter 500)
	(if (= counter 500)
	   #t
	   ; Get the new z for the next iteration, increase counter, and recurse
	   (begin
	      (set! z (+ (expt z 2) c))

	      ; Rounding Complex Numbers: Revised Report Pg. 24 (imag-num real-num)
	      (set! z (+ (/ (round (* (real-part  z) 100)) 100) (make-rectangular 0+0i (/ (round (* (imag-part z) 100)) 100))))
	      (mandelbrot z c zx zy (+ counter 1))
	   )
	 )
      )
   )
)

(define test-it
  (lambda (zx zy)
    (display "Starting test with: ")
    (display (+ zx (make-rectangular 0+0i zy)))
    (display "\n")
    (mandelbrot 0 (+ zx (make-rectangular 0+0i zy)) 0 0 0)
  )
)

(define start-mandelbrot
  (start-graph -2.00 0-2.00i)
)

