;;; Copyright 2013-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         oo-util ;; part of easy, though?
         2d-shape
         color
         dsssl
         cj-sxml
         cj-sxml-serializer
         (tempfile tempfile-incremental-at)
         (cj-functional-2 =>))


(def. (real.svg-string x)
  (let* ((integertostring
          (lambda (x)
            (number.string
             (if (exact? x)
                 x
                 (inexact->exact x))))))
    (if (integer? x)
        (integertostring x)
        (let ((x* (inexact.round-at (exact->inexact x) 4)))
          (if (integer? x*)
              (integertostring x*)
              (number.string x*))))))

(TEST
 > (real.svg-string 0)
 "0"
 > (real.svg-string -0.99999999999)
 "-1"
 > (real.svg-string 2/3)
 ".6667"
 ;; no need to add leading zero:
 ;; http://dev.w3.org/SVG/profiles/1.1F2/publish/types.html#DataTypeNumber
 > (real.svg-string -2/3)
 "-.6667"
 > (real.svg-string -4/3)
 "-1.3333"
 > (real.svg-string -4.)
 "-4"
 > (real.svg-string -0.)
 "0")


(def (optionS/default optionsS default-options)
     ;; yep optionsS, not optionS, XX naming wrong further down.
     (lambda (f)
       (or (and optionsS (improper-any f optionsS))
           (and default-options (f default-options)))))


(def default-2d-point-colors (colors (colorstring "blue")
                                     (colorstring "blue")))

(def. (2d-point.svg-fragment shape fit #!optional optionS)
  (let ((p (fit shape))
        (getopt (optionS/default optionS default-2d-point-colors)))
    `(circle (@ (cx ,(.svg-string (.x p)))
                (cy ,(.svg-string (.y p)))
                (r ,(or (getopt .maybe-stroke-width) 1.5))
                (stroke ,(.html-colorstring (getopt .maybe-stroke-color)))
                (stroke-width 0)
                (fill ,(.html-colorstring (getopt .maybe-fill-color)))))))


(def (_svg-point command p last?)
     (list command
           " "
           (.svg-string (.x p))
           " "
           (.svg-string (.y p))
           (if last? #f " ")))

(def (_svg-point* maybe-command p)
     (let ((cont (list (.svg-string (.x p))
                       ","
                       (.svg-string (.y p)))))
       (if maybe-command
           (cons* maybe-command
                  " "
                  cont)
           cont)))



(def default-2d-line-color (colorstring "black"))

(def. (2d-line.svg-fragment shape fit #!optional optionS)
  (let-2d-line
   ((from to) shape)
   (let ((getopt (optionS/default optionS default-2d-line-color)))
     `(path (@ (d ,(cons (_svg-point "M" (fit from) #f)
                         (_svg-point "L" (fit to) #t)))
               (stroke ,(.html-colorstring (getopt .maybe-stroke-color)))
               (stroke-width ,(or (getopt .maybe-stroke-width) 1)))))))


(def default-2d-path-colors (colors (colorstring "black")
                                    (colorstring "green")))

(def. (2d-path.svg-fragment shape fit #!optional optionS)
  (let ((ps (map fit (.points shape)))
        (getopt (optionS/default optionS default-2d-path-colors)))
    (let-pair
     ((p0 ps*) ps)
     `(path
       (@ (d ,(list (_svg-point* "M" p0)
                    " "
                    (list-join
                     (map (C _svg-point* #f _) ps*)
                     " ")
                    (if (.closed? shape)
                        " z"
                        "")))
          (stroke ,(.html-colorstring (getopt .maybe-stroke-color)))
          (stroke-width ,(or (getopt .maybe-stroke-width) 1))
          (fill ,(.html-colorstring (getopt .maybe-fill-color))))))))

(TEST
 > (def p (2d-path (list (2d-point 1 7) (2d-point 2 9)) #t))
 > (.svg-fragment p identity)
 (path (@ (d (("M" " " "1" "," "7") " " (("2" "," "9")) " z"))
          (stroke "black")
          (stroke-width 1)
          (fill "green")))
 > (.svg-fragment p identity (colorstring "yellow"))
 (path (@ (d (("M" " " "1" "," "7") " " (("2" "," "9")) " z"))
          (stroke "yellow")
          (stroke-width 1)
          (fill "yellow")))
 > (.svg-fragment p identity (paint stroke-width: 5
                                    fill-color: (colorstring "white")))
 (path (@ (d (("M" " " "1" "," "7") " " (("2" "," "9")) " z"))
          (stroke "black")
          (stroke-width 5)
          (fill "white")))
 > (.svg-fragment p identity (paint fill-color: (rgb01l 0 0.5 1)))
 (path (@ (d (("M" " " "1" "," "7") " " (("2" "," "9")) " z"))
          (stroke "black")
          (stroke-width 1)
          (fill "#00BCFF"))))


;; insert SVG code verbatim
(jclass (svg-fragment [sxml-element? value])

        (def-method (svg-fragment shape fit #!optional optionS)
          ;; simply ignore the arguments?
          value)

        ;; and those 2d-shape methods that are required for operation
        ;; in svg.scm
        (def-method (min+maxs/prev v min+max)
          ;; simply be invisible? XX totally unsafe and bad.
          min+max))



(def default-2d-square-colors (colors (colorstring "black")
                                      (colorstring "none")))

(def. (2d-square.svg-fragment shape fit #!optional optionS)
  (let ((getopt (optionS/default optionS default-2d-square-colors)))
    `(path
      (@ (d ,(let* ((ps (.points shape))
                    (p0 (car ps)))
               (cons (_svg-point "M" (fit p0) #f)
                     (fold-right (lambda (p r)
                                   (cons (_svg-point "L" (fit p) #f)
                                         r))
                                 (_svg-point "L" (fit p0) #t)
                                 (cdr ps)))))
         (stroke ,(.html-colorstring (getopt .maybe-stroke-color)))
         (stroke-width ,(or (getopt .maybe-stroke-width) "1"))
         (fill ,(.html-colorstring (getopt .maybe-fill-color)))))))


(def (svg #(2d-point? size)
          #(2d-window? window) ;; 2d-window into the shapes data
          shapes ;; flat list of shapes; no grouping supported (yet?)
          #!key
          #((maybe color?) background-color)
          (#(real? border) 5))
     (let* ((borderpoint (2d-point border border))
            (fit
             (let. ((mi range) window)
                   (let* ((stretch (../ size range)))
                     (lambda (p)
                       (.+ (..* (.- p mi) stretch) borderpoint))))))
       `(svg
         (@ (xmlns "http://www.w3.org/2000/svg")
            (xmlns:xlink "http://www.w3.org/1999/xlink")
            (height ,(integer-ceiling (+ (.y size) (* 2 border))))
            (width ,(integer-ceiling (+ (.x size) (* 2 border))))
            ,(and background-color
                  `(style ,(string-append
                            "background-color: "
                            (.html-colorstring background-color)
                            ";"))))
         ;; display from imagemagick 8:6.7.7.10-5 ignores any style
         ;; etc. attributes of the svg element that set the bgcolor,
         ;; thus:
         ,(and background-color
               `(rect (@ (width "100%")
                         (height "100%")
                         (fill ,(.html-colorstring background-color)))))
         ,(map ;;stream-map
           (lambda (shape)
             (if (painted? shape)
                 (let-painted ((optionS shape) shape)
                              (.svg-fragment shape fit optionS))
                 (.svg-fragment shape fit)))
           shapes))))


(def svg-path-generate
     (tempfile-incremental-at "out-" ".svg"))

(def use-eog-if-possible? #f)
(def svg-viewer
     (let ((els (lambda ()
                  (letv ((out s) (backtick "which" "display"))
                        (if (zero? s)
                            ;; oddly need to force output size
                            (list out "-resize" "800x800")
                            #f)))))
       (if use-eog-if-possible?
           (letv ((out s) (backtick "which" "eog"))
                 (if (zero? s)
                     (list out)
                     (els)))
           (els))))

(def default-svg-size (2d-point 800 800))

(defenum fit
  within
  clip
  stretch)

(def (_fit-to-props fit size)
     (let ((targetprops (.x/y size)))
       (xcase fit
              ((within)
               (C .fit-to-proportions _ targetprops #f))
              ((clip)
               (C .fit-to-proportions _ targetprops #t))
              ((stretch)
               identity))))

;; with auto-scaling/cropping to the given size
(def (showsvg shapes . options)
     (let ((fit (-> fit? (dsssl-ref options fit: 'within)))
           (size (dsssl-ref options size: default-svg-size))
           (path (or (dsssl-ref options path: #f)
                     (svg-path-generate)))
           (options (=> options
                        (dsssl-delete fit:)
                        (dsssl-delete size:)
                        (dsssl-delete path:))))
       ;; ah want regenerate stream(s) maybe? not cache? well. how to say har.
       (let (p0 (.start (car (force shapes))))
         (let-pair ((mi ma) (stream-fold-left .min+maxs/prev
                                              (cons p0 p0)
                                              shapes))
                   (let* ((usage-window (2d-window mi ma))
                          (cont
                           (lambda (size window)
                             (sxml>>pretty-xml-file
                              (apply svg
                                     size
                                     window
                                     shapes
                                     options)
                              path)))
                          (cont-size
                           (lambda (size)
                             (cont size usage-window)))
                          (cont-fit
                           (lambda (size)
                             (cont size
                                   ((_fit-to-props fit size) usage-window)))))

                     (if (partial-2d-point? size)
                         (xcase (.partial-kind size)
                                ((full)
                                 (cont-fit (.2d-point size)))
                                ;; In partial cases, |fit| is ignored.
                                ;; (Should the program instead require
                                ;; the user to give a 'partial
                                ;; ('complete-size, 'calculate-size)
                                ;; fit value and save the dispatching
                                ;; here?)
                                ((x-given)
                                 (let. ((x) size)
                                       (cont-size
                                        (2d-point x
                                                  (/ x (.x/y usage-window))))))
                                ((y-given)
                                 (let. ((y) size)
                                       (cont-size
                                        (2d-point (* y (.x/y usage-window))
                                                  y))))
                                ((empty)
                                 (error "given size has neither x nor y")))
                         (cont-fit size)))

                   (future (apply xsystem `(,@svg-viewer "--" ,path)))
                   path))))

;; with manual scaling/cropping
;; XX copy-paste
(def (showsvg* size window shapes . options)
     (let (path (or (dsssl-ref options path: #f)
                    (svg-path-generate)))
       ;; ah want regenerate stream(s) maybe? not cache? well. how to say har.
       (let (p0 (.start (car (force shapes))))
         (let-pair ((mi ma) (stream-fold-left .min+maxs/prev
                                              (cons p0 p0)
                                              shapes))
                   (sxml>>pretty-xml-file
                    (apply svg
                           size
                           window
                           shapes
                           (=> options
                               (dsssl-delete path:)))
                    path)
                   (future (apply xsystem `(,@svg-viewer "--" ,path)))
                   path))))
