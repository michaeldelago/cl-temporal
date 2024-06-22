(in-package :serapeum.tests)

(def-suite units :in serapeum)
(in-suite units)

(test si-prefix
  (is (equal "quecto" (si-prefix least-positive-double-float)))
  (is (equal "quetta" (si-prefix most-positive-double-float)))
  (is-true (every (equals "") (mapcar #'si-prefix '(0 1 0s0 1s0 0d0 1d0))))
  (is (equal "deca" (si-prefix 10 :base 10)))
  (is (equal "deca" (si-prefix 12 :base 10)))
  (is (equal "kilo" (si-prefix 1001)))
  (is (equal "kibi" (si-prefix 2048 :base 2)))
  (is (equal "kibi" (si-prefix 1024 :base 2)))
  (is (equal "" (si-prefix 1000 :base 2)))
  (is (equal "" (si-prefix 1000 :base 1024)))
  (is (equal "kilo" (si-prefix 1024)))
  (is (equal "kilo" (si-prefix 1000)))
  (is (equal "pico" (si-prefix 1s-9)))
  (is (equal "nano" (si-prefix 1s-9 :base 1024)))
  (is (equal "kilo" (si-prefix -20000)))
  (is (equal "" (si-prefix -20)))
  (is (equal "kilo" (si-prefix (expt 2 10))))
  (is (equal "kibi" (si-prefix (expt 2 10) :base 2)))
  (is (equal "yocto" (si-prefix (expt 10 -23)))))

(test format-human-size
  (is (equal "0" (format-human-size nil 0)))
  (is (equal "0" (format-human-size nil 0 :flavor :iec)))
  (is (equal "0" (format-human-size nil 0 :flavor :si)))
  (is (equal "1 k" (format-human-size nil 1000)))
  (is (equal "-1 k" (format-human-size nil -1000)))
  (is (equal "1Ki" (format-human-size nil 1024 :flavor :iec)))
  (is (equal "-1Ki" (format-human-size nil -1024 :flavor :iec)))
  (is (equal "500 k" (format-human-size nil 500000 :flavor :si)))
  (is (equal "-500 k" (format-human-size nil -500000 :flavor :si))))