(defpackage :day14
  (:use #:cl #:alexandria))

(in-package :day14)

(defun parse-input (input)
  (let ((reactions (make-hash-table)))
    (loop for line in (str:lines input) do
      (let ((list '()))
        (ppcre:do-register-groups (all amount name)
            ("((\\d+) ([^, ]+))+" line)
          (declare (ignore all))
          (push (cons (make-keyword name) (parse-integer amount)) list))
        (let ((output (car list))
              (input (cdr list)))
          (setf (gethash (car output) reactions) (list :output-amount (cdr output)
                                                       :input input)))))
    reactions))

(defun ore-required-for-chemical (reactions chemical total-output-amount
                                  &key (starting-ore 0) (starting-amounts (make-hash-table)))
  (let ((ore-count starting-ore)
        (amounts starting-amounts))
    (labels ((r (chemical total-output-amount)
               (let* ((reaction (gethash chemical reactions))
                      (inputs (getf reaction :input))
                      (ore-amount (cdr (assoc :ore inputs))))
                 (if ore-amount
                     (loop while (< (gethash chemical amounts 0) total-output-amount) do
                       (incf (gethash chemical amounts 0) (getf reaction :output-amount))
                       (incf ore-count ore-amount))
                     (progn
                       (loop for (required-chemical . required-amount) in inputs do
                         (loop while (< (gethash required-chemical amounts 0) required-amount) do
                           (r required-chemical required-amount))
                         (decf (gethash required-chemical amounts) required-amount))
                       (incf (gethash chemical amounts 0) (getf reaction :output-amount)))))))
      (loop while (< (gethash chemical amounts 0) total-output-amount) do
        (r chemical total-output-amount))
      (values ore-count amounts))))

(defun get-amount-of-fuel-from-ore (reactions ore)
  (destructuring-bind (repeating-ore repeating-i)
      (loop for i = 0 then (1+ i)
            with current-ore-count = 0
            with current-amounts = (make-hash-table)
            with leftovers
            do (multiple-value-bind (ore-count amounts)
                   (ore-required-for-chemical reactions :fuel (1+ i)
                                                        :starting-ore current-ore-count
                                                        :starting-amounts current-amounts)
                 (when (> ore-count ore)
                   (return-from get-amount-of-fuel-from-ore i))
                 (setf current-ore-count ore-count
                       current-amounts amounts)
                 (let ((fuel (gethash :fuel amounts)))
                   (setf (gethash :fuel amounts) 0)
                   (let ((amounts-pos (position amounts leftovers :test 'equalp :from-end t)))
                     (when amounts-pos
                       (return (list ore-count (- (1+ i) (- (length leftovers) amounts-pos))))))
                   (push (copy-hash-table amounts) leftovers)
                   (setf (gethash :fuel amounts) fuel))))
    (multiple-value-bind (fuel-estimate remainder) (floor ore repeating-ore)
      (loop for i = 0 then (1+ i)
            with current-ore-count = 0
            with current-amounts = (make-hash-table)
            do (multiple-value-bind (ore-count amounts)
                   (ore-required-for-chemical reactions :fuel (1+ i)
                                                        :starting-ore current-ore-count
                                                        :starting-amounts current-amounts)
                 (setf current-ore-count ore-count
                       current-amounts amounts)
                 (when (> ore-count remainder)
                   (return (+ (* fuel-estimate repeating-i) i))))))))

(defun main (&key (part 1))
  (let ((reactions (parse-input (read-file-into-string "input.txt"))))
    (case part
      (1 (ore-required-for-chemical reactions :fuel 1))
      (2 (error "unimplemented"))
      (otherwise (error "`part' must be either 1 or 2")))))

(defun tests ()
  (let ((reactions (parse-input "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")))
    (test reactions 31))
  (let ((reactions (parse-input "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")))
    (test reactions 165))
  (let ((reactions (parse-input "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")))
    (test reactions 13312)
    (test2 reactions 82892753))
  (let ((reactions (parse-input "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF")))
    (test reactions 180697)
    (test2 reactions 5586022))
  (let ((reactions (parse-input "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX")))
    (test reactions 2210736)
    (test2 reactions 460664)))

(defun test (reactions expected-result)
  (format t "~12:d ORE for 1 FUEL: " expected-result)
  (let ((actual-result (ore-required-for-chemical reactions :fuel 1)))
    (if (= actual-result expected-result)
        (format t "Passed.~%")
        (format t "FAILED! Got ~:d~%" actual-result))))

(defun test2 (reactions expected-result)
  (format t "could produce ~:d FUEL: " expected-result)
  (let ((actual-result (get-amount-of-fuel-from-ore reactions 1000000000000)))
    (if (= actual-result expected-result)
        (format t "Passed.~%")
        (format t "FAILED! Got ~:d~%" actual-result))))
