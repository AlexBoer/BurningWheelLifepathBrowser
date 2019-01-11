;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |burning wheel 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct lifepath (name next))
;; Lifepath is (make-lifepath String (listof Lifepath))
;; interp. a period in someone's life and their next steps

(define WORKER (make-lifepath "Worker" (list )))
(define INTERN (make-lifepath "Intern" (list WORKER )))
(define STUDENT (make-lifepath "Student" (list INTERN WORKER)))

;; Person is (listof String)
(define JOHN (list "Student" "Intern" "Worker"))

;; Lifepath is (listof Lifepath)
(define BORN-PEASANT (shared
                    ((-BP- (make-lifepath "Born Peasant" (list -F- -M- -MW-)))
                     (-F- (make-lifepath "Farmer" (list -F- -M- -MW-)))
                     (-M- (make-lifepath "Miller" (list -F- -M- -MW-)))
                     (-MW- (make-lifepath "Midwife" (list -F- -M- -MW-)))
                     )-BP-))

;; Setting Natural -> (listof Life)

;; template: structural recursion, encapsulated w/local
;; tail-recursive w/ worklist, context preserving accumulator w/ life so far
;; added WLE (worklist element)

(check-expect (list-lives BORN-PEASANT 1) (list (list "Born Peasant")))
(check-expect (list-lives BORN-PEASANT 2) (list (list "Born Peasant"
                                                 "Farmer")
                                           (list "Born Peasant"
                                                 "Miller")))

(define (list-lives lp0 limit)
  ; todo is (listof WLE); a worklist accumulator
  (local [
          (define-struct wle (life count))
          ; WLE is (make-wle (listof Lifepath) count)
          ; interp. someone's life so far

          ; (listof Lifepath) (listof Lifepath) Natural -> (listof WLE)
          ; produces a todo list from a list of lifepaths
          (define (lolp-to-lowle lolp now count)
            (local [(define (k w)
                      (make-wle (cons w now) count))]
              (map k lolp)))
          
          ; WLE -> (listof String)
          ; produces a list of all lifepath names in a WLE
          (define (write-lolp now)
            (cond [(empty? now) empty]
                  [else
                   (cons (lifepath-name (first now))
                         (write-lolp (rest now)))]))
          
          ; (listof Lifepath) (list WLE) Natural-> (listof Lifepath)
          (define (fn-for-lifepath now todo acc lives)
            (if (= acc limit)
                (fn-for-lolp todo                          ;(listof WLE)
                             (cons (write-lolp now) lives)) ;lives
                (fn-for-lolp (append (lolp-to-lowle (lifepath-next (first now)) now (add1 acc)) todo)
                             lives)))

          ; (listof Lifepath) (list WLE) (listof Life) -> ....
          (define (fn-for-lolp todo lives)
            (cond [(empty? todo) lives]
                  [else
                   (fn-for-lifepath (wle-life (first todo))      ;now
                                    (rest todo)      ;todo
                                    (wle-count (first todo))
                                    lives)]))] ;acc
    (fn-for-lifepath (list lp0) empty 1 (list (lifepath-name lp0)))))
> 
