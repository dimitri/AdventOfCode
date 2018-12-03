(in-package :advent/2017)

(defparameter *input/d4/p1*
  (asdf:system-relative-pathname :advent "2017/d04.input"))

#|

A new system policy has been put in place that requires all accounts to use
a passphrase instead of simply a password. A passphrase consists of a series
of words (lowercase letters) separated by spaces.

To ensure security, a valid passphrase must contain no duplicate words.

For example:

  - aa bb cc dd ee is valid.
  - aa bb cc dd aa is not valid - the word aa appears more than once.
  - aa bb cc dd aaa is valid - aa and aaa count as different words.

The system's full passphrase list is available as your puzzle input. How
many passphrases are valid?

|#

(defun split-line-into-words (line)
  (loop :for position := 0 :then (position-if (lambda (c) (char/= c #\Space))
                                              line
                                              :start next-space-pos)
     :for next-space-pos := (position #\Space line :start position)
     :collect (subseq line position next-space-pos)
     :while next-space-pos))

(defun passphrase-valid-p (passphrase)
  (let ((words (split-line-into-words passphrase)))
    (= (length words)
       (length (remove-duplicates words :test #'string=)))))

(defun d4/p1 ()
  (with-open-file (input *input/d4/p1*
                         :direction :input
                         :element-type 'character)
    (loop :for line := (read-line input nil nil)
       :while line
       :count (passphrase-valid-p line))))

#|

For added security, yet another system policy has been put in place. Now, a
valid passphrase must contain no two words that are anagrams of each other -
that is, a passphrase is invalid if any word's letters can be rearranged to
form any other word in the passphrase.

For example:

  - abcde fghij is a valid passphrase.

  - abcde xyz ecdab is not valid - the letters from the third word can be
    rearranged to form the first word.

  - a ab abc abd abf abj is a valid passphrase, because all letters need to
    be used when forming another word.

  - iiii oiii ooii oooi oooo is valid.

  - oiii ioii iioi iiio is not valid - any of these words can be rearranged
    to form any other word.

Under this new system policy, how many passphrases are valid?

|#

(defparameter *primes*
  (make-array 26
              :initial-contents '(2 3 5 7 11 13 17 19 23 29
                                  31 37 41 43 47 53 59 61 67
                                  71 73 79 83 89 97 101))
  ;; 103 107 109 113 127 131 137 139
  ;; 149 151 157 163 167 173 179 181 191 193 197 199
  ;; 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283
  ;; 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383
  ;; 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467
  ;; 479 487 491 499 503 509 521 523 541
  "https://en.wikipedia.org/wiki/List_of_prime_numbers")

(defun char-value (char)
  (aref *primes* (- (char-code char) #. (char-code #\a))))

(defun string-value (string)
  (reduce #'* (loop :for char :across string :collect (char-value char))))

(defun anagrams-p (string1 string2)
  (= (string-value string1) (string-value string2)))

(defun passphrase-valid-p2 (passphrase)
  (let* ((words (coerce (split-line-into-words passphrase) 'vector))
         (size  (length words)))
    (not
     (loop :for i :below size
        :thereis (loop :for j :from (+ 1 i) :below size
                    :thereis (anagrams-p (aref words i) (aref words j)))))))

(defun test/d4/p2 ()
  (every #'identity
         (list (eq t   (passphrase-valid-p2 "abcde fghij"))
               (eq nil (passphrase-valid-p2 "abcde xyz ecdab"))
               (eq t   (passphrase-valid-p2 "a ab abc abd abf abj"))
               (eq t   (passphrase-valid-p2 "iiii oiii ooii oooi oooo"))
               (eq nil (passphrase-valid-p2 "oiii ioii iioi iiio")))))

(defun d4/p2 ()
  (with-open-file (input *input/d4/p1*
                         :direction :input
                         :element-type 'character)
    (loop :for line := (read-line input nil nil)
       :while line
       :count (passphrase-valid-p2 line))))

(defun d4/summary ()
  (format t "Day 4: High-Entropy Passphrases~%")
  (format t "  Puzzle 1: passphrase must contain no duplicate words~%")
  (print-result (d4/p1))
  (format t "  Puzzle 2: passphrase must contain no two anagrams~%")
  (print-result (d4/p2)))
