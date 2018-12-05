;;; verdic-math-mode.el --- train your mental math skills

;; Copyright (C) 2018 Maximilian N. Diefenbach

;; Author: Max Diefenbach <maxdiefenbach@protonmail.com>
;; URL: https://github.com/maxdiefenbach/emacs-vedic-math
;; Package-Version: 20181201
;; Version: 0.0.1
;; Keywords: vedic maths, mental math, calc

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A simple package to train mental calculation with vedic maths.
;; It defines an entry function (vm/equation) that opens a buffer in
;; (vm/eqn-disp-mode) showing a math equation and asking for the solution.
;;
;; To define, which calculations to train, one can set the following variables:
;; (setq vm/ndigits '((random 5) (random 5)))
;; (setq vm/show-horz-format '(vm/random-bool))
;; (setq vm/operators '("+" "-"))

;;; Todo (someday maybe):
;; - reset go back
;; - toggle vert-horz
;; - add algebra problems
;; - track time
;; - save statistics
;; - define faces
;; - layout buffer
;; - fix single digit display error (see known bugs)

;;; Known bugs:
;; - single digit display error:
;;   Somehow some horizontal equations with single-digit term have an additional
;;   space, for example:
;;        318497931                     1
;;      +          6                - 17
;;      -----------        or       ----
;;      318497937                    -16
;; - vm/char-incorrect: Symbol’s value as variable is void: p1

;;; Code:

(defvar vm/operators '("+" "-" "*" "/")
  "mathematical operators")

(defvar vm/expr-format "%s%s%s%s%s%s%s"
  "prefix-operand1-separator-operator-separator-operand2-postfix")

(defvar vm/ndigits '(3 2)
  "number of digits for first and second operand")

(defvar vm/show-horz-format t
  "bool whether to use horizontal or vertical format")

(defcustom vm/eqn-disp-mode-face
  '(:height 500)
  "Face for vm/eqn-disp-mode")

(defun vm/set-buffer-face-eqn-disp-mode-face ()
  "sets vm/eqn-disp-mode-face"
  (let ((buffer-face-mode-face vm/eqn-disp-mode-face))
    (buffer-face-mode)))
(add-hook 'vm/equation-display-mode-hookpp
          'vm/set-buffer-face-eqn-disp-mode-face)

(defun vm/ndigit-random (n)
  "return random integer with n digits"
  (let ((res (number-to-string (+ (random 9) 1))))
    (dotimes (i (- n 1) res)
      (setq res (concat res (number-to-string (random 10)))))))

(defun vm/random-elt (l)
  "return random element of list l"
  (nth (random (length l)) l))

(defun vm/random-bool ()
  "randomly return t or nil"
  (vm/random-elt '(t nil)))

(defun vm/expression-result ()
  "return cons of math expression and result"
  (interactive)
  (let* ((ndigits vm/ndigits)
         (use-horz-format? (eval vm/show-horz-format))
         (op (vm/random-elt vm/operators))
         (n1 (eval (car ndigits)))
         (n2 (eval (cadr ndigits)))
         (ndif (abs (- n1 n2)))
         (op1 (vm/ndigit-random n1))
         (op2 (vm/ndigit-random n2))
         (sep1 " ")
         (sep2 " ")
         (pre " ")
         (post " = ")
         (expr (format vm/expr-format pre op1 sep1 op sep2 op2 post))
         (res (calc-eval (format "%s%s%s" op1 op op2)))
         (reslen (length res)))
    (when (and (equal op "/") (equal op2 "0"))
      (while (not (equal op2 "0"))
        (setq op2 (vm/ndigit-random n2))))
    (when use-horz-format?
      (message "use horizontal format")
      (setq spc (make-string ndif ? ))
      (setq sep1 (concat spc "\n"))
      (setq sep2 " ")
      (setq pre "  ")
      (setq post (concat "\n" (make-string (+ 2 (max n1 n2)) ?-)
                         "\n " (when (>= (string-to-number res) 0) " ")))
      (if (< n1 n2)
          (setq pre (concat spc "  "))
        (setq sep2 (concat spc sep2)))
      (setq expr (format vm/expr-format pre op1 sep1 op sep2 op2 post)))
    `(,expr ,res)))

(defun vm/equation ()
  "pose a mental math question"
  (interactive)
  (let* ((exprres (vm/expression-result))
         (expr (car exprres))
         (res (cadr exprres))
         (reslen (length res))
         (inhibit-read-only t))
    (switch-to-buffer "*vedic-maths*")
    (vm/equation-display-mode)
    (erase-buffer)
    (insert expr)
    (insert res)
    (put-text-property (point) (- (point) reslen) 'invisible t)
    (backward-char reslen)))

(defun vm/check-char-input? ()
  "compare input key with character at point"
  (let ((char-at-point (char-after (point)))
        (input-as-char last-command-event))
    (message "%s %s" char-at-point input-as-char)
    (equal char-at-point input-as-char)))

(defun vm/char-correct ()
  "do stuff if input char is correct"
  (let ((buffer-read-only nil)
        (p1 (point))
        (p2 (1+ (point))))
    (message "yeah, nailed it")
    (remove-overlays p1 p2)
    (put-text-property p1 p2 'invisible nil)
    (add-face-text-property p1 p2 '(:foreground "green"))
    (forward-char)
    (when (eq (point) (point-max))
      (vm/equation))))

(defun vm/char-incorrect ()
  "do stuff if input char is incorrect"
  (let ((buffer-read-only nil)
        (p1 (point))
        (p2 (1+ (point))))
    (message "wrong, try again")
    (overlay-put (make-overlay p1 p2) 'display "⚡")
    (overlay-put (make-overlay p1 p2) 'face '(:foreground "red"))))

(defun vm/check-answer ()
  "check whether input char via keypress and call corresponding response func"
  (interactive)
  (if (vm/check-char-input?)
      (vm/char-correct)
    (vm/char-incorrect)))

(defun vm/reveal-correct-digit ()
  "make correct digit visible"
  (let ((buffer-read-only nil)
        (p1 (point))
        (p2 (1+ (point))))
    (remove-overlays p1 p2)
    (put-text-property p1 p2 'invisible nil)))

(defvar vm/equation-display-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)         ;treat prefix-args as normal chars
    (define-key map "n" 'vm/equation)
    (mapc (lambda (x) (define-key map (char-to-string x) 'vm/check-answer))
          "-0123456789")
    (define-key map "r" 'vm/reveal)
    map)
  "The key map for the #<*vedic-math*> buffer.")

(define-derived-mode vm/equation-display-mode special-mode
  "vm/eqn-disp-mode"
  "major-mode for the equation display buffer opened by #`vm/equation
  from vedic-maths package")


(setq vm/ndigits '((+ 3 (random 4)) (+ 2 (random 2))))
(setq vm/show-horz-format '(vm/random-bool))
(setq vm/operators '("+" "-"))
(bind-key "<f1>" 'vm/equation)

(provide 'vedic-math-mode)
