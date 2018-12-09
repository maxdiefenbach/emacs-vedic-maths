;;; verdic-maths-mode.el --- train your mental math skills

;; Copyright (C) 2018 Maximilian N. Diefenbach

;; Author: Max Diefenbach <maxdiefenbach@protonmail.com>
;; URL: https://github.com/maxdiefenbach/emacs-vedic-maths
;; Package-Version: 20181208
;; Version: 0.0.2
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
;; It defines an entry function (vm/new-equation) that opens a buffer
;; *vedic-maths* in (vm/eqn-disp-mode) showing a math equation and
;; asking for the solution.
;;
;; To define, which calculations to train, one can set the following variables:
;; (setq vm/ndigits '((random 5) (random 5)))
;; (setq vm/show-horz-format '(vm/random-bool))
;; (setq vm/operators '("+" "-"))

;; (bind-key "<f1>" 'vm/new-equation)

;;; Todo (someday maybe):
;; - reset, go back
;; - add algebra problems
;; - track time
;; - save statistics
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

(defcustom vm/operators '("+" "-" "*" "/")
  "mathematical operators"
  :group 'vedic-maths)

(defcustom vm/expr-format "%s%s%s%s%s%s%s%s"
  "prefix-operand1-separator-operator-separator-operand2-postfix"
  :group 'vedic-maths)

(defcustom vm/ndigits '(3 2)
  "number of digits for first and second operand"
  :group 'vedic-maths)

(defcustom vm/show-horz-format t
  "bool whether to use horizontal or vertical format"
  :group 'vedic-maths)

(defcustom vm/eqn-disp-mode-face
  '(:height 500)
  "Face for vm/eqn-disp-mode"
  :group 'vedic-maths)

(defvar vm/eqn
  '(pre "" post "" op "" op1 "" op2 "" sep1 "" sep2 "" res "" isvert t)
  "plist holding math equation details")


(defvar vm/equation-display-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)         ;treat prefix-args as normal chars
    (define-key map "n" 'vm/new-equation)
    (mapc (lambda (x) (define-key map (char-to-string x) 'vm/check-answer))
          "-0123456789")
    (define-key map "r" 'vm/reveal-correct-digit)
    (define-key map "t" 'vm/change-format)
    (define-key map "<tab>" 'vm/swap-operands)
    map)
  "The key map for the #<*vedic-math*> buffer.")

(define-derived-mode vm/equation-display-mode special-mode
  "vm/eqn-disp-mode"
  "major-mode for the equation display buffer opened by #`vm/equation
  from vedic-maths package"
  (vm/set-buffer-face-eqn-disp-mode-face))

(defun vm/new-equation ()
  (interactive)
  (vm/create-equation)
  (vm/display-equation))

(defun vm/create-equation ()
  "create an equation with random integers"
  (interactive)
  (let* ((vm-eqn vm/eqn)
         (ndigits vm/ndigits)
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
         (post " = "))
    (when (and (equal op "/") (equal op2 "0"))
      (while (not (equal op2 "0"))
        (setq op2 (vm/ndigit-random n2))))
    (plist-put vm-eqn 'pre pre)
    (plist-put vm-eqn 'post post)
    (plist-put vm-eqn 'sep1 sep1)
    (plist-put vm-eqn 'sep2 sep2)
    (plist-put vm-eqn 'op op)
    (plist-put vm-eqn 'op1 op1)
    (plist-put vm-eqn 'op2 op2)
    (plist-put vm-eqn 'isvert t)
    (vm/set-result vm-eqn)
    (when (eval vm/show-horz-format)
      (vm/toggle-vert-horz vm-eqn))))

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

(defun vm/set-result (vm-eqn)
  "use calc to solve expression"
  (plist-put vm-eqn 'res
             (calc-eval (format "%s%s%s"
                                (plist-get vm-eqn 'op1)
                                (plist-get vm-eqn 'op)
                                (plist-get vm-eqn 'op2)))))

(defun vm/display-equation ()
  "render equation in dedicated buffer"
  (interactive)
  (switch-to-buffer "*vedic-maths*")
  (vm/equation-display-mode)
  (let* ((buffer-read-only nil)
         (vm-eqn vm/eqn)
         (res (plist-get vm-eqn 'res))
         (reslen (length res)))
    (erase-buffer)
    (insert (vm/build-equation vm-eqn))
    (put-text-property (- (point-max) reslen) (point-max) 'invisible t)
    (goto-char (- (point-max) reslen))))

(defun vm/build-equation (vm-eqn)
  "convert equation plist to string"
  (let ((pre (plist-get vm-eqn 'pre))
        (post (plist-get vm-eqn 'post))
        (sep1 (plist-get vm-eqn 'sep1))
        (sep2 (plist-get vm-eqn 'sep2))
        (op (plist-get vm-eqn 'op))
        (op1 (plist-get vm-eqn 'op1))
        (op2 (plist-get vm-eqn 'op2))
        (res (plist-get vm-eqn 'res)))
    (format vm/expr-format pre op1 sep1 op sep2 op2 post res)))

(defun vm/change-format ()
  (interactive)
  (vm/toggle-vert-horz vm/eqn)
  (vm/display-equation))

(defun vm/toggle-vert-horz (vm-eqn)
  "toggle vertical and horizontal equation display format
by changing properties of the plist vm-eqn"
  (if (plist-get vm-eqn 'isvert)
      (let* ((pre "  ")
             (n1 (length (plist-get vm-eqn 'op1)))
             (n2 (length (plist-get vm-eqn 'op2)))
             (nd (abs (- n1 n2)))
             (sep1 (concat (make-string nd ? ) "\n"))
             (sep2 " ")
             (spc (make-string nd ? ))
             (res (plist-get vm-eqn 'res))
             (line (make-string (+ 2 (max n1 n2)) ?-))
             (extra (when (>= (string-to-number res) 0) " "))
             (post (concat "\n" line "\n " extra)))
        (when (> n1 n2)
          (setq sep2 (concat spc sep2)))
        (when (< n1 n2)
          (setq sep1 (concat spc sep1)))
        (plist-put vm-eqn 'pre pre)
        (plist-put vm-eqn 'sep1 sep1)
        (plist-put vm-eqn 'sep2 sep2)
        (plist-put vm-eqn 'post post)
        (plist-put vm-eqn 'isvert nil))
    (plist-put vm-eqn 'pre " ")
    (plist-put vm-eqn 'sep1 " ")
    (plist-put vm-eqn 'sep2 " ")
    (plist-put vm-eqn 'post " = ")
    (plist-put vm-eqn 'isvert t))
  vm-eqn)

(defun vm/set-buffer-face-eqn-disp-mode-face ()
  "sets vm/eqn-disp-mode-face"
  (let ((buffer-face-mode-face vm/eqn-disp-mode-face))
    (buffer-face-mode)))

(defun vm/check-answer ()
  "check whether input char via keypress and call corresponding response func"
  (interactive)
  (if (vm/check-char-input?)
      (vm/char-correct)
    (vm/char-incorrect)))

(defun vm/check-char-input? ()
  "compare input key with character at point"
  (let ((char-at-point (char-after (point)))
        (input-as-char last-command-event))
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
      (vm/new-equation))))

(defun vm/char-incorrect ()
  "do stuff if input char is incorrect"
  (let ((buffer-read-only nil)
        (p1 (point))
        (p2 (1+ (point))))
    (message "wrong, try again")
    (overlay-put (make-overlay p1 p2) 'display "⚡")
    (overlay-put (make-overlay p1 p2) 'face '(:foreground "red"))))

(defun vm/reveal-correct-digit ()
  "make correct digit visible"
  (interactive)
  (let ((buffer-read-only nil)
        (p1 (point))
        (p2 (1+ (point))))
    (remove-overlays p1 p2)
    (put-text-property p1 p2 'invisible nil)))

(defun vm/swap-operands-and-redisplay ()
  (interactive)
  (vm/swap-operands vm/eqn)
  (vm/display-equation))

;; (defun vm/swap-operands (vm-eq)
;;   "swap operands in commutative operations"
;;   (interactive)
;;   (let ((op (plist-get vm-eqn 'op))
;;         (op1 (plist-get vm-eqn 'op1))
;;         (op2 (plist-get vm-eqn 'op2))
;;         (ishorz (not (plist-get vm-eqn 'isvert)))
;;         toggled)
;;     (when ishorz
;;       (setq toggled t)
;;       (setq vm-eqn (vm/toggle-vert-horz vm-eqn)))
;;     (when (eqal op "-")
;;       (setq op2 (concat "-" op2))
;;       (plist-put vm-eqn 'op "+"))
;;     (plist-put vm-eqn 'op1 op2)
;;     (plist-put vm-eqn 'op2 op1)
;;     (when toggled
;;       (vm/toggle-vert-horz vm-eqn))
;;     vm-eqn))

(provide 'vedic-maths-mode)
