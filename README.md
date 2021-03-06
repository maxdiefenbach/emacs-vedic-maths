

# vedic-maths-mode

<div class="verbatim">
;;; verdic-maths-mode.el &#x2014; train your mental math skills

;; Copyright (C) 2018 Maximilian N. Diefenbach

;; Author: Max Diefenbach <maxdiefenbach@protonmail.com>
;; URL: <https://github.com/maxdiefenbach/emacs-vedic-maths>
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
;; **vedic-maths** in (vm/eqn-disp-mode) showing a math equation and
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
;;      ------&#x2013;&#x2014;        or       -&#x2014;
;;      318497937                    -16

</div>
