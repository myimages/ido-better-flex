;;; ido-better-flex.el --- A better flex (fuzzy) algorithm for Ido.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Copyright 2012 Victor Hugo Borja.
;; Author: Victor Hugo Borja <vic.borja@gmail.com>
;; URL: http://github.com/vic/ido-better-flex
;; Version: 0.0.2
;; Keywords: ido, flex, fuzzy, match, algorithm

;; Commentary:
;;
;; This package implements just another algorithm for fuzzy matching.
;;
;; To use it with as IDO's default matching algorithm, add the following
;; to your emacs initialization file:
;;
;;     (require 'ido-better-flex)
;;     (ido-better-flex/enable)
;;
;;
;; `ido-better-flex' matches the list of candidates against a single
;; abbreviation by using the function `ido-better-flex/match'.
;; basically for each candidate the algorithm calculates an score based
;; on the characters that make the abbreviation and their position in
;; the candidate string. Unlike default flex match, the present one
;; allows you to match characters even if they are not forward the
;; already matched portion. That is, if a char if not found by forward-search
;; we try to match it backwards. So for example: the 'instpkg'
;; abbreviation will match both: 'package-install' and 'install-package'
;; but the second will get a higher score as all
;; matched characters were forward-matched and we did not backtrack.
;;
;; The matching algorithm implemented in this file is not limited to
;; ido, you could easily use it to do fuzzy matching in other packages,
;; the main entry point for that purpose is the `ido-better-flex/score'
;; function.
;;



(require 'cl)
(require 'module-test)

(defconst ido-better-flex/NO-MATCH 0.0
  "The score indicating a negative match")
(defconst ido-better-flex/MATCH 1.0
  "The score indicating a full-match.")

;;;###autoload
(defun ido-better-flex/enable nil
  (interactive)
  "Enable the IDO matching with `ido-better-flex'."
  (ad-enable-advice 'ido-set-matches-1 'around 'ido-better-flex-match)
  (ad-activate 'ido-set-matches-1))

;;;###autoload
(defun ido-better-flex/disable nil
  (interactive)
  "Disable the IDO matching with `ido-better-flex'."
  (ad-disable-advice 'ido-set-matches-1 'around 'ido-better-flex-match)
  (ad-activate 'ido-set-matches-1))

;;;###autoload
(defun ido-better-flex/score (string abbreviation)
  "Computes the score of matching string with abbreviation.
   The return value is in the range 0.0 to 1.0 the later being full-match."

          (calc-score abbreviation string))

;;;###autoload
(defun ido-better-flex/match (items)
  "Returns an ordered list (higher score first) of items that match the
   current `ido-text'. Items are included only if their score is greater than zero."
    (mapcar 'car (ido-better-flex/matches ido-text items)))

(defun ido-better-flex/matches (abbrev items)
  (let (score matches)
    (mapc (lambda (item)
              (let ((name (ido-name item)) score)
                (if (> (setq score (ido-better-flex/score name abbrev)) 0)
                    (setq matches (cons (cons item score) matches))))) items)
    (sort matches (lambda (x y) (> (cdr x) (cdr y))))))



;;;###autoload
(defadvice ido-set-matches-1 (around ido-better-flex-match)
  "An advice using `ido-better-flex' for IDO matching."
  (setq ad-return-value (ido-better-flex/match (ad-get-arg 0))))

;;;###autoload
(progn (ido-better-flex/enable))

(provide 'ido-better-flex)

;;; ido-better-flex.el ends here
