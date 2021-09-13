;;; benchmark.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.



;;; Emacs symbols
(defun ff-print (map)
  (maphash (lambda (k v) (print (format "%s: %s" k v))) map))

(defvar ff-srcs nil
  "Cash list of source files.")
(defvar ff-lmap nil
  "Load paths")
(defvar ff-pmap nil
  "Provided features")
(defvar ff-vmap nil
  "Variables")
(defvar ff-fmap nil
  "Functions and macros")

(defun ff-save-db (db-file)
  (with-temp-file db-file
    (prin1 ff-lmap (current-buffer))
    (prin1 ff-pmap (current-buffer))
    (prin1 ff-vmap (current-buffer))
    (prin1 ff-fmap (current-buffer))))

(defun ff-read-db (db-file)
  (with-temp-buffer
    (insert-file-contents db-file)
    (goto-char (point-min))
    (setq ff-lmap (read (current-buffer))
          ff-pmap (read (current-buffer))
          ff-vmap (read (current-buffer))
          ff-fmap (read (current-buffer)))))

(defun ff-collect-features (src index)
  (let (sxp)
    (with-current-buffer (get-buffer-create "*ff-buffer*")
      (erase-buffer)
      (insert-file-contents src)
      (goto-char (point-min))
      (while (setq sxp (ignore-errors (read (current-buffer))))
        (when (listp sxp)
          (cond ((or (equal (car sxp) 'defun)
                     (equal (car sxp) 'defmacro))
                 (puthash (cadr sxp) index ff-fmap))
                ((or (equal (car sxp) 'defvar)
                     (equal (car sxp) 'defcustom))
                 (puthash (cadr sxp) index ff-vmap))
                ((equal (car sxp) 'provide)
                 (puthash (cadr sxp) index ff-pmap))))))))

(defun ff-build-db (dir-tree)
  ;;(when (or (not ff-srcs) (equal (car ff-srcs) dir-tree))
      (setq ff-srcs (cons dir-tree (directory-files-recursively dir-tree "\\.el$"))
            ff-lmap (make-hash-table :test 'equal)
            ff-vmap (make-hash-table :test 'equal)
            ff-fmap (make-hash-table :test 'equal)
            ff-pmap (make-hash-table :test 'equal))
      ;; )
  (let ((index 0))
    (dolist (src (cdr ff-srcs))
      (puthash index src ff-lmap)
      (ff-collect-features src index)
      (setq index (1+ index)))))

(defun ff-build-emacs-db ()
  (ff-build-db (expand-file-name "lisp/" source-directory))
  (ff-save-db (expand-file-name "ff-db-emacs" user-emacs-directory)))

(defun ff-build-package-db ()
  (ff-build-db (expand-file-name "elpa/" user-emacs-directory))
  (ff-save-db (expand-file-name "ff-db-packages" user-emacs-directory)))


;;; Plato dialogues
(defun find-dialogues()
  (let (beg end dialogues)
    (with-temp-buffer
      (insert-file-contents-literally "Plato.org")
      (goto-char (point-min))
      (while (re-search-forward "^[ t]*\\*\\* Dialogue" nil t)
        (setq beg (point))
        (re-search-forward "-THE END-" nil t)
        (goto-char (line-beginning-position))
        (setq end (point))
        (push (cons beg end) dialogues)))
    (nreverse dialogues)))

(defun count-words-in-dialogues ()
  (let ((words 0))
    (with-temp-buffer
      (insert-file-contents-literally "Plato.org")
      (save-excursion
        (dolist (dlg (find-dialogues))
          (goto-char (car dlg))
          (while (re-search-forward "\\sw+" (cdr dlg) t)
            (setq words (1+ words))))
        words))))

(defun count-names-freq ()
  (let ((names (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents-literally "Plato.org")
      (save-excursion
        (dolist (dlg (find-dialogues))
          (goto-char (car dlg))
          (while (re-search-forward "^[A-Za-z]*:" (cdr dlg) t)
            (let ((count (or (cdr (gethash (match-string 0) names)) 0)))
              (puthash (match-string 0)
                       (cons (match-string-no-properties 0) (1+ count))
                       names)))))
      names)))

(defun count-socrates ()
  (let ((words 0))
    (with-temp-buffer
      (insert-file-contents-literally "Plato.org")
      (dolist (dlg (find-dialogues))
        (goto-char (car dlg))
        (while (re-search-forward "[Ss]ocrates" (cdr dlg) t)
          (setq words (1+ words))))
      words)))

(defun reverse-republic ()
  "Test compare strings, copy between buffers."
  (let (beg word (freq (make-hash-table :test 'equal)) (socrates 0))
    (with-temp-buffer
      (insert-file-contents-literally "Plato.org")
      (when (re-search-forward "* Republic" nil t)
        (when (re-search-forward "^[ t]*\\*\\* Dialogue" nil t)
          (setq beg (point)))
        (while (> (point) beg)
          (forward-word -1)
          (setq word (current-word))
          (with-current-buffer (get-buffer-create "  *The Republic*")
            (insert (concat word " ")))
          (when (equal word "Socrates")
            (setq socrates (1+ socrates)))
          (if (gethash word freq)
              (puthash word (1+ (gethash word freq)) freq)
            (puthash word 1 freq)))
        (when (get-buffer "  *The Republic*")
          (kill-buffer (get-buffer "  *The Republic*")))))))

(defun get-republic ()
  (let (beg end (text
                 (with-temp-buffer
                   (insert-file-contents-literally "Plato.org")
                   (buffer-string))))
    (when (setq beg (string-match "^\\*[ \t]*Republic" text))
      (setq beg (string-match "Dialogue" text beg)
            end (string-match "-THE END-" text beg))
      (substring text beg end))))

(defun replace-republic ()
  "Test to search/replace in strings"
  (let ((text (get-republic))
        (rpls '(("Republic" . "Imperium")
                ("Plato" . "Emperor")
                ("Socrates" . "Yoda")
                ("SOCRATES" . "YODA")
                ("Aristotle" . "Obi-Kenobi")
                ("Alcibiades" . "C-3PO")
                ("ALCIBIADES" . "C-3PO")
                ("GLAUCON" . "VADER")
                ("Glaucon" . "Vader")
                ("THRASYMACHUS" . "R2-D2")
                ("Thrasymachus" . "R2-D2")
                ("CEPHALUS" . "OBI-KENOBY")
                ("Cephalus" . "Obi-Kenobi")
                ("ADEIMANTUS" . "PALPATINE")
                ("Adeimantus" . "Palpatine")
                ("POLEMARCHUS" . "SKYWALKER")
                ("Polemarchus" . "Skywalker")
                ("CLEITOPHON" . "PADAWAN")
                ("Cleitophon" . "Padawan")
                ("Athens" . "Death Star")
                ("DIALOGUE" . "UPRISING")
                ("Dialogue" . "Uprising")
                ("dialogue" . "uprising")
                ("Philosophy" . "Force")
                ("philosophy" . "force")
                ("" . ""))))
    (dolist (rpl rpls)
      (setq text (string-replace (car rpl) (cdr rpl) text)))
    (with-current-buffer (get-buffer-create "New Republic")
      (erase-buffer)
      (insert text)
      ;;(switch-to-buffer (current-buffer))
      (kill-buffer)
      )))


;;; Image benchmark
(require 'svg)

(defun svg-position (image)
  "Return buffer position of the svg image."
  (let ((marker (cdr (assoc :image (car-safe (cdr image))))))
    (when (markerp marker)
      (marker-position marker))))

(defun svg-image-rotate (svg &optional angle)
  (let ((image (image--get-image (svg-position svg))))
    (setf (image-property image :rotation)
          (float (mod (+ (or (image-property image :rotation) 0)
                         (or angle 90))
                      360)))))

(defun svg-increase-size ()
  (with-temp-buffer
    (let ((svg (svg-create 10 10))
          (max-image-size t))
      (svg-rectangle svg 0 0 10 10)
      (svg-insert-image svg)
      (dotimes (_ 40)
        (image--change-size
         (1+ (/ (prefix-numeric-value 2) 10.0))
         (svg-position svg))))))

(defun svg-decrease-size ()
  (with-temp-buffer
    (let ((svg (svg-create 10 10)))
      (svg-rectangle svg 0 0 10 10)
      (svg-insert-image svg)
      (dotimes (_ 20)
        (image--change-size
         (- 1 (/ (prefix-numeric-value 0.1) 10.0))
         (svg-position svg))))))


;;; benchmarks
(eval-and-compile
  (defun with-gc-on-body (&rest benchmark)
    ;;(garbage-collect)
    (let (result name)
      (setq name (car (last (car benchmark))))
      (setq result (eval (car benchmark)))
      (cons (symbol-name (car name)) result)))

  (defun with-gc-off-body (&rest benchmark)
    (garbage-collect)
    (let ((gc-cons-threshold most-positive-fixnum) result name)
      (setq name (car (last (car benchmark))))
      (setq result (eval (car benchmark)))
      (garbage-collect)
      (cons (symbol-name (car name)) result))))

(defmacro with-gc-on (&rest benchmarks)
  (let (results result)
    (push "with-gc-on" results)
    (dolist (benchmark benchmarks)
      (setq result (with-gc-on-body `,@benchmark))
      (push result results))
    (setq results (nreverse results))
    `(list ,@(mapcan (lambda (res) (list `',res)) results))))

(defmacro with-gc-off (&rest benchmarks)
  (let (results result)
    (push "with-gc-off" results)
    (dolist (benchmark benchmarks)
      (setq result (with-gc-off-body `,@benchmark))
      (push result results))
    (setq results (nreverse results))
    `(list ,@(mapcan (lambda (res) (list `',res)) results))))

(defun round-float-to-decimals (float decimals)
  (let ((mult (expt 10 decimals)))
    (/ (fround (* mult float)) mult)))

(defun report-benchmark (report tofile)
  (with-temp-buffer
    (org-mode)
    (insert (concat "| " (car report)))
    (dolist (elt (cdr report))
      (insert (format " |%s " (car elt))))
    (insert "\n| ")
    (dolist (elt (cdr report))
      (setf (cadr elt) (round-float-to-decimals (cadr elt) 4))
      (insert (format " |%s " (cdr elt))))
    (insert "|\n")
    (goto-char (point-min))
    (while (re-search-forward "(\\|)" nil t)
      (replace-match ""))
    (append-to-file (point-min) (point-max) tofile)))

' ;; obs not a typo :-)
(progn
  (let (report)
    (setq report (with-gc-on
                  (benchmark-run 10 (replace-republic))
                  (benchmark-run 10 (reverse-republic))
                  (benchmark-run 10 (count-words-in-dialogues))
                  (benchmark-run 10 (count-names-freq))
                  (benchmark-run 10 (count-socrates))
                  ;; (benchmark-run (svg-increase-size))
                  ;; (benchmark-run (svg-decrease-size))
                  (benchmark-run (ff-build-emacs-db))
                  (benchmark-run (ff-build-package-db))))
    (report-benchmark report "report.org"))

  (let ((report 
         (with-gc-off
          (benchmark-run 10 (replace-republic))
          (benchmark-run 10 (reverse-republic))
          (benchmark-run 10 (count-words-in-dialogues))
          (benchmark-run 10 (count-names-freq))
          (benchmark-run 10 (count-socrates))
          ;; (benchmark-run (svg-increase-size))
          ;; (benchmark-run (svg-decrease-size))
          (benchmark-run (ff-build-emacs-db))
          (benchmark-run (ff-build-package-db)))))
    (report-benchmark report "report.org")))

(provide 'benchmark)
;;; benchmark.el ends here
