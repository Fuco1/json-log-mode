;;; json-log-mode.el --- Filter and transform streams of json objects -*- lexical-binding: t -*-

;; Copyright (C) 2018 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 22nd May 2018
;; Package-requires: ((dash "2.10.0"))
;; Keywords: files, convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)

(defvar json-log-mode-current-filter nil
  "Filter active in current buffer.")
(make-variable-buffer-local 'json-log-mode-current-filter)

;; The output of the jq filter is as follows:
;; - number of processed line,
;; - followed either by no output or the line with which we want to
;; replace the existing line.

;; If there are two consecutive numbers it means we need to hide all
;; the text between them as they produced errors or no output
;; (i.e. the input did not match the filter)

;; Example:
;; 1
;; {"foo": "bar"}
;; 12
;; 13
;; 14
;; {"foo": "baz"}
;; 15

;; This means to replace line 1 with {"foo": "bar"}, then delete all
;; the lines from after that line up to line 12, then delete 13,
;; replace 14 with {"foo": "baz"} and finally remove everything after
;; 15th line


(defun json-log-filter (filter &optional beg end dont-revert)
  "Filter the buffer."
  (interactive "sFilter: ")
  (setq beg (or beg (and (use-region-p) (region-beginning)) (point-min)))
  (setq end (or end (and (use-region-p) (region-end)) (point-max)))
  (unless dont-revert (json-log-revert))
  (setq json-log-mode-current-filter filter)
  (when json-log-mode-current-filter
    (let* ((inhibit-read-only t)
           (inhibit-modification-hooks t)
           (output-buffer (get-buffer-create (concat "*my-filter-json-log-" (buffer-name) "*")))
           (current-buffer (current-buffer))
           (filtered (progn
                       (with-current-buffer output-buffer (erase-buffer))
                       (shut-up
                         (shell-command-on-region
                          beg end
                          (concat "jq -R -c 'fromjson? | input_line_number, " filter "'")
                          output-buffer))))
           (current-line 0))
      (save-excursion
        (with-current-buffer (current-buffer)
          (goto-char beg))
        (with-current-buffer output-buffer
          (goto-char (point-min))
          ;; (message "%s" (buffer-substring-no-properties (point-min) (point-max)))
          (cond
           ;; if there is no output, make the whole input area invisible
           ((= 0 (buffer-size))
            (with-current-buffer current-buffer (put-text-property beg end 'invisible 'json-log)))
           (t
            (while (not (eobp))
              (let ((line-to-process (number-at-point)))
                (forward-line 1)
                (if (number-at-point)
                    (with-current-buffer current-buffer
                      (let ((invisible-from (point)))
                        (forward-line (- line-to-process current-line))
                        (put-text-property invisible-from (point) 'invisible 'json-log))
                      (setq current-line line-to-process))
                  ;; if the line here is empty it means we did not read
                  ;; anything and we are at the end of input, so hide
                  ;; whatever is left
                  (if (looking-at "$")
                      (with-current-buffer current-buffer (put-text-property (point) end 'invisible 'json-log))
                    (with-current-buffer current-buffer (forward-line 1))
                    (forward-line 1)
                    (cl-incf current-line))))))))
        (kill-buffer output-buffer)))))

(defun json-log-revert ()
  "Restore all the filtered lines and reset the filter."
  (interactive)
  (setq-local json-log-mode-current-filter nil)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(invisible))))

(defun json-log--update (beg end range)
  "Run on `after-change-functions'."
  ;; range === 0 => insertion
  ;; (message "Updating from %s to %s" beg end)
  (ignore-errors
    (when (= range 0)
      ;; we are assuming only whole lines are added at a time
      (json-log-filter json-log-mode-current-filter beg end 'dont-revert))))

(defvar json-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'json-log-filter)
    (define-key map "g" 'json-log-revert)
    map)
  "Keymap for `json-log-mode'.")

(define-minor-mode json-log-mode
  "Mode for working with buffers of json objects."
  :keymap json-log-mode-map
  (if json-log-mode
      (progn
        (add-hook 'after-change-functions 'json-log--update nil t))
    (remove-hook 'after-change-functions 'json-log--update t)
    (json-log-revert)))

;; (bind-key "C-c C-l" json-log-mode-map)

(provide 'json-log-mode)
;;; json-log-mode.el ends here
