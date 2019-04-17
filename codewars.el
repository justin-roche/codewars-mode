;;; codewars.el --- A emacs interface to codewars

;; Author: J Roche <jproche5@gmail.com>
;; URL: https://github.com/justin-roche/codewars-mode
;; Version: 0.0.1
;; Package-Requires: ((dizzee ))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this file, put something like the following in your
;; ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/codewars/")
;; (require 'codewars)
;;
;; Type M-x codewars to start.
;;
;; To set options for Codewars, type M-x customize, then select
;; Applications, Codewars.

;;; Code:
(require 'auth-source)
(require 'dizzee)

(defvar instruction-buffer nil)
(defvar code-buffer nil)
(defvar result-buffer nil)
(defvar right-panel nil)
(defvar left-panel nil)
(defvar kata-language-list nil)
(defvar kata-info-data nil)
(defvar codewars-language-mode-alist '(("python" . python-mode)
                                       ("javascript" . javascript-mode)
                                       ("ruby" . ruby-mode)
                                       ("java" . java-mode)
                                       ("c" . c-mode)
                                       ("haskell" . haskell-mode)
                                       ("clojure" . clojure-mode)))
(defvar codewars-server-url "http://localhost:3005/codewars/")
(defvar codewars-credentials nil)

(dz-defservice codewars-puppeteer-service
               "npm"
               :args ("start"):cd
               "~/ref/codewars-puppeteer")

;; AUTHORIZATION
(defun get-auth ()
  (let ((found (nth 0
                    (auth-source-search :max 1
                                        :host "www.codewars.com"))))
    (setq codewars-credentials (list (plist-get found :user)
                                     (funcall (plist-get found :secret))))))


(setq auth-source-debug 'trivia)
(get-auth)

(defun codewars-quit ()
  (interactive)
  (kill-buffer instruction-buffer)
  (kill-buffer code-buffer)
  (kill-buffer result-buffer)
  (kill-buffer "*codewars-puppeteer-service*"))

(defun codewars ()
  "Start codewars server, create buffers, and setup windows."
  (interactive)
  (codewars-puppeteer-service-start)
  (create-buffers)
  (setup-windows)
  (clear-buffers))

;; BUFFERS/WINDOWS

(defun create-buffers ()
  (setq instruction-buffer (get-buffer-create "*instructions*"))
  (setq code-buffer (get-buffer-create "*codewars*"))
  (setq result-buffer (get-buffer-create "*results*"))
  (setq server-buffer (get-buffer-create "*codewars-puppeteer-service*"))
  (with-current-buffer result-buffer
    (visual-line-mode))
  (with-current-buffer instruction-buffer
    (visual-line-mode)
    (org-mode))
  (with-current-buffer code-buffer
    (visual-line-mode)
    (codewars-mode)))

(defun setup-windows ()
  (select-frame-set-input-focus (make-frame))
  (delete-other-windows)
  (switch-to-buffer instruction-buffer)
  (setq left-panel (selected-window))
  (split-window-right-and-focus)
  (switch-to-buffer code-buffer)
  (setq right-panel (selected-window)))

;; (defun codewars-start ()
;; (codewars-puppeteer-service-start)
;; (with-current-buffer instruction-buffer
;; (clear-buffer))

;; (codewars-login)
;; (json-request "/status" 'handle-status-request)
;; )

(defun clear-buffer ()
  "Clear whole buffer add contents to the kill ring."
  (interactive)
  (delete-region (point-min)
                 (point-max)))

(defun clear-buffers ()
  (with-current-buffer instruction-buffer
    (clear-buffer))
  (with-current-buffer code-buffer
    (clear-buffer)))

(defun display-in-buffer (data buffer)
  (if (or (eq buffer result-buffer)
          (eq buffer instruction-buffer))
      (progn
        (select-window left-panel)
        (switch-to-buffer buffer)))
  (with-current-buffer buffer
    (clear-buffer)
    (insert data)
    (goto-char (point-min))
    (select-window right-panel)))

;; JSON

(defun json-read-data (status)
  (goto-char url-http-end-of-headers)
  (let ((json-object-type 'alist)
        (json-key-type 'symbol)
        (json-array-type 'vector)
        (result (json-read)))
    result))

(defun json-request (endpoint callback &optional data)
  (let ((script (buffer-string))
        (url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data data))
    (url-retrieve (concat codewars-server-url endpoint)
                  callback)))

(defun create-json (symbol value)
  "Create a json object with only one field:value pair, accepting SYMBOL and VALUE."
  (let ((rq `()))
    (add-to-list `rq
                 (cons symbol value))
    (json-encode rq)))

;; Requests

(defun codewars-login ()
  "Login and resume last kata."
  (interactive)
  (clear-buffers)
  (debug)
  (let ((password (nth 1 codewars-credentials))
        (user (nth 0 codewars-credentials)))
    (json-request "login"
                  'handle-kata-data
                  (json-encode `(("user" . ,user)
                                 ("password" . ,password))))))

(defun codewars-load-kata ()
  "Load the last kata."
  (interactive)
  (clear-buffers)
  (json-request "load" 'handle-kata-data))

(defun codewars-next-kata ()
  "Navigate to the next kata (skip current)."
  (interactive)
  (clear-buffers)
  (json-request "next" 'handle-kata-data))

(defun codewars-test ()
  "Run the example test suite for the current kata and user-entered code."
  (interactive)
  (with-current-buffer code-buffer
    (json-request "test"
                  'handle-test-data
                  (create-json 'code
                               (buffer-string)))))

(defun codewars-submit-final ()
  "Run the final test suite for the current kata and user-entered code."
  (interactive)
  (json-request "submit-final"
                'handle-test-data
                (create-json 'code
                             (buffer-string))))

(defun codewars-change-language ()
  "Change the language for the current kata."
  (interactive)
  (let ((new-language (completing-read "switch to: "
                                       (append kata-language-list nil))))
    (json-request "change-language"
                  'handle-kata-data
                  (create-json 'language new-language))))

;; HELPER FUNCTIONS

(defun handle-status-request (status)
  (switch-to-buffer (current-buffer))
  (codewars-load-kata))

(defun handle-kata-data (status)
  (if (eq status nil)
      (progn
        (let ((data (json-read-data status)))
          (display-in-buffer (cdr (assoc 'instructions data))
                             instruction-buffer)
          (display-in-buffer (cdr (assoc 'template data))
                             code-buffer)
          (set-major-mode (cdr (assoc 'info data)))
          (setq kata-info-data (cdr (assoc 'info data)))
          (setq kata-language-list (cdr (assoc 'languages data)))
          (codewars-display-kata-info)))
    (debug)))

(defun handle-test-data (status)
  (let* ((result (json-read-data status))
         (str (cdr (assoc 'results result))))
    (display-in-buffer str result-buffer)))

(defun set-major-mode (data)
  "Set major mode to match the language in codewars-language-mode-alist based using input DATA."
  (let* ((language (cdr (assoc 'language data)))
         (mode-function (cdr (assoc language codewars-language-mode-alist))))
    (with-current-buffer code-buffer
      (condition-case nil
          (funcall mode-function)
        (error)))))

(defun codewars-display-kata-info ()
  "Show the info for the current kata."
  (interactive)
  (princ (stringify-list kata-info-data)))

(defun stringify-list (l)
  "Print each element of L on a line of its own."
  ;; (debug)
  (let ((m ""))
    (dolist (c l)
      (setq m (concat m
                      (symbol-name (car c))
                      ": "
                      (cdr c)
                      "\n")))
    m))

(define-minor-mode codewars-mode
  nil
  " CW"
  :keymap (let ((map (make-sparse-keymap)))
            ;; (define-key map (kbd "C-c C-x l") 'codewars-load-kata)
            map))

(provide 'codewars)

;;; codewars.el ends here
