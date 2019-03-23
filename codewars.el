;; switch to defcustom
(defvar codewars-server-url "http://localhost:3005/codewars/")

(defvar codewars-language-mode-alist '(("python" . python-mode)
                                       ("javascript" . javascript-mode)
                                       ("ruby" . ruby-mode)
                                       ("java" . java-mode)
                                       ("c" . c-mode)
                                       ("haskell" . haskell-mode)
                                       ("clojure" . clojure-mode))) 

(defun codewars ()
  (interactive)
  ;; (create-buffers)
  ;; (codewars-setup-windows)
  (codewars-start)
  )

;; BUFFERS/WINDOWS

(defun create-buffers ()
  (setq result-buffer (get-buffer-create "*results*"))
  (setq instruction-buffer (get-buffer-create "*instructions*"))
  (setq code-buffer (get-buffer-create "*codewars*"))
  (with-current-buffer result-buffer
    (visual-line-mode))
  (with-current-buffer instruction-buffer
    (visual-line-mode)
    (org-mode))
  (with-current-buffer code-buffer
    (visual-line-mode)
    (codewars-mode)))

(defun codewars-setup-windows ()
  (interactive)
  (select-frame-set-input-focus (make-frame))
  (delete-other-windows)
  (switch-to-buffer instruction-buffer)
  (setq left-panel (selected-window))
  (split-window-right-and-focus)
  (switch-to-buffer code-buffer)
  (setq right-panel (selected-window))
  (codewars-clear-buffers))

(defun clear-buffer ()
  "clear whole buffer add contents to the kill ring"
  (interactive)
  (delete-region (point-min)
                 (point-max)))

(defun codewars-clear-buffers ()
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
    (beginning-of-buffer)
    (select-window right-panel)))

;; start


;; JSON 

(defun json-request (endpoint callback &optional data)
  (let ((script (buffer-string))
        (url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data data))
    (url-retrieve (concat codewars-server-url endpoint)
                  callback)))

(defun json-read-data (status)
  (if (url-http-end-of-headers)
      (progn
        (goto-char url-http-end-of-headers)
        (let ((json-object-type 'alist)
              (json-key-type 'symbol)
              (json-array-type 'vector)
              (result (json-read)))
          result))))
;; start


;; JSON 

(defun json-request (endpoint callback &optional data)
  (let ((script (buffer-string))
        (url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data data))
    (url-retrieve (concat codewars-server-url endpoint)
                  callback)))

(defun json-read-data (status)
  (goto-char url-http-end-of-headers)
  (let ((json-object-type 'alist)
        (json-key-type 'symbol)
        (json-array-type 'vector)
        (result (json-read)))
    result))

(defun create-json (symbol value)
  ;; create a json object with only one field:value pair
  (let ((rq `()))
    (add-to-list `rq
                 (cons symbol value))
    (json-encode rq)))

;; requests 

(defun codewars-start ()
  (interactive)
  (testium-start)
  (json-request "/status" 'handle-status-request))

(defun codewars-load-kata ()
  (interactive)
  (codewars-clear-buffers)
  (json-request "load" 'handle-kata-data))

(defun codewars-next-kata ()
  (interactive)
  (codewars-clear-buffers)
  (json-request "next" 'handle-kata-data))

(defun codewars-test ()
  (interactive)
  (with-current-buffer code-buffer
    (json-request "test"
                  'handle-test-data
                  (create-json 'code
                               (buffer-string)))))

(defun codewars-submit-final ()
  (interactive)
  (json-request "submit-final"
                'handle-test-data
                (create-json 'code
                             (buffer-string))))

(defun codewars-change-language ()
  (interactive)
  (let ((new-language (completing-read "switch to: "
                                       (append kata-language-list nil))))
    (json-request "change-language"
                  'handle-kata-data
                  (create-json 'language language))))

(defun handle-status-request (status)
  (switch-to-buffer (current-buffer))
  (codewars-load-kata)

  )

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
          ;; (message (concat status

          ;; (cdr (assoc 'title kata-info-data))))
          (codewars-display-kata-info)))
    (debug)))

(defun handle-test-data (status)
  (let ((result (json-read-data status)))
    (setq str (cdr (assoc 'results result)))
    (display-in-buffer str result-buffer)))

(defun set-major-mode (data)
  ;; set major mode to match the language in codewars-language-mode-alist
  (setq language (cdr (assoc 'language data)))
  (setq mode-function (cdr (assoc language codewars-language-mode-alist)))
  (with-current-buffer code-buffer
    (condition-case nil
        (funcall mode-function)
      (error))))

(defun codewars-display-kata-info ()
  (interactive)
  ;; (debug)
  (princ (stringify-list kata-info-data)))

(defun stringify-list (list)
  "Print each element of LIST on a line of its own."
  (setq m "")
  (dolist (c list)
    (setq m (concat m
                    (symbol-name (car c))
                    ": "
                    (cdr c)
                    "
")))
  m)


(define-minor-mode codewars-mode
  nil
  " CW"
  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            ;; (define-key map (kbd "C-c C-x l") 'codewars-load-kata)
            ;; (define-key map (kbd "C-c C-x t") 'codewars-test)
            ;; (define-key map (kbd "C-c C-x n") 'codewars-next-kata)
            ;; (define-key map (kbd "C-c C-x i") 'codewars-display-kata-info)
            ;; ;; (define-key map (kbd "C-c C-x n") 'codewars-change-language)
            ;; (define-key map (kbd "C-c C-x s") 'codewars-submit-final)
            map))
