;;; helm-sly.el --- Helm sources and some utilities for SLY. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;
;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-sly
;; Version: 0.4.0
;; Keywords: convenience, helm, sly, lisp
;; Package-Requires: ((emacs "24") (helm "3.2") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; A Helm for using SLY.
;;
;; The complete command list:
;;
;;  `helm-sly-list-connections'
;;    Yet another Lisp connection list with `helm'.
;;  `helm-sly-apropos'
;;    Yet another `slime-apropos' with `helm'.
;;  `helm-sly-mini'
;;    Like ~helm-sly-list-connections~, but include an extra source of
;;    Lisp-related buffers, like the events buffer or the scratch buffer.

;;; Installation:
;;
;; Add helm-sly.el to your load-path.
;; Set up SLIME or Sly properly.
;;
;; If you use SLIME, call `slime-setup' and include 'helm-sly as the
;; arguments:
;;
;;   (slime-setup '([others contribs ...] helm-sly))
;;
;; or simply require helm-sly in some appropriate manner.
;;
;; To use Helm instead of the Xref buffer, enable `global-helm-sly-mode'.
;;
;; To enable Helm for completion, install `helm-company'
;; (https://github.com/Sodel-the-Vociferous/helm-company).  With SLIME, you'll
;; also need `slime-company' (https://github.com/anwyn/slime-company/).  Then:
;;
;; - SLIME:
;;  (slime-setup '(slime-company))
;;  (require 'helm-company)
;;  (define-key slime-repl-mode-map (kbd "<tab>") 'helm-company)
;;
;; - Sly:
;;  (add-hook 'sly-mrepl-hook #'company-mode)
;;  (require 'helm-company)
;;  (define-key sly-mrepl-mode-map (kbd "<tab>") 'helm-company)

;;; Code:

(require 'helm)
(require 'helm-buffers)
(unless (require 'sly nil 'noerror)
  (require 'slime)
  (require 'slime-c-p-c)
  (require 'slime-fuzzy)
  (require 'slime-repl))
(require 'cl-lib)

(defun helm-sly-sly-p ()
  "Return non-nil if Sly is active."
  (require 'sly nil 'noerror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar helm-sly-connections-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-D") 'helm-sly-run-delete-buffers)
    (define-key map (kbd "M-R") 'helm-sly-run-rename-connection-buffer)
    map)
  "Keymap for Lisp connection source in Helm.")

(defun helm-sly-output-buffer (&optional connection)
  (if (helm-sly-sly-p)
      (sly-mrepl--find-buffer (or connection (sly-current-connection)))
    (let ((slime-dispatching-connection (or connection
                                            slime-dispatching-connection)))
      (slime-output-buffer))))

(defun helm-sly-go-to-repl (_candidate)
  "Switched to marked REPL(s)."
  (helm-window-show-buffers
   (mapcar (lambda (candidate)
             (let ((buffer (nth 1 candidate))
                   (connection (nth 0 candidate)))
               (unless buffer
                 (if (helm-sly-sly-p)
                     (sly-mrepl-new connection)
                   (let ((slime-dispatching-connection connection))
                     (slime-new-mrepl))))
               buffer))
           (helm-marked-candidates))))
(put 'helm-sly-go-to-repl 'helm-only t)

(defun helm-sly-process (connection)
  (if (helm-sly-sly-p)
      (sly-process connection)
    (slime-process connection)))

(defun helm-sly-connection-number (connection)
  (if (helm-sly-sly-p)
      (sly-connection-number connection)
    (slime-connection-number connection)))

(defun helm-sly-connection-name (connection)
  (if (helm-sly-sly-p)
      (sly-connection-name connection)
    (slime-connection-name connection)))

(defun helm-sly-pid (connection)
  (if (helm-sly-sly-p)
      (sly-pid connection)
    (slime-pid connection)))

(defun helm-sly-implementation-type (connection)
  (if (helm-sly-sly-p)
      (sly-lisp-implementation-type connection)
    (slime-lisp-implementation-type connection)))

(defun helm-sly-debug-buffers (connection)
  (if (helm-sly-sly-p)
      (sly-db-buffers connection)
    (sldb-buffers connection)))

(defun helm-sly-buffer-connection (buffer)
  (when (bufferp buffer)
    (with-current-buffer buffer
      (if (helm-sly-sly-p)
          sly-buffer-connection
        slime-buffer-connection))))

(defun helm-sly-go-to-inferior (_candidate)
  "Switched to inferior Lisps associated with the marked connections."
  (helm-window-show-buffers
   (cl-loop for c in (helm-marked-candidates)
            collect (process-buffer (helm-sly-process
                                     (helm-sly-buffer-connection c))))))
(put 'helm-sly-go-to-inferior 'helm-only t)

(defun helm-sly-go-to-debug (_candidate)
  "Switched to debug buffers associated with the marked connections."
  (helm-window-show-buffers
   (cl-loop for c in (helm-marked-candidates)
            append (helm-sly-debug-buffers (car c)))))
(put 'helm-sly-go-to-debug 'helm-only t)

(defun helm-sly-run-delete-buffers ()
  "Run `helm-sly-delete-buffers' action from `helm-sly--c-source-connection'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-sly-delete-buffers)))
(put 'helm-sly-run-delete-buffers 'helm-only t)

(defun helm-sly-set-default-connection (candidate)
  "Set connection to use by default to that of candidate buffer."
  (let ((connection (car candidate)))
    (if (helm-sly-sly-p)
        (sly-select-connection connection)
      (slime-select-connection connection))))
(put 'helm-sly-rename-connection-buffer 'helm-only t)

(defun helm-sly--net-processes ()
  (if (helm-sly-sly-p)
      sly-net-processes
    slime-net-processes))

(defun helm-sly-delete-buffers (&optional _candidate)
  "Kill marked REPL(s) and their inferior Lisps if they are the
last buffer connected to it."
  (let ((connections (helm-sly--net-processes))
        (repl-buffers (helm-sly--repl-buffers)))
    (dolist (c (helm-marked-candidates))
      (let ((last-connection?
             (not (memq (car c)
                        (mapcar #'helm-sly-buffer-connection
                                (delete (cadr c) repl-buffers))))))
        (when last-connection?
          (if (helm-sly-sly-p)
              (let ((sly-dispatching-connection (car c)))
                (sly-quit-lisp t))
            (let ((slime-dispatching-connection (car c)))
              (slime-repl-quit))))
        (kill-buffer (cadr c))))))
(put 'helm-sly-delete-buffers 'helm-only t)

(defun helm-sly-restart-connections (_candidate)
  "Restart marked REPLs' inferior Lisps."
  (dolist (c (helm-marked-candidates))
    (if (helm-sly-sly-p)
        (sly-restart-connection-at-point (helm-sly-buffer-connection c))
      (slime-restart-connection-at-point (helm-sly-buffer-connection c)))))
(put 'helm-sly-restart-connections 'helm-only t)

(defun helm-sly-run-rename-connection-buffer ()
  "Run `helm-sly-rename-connection-buffer' action from `helm-sly--c-source-connection'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-sly-rename-connection-buffer)))
(put 'helm-sly-run-rename-connection 'helm-only t)

(defun helm-sly-rename-connection-buffer (candidate)
  "Rename REPL buffer."
  (let* ((slime-dispatching-connection (car candidate)))
    (when (cadr candidate)
      (with-current-buffer (cadr candidate)
        (rename-buffer (helm-read-string "New name: " (buffer-name)))))))
(put 'helm-sly-rename-connection-buffer 'helm-only t)

(defcustom helm-sly-connection-actions
  `(("Go to REPL" . helm-sly-go-to-repl)
    ("Set default" . helm-sly-set-default-connection)
    ("Restart" . helm-sly-restart-connections)
    (,(substitute-command-keys "Rename REPL buffer \\<helm-sly-connections-map>`\\[helm-sly-run-rename-connection-buffer]'")
     . helm-sly-rename-connection-buffer)
    (,(substitute-command-keys "Quit \\<helm-sly-connections-map>`\\[helm-sly-run-delete-buffers]'")
     . helm-sly-delete-buffers)
    ("Go to inferior Lisp" . helm-sly-go-to-inferior)
    ("Go to debug buffers" . helm-sly-go-to-debug))
  "Actions for `helm-sly-list-connections`."
  :group 'helm-sly
  :type '(alist :key-type string :value-type function))

(defun helm-sly--connection-candidates (p &optional buffer)
  "Return (DISPLAY-VALUE . REAL-VALUE) for connection P.
The REAL-VALUE is (P BUFFER)."
  (setq buffer (or buffer
                   (helm-sly-output-buffer p)))
  (let ((fstring "%s%2s  %-10s  %-17s  %-7s %-s %s"))
    (cons
     (format fstring
             (if (eq (if (helm-sly-sly-p)
                         sly-default-connection
                       slime-default-connection)
                     p)
                 "*"
               " ")
             (helm-sly-connection-number p)
             (helm-sly-connection-name p)
             (or (process-id p) (process-contact p))
             (helm-sly-pid p)
             (helm-sly-implementation-type p)
             buffer)
     (list p buffer))))

(defun helm-sly--repl-buffers (&optional connection thread)
  ;; Inspired by `sly-mrepl--find-buffer'.
  (cl-remove-if-not
   (lambda (x)
     (with-current-buffer x
       (and (memq major-mode '(sly-mrepl-mode
                               slime-repl-mode
                               slime-mrepl-mode))
            (or (not connection)
                (eq (if (helm-sly-sly-p)
                        sly-buffer-connection
                      slime-buffer-connection)
                    connection))
            (or (not thread)
                (eq thread (if (boundp 'sly-current-thread)
                               sly-current-thread
                             slime-current-thread))))))
   (buffer-list)))

(defun helm-sly--repl-buffer-candidates ()
  "Return buffer/connection candidates.
It returns all REPL buffer candidates + connections without buffers."
  (let* ((repl-buffers (helm-sly--repl-buffers))
         (buffer-connections (cl-delete-duplicates
                              (mapcar #'helm-sly-buffer-connection
                                      repl-buffers))))
    (append (mapcar (lambda (b)
                      (helm-sly--connection-candidates
                       (helm-sly-buffer-connection b)
                       b))
                    repl-buffers)
            (mapcar #'helm-sly--connection-candidates
                    (cl-set-difference
                     (reverse (helm-sly--net-processes))
                     buffer-connections)))))

(defun helm-sly--c-source-connection ()
  (helm-build-sync-source "Lisp connections"
    :candidates (helm-sly--repl-buffer-candidates)
    :action helm-sly-connection-actions
    :keymap helm-sly-connections-map))

;;;###autoload
(defun helm-sly-list-connections ()
  "List Lisp connections with Helm."
  (interactive)
  (helm :sources (list (helm-sly--c-source-connection))
        :buffer "*helm-sly-list-connections*"))

(defun helm-sly--buffer-candidates ()
  "Collect Lisp-related buffers, like the `events' buffer.
If the buffer does not exist, we use the associated function to generate it.

The list is in the (DISPLAY . REAL) form.  Because Helm seems to
require that REAL be a string, we need to (funcall (intern
\"function\")) in `helm-sly-switch-buffers' to generate the
buffer."
  (if (helm-sly-sly-p)
      (list (cons (sly-buffer-name :events :connection (sly-current-connection))
                  "sly-pop-to-events-buffer")
            (cons (sly-buffer-name :threads :connection (sly-current-connection))
                  "sly-list-threads")
            (cons (sly-buffer-name :scratch :connection (sly-current-connection))
                  "sly-scratch"))
    (list (cons slime-event-buffer-name "slime-events-buffer")
            (cons slime-threads-buffer-name "slime-list-threads")
            (cons (slime-buffer-name :scratch) "slime-scratch-buffer"))))

(defun helm-sly-switch-buffers (_candidate)
  "Switch to buffer candidates and replace current buffer.

If more than one buffer marked switch to these buffers in separate windows.
If a prefix arg is given split windows vertically."
  (helm-window-show-buffers
   (cl-loop for b in (helm-marked-candidates)
            collect (funcall (intern b)))))

(defun helm-sly-build-buffers-source ()
  (helm-build-sync-source "Lisp buffers"
    :candidates (helm-sly--buffer-candidates)
    :action `(("Switch to buffer(s)" . helm-sly-switch-buffers))))

(defun helm-sly-new-repl (name)
  (if (helm-sly-sly-p)
      ;; TODO: Set new Sly connection?
      (sly-mrepl-new (sly-current-connection) name)
    (cl-flet ((slime-repl-buffer (&optional create _connection)
                                 (funcall (if create #'get-buffer-create #'get-buffer)
                                          (format "*slime-repl %s*" ;; (slime-connection-name connection)
                                                  name))))
      ;; TODO: Set REPL buffer name to *slime-repl NAME*.
      ;; The following does not work.
      ;; (rename-buffer (format "*slime-repl %s*" name))
      (if (fboundp 'slime-new-mrepl)
          (slime-new-mrepl)
        (slime)))))

(defun helm-sly-new-repl-choose-lisp (name)
  "Fetch URL and render the page in a new buffer.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `eww-search-prefix'."
  (let ((current-prefix-arg '-))
    (helm-sly-new-repl name)))

(defvar helm-sly-new
  (helm-build-dummy-source "Open new REPL"
    :action (helm-make-actions
             "Open new REPL" 'helm-sly-new-repl
             "Open new REPL with chosen Lisp" 'helm-sly-new-repl-choose-lisp)))

(defun helm-sly-mini ()
  "Helm for Lisp connections and buffers."
  (interactive)
  (helm :sources (list (helm-sly--c-source-connection)
                       helm-sly-new
                       (helm-sly-build-buffers-source))
        :buffer "*helm-sly-mini*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass helm-sly-apropos-type (helm-source-sync)
  ((action :initform `(("Describe SYMBOL" . ,(if (helm-sly-sly-p)
                                                 'sly-describe-symbol
                                               'slime-describe-symbol))
                       ("Edit definition" . ,(if (helm-sly-sly-p)
                                                 'sly-edit-definition
                                               'slime-edit-definition))))
   (persistent-action :initform (if (helm-sly-sly-p)
                                    #'sly-describe-symbol
                                  #'slime-describe-symbol))
   ;;(volatile :initform t)
   (requires-pattern :initform 2))
  "Lisp apropos.")

(defun helm-sly--format-designator (designator)
  (if (helm-sly-sly-p)
      (let ((package (cadr designator))
            (name (car designator)))
        (format "%s:%s" package name))
      designator))

(defun helm-sly--apropos-source (name external-only case-sensitive current-package)
  "Build source that provides Helm completion against `apropos'."
  (helm-make-source name 'helm-sly-apropos-type
    :candidates `(lambda ()
                   (with-current-buffer helm-current-buffer
                     (cl-loop for plist in (if (helm-sly-sly-p)
                                               (sly-eval (list 'slynk-apropos:apropos-list-for-emacs
                                                               helm-pattern
                                                               ,(not (null external-only))
                                                               ,(not (null case-sensitive))
                                                               (when ,current-package
                                                                 (helm-sly-current-package))))
                                             (slime-eval (list 'swank:apropos-list-for-emacs
                                                               helm-pattern
                                                               ,(not (null external-only))
                                                               ,(not (null case-sensitive))
                                                               (when ,current-package
                                                                 (helm-sly-current-package)))))
                              collect (helm-sly--format-designator (plist-get plist :designator)))))))

(defun helm-sly-current-package ()
  (if (helm-sly-sly-p)
      (or sly-buffer-package
          (sly-current-package))
    (or slime-buffer-package
        (slime-current-package))))

(defvar helm-sly--c-source-apropos-symbol-current-package
  (helm-sly--apropos-source
   "Apropos (current package)"
   nil
   nil
   (helm-sly-current-package)))

(defvar helm-sly--c-source-apropos-symbol-current-external-package
  (helm-sly--apropos-source
   "Apropos (current external package)"
   'external-only
   nil
   (helm-sly-current-package)))

(defvar helm-sly--c-source-apropos-symbol-all-external-package
  (helm-sly--apropos-source
   "Apropos (all external packages)"
   'external-only
   nil
   nil))

(defvar helm-sly--c-source-apropos-symbol-all-package
  (helm-sly--apropos-source
   "Apropos (all packages)"
   nil
   nil
   nil))

(defvar helm-sly-apropos-sources
  '(helm-sly--c-source-apropos-symbol-current-package
    helm-sly--c-source-apropos-symbol-current-external-package
    helm-sly--c-source-apropos-symbol-all-external-package
    helm-sly--c-source-apropos-symbol-all-package)
  "List of Helm sources for `helm-sly-apropos'.")

;;;###autoload
(defun helm-sly-apropos ()
  "Yet another Apropos with `helm'."
  (interactive)
  (helm :sources helm-sly-apropos-sources
        :buffer "*helm lisp apropos*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun helm-sly-normalize-xrefs (xref-alist)
  "Like `slime-insert-xrefs' but return a formatted list of strings instead.
The strings are formatted as \"GROUP: LABEL\"."
  (cl-loop for (group . refs) in xref-alist
           append
           (cl-loop for (label location) in refs
                    collect
                    (list group label location))))

(defun helm-sly-xref-lineno (location)
  "Return 0 if there is location does not refer to a proper file."
  (or
   (ignore-errors
     (save-window-excursion
       (if (helm-sly-sly-p)
           (sly--pop-to-source-location location
                                        'sly-xref)
         (slime-goto-location-buffer (nth 1 location)))
       (line-number-at-pos
        (car (alist-get :position (cdr location))))))
   0))

(defun helm-sly-xref-transformer (candidates)
  "Transform CANDIDATES (a list of (GROUP LABEL LOCATION) as per
`helm-sly-normalize-xrefs') to \"GROUP: LABEL\"."
  (cl-loop for (group label location) in candidates
           collect (cons (concat (propertize (abbreviate-file-name group)
                                             'face 'helm-grep-file)
                                 ":"
                                 (propertize
                                  (number-to-string (helm-sly-xref-lineno location))
                                  'face 'helm-grep-lineno)
                                 ":"
                                 (if (helm-sly-sly-p)
                                     (sly-one-line-ify label)
                                   (slime-one-line-ify label)))
                         (list group label location))))

(defun helm-sly-xref-goto (candidate)
  (let ((location (nth 2 candidate)))
    (switch-to-buffer
     (if (helm-sly-sly-p)
         (save-window-excursion
           (sly--pop-to-source-location location 'sly-xref))
       (save-window-excursion
         (slime-show-source-location location t 1)
         (slime-goto-location-buffer (nth 1 location)))))))

(defun helm-sly-build-xref-source (xrefs)
  (helm-build-sync-source "Lisp xrefs"
    :candidates (helm-sly-normalize-xrefs xrefs)
    :candidate-transformer 'helm-sly-xref-transformer
    :action `(("Switch to buffer(s)" . helm-sly-xref-goto))))

(defun helm-sly-show-xref-buffer (xrefs _type _symbol _package &optional _method)
  "See `sly-xref--show-results'."
  (helm :sources (list (helm-sly-build-xref-source xrefs))
        :buffer "*helm-sly-xref*"))

;;;###autoload
(define-minor-mode helm-sly-mode
  "Use Helm for Lisp xref selections.
Note that the local minor mode has a global effect, thus making
`global-helm-sly-mode' and `helm-sly-mode' equivalent."
  ;; TODO: Is it possible to disable the local minor mode?
  :init-value nil
  (let ((target (if (helm-sly-sly-p)
                    'sly-xref--show-results
                  'slime-show-xref-buffer)))
    (if (advice-member-p 'helm-sly-show-xref-buffer target)
        (advice-remove target 'helm-sly-show-xref-buffer)
      (advice-add target :override 'helm-sly-show-xref-buffer))))

;;;###autoload
(define-globalized-minor-mode global-helm-sly-mode
  helm-sly-mode
  helm-sly-mode)

(provide 'helm-sly)
;;; helm-sly.el ends here
