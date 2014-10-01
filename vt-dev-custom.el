;;; vt-dev-custom.el --- vt overrides of functionality in custom.el
;;; Version: 0.0.1
;;; Author: Vince Turner <vt5491@gmail.com>
;;; URL: http://github.com/vt5491/vt-dev-custom
;;; Description: Developmen
;;; Keywords: scratch


;; This file is not part of GNU Emacs. Licensed on the same terms as
;; GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;;
;;; Usage for emacs24 builtin themes:
;;;
;;;
;;; (add-hook 'java-mode-hook (lambda nil
;;;   (load-theme-buffer-local 'misterioso (current-buffer))))

;; helper functions

;; Note: going off frame name is not unique, since the frame title is that active
;; buffer title, and the same buffer can be active in more than one frame.  Consider
;; using 'vt-dev-find-frame-by-window-id' instead
(defun vt-dev-find-frame-by-name (frame-list frame-name)
  "Return the frame whose name is 'frame-name'"
  (let ((frame (car frame-list)))
    (cond ((null frame) nil)
          ((string-equal (cdr (assoc 'name (frame-parameters frame))) frame-name) frame)
          (t (vt-dev-find-frame-by-name (cdr frame-list) frame-name)))))

(defun vt-dev-find-frame-by-window-id (frame-list frame-window-id)
  "Return the frame whose window-id is 'frame-window-id'"
  (let ((frame (car frame-list)))
    (cond ((null frame) nil)
          ((string-equal (cdr (assoc 'window-id (frame-parameters frame))) frame-window-id) frame)
          (t (vt-dev-find-frame-by-window-id (cdr frame-list) frame-window-id)))))

;; find frame by the 'vt-info' parm I manually add to each frame I create.
(defun vt-dev-find-frame-by-vt-info (frame-list vt-info)
  "Return the frame whose vt-info is 'vt-info'"
  (message "vt-dev-find-frame-by-vt-info: frame=%s, vt-info=%s" (car frame-list) vt-info)
  (let ((frame (car frame-list)))
    (cond ((null frame) nil)
          ((string-equal (cdr (assoc 'vt-info (frame-parameters frame))) vt-info) frame)
          ;;((string-equal (cdr (assoc 'name (frame-parameters frame))) frame-name) frame)
          (t (vt-dev-find-frame-by-vt-info (cdr frame-list) vt-info)))))

;; TODO: change reference to 'symbol' to be 'face'
(defun vt-dev-find-theme-symbol (theme symbol)
  ;;(message "vt: top: theme=%s, symbol=%s" theme symbol)
  (defun find-symbol (theme-set symbol)
    (let ((elt (car theme-set)))
      ;;(message "cadr elt=%s,type-of=%s" (cadr elt) (type-of (cadr elt)))
      (cond ((null elt) nil)
            ((eq (cadr elt) symbol) elt)
            (t (find-symbol (cdr theme-set) symbol)))))
  (let ((theme-set (get (intern theme) 'theme-settings)))
    ;;(message "theme=%s,theme-set=%s" theme theme-set)
    (find-symbol theme-set symbol)))

;; end helper functions
(defun vt-dev-load-theme-frame (theme &optional frame no-confirm no-enable vt-reset)
  "Load Custom theme named THEME from its file.
The theme file is named THEME-theme.el, in one of the directories
specified by `custom-theme-load-path'.

If the theme is not considered safe by `custom-safe-themes',
prompt the user for confirmation before loading it.  But if
optional arg NO-CONFIRM is non-nil, load the theme without
prompting.

Normally, this function also enables THEME.  If optional arg
NO-ENABLE is non-nil, load the theme but don't enable it, unless
the theme was already enabled.

This function is normally called through Customize when setting
`custom-enabled-themes'.  If used directly in your init file, it
should be called with a non-nil NO-CONFIRM argument, or after
`custom-safe-themes' has been loaded.

Return t if THEME was successfully loaded, nil otherwise."
  ;;vt add
  (message "vt: entered vt-dev-load-theme under vtstuff/vt-plugins")
  ;;vt end
  (interactive
   (list
    (intern (completing-read "vt-dev-load-theme: Load custom theme: "
			     (mapcar 'symbol-name
				     (custom-available-themes))))
    ;;vtnil nil))
    ;;vt add
            ;;(read-string "on Frame: " (cdr (assoc 'name (frame-parameters (selected-frame)))))))
    ;; we get the name of the second buffer on the selected frame (the first buffer
    ;; will be "*miniprompt*", which is kind of confusing.  The name of the frame
    ;; will be that of the last buffer before we came into the mini-prompt
    ;;(read-string "on Frame: " (buffer-name (cadr (assoc 'buffer-list (frame-parameters (selected-frame))))))))
            ;;(read-string "on Frame: " (selected-frame) )))
    (read-string "on Frame (default <selected-frame>: " )))
    ;;(if (or (not frame) (equal frame "<current-frame>"))
    (if (stringp frame)
        (setq frame (vt-dev-find-frame-by-name (frame-list) frame)))
    (if (not frame)
        (setq frame (selected-frame)))
    (message "vt-dev-load-frame-theme: theme=%s, frame=%s" theme frame)
    ;;vt end
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  ;; If THEME is already enabled, re-enable it after loading, even if
  ;; NO-ENABLE is t.
  (if no-enable
      (setq no-enable (not (custom-theme-enabled-p theme))))
  ;; vt: note: commeting out this when clause seems to fix the problem
  ;;vt: note: this when clause has someting to do with the problem of side-effect
  ;; setting of themes in other frames
  ;; If reloading, clear out the old theme settings.
  ;;
  ;;   (when (custom-theme-p theme)
  ;; (when (and (custom-theme-p theme) (boundp 'vt-reset))
  ;;   (disable-theme theme)
  ;;   (put theme 'theme-settings nil)
  ;;   (put theme 'theme-feature nil)
  ;;   (put theme 'theme-documentation nil))
    (let ((fn (locate-file (concat (symbol-name theme) "-theme.el")
                           (custom-theme--load-path)
                           '("" "c")))
          hash)
      (unless fn
        (error "Unable to find theme file for `%s'" theme))
      (with-temp-buffer
        (insert-file-contents fn)
        (setq hash (secure-hash 'sha256 (current-buffer)))
        ;; Check file safety with `custom-safe-themes', prompting the
        ;; user if necessary.
        (when (or no-confirm
                  (eq custom-safe-themes t)
                  (and (memq 'default custom-safe-themes)
                       (equal (file-name-directory fn)
                              (expand-file-name "themes/" data-directory)))
                  (member hash custom-safe-themes)
                  (custom-theme-load-confirm hash))
          (let ((custom--inhibit-theme-enable t)
                (buffer-file-name fn))  ;For load-history.
            (eval-buffer))
          ;; Optimization: if the theme changes the `default' face, put that
          ;; entry first.  This avoids some `frame-set-background-mode' rigmarole
          ;; by assigning the new background immediately.
          (let* ((settings (get theme 'theme-settings))
                 (tail settings)
                 found)
            (while (and tail (not found))
              (and (eq (nth 0 (car tail)) 'theme-face)
                   (eq (nth 1 (car tail)) 'default)
                   (setq found (car tail)))
              (setq tail (cdr tail)))
            (if found
                (put theme 'theme-settings (cons found (delq found settings)))))
          ;; Finally, enable the theme.
          (unless no-enable
            ;;(vt-dev-enable-theme theme))
            (vt-dev-enable-theme theme frame))
          ;; note: commenting out the prior "(when custom-theme-p)" clause seems to fix
          ;; side-effect problem
          ;;(vt-dev-enable-theme 'light-blue (vt-dev-find-frame-by-window-id (frame-list) "4000266"))
          ;;(vt-dev-enable-theme 'deeper-blue (vt-dev-find-frame-by-window-id (frame-list) "2558984"))
          (vt-dev-custom-get-theme 'tango-dark)
          (vt-dev-enable-theme 'tango-dark (vt-dev-find-frame-by-vt-info (frame-list) "vt-frame-1"))
          (vt-dev-custom-get-theme 'deeper-blue)
          (vt-dev-enable-theme 'deeper-blue (vt-dev-find-frame-by-vt-info (frame-list) "vt-frame-3"))
          t))))

;;vt(defun vt-dev-enable-theme (theme)
(defun vt-dev-enable-theme (theme frame)  
  "Reenable all variable and face settings defined by THEME.
THEME should be either `user', or a theme loaded via `load-theme'.
After this function completes, THEME will have the highest
precedence (after `user')."
  ;;vt add
  (message "vt: entered enable-theme: frame=%s, theme=%s" frame theme)
  ;;vt end
  (interactive (list (intern
		      (completing-read
		       "Enable custom theme: "
		       obarray (lambda (sym) (get sym 'theme-settings)) t))))
  (if (not (custom-theme-p theme))
      (error "Undefined Custom theme %s" theme))
  (let ((settings (get theme 'theme-settings)))
    ;; Loop through theme settings, recalculating vars/faces.
    (dolist (s settings)
      (let* ((prop (car s))
	     (symbol (cadr s))
	     (spec-list (get symbol prop)))
	(put symbol prop (cons (cddr s) (assq-delete-all theme spec-list)))
	(cond
	 ((eq prop 'theme-face)
      ;;vt add progn
      (progn
        (message "vt: enable-theme: about to call custom-theme-recalc-face")
	    ;;(vt-dev-custom-theme-recalc-face symbol)
        ;;vt add when clause
        (when (not (eql symbol 'gnus-group-news-1))
          (vt-dev-custom-theme-recalc-face symbol frame))
        ;;vt
        ;;(message "vt: enable-theme: about to call custom-theme-recalc-face local version")
        ;;(custom-theme-buffer-local-recalc-face symbol vt-cb)
      ))
	 ((eq prop 'theme-value)
	  ;; Ignore `custom-enabled-themes' and `custom-safe-themes'.
	  (unless (memq symbol '(custom-enabled-themes custom-safe-themes))
	    (custom-theme-recalc-variable symbol)))))))
  (unless (eq theme 'user)
    (setq custom-enabled-themes
	  (cons theme (delq theme custom-enabled-themes)))
    ;; Give the `user' theme the highest priority.
    (enable-theme 'user)))

;;vt(defun vt-dev-custom-theme-recalc-face (face)
(defun vt-dev-custom-theme-recalc-face (face frame)  
  "Set FACE according to currently enabled custom themes."
  ;;vt add
  ;;(message "vt: entered non-overridden custom-theme-recalc-face, face=%s" face)
  ;;vt end
  (if (get face 'face-alias)
      (setq face (get face 'face-alias)))
  ;;(message "vt: vt-dev-custom-theme-recalc-face. back from get face")
  ;; Reset the faces for each frame.
  ;;vt(dolist (frame (frame-list))
  ;;vt  (face-spec-recalc face frame)))
  ;;vt add
  ;;(face-spec-recalc face (selected-frame)))
  ;;vt-works(face-spec-recalc face frame))
  ;; loop over all frames and just set on a frame by frame basis
  ;;(dolist (frame (frame-list))
    ;;(if (equal (cdr (assoc 'window-id (frame-parameters (vt-dev-find-frame-by-window-id (frame-list) "4523738")))) "4523738")
    ;; (if (equal (cdr (assoc 'window-id (frame-parameters frame))) "4000266")
    ;;     (progn
    ;;       (let* ((theme-row (vt-dev-find-theme-symbol "light-blue" face))
    ;;              (prop (car theme-row))
    ;;              (symbol (cadr theme-row))
    ;;              (spec-list (get symbol prop)))
    ;;         (put symbol prop (cons (cddr s) (assq-delete-all 'light-blue spec-list)))
    ;;         (message "vt: now setting face=%s,symbol=%s on window-id 4000266" face symbol)
    ;;         ;;(face-spec-recalc (cadr (vt-dev-find-theme-symbol "light-blue" face)) frame))
    ;;         (face-spec-recalc symbol frame)))
    ;;         ;;(face-spec-recalc face frame)))
         (progn
          (message "vt: normal face=%s" face)
          (face-spec-recalc face frame)
        )
    ;;)
    ;;(message "vt: vt-dev-custom-theme-recalc-face. about to call face-spec-recalc")
    ;;(face-spec-recalc face frame)
    )
  
  ;;vt end
;;vt new functions add

;; this is just the portion of 'custom.el->load_theme' that loads and evals
;; the theme, without the subsequent calss that then draw the theme.  In other
;; words, this is a single concern with a much narrower focus than the original.
(defun vt-dev-custom-get-theme (theme)
  "load the theme from disk and return the data structure"
    (let ((fn (locate-file (concat (symbol-name theme) "-theme.el")
                           (custom-theme--load-path)
                           '("" "c")))
          hash)
      (message "vt-dev-custom-get-theme: fn=%s" fn)
      (unless fn
        (error "Unable to find theme file for `%s'" theme))
      (with-temp-buffer
        (insert-file-contents fn)
        (setq hash (secure-hash 'sha256 (current-buffer)))
        ;; Check file safety with `custom-safe-themes', prompting the
        ;; user if necessary.
        ;; (when (or no-confirm
        ;;           (eq custom-safe-themes t)
        ;;           (and (memq 'default custom-safe-themes)
        ;;                (equal (file-name-directory fn)
        ;;                       (expand-file-name "themes/" data-directory)))
        ;;           (member hash custom-safe-themes)
        ;;           (custom-theme-load-confirm hash))
        ;;   (let ((custom--inhibit-theme-enable t)
        ;;         (buffer-file-name fn))  ;For load-history.
        (let ((custom--inhibit-theme-enable t))
          (eval-buffer)))
        (message "vt-dev-custome-get-theme: theme=%s" theme)
        (message "vt-dev-custome-get-theme: custom-theme-p=%s" (custom-theme-p theme))
        ;; note: 'theme-settings is a global variable that is not passed
        ;; to vt-dev-enable-theme directly, but is relied upon it be a global.
        (get theme 'theme-settings)
))

(defvar vt-dev-custom-frame-theme-hash (make-hash-table :test 'equal)
  "This is where we store the theme in effect for any frame that we set.
   The key is the window-id (as a string) and the value is the theme (as a symbol)")

;; a condensed version of all the prior stuff.  We k
;; Note: window-id is essentially the frame-id
(defun vt-dev-load-theme-frame-2
  (theme)
  (interactive
   (list
    (intern
     (completing-read "vt-dev-load-theme: Load custom theme: "
                      (mapcar 'symbol-name
                              (custom-available-themes))))))
  (message "vt-dev-load-theme-frame-2: theme=%s" theme)
  (unless
      (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  ;; setup the frame-theme key hash
  (let
      ((window-id
        (cdr
         (assoc 'window-id
                (frame-parameters
                 (selected-frame))))))
    ;; (when
    ;;     (not
    ;;      (gethash window-id vt-dev-custom-frame-theme-hash))
    ;;     ;; frame is not registered, so add it
    ;;     (puthash window-id theme vt-dev-custom-frame-theme-hash)
    ;;   )
    ;; update the hash with the theme. Note, if the key does not exist then
    ;; this will also add it.
    (puthash window-id theme vt-dev-custom-frame-theme-hash)
    )
  ;; at this point we just have to loop over the hash and update the
  ;; specified frame with its' theme.
  ;;(let ((frame)))
  (dolist
      (window-id
       (vt-dev-custom-get-hash-keys vt-dev-custom-frame-theme-hash))
    (let
        ((frame-theme
          (gethash window-id vt-dev-custom-frame-theme-hash)))
      (message "vt-dev-custom: now setting theme=%s on frame=%s" frame-theme window-id)
      (vt-dev-custom-get-theme frame-theme)
      (vt-dev-enable-theme frame-theme
                           (vt-dev-find-frame-by-window-id
                            (frame-list)
                            window-id)))
    )
  )

;; helper method
(defun vt-dev-custom-get-hash-keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys
  )
)

;;vt new functions end



(provide 'vt-dev-custom)
