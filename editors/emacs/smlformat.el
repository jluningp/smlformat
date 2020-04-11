(defcustom smlformat-command "/home/jeanne/smlformat/smlformat"
  "smlformat command"
  :type 'string
  :group 'smlformat)

;; Most of the functions below are taken from OCamlFormat's emacs setup
;; https://github.com/ocaml-ppx/ocamlformat
(defun smlformat--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun smlformat--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun smlformat--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in smlformat--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (smlformat--goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (smlformat--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in smlformat--apply-rcs-patch")))))))))

(defun smlformat--patch-buffer (outputfile)
  (let ((patchbuf (get-buffer-create "*SMLFormat patch*")))
    (with-current-buffer patchbuf (erase-buffer))
    (call-process-region
     (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" outputfile)
    (smlformat--apply-rcs-patch patchbuf)
    (kill-buffer patchbuf)))

(defun smlformat ()
  "smlformat"
  (interactive)
  (let*
      ((ext (file-name-extension buffer-file-name t))
       (outputfile (make-temp-file "smlformat" nil ext)))
    (call-process smlformat-command nil nil nil buffer-file-name outputfile)
    (smlformat--patch-buffer outputfile)
    (delete-file outputfile)
    ))

(defun smlformat-on-save ()
  (when (eq major-mode 'sml-mode)
    (let ((b (current-buffer)))
      (with-temp-buffer
        ;; If we don't temporarily remove the smlformat-on-save hook, we'll get stuck
        ;; in an endless loop, since this function saves
        (let ((after-save-hook (remove 'smlformat-on-save after-save-hook)))
          (with-current-buffer b
            (let ((after-save-hook (remove 'smlformat-on-save after-save-hook)))
              (smlformat)
              (save-buffer))))))))

;; Uncomment this to enable smlformat on save in sml-mode
;; (add-hook 'after-save-hook #'smlformat-on-save)
