(defcustom yeet-command "/home/jeanne/smlformat/smlformat"
  "rg command"
  :type 'string
  :group 'yeet)

(defun buffer-whole-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

;(defun replace-buffer-contents (output)
;  (let ((old-point (point)))
;    (erase-buffer)
;    (insert (buffer-whole-string output))))

(defun my-replace-buffer-contents (outputfile)
  (replace-buffer-contents (find-file-noselect outputfile))
  (kill-buffer (get-file-buffer outputfile)))

(defun yeet ()
  "yeet"
  (interactive)
  (let*
      ((ext (file-name-extension buffer-file-name t))
       (outputfile (make-temp-file "smlformat" nil ext)))
    (call-process yeet-command nil outputfile nil "foo.sml")
    (my-replace-buffer-contents outputfile)))
