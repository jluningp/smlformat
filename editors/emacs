(defcustom smlformat-command "/home/jeanne/smlformat/smlformat"
  "smlformat command"
  :type 'string
  :group 'smlformat)

(defun buffer-whole-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun smlformat ()
  "smlformat"
  (interactive)
  (let*
      ((ext (file-name-extension buffer-file-name t))
       (outputfile (make-temp-file "smlformat" nil ext)))
    (call-process smlformat-command nil outputfile nil buffer-file-name)
    (erase-buffer)
    (insert (buffer-whole-string outputfile))
    (kill-buffer outputfile)
    (delete-file outputfile)
    ))
