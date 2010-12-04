;;; dired-synchronize.el --- Synchronize directories within dired

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, dired, rsync
;; Created: 2010-12-02
;; Last changed: 2010-12-04 01:18:01
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(defcustom dired-synchronize-bin "rsync"
  "Path to synchonization tool."
  :type 'string
  :group 'dired-synchonize)

(defcustom dired-synchronize-args '("--delete" "-a" "-i")
  "Path to synchonization tool."
  :type 'list
  :group 'dired-synchonize)

(defcustom dired-synchronize-time 10
  "Timeout when performing ssh login tests."
  :type 'integer
  :group 'dired-synchonize)


(defun dired-synchronize-get-user (host &optional target)
  "Return username on HOST when connecting using ssh.

If TARGET is provided, try to connect to TARGET using HOST as a
proxy.

If an error occurs, returns nil."
  (let ((err (get-buffer-create "*err*"))
	(out (get-buffer-create "*out*"))
	(default-directory host)
	(cmd 
	 (if target
	     (format 
	      (concat
	       "ssh -q -o StrictHostKeyChecking=no "
	       "-o PasswordAuthentication=no "
	       "-o UserKnownHostsFile=/dev/null %s whoami") target)
	   "whoami"))
	in-s out-s)
    (with-timeout 
	(10 (message 
	     (format
	      "dired-synchronize-get-user timeout on %s : %s" host cmd)))
      (shell-command cmd out err))
    (set-buffer out)
    ;; Just keep the last line in case of error such as
    ;; cd: 1149: can't cd to /path/to
    (point-max)
    (setq out-s (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
    (kill-buffer out)
    (set-buffer err)
    (setq err-s (buffer-string))
    (kill-buffer err)
    (when (string= "" out-s) (setq out-s nil))
    out-s))



(defun dired-synchronize-parse-uri (file)
  "Parse FILE.

Returned value is a PLIST with following properties.

:file

    The original FILE name.

:host

    The remote hostname if applicable.

:user

    The remove user (login) name if FILE is remote.

:path

    The full pathname.

:tunnel-port

    Random port used to for tunnel setup."
  (let* ((file (expand-file-name file))
	 (file-vec (or (ignore-errors (tramp-dissect-file-name file))
		       (tramp-dissect-file-name (concat "/:" file) 1)))
	 (host (tramp-file-name-host file-vec))
	 (user (tramp-file-name-user file-vec))
	 (path (tramp-file-name-localname file-vec))
	 (method (tramp-file-name-method file-vec))
	 tunnel-port)
    (when (and host (not user))
      (setq user (dired-synchronize-get-user file)
	    tunnel-port (+ 1024 (random (- 32767 1024)))))
    (list :file file :user user :method method :host host :path path
	  :tunnel-port tunnel-port)))



(defun dired-synchronize-read-src-dst (&optional source destination)
  ""
  (let* ((src (dired-synchronize-parse-uri
	       (or source
		   (if (eq major-mode 'dired-mode) default-directory nil)
		   (read-file-name "Synchonize source: " nil nil t nil))))
	 (dst (dired-synchronize-parse-uri
	       (or destination
		   (read-file-name
		    (format "Synchonize %s to: " (plist-get src :file)))
		nil nil t nil 'file-directory-p)))
	 direct)
    ;; remove tailing / for source file.
    ;; Prevent from copying all source files into destination without
    ;; creating a new directory
    (unless (string= "/" (plist-get src :path))
      (setq src (plist-put src :file
			   (replace-regexp-in-string "/*$" ""
						     (plist-get src :file))))
      (setq src (plist-put src :path
			   (replace-regexp-in-string "/*$" ""
						     (plist-get src :path)))))
    ;; try to get e direct link between the hosts
    (when (and
	   (plist-get src :host)
	   (plist-get dst :host))
      (setq direct 
	    (dired-synchronize-get-user
	     (format "/%s:" (plist-get src :host))
	     (format "%s@%s" (plist-get dst :user) (plist-get dst :host)))))
    (setq src (plist-put src :direct direct))
    (setq dst (plist-put dst :direct direct))
    (list :src src :dst dst)))


(defun dired-synchronize (&optional source destination)
  "Synchronize 2 directories using `dired-synchonize-bin'."
  (interactive)
  (let* ((files (dired-synchronize-read-src-dst source destination))
	 (src (plist-get files :src))
	 (dst (plist-get files :dst))
	 cmd1 cmd2)
    (cond
     ;; both files are remote and src cannot connect to dst
     ((and
       (plist-get src :host)
       (plist-get dst :host)
       (not (plist-get src :direct)))
      (setq cmd1 `("ssh" "-L" 
		   ,(format "%d:127.0.0.1:22" (plist-get dst :tunnel-port))
		   ,(plist-get dst :host)))
      (setq cmd2 `("ssh" "-A"  "-R"
		   ,(format "%d:127.0.0.1:%d" (plist-get src :tunnel-port) 
			    (plist-get dst :tunnel-port))
		   ,(plist-get src :host)
		   ,(concat dired-synchronize-bin " "
			    (mapconcat 'concat dired-synchronize-args " ")
			    (format " -e 'ssh -A -p %d " 
				    (plist-get src :tunnel-port))
			    "-o StrictHostKeyChecking=no "
			    "-o UserKnownHostsFile=/dev/null' "
			    (format "%s %s@localhost:%s" 
				    (plist-get src :path)
				    (plist-get dst :user) 
				    (plist-get dst :path))))))

     ;; both files are remote and src cannot connect to dst
     ((and
       (plist-get src :host)
       (plist-get dst :host))
      (setq cmd1 `("ssh" ,(plist-get src :host)
		   ,(concat dired-synchronize-bin " " 
			    (mapconcat 'concat dired-synchronize-args " ")
			    " -e ssh "
			    (shell-quote-argument (plist-get src :path))
			    (format " %s@%s:%s" (plist-get dst :user)
				    (plist-get dst :host)
				    (shell-quote-argument (plist-get dst :path)))))
	    cmd2 nil))
     ;; one file is remote
     ((or
       (plist-get src :host)
       (plist-get dst :host))
      (setq cmd1 (apply 'append `((,dired-synchronize-bin)
				  ,dired-synchronize-args
				  ("-e") ("ssh")
				  (,(if (plist-get src :host)
					(format "%s:%s" (plist-get src :host)
						(plist-get src :path))
				      (plist-get src :file)))
				  (,(if (plist-get dst :host)
					(format "%s:%s" (plist-get dst :host)
						(plist-get dst :path))
				      (plist-get dst :file)))))
	    cmd2 nil))

     ;; all files are local
     (t
      (setq cmd1 (apply 'append `((,dired-synchronize-bin)
				  ,dired-synchronize-args
				  (,(plist-get src :file))
				  (,(plist-get dst :file))))
	    cmd2 nil)))

    (let* ((p1-str (format "dired-sync %s to %s"
			   (plist-get src :file)
			   (plist-get dst :file)))
	   (p1-buf (format "*%s*" p1-str))
	   (p1 (apply 'start-process p1-str p1-buf (car cmd1) (cdr cmd1)))
	   (p2-str (format "%s (syncing)" p1-str))
	   (p2-buf (format "*%s*" p2-str))
	   p2)

      (process-put p1 :buf p1-buf)
      (unless cmd2
	(set-process-sentinel p1 'dired-synchronize-proc-sentinel))
      (when cmd2
	(setq p2 (apply 'start-process p2-str p2-buf (car cmd2) (cdr cmd2)))
	(process-put p2 :related p1)
	(process-put p2 :buf p2-buf)
	(set-process-sentinel p2 'dired-synchronize-proc-sentinel)))
    t))


(defun dired-synchronize-proc-sentinel (proc change)
  (when (eq (process-status proc) 'exit)
    (let ((status (process-exit-status proc))
	  (buf (process-get proc :buf))
	  (related (process-get proc :related)))
      (if (not (eq 0 status))
	  (progn
	    (when (process-buffer proc)
	      (set-window-buffer (selected-window) buf))
	    (error "dired-synchronize failled"))
	(message "dired-synchronize success")
	(kill-buffer buf))
      (when related
	(kill-process related)))))

(provide 'dired-synchronize)
