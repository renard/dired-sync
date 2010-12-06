;;; dired-sync.el --- sync directories within dired

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, dired, rsync
;; Created: 2010-12-02
;; Last changed: 2010-12-06 16:14:42
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; History:
;;  * 0.1 - first release

;;; TODO
;;  * move both rsync / ssh commands to variables.

;;; Commentary:
;; 
;; dired-sync provide a simple and easy way to synchronize directories from
;; dired. This tool is based upon both rsync(1) and ssh(1).
;;
;; To install `dired-sync' you simply need to drop dired-sync.el in you
;; load-path and bind (suggested) C-s S key to `dired-sync':
;;
;; (when (require 'dired-sync nil t)
;;   (define-key dired-mode-map (kbd "C-c S") 'dired-sync))
;;
;; There are 3 types of directories synchronizations as explained bellow.
;;
;; * local / local
;;   This is the easiest way. rsync(1) would be enough.
;;
;; * local / remote or remote / local
;;   This is also a simple way, the only requirement is a working ssh
;;   connection to the remote host.
;;
;; * remote / remote
;;   This is a bit more complexe since there are 2 types of remote / remote
;;   syncs.
;;   - source server can reach destination server
;;     In that case `dired-sync' would optimize files synchronisation by
;;     running rsync(1) on the source server through a ssh connection.
;;   - source server cannot reach destination server
;;     This is the more complexe case. `dired-sync' would create a ssh
;;     tunnel from source to destination using your local machine as a
;;     jumphost.
;;     Be aware this mode is greedy regarding bandwidth consumption since
;;     data are transfered twice: from the source server to localhost AND
;;     from localhost to destination server.
;;  If a direct connection could not be established from source to
;;  destination, `dired-sync' would automatically fall back to the tunneled
;;  sync mode.
;;
;;  `dired-sync' is heavily based on ssh(1) configuration hence your
;;  ~/.ssh/config file should be as accurate as possible. It doesn't matter
;;  how many jumphosts you need to use to reach both source and destination
;;  as long as they are declared in you ssh configuration file.
;;
;;      Source                       Destination
;;
;;    +---------+   (if possible)    +---------+
;;    |  HostA  |<- - - - - - - - - >|  HostB  |
;;    |  UserA  |                    |  UserB  |
;;    +---------+                    +---------+
;;         ^                              ^    
;;    +---------+                    +---------+
;;    |JumphostA|                    |JumphostB|
;;    | UserJHA |                    | UserJHB |
;;    +---------+                    +---------+
;;         ^                              ^
;;         |          +---------+         |
;;         -----------|localhost|----------
;;                    +---------+
;;	      
;; To use that configuration, you ~/.ssh/config may be something like:
;;
;;   Host *
;;        ForwardAgent yes
;;        RhostsRSAAuthentication yes
;;        RSAAuthentication yes
;;        HashKnownHosts yes
;;        IdentityFile ~/.ssh/id_rsa
;;        TCPKeepAlive yes
;;        ServerAliveInterval 30
;;        Port 22
;;        Protocol 2,1
;;
;;   Host jumphostA
;;        User userJHA
;;        HostName jumphostA.example.com
;;
;;   Host hostA
;;        User userA
;;        HostName jumphostA.internal.example.com
;;        ProxyCommand ssh -q -t jumphostA  nc -w 1 %h %p
;;
;;   Host jumphostB
;;        User userJHB
;;        HostName jumphostB.other-example.com
;;
;;   Host hostB
;;        User userB
;;        HostName jumphostV.internal.other-example.com
;;        ProxyCommand ssh -q -t jumphostB  nc -w 1 %h %p
;;
;;
;; then you would be able to connect to hostA by simply typing:
;;
;;    ssh hostA
;;
;; Or opening a dired buffer to hostB: C-X C-f /scp:hostB:
;;
 
;;; Code:


(defcustom dired-sync-bin "rsync"
  "Path to sync tool."
  :type 'string
  :group 'dired-sync)

(defcustom dired-sync-args '("--delete" "-a" "-D" "-i")
  "Args for sync tool."
  :type 'list
  :group 'dired-sync)

(defcustom dired-sync-time 10
  "Timeout when performing ssh login tests."
  :type 'integer
  :group 'dired-sync)


(defun dired-sync-get-user (host &optional target)
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
	(dired-sync-time (message 
	     (format
	      "dired-sync-get-user timeout on %s : %s" host cmd)))
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



(defun dired-sync-parse-uri (file)
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
      (setq user (dired-sync-get-user file)
	    tunnel-port (+ 1024 (random (- 32767 1024)))))
    (list :file file :user user :method method :host host :path path
	  :tunnel-port tunnel-port)))



(defun dired-sync-read-src-dst (&optional source destination)
  "Read both source and detination directories from minibuffer if not provided.

If called from a `dired-mode' buffer, use `default-directory' for SOURCE.
"
  (let* ((src (dired-sync-parse-uri
	       (or source
		   (if (eq major-mode 'dired-mode) default-directory nil)
		   (read-file-name "Sync source: " nil nil t nil))))
	 (dst (dired-sync-parse-uri
	       (or destination
		   (read-file-name
		    (format "Sync %s to: " (plist-get src :file)))
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
	    (dired-sync-get-user
	     ;; Change to / on remote host to prevent from remote dir not
	     ;; found errors.
	     (format "/%s:/" (plist-get src :host))
	     ;; connecter on remote host using appropriated user.
	     (format "%s@%s" (plist-get dst :user) (plist-get dst :host)))))
    (setq src (plist-put src :direct direct))
    (setq dst (plist-put dst :direct direct))
    (list :src src :dst dst)))


(defun dired-sync (&optional source destination)
  "sync 2 directories using `dired-sync-bin'."
  (interactive)
  (let* ((files (dired-sync-read-src-dst source destination))
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
		   ,(concat dired-sync-bin " "
			    (mapconcat 'concat dired-sync-args " ")
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
		   ,(concat dired-sync-bin " " 
			    (mapconcat 'concat dired-sync-args " ")
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
      (setq cmd1 (apply 'append `((,dired-sync-bin)
				  ,dired-sync-args
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
      (setq cmd1 (apply 'append `((,dired-sync-bin)
				  ,dired-sync-args
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
	(set-process-sentinel p1 'dired-sync-proc-sentinel))
      (when cmd2
	;;make sur shh tunnel is up
	(sit-for dired-sync-time)
	(setq p2 (apply 'start-process p2-str p2-buf (car cmd2) (cdr cmd2)))
	(process-put p2 :related p1)
	(process-put p2 :buf p2-buf)
	(set-process-sentinel p2 'dired-sync-proc-sentinel)))
    ;;(message (concat "C1: " (mapconcat 'append cmd1 " "))))
    ;;(message (concat "C2: " (mapconcat 'append cmd2 " "))))
    t))


(defun dired-sync-proc-sentinel (proc change)
  (when (eq (process-status proc) 'exit)
    (let ((status (process-exit-status proc))
	  (buf (process-get proc :buf))
	  (related (process-get proc :related)))
      (if (not (eq 0 status))
	  (progn
	    (when (process-buffer proc)
	      (set-window-buffer (selected-window) buf))
	    (error "dired-sync failled"))
	(message "dired-sync success")
	(kill-buffer buf))
      (when related
	(kill-process related)))))

(provide 'dired-sync)
