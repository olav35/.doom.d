(defun get-string-from (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(when IS-MAC (setq mac-option-key-is-meta t)
      (setq mac-right-option-modifier nil))

(setq user-full-name "Olav Fosse"
      user-mail-address "fosseolav@gmail.com")

(defvar er-channels '(("def con | soma fm" . "http://somafm.com/defcon256.pls")
                      ("cyberia | lainon". "http://lainon.life:8000/cyberia.ogg")
                      ("cafe | lainon" . "http://lainon.life:8000/cafe.ogg")
                      ("swing | lainon" . "http://lainon.life:8000/swing.ogg")
                      ("everything | lainon" . "http://lainon.life:8000/everything.ogg")
                      ("metal | soma fm" . "http://somafm.com/metal130.pls")
                      ("groove salad | soma fm" . "http://somafm.com/groovesalad256.pls")
                      ("secret agent | soma fm" . "http://www.somafm.com/secretagent.pls")
                      ("Ryno The Bearded" . "http://stream.ryno.cc/oo")
                      ))

(defun er-alist-keys (alist) (mapcar 'car alist))

(defun er-stop () (interactive) (when (boundp 'radio-process) (delete-process radio-process)))

(defun er-play-low-level (url) (setq radio-process (start-process "emacs-radio" nil "vlc" "--no-video" "-I" "rc" url)))

(defun er-get-url ()
  (setq er-channel (completing-read
                       "Channel: "
                       (er-alist-keys er-channels)
                       nil nil))
  (or (cdr (assoc er-channel er-channels)) er-channel))

(defun er-play ()
  (interactive)
  (setq url (er-get-url))
  (er-stop)
  (er-play-low-level url))

(map! :leader (:prefix ("r" . "radio") :desc "Play radio" "p" 'er-play))
(map! :leader (:prefix ("r" . "radio") :desc "Stop radio" "s" 'er-stop))

(setq projectile-project-search-path '("~/code/"))

(require 'org-tempo)

(setq org-roam-directory "~/roam")
(setq org-roam-index-file "index.org")

(require 'org-habit)

(set-file-template! 'c++-mode :trigger "template")
(setq yas--default-user-snippets-dir "~/.doom.d/snippets")

(defun comp-open-buffer (buffer &optional switch-window)
      (setq temp kill-buffer-query-functions)
      (setq kill-buffer-query-functions nil)
      (+popup/close-all)
      (+popup-buffer buffer)
      (setq kill-buffer-query-functions temp)
      (when switch-window (switch-to-buffer-other-window buffer))
      )

(defun comp-compile () (interactive)
       (setq temp compilation-read-command)
       (setq compilation-read-command nil)
       (comp-open-buffer (compile (concat "g++ \"" buffer-file-name "\" --std=c++11 -o /tmp/comp-a.out")))
       (setq compilation-read-command temp)
       )

(defun comp-run () (interactive)
       (comp-open-buffer (make-comint "comp-run" "/tmp/comp-a.out")) t)

(defun comp-test () (interactive)
       (setq buffer (current-buffer))
       (comp-open-buffer (make-comint "comp-test" "/tmp/comp-a.out") t)
       (setq y (clipboard-yank))
       (insert (if (eq y nil) "" y)
               (comint-send-input nil nil))
       (other-window)
       )

(map! :leader (:prefix ("k" . "competitive") :desc "Comp compile" "c" 'comp-compile))
(map! :leader (:prefix ("k" . "competitive") :desc "Comp run" "r" 'comp-run))
(map! :leader (:prefix ("k" . "competitive") :desc "Comp test" "t" 'comp-test))

(after! circe
  (set-irc-server! "trigex.moe-znc"
    `(:host "znc.trigex.moe"
      :port 5597
      :user "fossegrim/trigex"
      :nick "fossegrim"
      :realname "fossegrim"
      :pass (lambda (&rest _) (get-string-from "~/.znc")) ; Relax it's just a randomly generated string. I don't use it anywhere else.
      :channels ("#clan"))))
