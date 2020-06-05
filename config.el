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

(after! circe
   (set-irc-server! "trigex.moe"
                    `(:port 6667
                      :nick "fossegrim"
                      :user "fossegrim"
                      :realname "fossegrim"
                      :pass , (lambda (&rest _) (+pass-get-secret "irc/trigex.moe"))
                      :channels ("#clan"))))
