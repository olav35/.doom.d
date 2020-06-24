(setq doom-theme 'doom-one)

(defun get-string-from (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun olav-open-safari ()
  (interactive)
  (shell-command "open -a Safari.app"))

(defun olav-open-discord ()
  (interactive)
  (shell-command "open -a Discord.app"))

(defun olav-open-riot ()
  (interactive)
  (shell-command "open -a Riot.app"))

(defun olav-open-chrome ()
  (interactive)
  (shell-command "open -a \"Google Chrome\".app"))

(defun olav-open-chrome ()
  (interactive)
  (shell-command "open -a \"Google Chrome\".app"))

(defun olav-open-visual-studio-code ()
  (interactive)
  (shell-command "open -a \"Visual Studio Code\".app"))

(defun olav-open-spotify ()
  (interactive)
  (shell-command "open -a Spotify.app"))

(defun olav-open-obs ()
  (interactive)
  (shell-command "open -a OBS.app"))

(defun olav-open-finder ()
  (interactive)
  (shell-command "open -a Finder.app"))

(when IS-MAC (setq mac-option-key-is-meta t)
      (setq mac-right-option-modifier nil)
      (setq frame-resize-pixelwise t)
      (map! :leader (:prefix ("e" . "open external") :desc "Visual Studio Code" "v" 'olav-open-visual-studio-code))
      (map! :leader (:prefix ("e" . "open external") :desc "Safari" "s" 'olav-open-safari))
      (map! :leader (:prefix ("e" . "open external") :desc "Chrome" "c" 'olav-open-chrome))
      (map! :leader (:prefix ("e" . "open external") :desc "Discord" "d" 'olav-open-discord))
      (map! :leader (:prefix ("e" . "open external") :desc "Riot" "r" 'olav-open-riot))
      (map! :leader (:prefix ("e" . "open external") :desc "Spotify" "p" 'olav-open-spotify))
      (map! :leader (:prefix ("e" . "open external") :desc "OBS" "o" 'olav-open-obs))
      (map! :leader (:prefix ("e" . "open external") :desc "Finder" "f" 'olav-open-finder))
      )

(setq user-full-name "Olav Fosse"
      user-mail-address "fosseolav@gmail.com")

; should be defvar when packaged
(setq er-channels '(("def con | soma fm" . "http://somafm.com/defcon256.pls")
                    ("cyberia | lainon". "http://lainon.life:8000/cyberia.ogg")
                    ("cafe | lainon" . "http://lainon.life:8000/cafe.ogg")
                    ("swing | lainon" . "http://lainon.life:8000/swing.ogg")
                    ("everything | lainon" . "http://lainon.life:8000/everything.ogg")
                    ("metal | soma fm" . "http://somafm.com/metal130.pls")
                    ("groove salad | soma fm" . "http://somafm.com/groovesalad256.pls")
                    ("secret agent | soma fm" . "http://www.somafm.com/secretagent.pls")
                    ("ryno the bearded" . "http://stream.ryno.cc/oo")
                    ("jamendo lounge" . "http://streaming.radionomy.com/JamendoLounge") ; Great music, but the songs repeat themselfs frequently
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

(setq org-roam-directory "~/org/roam")
(setq org-roam-index-file "index.org")

(require 'org-habit)

(set-file-template! 'c++-mode :trigger "template")
(setq yas--default-user-snippets-dir "~/.doom.d/snippets")

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

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
       (comp-open-buffer (setq buffer (make-comint "comp-run" "/tmp/comp-a.out")) t))

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

;(require 'circe-display-images)
;(enable-circe-display-images)

(map! :leader (:prefix ("o" . "+open") :desc "Open circe" "i" '=irc))
(after! circe
  (set-irc-server! "trigex.moe-znc"
                   `(:host "znc.trigex.moe"
                     :port 5597
                     :user "fossegrim/trigex"
                     :nick "fossegrim"
                     :realname "fossegrim"
                     :pass (lambda (&rest _) (get-string-from "~/.znc")) ; relax it's randomly generated, not used anywhere else and my disk is encrypted
                     :channels ("#clan" "#img-dump" "#sethhateclub" "#bunker"))))

(when nil (set-irc-server! "chat.freenode.net"
                   `(:tls t
                     :port 6697
                     :nick "fossegrim"
                     :sasl-username "fossegrim"
                     :sasl-password (lambda (&rest _) (get-string-from "~/.freenode")) ; relax it's randomly generated, not used anywhere else and my disk is encrypted
                     :channels ("#emacs" "#haskell" "##c++"))))

(defun olav-is-xwidget-webkit-buffer-p (buffer) (string-prefix-p "*xwidget webkit: " (buffer-name buffer)))

(defun olav-xwidget-webkit-buffer ()
  "xwidget-webkit buffer or nil if doesn't exist"
  (seq-find #'olav-is-xwidget-webkit-buffer-p (buffer-list)))

(defun olav-browse (&optional url second-argument)
  (interactive)
  (persp-switch "*BROWSER*")
  (if (called-interactively-p)
      (when (not (olav-xwidget-webkit-buffer)) (xwidget-webkit-browse-url "https://fossegr.im" nil))
    (xwidget-webkit-browse-url url nil))
    (switch-to-buffer (olav-xwidget-webkit-buffer)))
(setq browse-url-browser-function 'olav-browse)
(map! :leader (:prefix ("o" . "open") :desc "Open browser" "b"  'olav-browse))

(setq elfeed-feeds
      '("http://fossegr.im/feed.xml"
        "https://www.youtube.com/feeds/videos.xml?channel_id=UCWQ1f0ZhD-qhJB3AfJEoW0w" ; My channel ? (haven't checked)
        "https://protesilaos.com/codelog.xml"
        "https://www.distrotube.com/phpbb/app.php/feed"
        ;"https://www.kode24.no/?lab_viewport=rss"
        "https://xkcd.com/atom.xml"
        ;"https://news.ycombinator.com/rss"
        ;"https://www.reddit.com/r/emacs/new.rss"
        "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" ; Luke Smith
        "https://www.kode24.no/?lab_viewport=rss"
        "https://nitter.net/olebullsplass/rss"
        ))
(defun olav-rss ()
  (interactive)
  (elfeed-update)
  (persp-switch "*RSS*")
  (=rss))
(map! :leader (:prefix ("o" . "open") :desc "Open elfeed" "l"  'olav-rss))

;(defun olav-mentor ()
;  (interactive)
;  (persp-switch "*TORRENT*")
;  (mentor))
;(map! :leader (:prefix ("o" . "open") :desc "Open mentor" "m" 'olav-mentor))
;(setq mentor-rtorrent-download-directory "~/Downloads")
;(after! 'mentor
;  (define-key mentor-files-mode-map "j" 'mentor-decrease-priority)
;  (define-key mentor-files-mode-map "k" 'mentor-increase-priority))

;(defun olav-torrent ()
;  (interactive)
;  (persp-switch "*TORRENT*")
;  (transmission)
;  )
;(evil-set-initial-state 'transmission-mode 'normal)
;(add-hook 'transmission-mode 'disable-evil-mode)
;(map! :leader (:prefix ("o" . "open") :desc "Open torrent" "m" 'olav-torrent))

;(after! transmission
;  (defun olav-add-magnet (magnet)
;    (interactive "sMagnet: ")
;    (transmission-add magnet))
;  (define-key transmission-mode-map "a" 'olav-add-magnet)
  ;)

(defun olav-open-book ()
  (interactive)
  (setq temp default-directory)
  (cd "~/Google Drive/Books")
  (call-interactively 'counsel-find-file-extern)
;  (counsel-find-file-extern (+default/find-file-under-here))
  (setq default-directory temp)
)

(defun olav-open-anime ()
  (interactive)
  (setq temp default-directory)
  (cd "~/anime")
  (call-interactively 'counsel-find-file-extern)
  ;(counsel-find-file-extern (+default/find-file-under-here)
  (setq default-directory temp)
)

(map! :leader (:prefix ("o" . "open") :desc "Open a book" "B" 'olav-open-book))
(map! :leader (:prefix ("o" . "open") :desc "Open an anime episode" "A" 'olav-open-anime))

(defun olav-scratch ()
  (interactive)
  (if (+workspace-exists-p "*SCRATCH*")
      (persp-switch "*SCRATCH*")
      (progn (persp-switch "*SCRATCH*")
             (switch-to-buffer "*scratch*")))
)

(map! :leader (:prefix ("o" . "open") :desc "Open scratch" "s" 'olav-scratch))

(map! :leader (:prefix ("w" . "window") (:prefix ("m" . "maximize") :desc "Actually maximize (as opposed to the default behaviour)" "m" 'delete-other-windows)))

(setq doom-line-numbers-style 'relative)

;(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
