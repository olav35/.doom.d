(setq doom-theme 'doom-one-light)

(defun get-string-from (filename)
  Return tne contents of FILENAME."
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

(defun olav-open-postman ()
  (interactive)
  (shell-command "open -a Postman.app"))

(when IS-MAC (setq mac-option-key-is-meta t)
      (setq mac-right-option-modifier nil)
      (setq frame-resize-pixelwise t)
      (map! :leader (:prefix ("e" . "open external") :desc "Visual Studio Code" "v" 'olav-open-visual-studio-code))
      (map! :leader (:prefix ("e" . "open external") :desc "Safari" "s" 'olav-open-safari))
      (map! :leader (:prefix ("e" . "open external") :desc "Chrome" "c" 'olav-open-chrome))
      (map! :leader (:prefix ("e" . "open external") :desc "Discord" "d" 'olav-open-discord))
      (map! :leader (:prefix ("e" . "open external") :desc "Riot" "r" 'olav-open-riot))
      (map! :leader (:prefix ("e" . "open external") :desc "Spotify" "m" 'olav-open-spotify))
      (map! :leader (:prefix ("e" . "open external") :desc "OBS" "o" 'olav-open-obs))
      (map! :leader (:prefix ("e" . "open external") :desc "Finder" "f" 'olav-open-finder))
      (map! :leader (:prefix ("e" . "open external") :desc "Postman" "p" 'olav-open-finder))
      )

(defun olav-open-book ()
  (interactive)
  (setq temp default-directory)
  (cd "~/Desktop/Books")
  (call-interactively 'counsel-find-file-extern)
  (setq default-directory temp))

(map! :leader (:prefix ("o" . "open") :desc "Open a book" "B" 'olav-open-book))

(setq user-full-name "Olav Fosse"
      user-mail-address "fosseolav@gmail.com")

(setq projectile-project-search-path '("~/code/"))

(require 'org-tempo)

(setq org-roam-graph-viewer 'counsel-find-file-extern)

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
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(map! :leader (:prefix ("o" . "open") :desc "Open browser" "b"  'olav-browse))

(setq elfeed-feeds
      '(;"http://fossegr.im/feed.xml"
        "https://www.youtube.com/feeds/videos.xml?channel_id=UCWQ1f0ZhD-qhJB3AfJEoW0w" ; My channel ? (haven't checked)
        "https://protesilaos.com/codelog.xml"
        "https://www.distrotube.com/phpbb/app.php/feed"
        ;"https://www.kode24.no/?lab_viewport=rss"
        "https://xkcd.com/atom.xml"
        ;"https://news.ycombinator.com/rss"
        ;"https://www.reddit.com/r/emacs/new.rss"
        "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" ; Luke Smith
        "https://www.kode24.no/?lab_viewport=rss"
        ;"https://nitter.net/olebullsplass/rss"
        ))
(defun olav-rss ()
  (interactive)
  (elfeed-update)
  (persp-switch "*RSS*")
  (=rss))
(map! :leader (:prefix ("o" . "open") :desc "Open elfeed" "l"  'olav-rss))

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
