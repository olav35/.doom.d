(when IS-MAC
  (setq mac-option-key-is-meta t)
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

  (defun olav-open-terminal ()
    (interactive)
    (shell-command "open -a Terminal.app"))

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
  (map! :leader (:prefix ("e" . "open external") :desc "Terminal" "t" 'olav-open-terminal))
  )

(defun olav-counsel-find-file-extern-directory (directory)
  (interactive)
  (setq temp default-directory)
  (cd directory)
  (call-interactively 'counsel-find-file-extern)
  (setq default-directory temp))

(defun olav-open-book ()
  (interactive)
  (olav-counsel-find-file-extern-directory "~/Desktop/Books"))
(defun olav-open-manga ()
  (interactive)
  (olav-counsel-find-file-extern-directory "~/Desktop/Manga"))

(map! :leader (:prefix ("o" . "open") :desc "Open a book" "b" 'olav-open-book))
(map! :leader (:prefix ("o" . "open") :desc "Open a manga" "m" 'olav-open-manga))

(setq user-full-name "Olav Fosse"
      user-mail-address "fosseolav@gmail.com")

(setq projectile-project-search-path '("~/code/"))

(require 'org-tempo)

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

(setq elfeed-feeds
      '("https://protesilaos.com/codelog.xml"
        "https://www.kode24.no/?lab_viewport=rss"
        "http://blog.cleancoder.com/atom.xml"))
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

(use-package! eradio
  :load-path "~/code/eradio"
  :config
    (map! :leader (:prefix ("r" . "eradio") :desc "Play a radio channel" "p" 'eradio-play))
    (map! :leader (:prefix ("r" . "eradio") :desc "Stop the radio player" "s" 'eradio-stop))
    (setq eradio-channels '(("def con - soma fm" . "https://somafm.com/defcon256.pls")
                            ("metal - soma fm" . "https://somafm.com/metal130.pls")
                            ("groove salad - soma fm" . "https://somafm.com/groovesalad256.pls")
                            ("secret agent - soma fm" . "https://www.somafm.com/secretagent.pls")
                            ("cafe - lainon" . "https://lainon.life/radio/cafe.ogg.m3u")
                            ("cyberia - lainon" . "https://lainon.life/radio/cyberia.ogg.m3u")
                            ("everything - lainon" . "https://lainon.life/radio/everything.ogg.m3u")
                            ("swing - lainon" . "https://lainon.life/radio/swing.ogg.m3u")))

(defun olav-open-current-project-in-visual-studio-code ()
  (interactive)
  (shell-command (concat "code " projectile-project-root)))

(map! :leader (:prefix ("p" . "+project") :desc "Open project in Visual Studio Code" "v" 'olav-open-current-project-in-visual-studio-code))

(evil-escape-mode)

(set-frame-parameter (selected-frame) 'alpha '(97 97))
(add-to-list 'default-frame-alist '(alpha 97 97))

(setq org-roam-directory "~/kasten")
(setq org-roam-tag-sources '(all-directories))
(add-hook 'after-init-hook 'org-roam-mode)

(map! :leader (:prefix ("w" . "window") (:prefix ("m" . "maximize") :desc "Actually maximize (as opposed to the default behaviour)" "m" 'delete-other-windows)))

(setq display-line-numbers-type 'relative)

;(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
