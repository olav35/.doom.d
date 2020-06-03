;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq doom-incremental-load-immediately t)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Olav Fosse"
      user-mail-address "fosseolav@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(defvar er-channels '(("def con | soma fm" . "http://somafm.com/defcon256.pls")
                      ("cyberia | lainon". "http://lainon.life:8000/cyberia.ogg")
                      ("cafe | lainon" . "http://lainon.life:8000/cafe.ogg")
                      ("swing | lainon" . "http://lainon.life:8000/swing.ogg")
                      ("everything | lainon" . "http://lainon.life:8000/everything.ogg")
                      ("metal | soma fm" . "http://somafm.com/metal130.pls")
                      ("groove salad | soma fm" . "http://somafm.com/groovesalad256.pls")
                      ("secret agent | soma fm" . "http://www.somafm.com/secretagent.pls")
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


(set-file-template! 'c++-mode :trigger "template")
(setq yas--default-user-snippets-dir "~/.doom.d/snippets")

;(kill-buffer (find-file "/tmp/emacs-hack.cpp"))

(setq projectile-project-search-path '("~/code/"))

(when IS-MAC (setq mac-option-key-is-meta t)
      (setq mac-right-option-modifier nil))

(require 'org-habit)

(setq org-roam-directory "~/roam")
(setq org-roam-index-file "index.org")
