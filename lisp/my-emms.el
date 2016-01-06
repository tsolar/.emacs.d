;;; EMMS

(require 'emms-setup)
(emms-standard)
(emms-default-players)

(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-info)
(require 'emms-cache)
(require 'emms-playlist-mode)
;; (require 'emms-mode-line)
(require 'emms-playing-time)
(require 'emms-player-mpd)
(require 'emms-playlist-sort)
(require 'emms-mark)
(require 'emms-browser)
(require 'emms-lyrics)
(require 'emms-last-played)
(require 'emms-score)
;;(require 'emms-lastfm)

(setq emms-playlist-default-major-mode 'emms-playlist-mode)

(add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
;; (emms-mode-line 1)
;; (emms-mode-line-blank)
(emms-playing-time 1)
(emms-lyrics 1)
(add-hook 'emms-player-started-hook 'emms-last-played-update-current)
(emms-score 1)
(when (fboundp 'emms-cache)           ; work around compiler warning
  (emms-cache 1))
(setq emms-score-default-score 3)

(require 'emms-player-mpd)
;;Set the variables emms-player-mpd-server-name and emms-player-mpd-server-port to the location and port (respectively) of your MusicPD server. For example:
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6601")

;; If your MusicPD setup requires a password, you will to set emms-player-mpd-server-password as follows.
;;(setq emms-player-mpd-server-password "mypassword")

;; To get track information from MusicPD, invoke the following:
(add-to-list 'emms-info-functions 'emms-info-mpd)

;; Adding `emms-player-mpd' to your Emms player list is accomplished by invoking:
(add-to-list 'emms-player-list 'emms-player-mpd)

(setq emms-player-mpd-sync-playlist t)

(defvar emms-browser-info-title-format "%i%n  :: %a - %A - %T. %t")
(defvar emms-browser-playlist-info-title-format
  emms-browser-info-title-format)

;; Playlist format
(defun my-describe (track)
  (let* ((empty "...")
         (name (emms-track-name track))
         (type (emms-track-type track))
         (short-name (file-name-nondirectory name))
         (play-count (or (emms-track-get track 'play-count) 0))
         (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
         (artist (or (emms-track-get track 'info-artist) empty))
         (year (emms-track-get track 'info-year))
         (playing-time (or (emms-track-get track 'info-playing-time) 0))
         (min (/ playing-time 60))
         (sec (% playing-time 60))
         (album (or (emms-track-get track 'info-album) empty))
         (tracknumber (emms-track-get track 'info-tracknumber))
         (short-name (file-name-sans-extension
                      (file-name-nondirectory name)))
         (title (or (emms-track-get track 'info-title) short-name))
         (rating (emms-score-get-score name))
         (rate-char ?☭)
         )
    (format "%15s - %.4s [%-20s] - %2s. %-30s |%2d %s"
            artist
            year
            album
            tracknumber
            title
            play-count
            (make-string rating rate-char)))
)

(setq emms-track-description-function 'my-describe)
;; end EMMS

(provide 'my-emms)
