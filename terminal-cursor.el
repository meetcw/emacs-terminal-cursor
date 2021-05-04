;;; init.el --- Allow change cursor shape and color in terminal  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  meetcw

;; Author: meetcw <meetcw@outlook.com>
;; Keywords: terminals

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun terminal-cursor--terminal-type()
  "Detect terminal type"
  (when (not (display-graphic-p))
    (cond ((getenv "XTERM_VERSION") 'xterm)
          ((string= (getenv "TERM_PROGRAM") "Apple_Terminal") 'apple-terminal)
          ((getenv "KONSOLE_PROFILE_NAME") 'konsole)
          ((string= (getenv "COLORTERM") "gnome-terminal") 'gnome-terminal)
          ( (string= (getenv "TERM_PROGRAM") "iTerm.app") 'iterm)
          ((string= (getenv "TERM") "dumb") 'dumb))))

(defun terminal-cursor--in-tmux()
  "Determine emacs is in tmux"
  (when (not (display-graphic-p))
    (getenv "TMUX")))

(defun terminal-cursor--make-tmux-shape (seq)
  "Make escape sequence for tmux."
  ;; (let ((prefix "\ePtmux;\e")
  ;;       (suffix "\e\\"))
  ;;   (concat prefix seq suffix))
  seq)
(defun terminal-cursor--make-konsole-cursor-shape (shape)
  "Make escape sequence for konsole."
  (let ((prefix  "\e]50;CursorShape=")
        (suffix  "\x7")
        (box     "0")
        (bar     "1")
        (hbar    "2")
        (seq     nil))
    (unless (member shape '(box bar hbar))
      (setq shape 'box))
    (cond ((eq shape 'box)
           (setq seq (concat prefix box suffix)))
          ((eq shape 'bar)
           (setq seq (concat prefix bar suffix)))
          ((eq shape 'hbar)
           (setq seq (concat prefix hbar suffix))))
    (if (terminal-cursor--in-tmux)
        (terminal-cursor--make-tmux-shape seq) seq)))

;; https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
;; Set cursor style (DECSCUSR), VT520.
;;   Ps = 0  ⇒  blinking block.
;;   Ps = 1  ⇒  blinking block (default).
;;   Ps = 2  ⇒  steady block.
;;   Ps = 3  ⇒  blinking underline.
;;   Ps = 4  ⇒  steady underline.
;;   Ps = 5  ⇒  blinking bar, xterm.
;;   Ps = 6  ⇒  steady bar, xterm.

(defun terminal-cursor--make-xterm-cursor-shape (shape)
  "Make escape sequence for XTerm."
  (let ((prefix      "\e[")
        (suffix      " q")
        (box-blink   "1")
        (box         "2")
        (hbar-blink  "3")
        (hbar        "4")
        (bar-blink   "5")
        (bar         "6")
        (seq        nil))
    (unless (member shape '(box bar hbar))
      (setq shape 'box))
    (cond ((eq shape 'box)
           (setq seq (concat prefix (if blink-cursor-mode box-blink box) suffix)))
          ((eq shape 'bar)
           (setq seq (concat prefix (if blink-cursor-mode bar-blink bar) suffix)))
          ((eq shape 'hbar)
           (setq seq (concat prefix (if blink-cursor-mode hbar-blink hbar) suffix))))
    (if (terminal-cursor--in-tmux)
        (terminal-cursor--make-tmux-shape seq) seq)))

(defun terminal-cursor--make-cursor-shape (shape)
  "Make escape sequence for cursor shape."
  (let ((terminal-type (terminal-cursor--terminal-type))
        (shape (if (consp shape)
                   (car shape) shape)))
    (cond ((eq terminal-type 'konsole)
           (terminal-cursor--make-konsole-cursor-shape shape))
          (terminal-type (terminal-cursor--make-xterm-cursor-shape shape))
          (t ""))))

(defun terminal-cursor--make-cursor-color (color)
  "Make escape sequence for cursor color."
  (when-let ((color (color-name-to-rgb color))
             (hex-color (apply 'color-rgb-to-hex color))
             (terminal-type (terminal-cursor--terminal-type))
             (prefix (if (eq terminal-type 'iterm) "\e]Pl" "\e]12;"))
             (suffix (if (eq terminal-type 'iterm) "\e\\" "\a")))
    ;; https://www.iterm2.com/documentation-escape-codes.html
    (concat prefix
            ;; https://www.iterm2.com/documentation-escape-codes.html
            ;; Remove #, rr, gg, bb are 2-digit hex value for iTerm.
            (if (and (terminal-cursor--in-tmux)
                     (string-prefix-p "#" hex-color))
                (substring hex-color 1) hex-color) suffix)))

(defun terminal-cursor-update-cursor()
  (interactive)
  (unless (display-graphic-p)
    (let ((shape cursor-type)
          (color (face-background 'cursor)))
      (when-let ((color (terminal-cursor--make-cursor-color color)))
        (send-string-to-terminal color))
      (when-let ((shape (terminal-cursor--make-cursor-shape shape)))
        (send-string-to-terminal shape)))))

(defun terminal-cursor--set-cursor-color-advice(original-function &rest args)
  (let ((color (car args)))
    (if (display-graphic-p)
        (apply original-function args)
      (when-let ((color (terminal-cursor--make-cursor-color color)))
        (send-string-to-terminal color)))))

(defun terminal-cursor--cursor-type-watcher(symbol value operation where)
  (unless (display-graphic-p)
    (when-let ((shape (terminal-cursor--make-cursor-shape value)))
      (send-string-to-terminal shape))))

;;;###autoload
(define-minor-mode terminal-cursor-mode "Enable set curser color and shape in terminal."
  :init-value nil
  :lighter " terminal-cursor"
  :global t
  (if terminal-cursor-mode              ;
      (progn                            ;
        (terminal-cursor-update-cursor)
        (advice-add #'set-cursor-color
                    :around #'terminal-cursor--set-cursor-color-advice)
        (add-variable-watcher 'cursor-type #'terminal-cursor--cursor-type-watcher))
    (advice-remove #'set-cursor-color #'terminal-cursor--set-cursor-color-advice)
    (remove-variable-watcher 'cursor-type #'terminal-cursor--cursor-type-watcher)))

(provide 'terminal-cursor)
;;; init.el ends here
