;;; org-download-z.el --- Helpers for org-download.

;; Author: Chris Hunt
;; Version: 1.0
;; Package-Requires: ((org-download))

(require 'org-download)

(defun org-download-z--xclip-save-clipboard-image (path)
  "xclip that doesn't hang when emacs itself owns the clipboard"
  ;; When xclip is invoked synchronously (like shell-command or
  ;; call-process), and when emacs itself owns the clipboard, the xclip
  ;; command hangs waiting for a response. For that reason, we invoke
  ;; xclip asynchronously (with start-process-shell-command) and wait for
  ;; it.
  (let*
    (
      (save-image-cmd
        (concat
          "xclip -sel clip -t image/png -o 2>/dev/null </dev/null >"
          (shell-quote-argument path)
        )
      )
      (save-image-process (start-process-shell-command "xclip" nil save-image-cmd))
    )
    ;; Wait for up to 2 seconds
    (accept-process-output save-image-process 2)
    (let*
      (
        (is-alive (process-live-p save-image-process))
        (exit-code (process-exit-status save-image-process))
        (exited-ok (and (not is-alive) (eq exit-code 0)))
      )
      (unless exited-ok
        (if (not is-alive)
          (princ (format "xclip exited with code %d" exit-code))
          (princ (format "timeout waiting for xclip, killing it"))
          (kill-process save-image-process)
        )
        (delete-file path)
      )
    )
  )
)

(defun org-download-z--wsl-save-clipboard-image (path)
  (let*
    (
     ;; Resolve all symlinks first, since Windows can't handle them by default.
     (resolve-link-cmd (concat "readlink -f " (shell-quote-argument path)))
     (img-abspath (string-trim-right (shell-command-to-string resolve-link-cmd)))
     ;; PowerShell is going to be executing in a Windows context, and needs the Windows
     ;; path.
     (to-windows-path-cmd (concat "wslpath -aw " (shell-quote-argument img-abspath)))
     (img-abspath-win (string-trim-right (shell-command-to-string to-windows-path-cmd)))
     ;; This should work by default in Windows 10 (tested with 1903).
     (powershell-save-clipboard-image-fn
       (concat
         "Add-Type -AssemblyName System.Windows.Forms;"
         "if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {"
           "$image = [System.Windows.Forms.Clipboard]::GetImage();"
           "[System.Drawing.Bitmap]$image.Save('"
             img-abspath-win
           "', [System.Drawing.Imaging.ImageFormat]::Png);"
           "Write-Output 'clipboard content saved as file'"
         "} else {"
           "Write-Output 'clipboard does not contain image data'"
         "}"
       )
     )
     (save-clipboard-image-cmd
       (concat
         "PowerShell.exe -Command " (shell-quote-argument powershell-save-clipboard-image-fn)
       )
     )
   )
   (shell-command save-clipboard-image-cmd)
  )
)

(defun org-download-z--wsl-take-screenshot (path)
  "Using the snipping tool then PowerShell to get the image"
  ;; NOTE: Doesn't handle cancellation/failure.
  ;; In Windows 10 there are two built-in options for taking screenshots:
  ;;
  ;; - legacy SnippingTool (invoked like SnippingTool.exe /clip)
  ;; - Snip & Sketch (invoked like explorer.exe ms-screenclip:)
  ;;
  ;; Snip & Sketch spawns a separate independent activity, making it harder to tell when the
  ;; screenshot is done, whereas SnippingTool stays alive until the screenshot is done or
  ;; the operation is cancelled. Since it is easier, for now we use SnippingTool.
  (shell-command "SnippingTool.exe /clip")
  ;; SnippingTool doesn't propagate failure or cancellation as an exit code, so just continue
  ;; either way.
  ;; This may race with other items setting the clipboard, but it's not likely.
  (org-download-z--wsl-save-clipboard-image path)
)

;;;###autoload
(defun org-download-z-paste ()
  "Paste image from clipboard into current buffer, see org-download for more
nfiguration options."
  (interactive)
  (let*
    (
      ;; WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"
      (is-wsl (string-match "-[Mm]icrosoft" operating-system-release))
      (org-download-screenshot-method
        (if is-wsl
          #'org-download-z--wsl-save-clipboard-image
          #'org-download-z--xclip-save-clipboard-image
        )
      )
    )
    (org-download-screenshot)
  )
)

(setq org-download-screenshot-method
  (if (string-match "-[Mm]icrosoft" operating-system-release)
    #'org-download-z--wsl-take-screenshot
    ;; On other platforms, just assume ImageMagick is available.
    "import %s"
  )
)

(provide 'org-download-z)
