;;; osx-browse-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "osx-browse" "osx-browse.el" (22781 64561 0
;;;;;;  0))
;;; Generated autoloads from osx-browse.el

(let ((loads (get 'osx-browse 'custom-loads))) (if (member '"osx-browse" loads) nil (put 'osx-browse 'custom-loads (cons '"osx-browse" loads))))

(let ((loads (get 'osx-browse-keys 'custom-loads))) (if (member '"osx-browse" loads) nil (put 'osx-browse-keys 'custom-loads (cons '"osx-browse" loads))))

(defvar osx-browse-mode nil "\
Non-nil if Osx-Browse mode is enabled.
See the `osx-browse-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `osx-browse-mode'.")

(custom-autoload 'osx-browse-mode "osx-browse" nil)

(autoload 'osx-browse-mode "osx-browse" "\
Turn on `osx-browse-mode'.

Turning on osx-browse-mode will direct `browse-url' to use this
package when opening external browsers, and activate keybindings
as defined in `customize'.  It may also set up certain aliases
when `osx-browse-install-aliases' is set.

When called interactively with no prefix argument, this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle.

\(fn &optional ARG)" t nil)

(autoload 'osx-browse-url "osx-browse" "\
Open URL in an external browser on OS X.

When called interactively, `browse-url-dwim-get-url' is used
to detect URL from the edit context and prompt for user input
as needed.

Optional NEW-WINDOW is not currently respected.

Optional BROWSER requests a specific browser, using an Apple
bundle ID, eg \"com.apple.Safari\" or application name, eg
\"Webkit.app\".  When BROWSER is not set, the customizable
variable `osx-browse-prefer-browser' is consulted, and if that
value is nil, the system default is used.

Optional FOCUS can be set to 'foreground or 'background to
control whether the external process changes the focus.  If
BACKGROUND is not set, the customizable variable
`osx-browse-prefer-background' is consulted.

When called interactively, specifying a negative prefix argument
is equivalent to setting FOCUS to 'background.  Any other prefix
argument is equivalent to setting FOCUS to 'foreground.

\(fn URL &optional NEW-WINDOW BROWSER FOCUS)" t nil)

(autoload 'osx-browse-search "osx-browse" "\
Perform an Internet search for TEXT, or region, or interactive input.

If TEXT is a URL, browse to page directly.  Otherwise invoke an
Internet search using TEXT.  When called interactively, TEXT may
be taken from the region or entered at a prompt.

NEW-WINDOW, BROWSER, and FOCUS are as documented for
`osx-browse-url'.

Optional SEARCH-URL specifies the URL fragment used to construct
the search request.  If not specified, the customizable variable
`browse-url-dwim-search-url' is consulted.

\(fn TEXT &optional NEW-WINDOW BROWSER FOCUS SEARCH-URL)" t nil)

(autoload 'osx-browse-guess "osx-browse" "\
Perform an Internet search or browses to URL according to context.

Identical to `osx-browse-url-search' except that an attempt will
be made to extract a URL from TEXT or edit context before
prompting the user.

When a region is not active and the point is on a probable URL,
that value will be used and the user will not be prompted.

NEW-WINDOW, BROWSER, and FOCUS are as documented for
`osx-browse-url'.

Optional SEARCH-URL specifies the URL fragment used to construct
the search request.  If not specified, the customizable variable
`browse-url-dwim-search-url' is consulted.

\(fn TEXT &optional NEW-WINDOW BROWSER FOCUS SEARCH-URL)" t nil)

(autoload 'osx-browse-url-safari "osx-browse" "\
Open URL in Safari on OS X.

BROWSER defaults to \"com.apple.Safari\".

URL, NEW-WINDOW, and FOCUS are as documented for
`osx-browse-url'.

\(fn URL &optional NEW-WINDOW BROWSER FOCUS)" t nil)

(autoload 'osx-browse-url-chrome "osx-browse" "\
Open URL in Google Chrome on OS X.

BROWSER defaults to \"com.google.Chrome\".

URL, NEW-WINDOW, and FOCUS are as documented for
`osx-browse-url'.

\(fn URL &optional NEW-WINDOW BROWSER FOCUS)" t nil)

(autoload 'osx-browse-url-firefox "osx-browse" "\
Open URL in Firefox on OS X.

BROWSER defaults to \"org.mozilla.Firefox\".

URL, NEW-WINDOW, and FOCUS are as documented for
`osx-browse-url'.

\(fn URL &optional NEW-WINDOW BROWSER FOCUS)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; osx-browse-autoloads.el ends here
