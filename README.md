Suggest to use
[firefox-bookmarks](https://github.com/tangxinfa/firefox-bookmarks) which use
native api, and should support extensive completion frameworks.

-------------------------------------------------------------------------

Complete Firefox bookmarks with [Ivy](https://github.com/abo-abo/swiper/).

There is a pull request [feat: firefox bookmarks by tangxinfa · Pull Request #1593 · abo-abo/swiper](https://github.com/abo-abo/swiper/pull/1593)
devote to include it in [Ivy](https://github.com/abo-abo/swiper/), once this
objective reached, this project will destroy.

Install `counsel-firefox-bookmarks`

    (add-to-list 'load-path "<path-of-counsel-firefox-bookmarks>")
    (require 'counsel-firefox-bookmarks)

Start search firefox bookmarks with [Ivy](https://github.com/abo-abo/swiper/)

    M-x counsel-firefox-bookmarks

This requires HTML bookmark export to be enabled in Firefox. To do this, open
URL [about:config](about:config) in Firefox, make sure that the value of the
setting `"browser.bookmarks.autoExportHTML"` is `"true"` by, say,
double-clicking it, and then restart Firefox.
