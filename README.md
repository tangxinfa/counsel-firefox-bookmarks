Ivy interface of firefox bookmarks.

Install first

    (add-to-list 'load-path "<path-of-counsel-firefox-bookmarks>")
    (require 'counsel-firefox-bookmarks)

Search firefox bookmarks

    M-x counsel-firefox-bookmarks

You will have to enable html bookmarks in firefox:

open "about:config" in firefox address bar, double click on this line to set
value to true:

    user_pref("browser.bookmarks.autoExportHTML", false);

You should have now:

    user_pref("browser.bookmarks.autoExportHTML", true);

After closing firefox, you will be able to browse your bookmarks.
