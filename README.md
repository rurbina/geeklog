 Geeklog
---------

Geeklog is a minimalist CMS, racket-based. It depends on ratamarkup, and it's meant to be used as a backend for Nginx (lighttpd and apache should work as well) in a reverse-proxy setup.

Along its virtues are:
 - clean, proper html5
 - you can extend it "in user space" via executable documents
 - or you can write plugins with a very simple api
 - urls are clean

It uses a very simple MIME-like file format as its backend. There's currently no web browser editor. Instead you should edit the files directly, our favorite solution is offline edition via Dropbox, but other methods should work too. This is so that you can edit files with your favorite text editor, instead of using a crippled text-area, or burdiening us with maintaining a browser editor. (But we might add support for one in the future).

