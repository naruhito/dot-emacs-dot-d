Installation
==================

Linux (GUI/CUI)
------------------

### Download/build

1. Supported versions are 24-25. Install a compatible package with yum/apt, or build from source available at [GNU Emacs](https://www.gnu.org/software/emacs/download.html).
1. Build and local install with the following commands after installing necessary dependencies like `ncurses-devel`, `gcc-c++`, etc.

Local install with or without X11.

	tar zxvf emacs-25.3.tar.gz
	cd emacs-25.3/
	./configure --prefix=$HOME/local --without-x
	make
	make install

### Settings

1. Download and rename this repository as `~/.emacs.d`.
1. Copy and configure the following files.
    - `~/.emacs.d/etc/web-bookmarks.el.copy`
    - `~/.emacs.d/etc/set-env-vars.el.copy`
1. Run `~/local/bin/emacs`.


macOS
------------------
1. Supported versions are 24-25. Download a DMG file into the `/Applications` directory from [here](https://emacsformacosx.com/).
1. Download and rename this repository as `~/.emacs.d`.
1. Copy and configure the following files.
    - `~/.emacs.d/etc/web-bookmarks.el.copy`
    - `~/.emacs.d/etc/set-env-vars.el.copy`
1. Run `Emacs.app`.


Windows
------------------
1. Supported versions are 24-25. Download a EXE file into the `~/emacs` directory from [here](https://ftp.gnu.org/gnu/emacs/windows/).
1. Install necessary commands with [Cygwin](https://www.cygwin.com/) to be used on Emacs-eshell.
1. Set the following environment variables from windows control panel.
    - HOME: `C:\Users\YOUR_USERNAME`
    - PATH: `C:\cygwin\bin`
    - CYGWIN: `nodosfilewarning`
1. Download and rename this repository as `~/.emacs.d`.
1. Copy and configure the following files.
    - `~/.emacs.d/etc/web-bookmarks.el.copy`
    - `~/.emacs.d/etc/set-env-vars.el.copy`
1. Run `~/emacs/bin/runemacs.exe`.


Tutorials
==================
- Emacs: `M-x help-with-tutorial`
- SKK: `M-x skk-tutorial`
- Package list: `~/.emacs.d/init.el`
- Key bindings: `~/.emacs.d/conf/init-emacs.el`
