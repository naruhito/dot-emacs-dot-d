# dot-emacs-dot-d

Compatibility: GNU Emacs 27

## Linux

Install with package managers, or build from [source](https://www.gnu.org/software/emacs/download.html):

```
tar zxvf emacs-27.1.tar.gz
cd emacs-27.1/
./configure --prefix=$HOME/local
make
make install
```

## macOS

Download and copy [the binary](https://emacsformacosx.com) into `/Applications`.

## Windows

Download and copy [the binary](https://ftp.gnu.org/gnu/emacs/windows) into `$HOME/emacs`.

Install linux commands using [Cygwin](https://www.cygwin.com).

Set the following environment variables via windows control panel.

- HOME: `C:\Users\YOUR_USERNAME`
- PATH: `C:\cygwin\bin`
- CYGWIN: `nodosfilewarning`

## Settings

Git clone this repository as `$HOME/.emacs.d`.

Copy and configure the following files:

- `$HOME/.emacs.d/etc/web-bookmarks.el.copy`
- `$HOME/.emacs.d/etc/set-env-vars.el.copy`

## Tutorials

- Emacs: `M-x help-with-tutorial`
- SKK: `M-x skk-tutorial`
