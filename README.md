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

Download `installer.exe` from [here](https://ftp.gnu.org/gnu/emacs/windows).

Install linux commands using [git for windows](https://gitforwindows.org/).

Set the following environment variables via windows control panel.

- HOME: `C:\Users\YOUR_USERNAME`
- PATH: `C:\Program Files\Git\usr\bin;C:\Program Files\Git\mingw64\bin;C:\Program Files\Git\cmd`

## Settings

Git clone this repository as `$HOME/.emacs.d`.

Copy and configure the following files:

- `$HOME/.emacs.d/etc/web-bookmarks.el.copy`
- `$HOME/.emacs.d/etc/set-env-vars.el.copy`

## Tutorials

- Emacs: `M-x help-with-tutorial`
- SKK: `M-x skk-tutorial`
