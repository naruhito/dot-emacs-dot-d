インストール手順
==================

Windows
------------------

### Emacs ダウンロード
対応しているバージョンは 23 および 24 です。[こちら](https://ftp.gnu.org/gnu/emacs/windows/)から Windows 用のバイナリファイル emacs-24.3-bin-i386.zip (v24 の場合) をダウンロードして解凍してください。解凍したものを、例えば ~/emacs に配置します。

### Cygwin ダウンロード
Cygwin で Unix コマンドをインストールして Emacs の Eshell で使用します。[こちらのサイト](https://www.cygwin.com/)からパッケージマネージャをダウンロードして、必要なコマンドをインストールしてください。

### 環境変数の設定
コントロールパネル等から環境変数の設定画面を開き、以下の環境変数を設定してください。

- HOME: C:\Users\YOUR_USERNAME (.emacs.d を認識させるため)
- PATH: C:\cygwin\bin (既存の PATH 環境変数がある場合は追記)
- CYGWIN: nodosfilewarning (Emacs からコマンドを使用した場合の Cygwin 警告を抑制するため)

### .emacs.d の配置
本レポジトリを ~/.emacs.d に配置してください。

### 設定ファイルのコピー
コピーして必要な事項を書き換えてください。

- ~/.emacs.d/etc/web-bookmarks.el.copy
- ~/.emacs.d/etc/set-env-vars.el.copy

### Emacs 実行
~/emacs/bin/runemacs.exe を実行すると設定ファイルが読み込まれて Emacs が起動します。


UNIX 系 OS
------------------

### Emacs ダウンロード
対応しているバージョンは 23 および 24 です。パッケージ管理ツール (yum 等) でインストールできる Emacs のバージョンが 22 以下の場合やシステムインストールの権限がない場合などは、ソースコードからビルドしてホームディレクトリ以下にインストールします。[GNU Emacs](http://www.gnu.org/software/emacs/) の Obtaining/Downloading GNU Emacs セクションの nearby GNU mirror リンクをクリックして emacs-24.3.tar.gz (v24 の場合) をダウンロードしてください。

### 解凍

	$ tar zxvf emacs-24.3.tar.gz

### ビルド
事前にビルドのために必要なパッケージ (ncurses-devel、gcc-c++ など) をインストールしてください。

	$ cd emacs-24.3/
	$ ./configure --prefix=$HOME/local --without-x
	$ make

### インストール
ホームディレクトリ以下にインストールする configure 引数を指定したため sudo は不要です。

	$ make install

### .emacs.d の配置
本レポジトリを ~/.emacs.d に配置してください。

### 設定ファイルのコピー
コピーして必要な事項を書き換えてください。

- ~/.emacs.d/etc/web-bookmarks.el.copy
- ~/.emacs.d/etc/set-env-vars.el.copy

### Emacs 実行
~/local/bin/emacs を実行すると設定ファイルが読み込まれて Emacs が起動します。


使い方
==================

### Emacs の基本的な使用方法

	M-x help-with-tutorial

### SKK の基本的な使用方法

	M-x skk-tutorial

### 基本的なコマンド一覧の表示

	C-z

例えば C-z C-i で Eshell を実行できることが分かります。

### 基本的なコマンドのキーバインド一覧
~/.emacs.d/conf/init-emacs.el に記載されています。

### パッケージ一覧
~/.emacs.d/init.el に記載されています。
