インストール手順
==================

Windows
------------------

### Emacs ダウンロード
[こちら](https://ftp.gnu.org/gnu/emacs/windows/)から Windows 用のバイナリファイル emacs-23.4-bin-i386.zip をダウンロードして解凍してください。解凍したものを、例えば ~/emacs に配置します。

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

- ~/.emacs.d/etc/additional-web-services.el.copy
- ~/.emacs.d/etc/set-env-vars.el.copy

### Emacs 実行
~/emacs/bin/runemacs.exe を実行すると設定ファイルが読み込まれて Emacs が起動します。


UNIX 系 OS
------------------

### Emacs ダウンロード
パッケージ管理ツール (yum 等) でインストールできる Emacs のバージョンが 23 系でない場合やシステムインストールの権限がない場合は、ソースコードからビルドしてホームディレクトリ以下にインストールします。[GNU Emacs](http://www.gnu.org/software/emacs/) の Obtaining/Downloading GNU Emacs セクションの nearby GNU mirror リンクをクリックして emacs-23.4.tar.gz をダウンロードしてください。

### 解凍

	$ tar zxvf emacs-23.4.tar.gz

### ビルド

	$ cd emacs-23.4/
	$ ./configure --prefix=$HOME/local --without-x
	$ make

### インストール
ホームディレクトリ以下にインストールする場合は sudo は不要です。

	$ make install

### .emacs.d の配置
本レポジトリを ~/.emacs.d に配置してください。

### 設定ファイルのコピー
コピーして必要な事項を書き換えてください。

- ~/.emacs.d/etc/additional-web-services.el.copy
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
