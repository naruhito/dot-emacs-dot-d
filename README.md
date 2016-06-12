インストール手順
==================

Windows
------------------

### Emacs ダウンロード
対応しているバージョンは 23-24 です。[こちら](https://ftp.gnu.org/gnu/emacs/windows/)から Windows 用のバイナリファイル `emacs-24.3-bin-i386.zip` をダウンロードして解凍してください。解凍したものを、例えば `~/emacs` に配置します。

### Cygwin ダウンロード
Cygwin で Unix コマンドをインストールして Emacs の Eshell で使用します。[こちらのサイト](https://www.cygwin.com/)からパッケージマネージャをダウンロードして、必要なコマンドをインストールしてください。

### 環境変数の設定
コントロールパネル等から環境変数の設定画面を開き、以下の環境変数を設定してください。

- HOME: `C:\Users\YOUR_USERNAME` (.emacs.d を認識させるため)
- PATH: `C:\cygwin\bin` (既存の PATH 環境変数がある場合は追記)
- CYGWIN: `nodosfilewarning` (Emacs からコマンドを使用した場合の Cygwin 警告を抑制するため)

### .emacs.d の配置
本レポジトリを `~/.emacs.d` に配置してください。

### 設定ファイルのコピー
コピーして必要な事項を書き換えてください。

	~/.emacs.d/etc/web-bookmarks.el.copy
	~/.emacs.d/etc/set-env-vars.el.copy

### Emacs 実行
`~/emacs/bin/runemacs.exe` を実行すると設定ファイルが読み込まれて Emacs が起動します。


Mac OS X (GUI)
------------------

### Emacs ダウンロード
対応しているバージョンは 23-24 です。[こちら](http://emacsformacosx.com/)からバイナリファイル `Emacs-24.5-1-universal.dmg` をダウンロードして Applications フォルダに保存してください。

### .emacs.d の配置
本レポジトリを `~/.emacs.d` に配置してください。

### 設定ファイルのコピー
コピーして必要な事項を書き換えてください。

	~/.emacs.d/etc/web-bookmarks.el.copy
	~/.emacs.d/etc/set-env-vars.el.copy

### Emacs 実行
Emacs.app を実行すると設定ファイルが読み込まれて Emacs が起動します。


ターミナル
------------------

### Emacs ダウンロード
対応しているバージョンは 23-24 です。yum 等のパッケージ管理ツールでインストールできる Emacs のバージョンが 22 以下の場合やシステムインストールの権限がない場合などは、ソースコードからビルドしてホームディレクトリ以下にインストールします。[GNU Emacs](https://www.gnu.org/software/emacs/download.html) の nearby GNU mirror リンクをクリックして `emacs-24.5.tar.gz` をダウンロードしてください。

### 解凍

	$ tar zxvf emacs-24.5.tar.gz

### ビルド
`ncurses-devel`, `gcc-c++` 等、事前にビルドのために必要なパッケージをインストールしてから以下のコマンドを実行してください。

	$ cd emacs-24.5/
	$ ./configure --prefix=$HOME/local --without-x
	$ make

### インストール
ホームディレクトリ以下にインストールする configure 引数を指定したため sudo は不要です。

	$ make install

### .emacs.d の配置
本レポジトリを `~/.emacs.d` に配置してください。

### 設定ファイルのコピー
コピーして必要な事項を書き換えてください。

	~/.emacs.d/etc/web-bookmarks.el.copy
	~/.emacs.d/etc/set-env-vars.el.copy

### Emacs 実行
`~/local/bin/emacs` を実行すると設定ファイルが読み込まれて Emacs が起動します。


使い方
==================

Emacs の基本的な使用方法
------------------
一般的な使用方法は、チュートリアルで把握できます。

	M-x help-with-tutorial

SKK の基本的な使用方法は以下のチュートリアルで把握できます。

	M-x skk-tutorial

よく使用するコマンド一覧は `C-z` で表示できます。例えば `C-z C-i` で Eshell を実行できることが分かります。本レポジトリで設定されているキーバインド一覧は `~/.emacs.d/conf/init-emacs.el` に記載されています。パッケージ一覧は `~/.emacs.d/init.el` に記載されています。


Scala 開発環境 Ensime の設定 (v24 のみ対応; 設定は任意です)
==================
一般的な sbt の設定を行った後に、追加で以下の設定を行います。

### ensime-sbt グローバルプラグインの設定

	$ vi ~/.sbt/0.13/plugins/plugins.sbt
	addSbtPlugin("org.ensime" % "ensime-sbt" % "0.3.2")

### 使用方法
Scala プロジェクトに移動した後、初回のみ `.ensime` ファイルを生成してください。

	$ cd /path/to/your-scala-project
	$ sbt gen-ensime

`ensime-server` を起動してください。

	M-x ensime

終了したい場合は以下のようにします。

	M-x ensime-shutdown

build.sbt の `libraryDependencies` を変更した場合 `.ensime` ファイルを更新して ensime 設定を再読み込みしてください。

	$ cd /path/to/your-scala-project
	$ sbt gen-ensime
	M-x ensime-reload
