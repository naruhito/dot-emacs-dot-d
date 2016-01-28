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
対応しているバージョンは 23-24 です。yum 等のパッケージ管理ツールでインストールできる Emacs のバージョンが 22 以下の場合やシステムインストールの権限がない場合などは、ソースコードからビルドしてホームディレクトリ以下にインストールします。[GNU Emacs](http://www.gnu.org/software/emacs/) の Obtaining/Downloading GNU Emacs セクションの nearby GNU mirror リンクをクリックして `emacs-24.3.tar.gz` (v24 の場合) をダウンロードしてください。

### 解凍

	$ tar zxvf emacs-24.3.tar.gz

### ビルド
`ncurses-devel`, `gcc-c++` 等、事前にビルドのために必要なパッケージをインストールしてから以下のコマンドを実行してください。

	$ cd emacs-24.3/
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


Scala 開発環境 Ensime の設定 (v24 のみ)
------------------

### Java 1.7+ インストール
yum などで rpm をインストールする方法や [Oracle のサイトからダウンロード](http://www.oracle.com/technetwork/java/javase/downloads/) する方法があります。

### sbt 0.13+ インストール

Windows

[msi installer](http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Windows.html) が提供されています。

Mac OS X

	$ brew install sbt

その他 Unix 系 OS

[sbt-launcher.jar](http://www.scala-sbt.org/0.13/docs/Manual-Installation.html) ダウンロードして `~/bin/sbt-launcher.jar` に保存してください。

	$ vi ~/bin/sbt
	
	#!/bin/bash
	SBT_OPTS="-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxMetaspaceSize=256M" # Java 1.8
	# SBT_OPTS="-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M" # Java 1.7
	java $SBT_OPTS -jar `dirname $0`/sbt-launch.jar "$@"
	
	$ chmod u+x ~/bin/sbt

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

### 基本コマンド

情報の調査

- `C-c C-v i` カーソル上の要素の情報を表示
- `C-c C-v t` カーソル上の要素の型を表示
- `C-c C-v d` カーソル上の要素のドキュメントをブラウザ表示

インポート

- `C-c C-r t` カーソル上の要素用の import 候補を表示
- `C-c C-v v` パッケージを検索して import を挿入
- `C-c C-r o` import 設定を整理

リファクタリング

- `C-c C-r r` 変数名を一括編集

その他

- `C-c C-c a` エラーチェック
- `M-.` 定義にジャンプ
- `M-,` ジャンプもとに戻る
- `C-c C-v .` 範囲選択 ("." と "," で拡大縮小)
