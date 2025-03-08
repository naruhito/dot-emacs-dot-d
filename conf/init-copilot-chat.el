;;; copilot-chat.el --- Copilot chat interface -*- indent-tabs-mode: nil; lisp-indent-offset: 2; lexical-binding: t -*-
(use-package copilot-chat)

(with-eval-after-load 'copilot-chat-common
  (setq my/copilot-chat-prompt-original copilot-chat-prompt)
  (setopt copilot-chat-prompt (concat my/copilot-chat-prompt-original "\n出力には日本語を用います")))

(setq copilot-chat-prompt-explain "以下のコードを説明してください\n")
(setq copilot-chat-prompt-review "以下のコードをレビューしてください\n")
(setq copilot-chat-prompt-doc "ドキュメントを教えてください\n")
(setq copilot-chat-prompt-fix "修正してください\n")
(setq copilot-chat-prompt-optimize "最適化してください\n")
(setq copilot-chat-prompt-test "テストを書いてください\n")
