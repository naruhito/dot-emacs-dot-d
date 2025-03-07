;;; copilot-chat.el --- Copilot chat interface -*- indent-tabs-mode: nil; lisp-indent-offset: 2; lexical-binding: t -*-
(use-package copilot-chat)
(with-eval-after-load 'copilot-chat-common
  (setq my/copilot-chat-prompt-original copilot-chat-prompt)
  (setopt copilot-chat-prompt (concat my/copilot-chat-prompt-original "\n出力には日本語を用います")))
