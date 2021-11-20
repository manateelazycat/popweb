# What is popweb?
I want a pop-up window display web multimedia content, such as, translation tooltip, LaTex preview, code completion etc.

Browser related technology comes from my other project [Emacs Application Framework](https://github.com/emacs-eaf/emacs-application-framework)

## Installation
1. Make sure install PyQt5 from your operating system repository (DON'T install PyQt5 from pip, otherwise browser will crash)
2. Install library [python-epc](https://github.com/tkf/python-epc) by ```pip install epc```
3. Clone or download this repository (path of the folder is the `<path-to-popweb>` used below).
4. In your `~/.emacs`, add the following two lines:
```elisp
(add-to-list 'load-path "<path-to-popweb>") ; add popweb to your load-path
(require 'popweb-dict-bing)
(require 'popweb-dict-youdao)
(require 'popweb-latex)
```

## Usage
* popweb-dict-bing-input: popup input translation window with Bing
* popweb-dict-youdao-input: popup input translation window with Youdao
* popweb-dict-bing-pointer: popup pointer translation window with Bing
* popweb-dict-youdao-pointer: popup pointer translation window with Youdao
* popweb-latex-mode: preview window will popup when cursor over LaTeX string.

## Proxy
If you need to use a proxy to access the internet, one can configure the proxy settings.

```Elisp
(setq popweb-proxy-type "http")
(setq popweb-proxy-host "127.0.0.1")
(setq popweb-proxy-port "1080")
```

If you use Socks5 as a local proxy, one can set proxy type with:

```Elisp
(setq popweb-proxy-type "socks5")
```

# Screenshot
## Popweb with Bing translate.
<p align="center">
  <img width="800" src="./img/dict-bing.png">
</p>

## Popweb with Youdao translate.
<p align="center">
  <img width="800" src="./img/dict-youdao.png">
</p>

## Popweb with LaTeX preview.
<p align="center">
  <img width="800" src="./img/katex-preview.png">
</p>

## Report bug
Please use `emacs -q` and load a minimal setup with only popweb to verify that the bug is reproducible. If `emacs -q` works fine, probably something is wrong with your Emacs config.

If the problem persists, please report it [here](https://github.com/manateelazycat/popweb/issues/new) with `*popweb*` buffer content, it contains many clues that can help us locate the problem faster.

If you get a segfault error, please use the following way to collect crash information:
1. Install gdb and turn on option `popweb-enable-debug`
2. Use the command `popweb-stop-process` to stop the current process
3. Restart popweb, send issue with `*popweb*` buffer content when next crash

## Contributor
<a href = "https://github.com/manateelazycat/popweb/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/popweb"/>
</a>
