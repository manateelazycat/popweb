# What is popweb?
I want a pop-up window display web multimedia content, such as, translation tooltip, LaTex preview, code completion etc.

Browser related technology comes from my other project [Emacs Application Framework](https://github.com/emacs-eaf/emacs-application-framework)

## Installation
1. Make sure install PyQt5 from your operating system repository (DON'T install PyQt5 from pip, otherwise browser will crash)
2. Install library python-epc
3. Clone or download this repository (path of the folder is the `<path-to-popweb>` used below).
4. In your `~/.emacs`, add the following two lines:
```elisp
(add-to-list 'load-path "<path-to-popweb>") ; add popweb to your load-path
(require 'popweb-dict-bing)
(require 'popweb-dict-youdao)
(require 'popweb-katex)
```

## Usage
* popweb-dict-bing-input: popup input translation window with Bing
* popweb-dict-youdao-input: popup input translation window with Youdao
* popweb-dict-bing-pointer: popup pointer translation window with Bing
* popweb-dict-youdao-pointer: popup pointer translation window with Youdao
* popweb-katex-show: show LaTex preview
* popweb-katex-update: update LaTex preview
* popweb-katex-hide: hide LaTex preview

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

## Popweb with KaTex.js preview.
<p align="center">
  <img width="800" src="./img/katex-preview.png">
</p>
