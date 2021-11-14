#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2021 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# NOTE
# QtWebEngine will throw error "ImportError: QtWebEngineWidgets must be imported before a QCoreApplication instance is created"
from PyQt5.QtWebEngineWidgets import QWebEngineView

from PyQt5 import QtCore
from PyQt5.QtCore import QUrl, Qt
from PyQt5.QtNetwork import QNetworkProxy, QNetworkProxyFactory
from PyQt5.QtWidgets import QHBoxLayout, QPushButton, QWidget, QApplication, QVBoxLayout, QMessageBox
from PyQt5.QtWebEngineWidgets import QWebEngineView, QWebEnginePage, QWebEngineScript, QWebEngineProfile, QWebEngineSettings
from epc.server import ThreadingEPCServer
from functools import wraps
from epc.client import EPCClient
import sys
import base64
import functools
import logging
import os
import platform
import threading

class PostGui(QtCore.QObject):

    through_thread = QtCore.pyqtSignal(object, object)

    def __init__(self, inclass=True):
        super(PostGui, self).__init__()
        self.through_thread.connect(self.on_signal_received)
        self.inclass = inclass

    def __call__(self, func):
        self._func = func

        @functools.wraps(func)
        def obj_call(*args, **kwargs):
            self.emit_signal(args, kwargs)
        return obj_call

    def emit_signal(self, args, kwargs):
        self.through_thread.emit(args, kwargs)

    def on_signal_received(self, args, kwargs):
        if self.inclass:
            obj, args = args[0], args[1:]
            self._func(obj, *args, **kwargs)
        else:
            self._func(*args, **kwargs)

epc_client = None

def init_epc_client(emacs_server_port):
    global epc_client

    if epc_client == None:
        try:
            epc_client = EPCClient(("localhost", emacs_server_port), log_traceback=True)
        except ConnectionRefusedError:
            import traceback
            traceback.print_exc()

def close_epc_client():
    global epc_client

    if epc_client != None:
        epc_client.close()

def convert_arg_to_str(arg):
    if type(arg) == str:
        return arg
    elif type(arg) == bool:
        arg = str(arg).upper()
    elif type(arg) == list:
        new_arg = ""
        for a in arg:
            new_arg = new_arg + " " + convert_arg_to_str(a)
        arg = "(" + new_arg[1:] + ")"
    return arg

def string_to_base64(text):
    return str(base64.b64encode(str(text).encode("utf-8")), "utf-8")

def eval_in_emacs(method_name, args):
    global epc_client

    if epc_client == None:
        print("Please call init_epc_client first before callling eval_in_emacs.")
    else:
        args = list(map(convert_arg_to_str, args))
        # Make argument encode with Base64, avoid string quote problem pass to elisp side.
        args = list(map(string_to_base64, args))

        args.insert(0, method_name)

        # Call eval-in-emacs elisp function.
        epc_client.call("eval-in-emacs", args)

def get_emacs_vars(args):
    global epc_client

    return list(map(lambda result: result if result != [] else False, epc_client.call_sync("get-emacs-vars", args)))

class WebWindow(QWidget):
    def __init__(self):
        super().__init__()
        global screen_size

        self.setWindowFlags(Qt.FramelessWindowHint | Qt.WindowStaysOnTopHint | Qt.ToolTip)
        self.setContentsMargins(0, 0, 0, 0)

        self.vbox = QVBoxLayout(self)

        self.zoom_factor = 1
        if screen_size.width() > 3000:
            self.zoom_factor = 2

        self.js_code = ""

        self.webview = QWebEngineView()
        self.webview.loadStarted.connect(lambda : self.webview.setZoomFactor(self.zoom_factor))
        self.webview.loadProgress.connect(lambda progress: self.execute_js_code())
        self.webview.setZoomFactor(self.zoom_factor)

        self.vbox.addWidget(self.webview)
        self.setLayout(self.vbox)

        self.webview.installEventFilter(self)

        self.settings = QWebEngineSettings.globalSettings()
        try:
            self.settings.setAttribute(QWebEngineSettings.FullScreenSupportEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.DnsPrefetchEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.FocusOnNavigationEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.PlaybackRequiresUserGesture, False)
            self.settings.setAttribute(QWebEngineSettings.PluginsEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.JavascriptEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.ShowScrollBars, False)
        except Exception:
            import traceback
            traceback.print_exc()

    def execute_js_code(self):
        if self.js_code != "":
            self.webview.page().runJavaScript(self.js_code)

    def eventFilter(self, source, event):
        if event.type() == QtCore.QEvent.WindowDeactivate:
            self.hide()
            return True

        return super(WebWindow, self).eventFilter(source, event)

class POPWEB(object):
    def __init__(self, args):
        global proxy_string

        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        # self.server = ThreadingEPCServer(('localhost', 0)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.join(popweb_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)

        self.server.register_instance(self) # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        self.web_window = WebWindow()

        # Pass epc port and webengine codec information to Emacs when first start POPWEB.
        eval_in_emacs('popweb--first-start', [self.server.server_address[1]])

        # Disable use system proxy, avoid page slow when no network connected.
        QNetworkProxyFactory.setUseSystemConfiguration(False)

        # Set Network proxy.
        (proxy_host, proxy_port, proxy_type) = get_emacs_vars([
            "popweb-proxy-host",
            "popweb-proxy-port",
            "popweb-proxy-type"])

        self.proxy = (proxy_type, proxy_host, proxy_port)
        self.is_proxy = False

    def enable_proxy(self):
        global proxy_string

        proxy_string = "{0}://{1}:{2}".format(self.proxy[0], self.proxy[1], self.proxy[2])

        proxy = QNetworkProxy()
        if self.proxy[0] == "socks5":
            proxy.setType(QNetworkProxy.Socks5Proxy)
        elif self.proxy[0] == "http":
            proxy.setType(QNetworkProxy.HttpProxy)
        proxy.setHostName(self.proxy[1])
        proxy.setPort(int(self.proxy[2]))

        self.is_proxy = True
        QNetworkProxy.setApplicationProxy(proxy)

    def disable_proxy(self):
        global proxy_string

        proxy_string = ""

        proxy = QNetworkProxy()
        proxy.setType(QNetworkProxy.NoProxy)

        self.is_proxy = False
        QNetworkProxy.setApplicationProxy(proxy)

    def toggle_proxy(self):
        if self.is_proxy:
            self.disable_proxy()
        else:
            self.enable_proxy()

    @PostGui()
    def pop_web_window(self, x, y, offset, width_scale, height_scale, url, js_code, use_proxy):
        global screen_size

        if use_proxy == "true":
            self.enable_proxy()
        else:
            self.disable_proxy()

        self.web_window.js_code = js_code
        self.web_window.webview.load(QUrl(url))

        window_width = screen_size.width() * width_scale
        window_height = screen_size.height() * height_scale
        window_x = x + offset
        if window_x + window_width > screen_size.width():
            window_x = x - window_width - offset
        window_y = y + offset
        if window_y + window_height > screen_size.height():
            window_y = y - window_height - offset

        self.web_window.resize(window_width, window_height)
        self.web_window.move(window_x, window_y)
        self.web_window.show()

    @PostGui()
    def hide_web_window(self):
        self.web_window.hide()
        self.web_window.webview.load(QUrl(""))

    def cleanup(self):
        '''Do some cleanup before exit python process.'''
        close_epc_client()

if __name__ == "__main__":
    import sys
    import signal

    proxy_string = ""

    destroy_view_list = []

    hardware_acceleration_args = []
    if platform.system() != "Windows":
        hardware_acceleration_args += [
            "--ignore-gpu-blocklist",
            "--enable-gpu-rasterization",
            "--enable-native-gpu-memory-buffers"]

    app = QApplication(sys.argv + ["--disable-web-security"] + hardware_acceleration_args)
    screen = app.primaryScreen()
    screen_size = screen.size()

    popweb = POPWEB(sys.argv[1:])

    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec_())
