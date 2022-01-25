#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from PyQt5.QtCore import QUrl, QTimer
import os


def adjust_org_roam_link_window(popweb, web_window, window_x, window_y,
                                x_offset, y_offset, frame_x, frame_y, frame_w,
                                frame_h, show_window, new_html):
    if show_window:
        web_window.show()


def pop_org_roam_link_window(popweb, module_name, index_file, x, y, x_offset,
                             y_offset, frame_x, frame_y, frame_w, frame_h,
                             show_window, new_html, html_string):
    web_window = popweb.get_web_window(module_name)
    web_window.loading_js_code = ""
    if new_html == True:
        web_window.webview.setHtml(html_string, QUrl(index_file))

    QTimer().singleShot(
        100, lambda: adjust_org_roam_link_window(
            popweb, web_window, x + x_offset, y + y_offset, x_offset, y_offset,
            frame_x, frame_y, frame_w, frame_h, show_window, new_html))
