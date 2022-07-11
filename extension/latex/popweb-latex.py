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

from PyQt6.QtCore import QUrl, QTimer
import os

def adjust_latex_window(popweb, web_window, window_x, window_y, x_offset, y_offset, frame_x, frame_y, frame_w, frame_h, show_window, new_latex):
    if new_latex == True:
        render_width = web_window.web_page.execute_javascript("document.getElementById('katex-preview').offsetWidth;")
        render_height = web_window.web_page.execute_javascript("document.getElementById('katex-preview').offsetHeight;")
        if render_width == None or render_height == None:
            render_width = 0
            render_height = 0

        render_width = int(render_width * web_window.zoom_factor * 1.2)
        render_height = int(render_height * web_window.zoom_factor)

        web_window.render_width = render_width
        web_window.render_height = render_height
        web_window.update_theme_mode()
        web_window.resize(render_width, render_height)

    window_x, window_y = popweb.adjust_render_pos(window_x, window_y, x_offset, y_offset, web_window.render_width, web_window.render_height, frame_x, frame_y, frame_w, frame_h)
    web_window.move(window_x, window_y)

    if show_window:
        web_window.show()

def pop_latex_window(popweb, module_name, index_file, x, y, x_offset, y_offset, frame_x, frame_y, frame_w, frame_h, show_window, new_latex, latex_string):
    web_window = popweb.get_web_window(module_name)
    index_html = open(index_file, "r").read().replace(
        "BACKGROUND", popweb.get_emacs_func_result("popweb-get-theme-background", [])).replace(
            "INDEX_DIR", os.path.dirname(index_file)).replace(
                "LATEX", latex_string)
    web_window.loading_js_code = ""
    if new_latex == True:
        web_window.webview.setHtml(index_html, QUrl("file://"))

    QTimer().singleShot(100, lambda : adjust_latex_window(popweb, web_window, x + x_offset, y + y_offset, x_offset, y_offset, frame_x, frame_y, frame_w, frame_h, show_window, new_latex))
