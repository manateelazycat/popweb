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

def adjust_color_picker_window(popweb, web_window, window_x, window_y, x_offset, y_offset, frame_x, frame_y, frame_w, frame_h):
    render_width = 200
    render_height = 309

    render_width = int(render_width * web_window.zoom_factor)
    render_height = int(render_height * web_window.zoom_factor)

    web_window.render_width = render_width
    web_window.render_height = render_height
    web_window.update_theme_mode()
    web_window.resize(render_width, render_height)

    window_x, window_y = popweb.adjust_render_pos(window_x, window_y, x_offset, y_offset,
                                                  web_window.render_width, web_window.render_height,
                                                  frame_x, frame_y, frame_w, frame_h)
    web_window.move(window_x, window_y)

    web_window.popup()

def pop_color_picker_window(popweb, module_name, index_file, x, y, x_offset, y_offset, frame_x, frame_y, frame_w, frame_h, init_color):
    web_window = popweb.get_web_window(module_name)
    index_html = open(index_file, "r").read().replace(
        "INDEX_DIR", os.path.dirname(index_file)).replace(
            "DEFAULT_COLOR", init_color if init_color != "" else "#000000")

    web_window.loading_js_code = ""
    web_window.webview.setHtml(index_html, QUrl("file://"))

    QTimer().singleShot(100, lambda : adjust_color_picker_window(popweb, web_window, x + x_offset, y + y_offset, x_offset, y_offset,
                                                                 frame_x, frame_y, frame_w, frame_h))
