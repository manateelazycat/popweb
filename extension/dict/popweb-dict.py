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

from PyQt6.QtCore import QUrl

def pop_translate_window(popweb, module_name, x, y, x_offset, y_offset, frame_x, frame_y, frame_w, frame_h, width_scale, height_scale, url, loading_js_code):
    web_window = popweb.get_web_window(module_name)
    window_width = frame_w * width_scale
    window_height = frame_h * height_scale
    window_x, window_y = popweb.adjust_render_pos(x + x_offset, y + y_offset, x_offset, y_offset, window_width, window_height, frame_x, frame_y, frame_w, frame_h)

    web_window.loading_js_code = loading_js_code
    web_window.webview.load(QUrl(url))
    web_window.update_theme_mode()
    web_window.resize(int(window_width), int(window_height))
    web_window.move(int(window_x), int(window_y))
    web_window.show()
