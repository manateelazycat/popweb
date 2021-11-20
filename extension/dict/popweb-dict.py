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

from PyQt5.QtCore import QUrl

def pop_translate_window(popweb, module_name, x, y, x_offset, y_offset, width_scale, height_scale, url, loading_js_code):
    screen_size = popweb.get_screen_size()
    web_window = popweb.get_web_window(module_name)

    window_width = screen_size.width() * width_scale
    window_height = screen_size.height() * height_scale
    window_x = x + x_offset
    if window_x + window_width > screen_size.width():
        window_x = x - window_width - x_offset
    window_y = y + y_offset
    if window_y + window_height > screen_size.height():
        window_y = y - window_height

    web_window.loading_js_code = loading_js_code
    web_window.webview.load(QUrl(url))
    web_window.update_theme_mode()
    web_window.resize(window_width, window_height)
    web_window.move(window_x, window_y)
    web_window.show()
