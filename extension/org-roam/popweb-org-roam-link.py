#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from PyQt6.QtCore import QUrl, QTimer
import os


def pop_org_roam_link_window(popweb, module_name, index_file, x, y, x_offset,
                             y_offset, frame_x, frame_y, frame_w, frame_h,
                             width_scale, height_scale, show_window, new_html,
                             html_string):
    web_window = popweb.get_web_window(module_name)
    window_width = frame_w * width_scale
    window_height = frame_h * height_scale
    window_x, window_y = popweb.adjust_render_pos(x + x_offset, y + y_offset,
                                                  x_offset, y_offset,
                                                  window_width, window_height,
                                                  frame_x, frame_y, frame_w,
                                                  frame_h)
    web_window.webview.setHtml(html_string, QUrl(index_file))
    web_window.update_theme_mode()
    web_window.resize(int(window_width), int(window_height))
    web_window.move(int(window_x), int(window_y))
    web_window.show()
