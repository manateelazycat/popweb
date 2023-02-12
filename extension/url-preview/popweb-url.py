from PyQt6.QtCore import QUrl

def pop_url_window(popweb, module_name, x, y, x_offset, y_offset, frame_x, frame_y, frame_w, frame_h, width_scale, height_scale, width_absolute, height_absolute, use_absolute, url):
    web_window = popweb.get_web_window(module_name)
    if not use_absolute:
        window_width = frame_w * width_scale
        window_height = frame_h * height_scale
    else:
        window_width = width_absolute
        window_height = height_absolute
    window_x, window_y = popweb.adjust_render_pos(x + x_offset, y + y_offset, x_offset, y_offset, window_width, window_height, frame_x, frame_y, frame_w, frame_h)

    web_window.webview.setUrl(QUrl(url))
    web_window.update_theme_mode()
    web_window.resize(int(window_width), int(window_height))
    web_window.move(int(window_x), int(window_y))
    web_window.popup()
