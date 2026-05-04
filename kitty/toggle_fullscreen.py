from kittens.tui.handler import result_handler


def main(args):
    pass


@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    w = boss.window_id_map.get(target_window_id)
    if w is None:
        return

    os_window_id = w.os_window_id
    tm = boss.os_window_map.get(os_window_id)
    if tm is None:
        return

    tm.tab_bar_hidden = not tm.tab_bar_hidden
    tm.layout_tab_bar()
    boss.toggle_fullscreen()
