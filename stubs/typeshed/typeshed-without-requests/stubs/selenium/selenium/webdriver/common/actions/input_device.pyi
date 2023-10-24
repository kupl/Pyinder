from typing import Any

class InputDevice:
    name: Any
    actions: Any
    def __init__(self, name: Any | None = ...) -> None: ...
    def add_action(self, action) -> None: ...
    def clear_actions(self) -> None: ...
    def create_pause(self, duraton: int = ...) -> None: ...
