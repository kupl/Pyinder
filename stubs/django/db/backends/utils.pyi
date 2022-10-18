# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

class CursorWrapper:
    def __init__(self, cursor, db): ...
    def __enter__(self) -> "CursorWrapper": ...
    def execute(self, sql, params=None): ...
    def executemany(self, sql, param_list): ...
    def fetchone(self): ...
