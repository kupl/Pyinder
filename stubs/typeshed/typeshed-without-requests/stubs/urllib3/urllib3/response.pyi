import io
from _typeshed import Self
from http.client import HTTPMessage as _HttplibHTTPMessage, HTTPResponse as _HttplibHTTPResponse
from typing import Any, Iterator, Mapping
from typing_extensions import Literal

from urllib3.connectionpool import HTTPConnection

from . import HTTPConnectionPool, Retry
from ._collections import HTTPHeaderDict
from .connection import _TYPE_BODY

class DeflateDecoder:
    def __init__(self) -> None: ...
    def __getattr__(self, name: str) -> Any: ...
    def decompress(self, data: bytes) -> bytes: ...

class GzipDecoderState:
    FIRST_MEMBER: Literal[0]
    OTHER_MEMBERS: Literal[1]
    SWALLOW_DATA: Literal[2]

class GzipDecoder:
    def __init__(self) -> None: ...
    def __getattr__(self, name: str) -> Any: ...
    def decompress(self, data: bytes) -> bytes: ...

# This class is only available if
# `brotli` is available for import.
class BrotliDecoder:
    def __init__(self) -> None: ...
    def flush(self) -> bytes: ...

class MultiDecoder:
    def __init__(self, modes: str) -> None: ...
    def flush(self) -> bytes: ...
    def decompress(self, data: bytes) -> bytes: ...

class HTTPResponse(io.IOBase):
    CONTENT_DECODERS: list[str]
    REDIRECT_STATUSES: list[int]
    headers: HTTPHeaderDict
    status: int
    version: int
    reason: str | None
    strict: int
    decode_content: bool
    retries: Retry | None
    enforce_content_length: bool
    auto_close: bool
    msg: _HttplibHTTPMessage | None
    chunked: bool
    chunk_left: int | None
    length_remaining: int | None
    def __init__(
        self,
        body: _TYPE_BODY = ...,
        headers: Mapping[str, str] | Mapping[bytes, bytes] | None = ...,
        status: int = ...,
        version: int = ...,
        reason: str | None = ...,
        strict: int = ...,
        preload_content: bool = ...,
        decode_content: bool = ...,
        original_response: _HttplibHTTPResponse | None = ...,
        pool: HTTPConnectionPool | None = ...,
        connection: HTTPConnection | None = ...,
        msg: _HttplibHTTPMessage | None = ...,
        retries: Retry | None = ...,
        enforce_content_length: bool = ...,
        request_method: str | None = ...,
        request_url: str | None = ...,
        auto_close: bool = ...,
    ) -> None: ...
    def get_redirect_location(self) -> Literal[False] | str | None: ...
    def release_conn(self) -> None: ...
    def drain_conn(self) -> None: ...
    @property
    def data(self) -> bytes | Any: ...
    @property
    def connection(self) -> HTTPConnection | Any: ...
    def isclosed(self) -> bool: ...
    def tell(self) -> int: ...
    def read(self, amt: int | None = ..., decode_content: bool | None = ..., cache_content: bool = ...) -> bytes: ...
    def stream(self, amt: int | None = ..., decode_content: bool | None = ...) -> Iterator[bytes]: ...
    @classmethod
    def from_httplib(cls: type[Self], r: _HttplibHTTPResponse, **response_kw: Any) -> Self: ...
    def getheaders(self) -> HTTPHeaderDict: ...
    def getheader(self, name, default=...) -> str | None: ...
    def info(self) -> HTTPHeaderDict: ...
    def close(self) -> None: ...
    @property
    def closed(self) -> bool: ...
    def fileno(self) -> int: ...
    def flush(self) -> None: ...
    def readable(self) -> bool: ...
    def readinto(self, b: bytearray) -> int: ...
    def supports_chunked_reads(self) -> bool: ...
    def read_chunked(self, amt: int | None = ..., decode_content: bool | None = ...) -> Iterator[bytes]: ...
    def geturl(self) -> bool | str: ...
