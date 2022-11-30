# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import contextlib
import dataclasses
import json
import logging
import subprocess
from pathlib import Path
from typing import Any, Dict, Iterator, List, Sequence

from .. import command_arguments, configuration as configuration_module, error
from . import backend_arguments, commands, frontend_configuration, incremental, start


LOG: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass(frozen=True)
class Arguments:
    """
    Data structure for configuration options the backend check command can recognize.
    Need to keep in sync with `source/command/checkCommand.ml`
    """

    base_arguments: backend_arguments.BaseArguments

    additional_logging_sections: Sequence[str] = dataclasses.field(default_factory=list)
    show_error_traces: bool = False
    strict: bool = False

    taint_model_paths: Sequence[str] = dataclasses.field(default_factory=list)

    def serialize(self) -> Dict[str, Any]:
        return {
            **self.base_arguments.serialize(),
            "additional_logging_sections": self.additional_logging_sections,
            "show_error_traces": self.show_error_traces,
            "strict": self.strict,
            "taint_model_paths": self.taint_model_paths,
        }


def create_check_arguments(
    configuration: frontend_configuration.Base,
    check_arguments: command_arguments.CheckArguments,
) -> Arguments:
    """
    Translate client configurations to backend check configurations.

    This API is not pure since it needs to access filesystem to filter out
    nonexistent directories. It is idempotent though, since it does not alter
    any filesystem state.
    """
    source_paths = backend_arguments.get_source_path_for_check(configuration)

    logging_sections = check_arguments.logging_sections
    additional_logging_sections = (
        [] if logging_sections is None else logging_sections.split(",")
    )
    if check_arguments.noninteractive:
        additional_logging_sections.append("-progress")

    log_directory = configuration.get_log_directory()
    profiling_output = (
        backend_arguments.get_profiling_log_path(log_directory)
        if check_arguments.enable_profiling
        else None
    )
    memory_profiling_output = (
        backend_arguments.get_profiling_log_path(log_directory)
        if check_arguments.enable_memory_profiling
        else None
    )

    logger = configuration.get_remote_logger()
    remote_logging = (
        backend_arguments.RemoteLogging(
            logger=logger, identifier=check_arguments.log_identifier or ""
        )
        if logger is not None
        else None
    )

    taint_models_path = check_arguments.taint_models_path
    if len(taint_models_path) == 0:
        taint_models_path = configuration.get_taint_models_path()

    return Arguments(
        base_arguments=backend_arguments.BaseArguments(
            log_path=str(log_directory),
            global_root=str(configuration.get_global_root()),
            checked_directory_allowlist=backend_arguments.get_checked_directory_allowlist(
                configuration, source_paths
            ),
            checked_directory_blocklist=(configuration.get_ignore_all_errors()),
            debug=check_arguments.debug,
            excludes=configuration.get_excludes(),
            extensions=configuration.get_valid_extension_suffixes(),
            relative_local_root=configuration.get_relative_local_root(),
            memory_profiling_output=memory_profiling_output,
            number_of_workers=configuration.get_number_of_workers(),
            parallel=not check_arguments.sequential,
            profiling_output=profiling_output,
            python_version=configuration.get_python_version(),
            shared_memory=configuration.get_shared_memory(),
            remote_logging=remote_logging,
            search_paths=configuration.get_existent_search_paths(),
            source_paths=source_paths,
        ),
        additional_logging_sections=additional_logging_sections,
        show_error_traces=check_arguments.show_error_traces,
        strict=configuration.is_strict(),
        taint_model_paths=taint_models_path
    )


@contextlib.contextmanager
def create_check_arguments_and_cleanup(
    configuration: frontend_configuration.Base,
    check_arguments: command_arguments.CheckArguments,
) -> Iterator[Arguments]:
    arguments = create_check_arguments(configuration, check_arguments)
    try:
        yield arguments
    finally:
        # It is safe to clean up source paths after check command since
        # any created artifact directory won't be reused by other commands.
        arguments.base_arguments.source_paths.cleanup()


class InvalidCheckResponse(Exception):
    pass


def parse_type_error_response_json(response_json: object) -> List[error.Error]:
    try:
        # The response JSON is expected to have the following form:
        # `{"errors": [error_json0, error_json1, ...]}`
        if isinstance(response_json, dict):
            errors_json = response_json.get("errors", [])
            if isinstance(errors_json, list):
                return [error.Error.from_json(error_json) for error_json in errors_json]

        raise InvalidCheckResponse(
            f"Unexpected JSON response from check command: {response_json}"
        )
    except error.ErrorParsingFailure as parsing_error:
        message = f"Unexpected error JSON from check command: {parsing_error}"
        raise InvalidCheckResponse(message) from parsing_error


def parse_type_error_response(response: str) -> List[error.Error]:
    try:
        # split_json = response.split('\n')
        # response_json = json.loads(split_json[-2])
        response_json = json.loads(response)
        with open('result.json', 'w') as f :
            json.dump(response_json, f, indent=4)
        # response_json = json.loads(split_json[-1])
        return parse_type_error_response_json(response_json)
    except json.JSONDecodeError as decode_error:
        message = f"Cannot parse response as JSON: {decode_error}"
        raise InvalidCheckResponse(message) from decode_error


def _run_check_command(command: Sequence[str], output: str, mine: bool) -> commands.ExitCode:
    with backend_arguments.backend_log_file(prefix="pyre_check") as log_file:
        with start.background_logging(Path(log_file.name)):
            if mine :
                command[1] = 'newmine'
            # lint-ignore: NoUnsafeExecRule
            result = subprocess.run(
                command,
                stdout=subprocess.PIPE,
                stderr=log_file.file,
                universal_newlines=True,
                errors="replace",
            )
            return_code = result.returncode
            # Interpretation of the return code needs to be kept in sync with
            # `source/command/checkCommand.ml`.
            if return_code == 0:
                type_errors = parse_type_error_response(result.stdout)
                incremental.display_type_errors(type_errors, output=output)
                return (
                    commands.ExitCode.SUCCESS
                    if len(type_errors) == 0
                    else commands.ExitCode.FOUND_ERRORS
                )
            elif return_code == 2:
                LOG.error("Pyre encountered a failure within buck.")
                return commands.ExitCode.BUCK_INTERNAL_ERROR
            elif return_code == 3:
                LOG.error("Pyre encountered an error when building the buck targets.")
                return commands.ExitCode.BUCK_USER_ERROR
            else:
                print(result.stdout)
                print(result.stderr)
                LOG.error(
                    f"Check command exited with non-zero return code: {return_code}."
                )
                return commands.ExitCode.FAILURE


def run_check(
    configuration: configuration_module.Configuration,
    check_arguments: command_arguments.CheckArguments,
    mine
) -> commands.ExitCode:
    binary_location = configuration.get_binary_location(download_if_needed=True)
    if binary_location is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot locate a Pyre binary to run."
        )

    with create_check_arguments_and_cleanup(
        configuration, check_arguments
    ) as arguments:
        with backend_arguments.temporary_argument_file(arguments) as argument_file_path:
            check_command = [binary_location, "newcheck", str(argument_file_path)]
            return _run_check_command(
                command=check_command, output=check_arguments.output, mine=mine
            )


def run(
    configuration: configuration_module.Configuration,
    check_arguments: command_arguments.CheckArguments, mine=False
) -> commands.ExitCode:
    return run_check(frontend_configuration.OpenSource(configuration), check_arguments, mine)
