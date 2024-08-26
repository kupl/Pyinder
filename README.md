Pyinder is built on top of [Pyre](https://github.com/facebook/pyre-check) that is a type analysis tool developed by [Meta](https://github.com/facebook).
If you are interested in using our tool, please read our [artifact](https://github.com/kupl/PyinderArtifact). 

## Requirements
We tested Pyinder on *Ubuntu 22.04 LTS*.
- Python 3.8 or later
- ocaml-4.10 (with opam)

## Install Pyinder

```bash
git clone https://github.com/kupl/Pyinder
cd <Pyinder-path>/source
make
```

## Run Pyinder

First, you need to make a configuration file `.pyre_configuration` such as:

```json
{
  "source_directories": [
    {
        "root": "<source-root-directory-1>",
        "subdirectory": "<source-sub-directory-1>"
    },
    {
        "root": "<source-root-directory-2>",
        "subdirectory": "<source-sub-directory-2>"
    },
  ],
  "exclude": ["<path-to-exclude-1>", "<path-to-exclude-2>"],
  "strict": true,
  "taint_models_path": ".",
  "typeshed": "<Pyinder-path>/stubs/typeshed/typeshed-master"
}
```

Then, you can run Pyinder from the directory where the `.pyre_configuration` file is located as follows:

```bash
cd <directory-where-.pyre_configuration-is-located>
pyinder mine
```

