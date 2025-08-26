# nix-buildkite-plugin

`nix-buildkite-plugin` is a Buildkite plugin that can take a Nix
expression that describes a set of builds and transforms them into
separate Buildkite jobs. `nix-buildkite-plugin` evaluates Nix to
create derivations and then analyses these derivations to find a
topological ordering that will ensure steps have the correct
dependencies between them.

Note: this project is a fork of [Circuithub's
`nix-buildkite-buildkite-plugin`](https://github.com/circuithub/nix-buildkite-buildkite-plugin),
and we would like to thank them for their work, without which this
project wouldn't be possible!

# Usage

To use the plugin, add a `.buildkite/pipeline.yml` file to your repo.
The plugin will use
[`nix-eval-jobs`](https://github.com/nix-community/nix-eval-jobs) to
evaluate the given expression and generate a list of derivations to
build. For example, here we use a file named `jobs.nix` which, when
evaluated, will produce a list of derivations:

```yaml
steps:
  - command: nix-buildkite
    label: ":nixos: :buildkite:"
    plugins:
      - hackworthltd/nix#v2.1.0:
          expr: jobs.nix
```

Next, configure your Buildkite pipeline to upload the dynamic pipeline
created by the plugin when builds are triggered on your repo. See
https://buildkite.com/docs/pipelines/defining-steps#getting-started
for details on how to do this.

Your static Buildkite pipeline should look something like this:

```yaml
steps:
  - command: buildkite-agent pipeline upload
    label: ":pipeline:"
```

# Plugin options

In addition to the mandatory `expr` argument, the plugin supports the
following additional options:

## `use-nix-build`

If `false` (the default), the plugin will generate a Buildkite pipeline that uses `nix-store -r` to "realize" each derivation in the evaluated Nix expression.

If `true`, the plugin will instead generate a Buildkite pipeline that uses `nix build` to build each derivation's corresponding Nix attribute. `nix build` is generally more flexible than `nix-store`, so you may prefer this mode.

## `ignore-cache-status`

By default, the plugin will not bother building any derivation that's already available in a binary cache or the local Nix store. (Buildkite will still report these derivations as having been successfully built, however.) If you want to ignore all derivations' cache status and run their corresponding `nix build` or `nix-store` command anyway, set `ignore-cache-status` to `true`.

## `attr-prefix`

When `use-nix-build` is `true`, the plugin prefixes each built attribute with the string provided in this option. (The default value of this option is the empty string `""`.) This is required when building a flake, because `nix-eval-jobs` does not include the input flake expression in the attributes produced by its output.

For example, to build the `.#hydraJobs` attribute of the checked-out flake using `nix build`:

```yaml
steps:
  - command: nix-buildkite
	label: ":nixos: :buildkite:"
	plugins:
		- hackworthltd/nix#v2.1.0:
			  expr: ".#hydraJobs"
			  use-nix-build: "true"
			  attr-prefix: ".#hydraJobs."
			  nix-eval-jobs-args: --workers 8 --max-memory-size 8GiB --flake --force-recurse
```

(In general, when building a flake from the checked-out repository, the value of this option should be the flake expression followed by `.`.)

Note that this option is ignored when `use-nix-build` is `false`.

## `nix-eval-jobs-args`

These are passed to `nix-eval-jobs`. For example, to evaluate a
flake's `hydraJobs` output and increase the available resources for
`nix-eval-jobs`:

```yaml
steps:
  - command: nix-buildkite
    label: ":nixos: :buildkite:"
    plugins:
      - hackworthltd/nix#v2.1.0:
          expr: ".#hydraJobs"
		  nix-eval-jobs-args: --workers 8 --max-memory-size 8GiB --flake --force-recurse
```

## `jq-filter`

The plugin uses `jq` to convert the JSON output of `nix-eval-jobs` to
a list of derivations. The default `jq` filter is `try .drvPath catch halt_error`, or `try ([.drvPath, .attr] | join(" ")) catch halt_error` if `use-nix-build` is `true`, but you can change it via this plugin option. For example, assuming your flake's `hydraJobs` has a `required` attribute
that aggregates a number of other flake outputs, you may want to add the job's `constituents` to the list of derivations to build:

```yaml
steps:
  - command: nix-buildkite
    label: ":nixos: :buildkite:"
    plugins:
      - hackworthltd/nix#v2.1.0:
          expr: ".#hydraJobs.required"
          nix-eval-jobs-args: --workers 8 --max-memory-size 8GiB --flake --constituents
          jq-filter: .drvPath, .constituents[]
```

## `jq-opts`

The plugin runs `jq -re` by default, but you can change the `-re` options to `jq` via this plugin option.

## `nix-build-opts`

When `use-nix-build` is `true`, then when the plugin generates the dynamic Buildkite plugin, it uses `nix build <attr>` to build each attribute `<attr>` that results from evaluating the Nix expression specified by the plugin's `expr` option. If you want to pass additional options to every instantiation of `nix build`, you can pass those additional arguments here. For example, to use a Nix post-build hook:

```yaml
steps:
  - command: nix-buildkite
    label: ":nixos: :buildkite:"
    plugins:
      - hackworthltd/nix#v2.1.0:
          expr: jobs.nix
          nix-build-opts: --post-build-hook /etc/nix/upload-to-cache.sh
```

Note that this option is ignored when `use-nix-build` is `false`.

## `nix-store-opts`

When `use-nix-build` is `false`, then when the plugin generates the dynamic Buildkite plugin, it uses `nix-store -r <drv>` to "realize" each derivation `<drv>` that results from evaluating the Nix expression specified by the plugin's `expr` option. If you want to pass additional options to every instantiation of `nix-store`, you can pass those additional arguments here. For example, to use a Nix post-build hook:

```yaml
steps:
  - command: nix-buildkite
    label: ":nixos: :buildkite:"
    plugins:
      - hackworthltd/nix#v2.1.0:
          expr: jobs.nix
          nix-store-opts: --post-build-hook /etc/nix/upload-to-cache.sh
```

Note that this option is ignored when `use-nix-build` is `true`.
