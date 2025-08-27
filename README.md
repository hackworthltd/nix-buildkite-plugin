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

# FAQ

## Should I use `nix build`, or `nix-store`?

When `use-nix-build` is `false` (the default), the plugin will evaluate the top level Nix expression, and generate a Buildkite pipeline where each step in the pipeline uses `nix-store -r` to build the resulting Nix derivations (e.g., `nix-store -r /nix/store/awj3yp49zvak4qnilr4cva6mv80hld37-pre-commit-run.drv`).

When `use-nix-build` is `true`, the plugin will evaluate the top level Nix expression, and generate a Buildkite pipeline where each step in the pipeline uses `nix build` to build the expression's installable Nix attributes (e.g., `nix build .#checks.x86_64-linux:pre-commit-run`).

Whether to use `nix build` or `nix-store` depends on many factors:

- The primary advantage of using `nix-store` is that the build will only run the Nix evaluation phase once, at the beginning of the build in order to generate the derivations. After that, Nix only builds derivations.

- `nix build` can't build derivations, only Nix expressions that produce derivations. In this mode, the plugin runs a Nix evaluation at the beginning of the build in order to generate the necessary `nix build` commands, and then each one of those `nix build` commands will _also_ run a Nix evaluation to reduce the constituent installable Nix expressions to their derivations.

- The individual `nix build` commands will generally evaluate much faster than the top level Nix expression in the initial Nix evaluation phase, but all else being equal, each build step will almost certainly take longer than the equivalent build step that uses `nix-store` on the derivation because of the additional time spent in Nix evaluation.

- The difference between `nix build` and `nix-store` in step start-up times is particularly evident when the derivation has already been built. However, by default the plugin will check whether the derivation has already been built, and if so, skip the build (whether via `nix build` or `nix-store`), which eliminates any advantage that using `nix-store` might otherwise have for already-built derivations.

- `nix-store -r` only works when the given derivation's dependencies are already in the local Nix store. Therefore, in order to use this plugin in `nix-store` mode, you must ensure that all steps are run against a single Nix store. In practice, this likely means that either the build must run on a single host, or you're using a distributed filesystem such as NFS to share a Nix store across multiple systems.

- `nix build` will fetch all of its input expression's dependencies before starting a build, so when using this plugin with `use-nix-build: true`, you can take advantage of multiple builders irrespective of whether they share a Nix store.

In general, our advice is:

- If you only have a single Nix build host, or if you have a shared Nix store across multiple build hosts, you should probably set `use-nix-build: false` (the default), as you only need to run the Nix evaluation step once in this mode.

- If you have multiple Nix build hosts that don't share a Nix store, and you need to build multiple derivations with non-trivial build times, you may benefit from `use-nix-build: true`. However, if the derivations in your project tend to build quickly &mdash; say, on the order of a few seconds &mdash; then you may still be better off setting `use-nix-build: false` and running your builds on a single build host, because any benefits you might get from distributing your builds may be swamped by Nix evaluation times, and/or by the overhead of copying derivations from your binary cache to your builders.
