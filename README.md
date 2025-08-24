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
      - hackworthltd/nix#v2.0.0:
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

## Post-build hooks

The plugin accepts an optional `post-build-hook` argument, whose value
is the name (or path) of an executable that's compatible with Nix's
[post-build hook
semantics](https://nixos.org/manual/nix/stable/advanced-topics/post-build-hook.html):

```yaml
steps:
  - command: nix-buildkite
    label: ":nixos: :buildkite:"
    plugins:
      - hackworthltd/nix#v2.0.0:
          expr: jobs.nix
          post-build-hook: /etc/nix/upload-to-cache.sh
```

When specified, the plugin will run this hook after its
`nix-instantiate` phase, and after each individual job that it
creates. This option is useful when you want to take advantage of
Nix's post-build hook feature (e.g., to upload the derivations created
by the pipeline), but you don't want to enable a system-wide
post-build hook. For example, you might only want to upload some
pipelines' outputs to your binary cache, or you might want to upload
different pipelines' outputs to different binary caches.

Note that in order to use this feature, you'll need to add the user
that the Buildkite agent runs as to `nix.trustedUsers`, as only
trusted users can run post-build hooks.
