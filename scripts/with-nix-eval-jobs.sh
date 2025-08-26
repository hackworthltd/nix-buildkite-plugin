# shellcheck shell=bash

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <expr>" >&2
  exit 1
fi

: "${NIX_EVAL_JOBS_ARGS:=}"
: "${JQ_OPTS:=-re}"

if [[ -n "${USE_NIX_BUILD:-}" ]]; then
    default_jq_filter='try ([.drvPath, .attr, .cacheStatus] | join(" ")) catch halt_error'
else
    default_jq_filter='try ([.drvPath, .cacheStatus] | join(" ")) catch halt_error'
fi
: "${JQ_FILTER:=$default_jq_filter}"

read -r -a nix_eval_job_args <<< "$NIX_EVAL_JOBS_ARGS"
read -r -a jq_opts <<< "$JQ_OPTS"

nix-eval-jobs "$1" --check-cache-status "${nix_eval_job_args[@]}" | jq "${jq_opts[@]}" "$JQ_FILTER" | nix-buildkite
