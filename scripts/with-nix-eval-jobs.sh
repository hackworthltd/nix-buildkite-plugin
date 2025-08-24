# shellcheck shell=bash

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <expr>" >&2
  exit 1
fi

: "${NIX_EVAL_JOBS_ARGS:=}"
: "${JQ_OPTS:=-re}"
: "${JQ_FILTER:=try (.drvPath) catch halt_error}"

read -r -a nix_eval_job_args <<< "$NIX_EVAL_JOBS_ARGS"
read -r -a jq_opts <<< "$JQ_OPTS"

nix-eval-jobs "$1" "${nix_eval_job_args[@]}" | jq "${jq_opts[@]}" "$JQ_FILTER" | nix-buildkite
