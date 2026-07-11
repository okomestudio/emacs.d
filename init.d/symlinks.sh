#!/usr/bin/env bash

set -e

readonly scriptname="${0##*/}"

# Map priorities to Emacs Lisp files in the `lisp' directory
declare -A ptom

function usage() {
  cat <<USAGE >&2
Usage: $scriptname [-dh] [unit]

Create or remove symlinks under init.d/<unit>.

  -d  Remove symlinks
  -h  Show help
  -r  Refresh (remove and create) symlinks

USAGE
  exit "${1:-1}"
}

function error() {
  >&2 echo "$scriptname: $1"
  >&2 echo "Try '$scriptname -h' for more information."
  exit "${2:-1}"
}

function create_symlinks() {
  local unit="${1:-default}"
  source "./init.d/unit-${unit}.sh"
  for priority in "${!ptom[@]}"; do
    for module in ${ptom[$priority]}; do
      src="../../lisp/${module}.el"
      if [[ $priority = linux-* ]] && [[ $module = linux-* ]]; then
        target="./init.d/$unit/$priority-${module#linux-}.el"
      else
        target="./init.d/$unit/$priority-$module.el"
      fi
      if [ ! -h "$target" ]; then
        mkdir -p "$(dirname "$target")"
        cmd="ln -s $src $target"
        $cmd
      fi
    done
  done
}

function delete_symlinks() {
  local unit="${1:-default}"
  if [ -d "./init.d/$unit" ]; then
    find "./init.d/$unit" -type l -exec rm {} \;
  fi
}

delete_mode=0

function main() {
  local unit="${1:-default}"
  if [ "$delete_mode" = 1 ]; then
    delete_symlinks "$unit"
  elif [ "$refresh_mode" = 1 ]; then
    delete_symlinks "$unit"
    create_symlinks "$unit"
  else
    create_symlinks "$unit"
  fi
}

if [ "$0" = "${BASH_SOURCE[0]}" ]; then
  while getopts "dhr" opt; do
    case $opt in
    d)
      delete_mode=1
      ;;
    h | \?)
      if [ "$opt" = "h" ]; then usage 0; else usage; fi
      ;;
    r)
      refresh_mode=1
      ;;
    esac
  done
  shift $((OPTIND - 1))

  main "$@"
fi
