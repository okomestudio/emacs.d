#!/usr/bin/env bash

set -e

readonly scriptname="${0##*/}"


function usage() {
  cat <<USAGE >&2
Usage: $scriptname arg1 [optarg2 ...]

This is a template for bash script.

  arg1     Required argument
  optarg2  Optional argument description

USAGE
  exit "${1:-1}"
}


function error() {
  >&2 echo "$scriptname: $1"
  >&2 echo "Try '$scriptname -h' for more information."
  exit "${2:-1}"
}


function main() {
  echo "$@"
}


if [ "$0" = "${BASH_SOURCE[0]}" ]; then
  while getopts "ha:" opt; do
    case $opt in
      a)
        optarg=$OPTARG
        echo "$optarg"
        ;;
      h|\?)
        if [ "$opt" = "h" ]; then usage 0; else usage; fi
        ;;
    esac
  done
  shift $((OPTIND - 1))

  main "$@"
fi
