#!/usr/bin/env bash

set -e

# Maps priority to the modules found in the lisp directory
declare -A ptom
ptom["00"]="optimizations startup"
ptom["02"]="search"
ptom["04"]="consult embark subsys-help minibuffer navigation treemacs"
ptom["05"]="atomic-chrome"
ptom["06"]="faces themes"
ptom["10"]="ime projectile"
ptom["12"]="lsp"
ptom["13"]="lsp-grammarly lsp-ltex writing-en"
ptom["14"]="org"
ptom["15"]="org-roam"
ptom["20"]="auth-source dir-locals subsys-term"
ptom["40"]="completion editing flycheck git lookup polymode tesseract"
ptom["50"]="text-mode"
ptom["52"]="markdown rst yaml"
ptom["54"]="conf-mode"
ptom["60"]="prog-mode formatter"
ptom["62"]="c css docker elisp html javascript kotlin python rust scala shell sql"
ptom["64"]="ansible excalidraw graphviz json plantuml restclient"
ptom["70"]="browse-url epub openwith pdf"
ptom["80"]="ai anki elfeed emms eww games gnus osm"
ptom["88"]="dashboard"
ptom["linux-00"]="linux-gui"

# DO NOT MODIFY BELOW

readonly scriptname="${0##*/}"


function usage() {
  cat <<USAGE >&2
Usage: $scriptname [-dh]

Create or remove symlinks under init.d/.

  -d  Remove symlinks
  -h  Show help

USAGE
  exit "${1:-1}"
}


function error() {
  >&2 echo "$scriptname: $1"
  >&2 echo "Try '$scriptname -h' for more information."
  exit "${2:-1}"
}


function create_symlinks() {
  for priority in "${!ptom[@]}"; do
    for module in ${ptom[$priority]}; do
      src="../lisp/${module}.el"
      if [[ $priority = linux-* ]] && [[ $module = linux-* ]]; then
        target=$priority-${module#linux-}.el
      else
        target=$priority-$module.el
      fi
      if [ ! -h "init.d/$target" ]; then
        cmd="ln -s $src init.d/$target"
        $cmd
      fi
    done
  done
}


function delete_symlinks() {
  find "init.d/" -type l -exec rm {} \;
}


delete_mode=0

function main() {
  if [ "$delete_mode" = 1 ]; then
    delete_symlinks
  else
    create_symlinks
  fi
}


if [ "$0" = "${BASH_SOURCE[0]}" ]; then
  while getopts "dh" opt; do
    case $opt in
      d)
        delete_mode=1
        ;;
      h|\?)
        if [ "$opt" = "h" ]; then usage 0; else usage; fi
        ;;
    esac
  done
  shift $((OPTIND - 1))

  main "$@"
fi
