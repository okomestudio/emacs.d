#!/usr/bin/env bash

set -e

# Maps priority to the modules found in the lisp directory
declare -A ptom
ptom["00"]="optimizations subsys-startup"
ptom["01"]="maj-minibuffer subsys-treesit"
ptom["02"]="subsys-flycheck subsys-text-match"
ptom["04"]="subsys-help subsys-buffer subsys-navigation"
ptom["05"]="subsys-os-integration"
ptom["06"]="subsys-faces themes"
ptom["08"]="subsys-consult"
ptom["10"]="maj-org"
ptom["11"]="maj-org-roam"
ptom["20"]="subsys-ime subsys-projectile"
ptom["22"]="subsys-writing-en"
ptom["30"]="subsys-auth subsys-term subsys-embark"
ptom["32"]="subsys-lsp subsys-eglot"
ptom["40"]="subsys-completion subsys-editing subsys-git subsys-reference"
ptom["50"]="maj-text-mode"
ptom["52"]="maj-markdown maj-rst maj-yaml"
ptom["54"]="maj-conf-mode"
ptom["60"]="maj-prog-mode"
ptom["61"]="maj-polymode"
ptom["62"]="maj-c maj-css maj-docker maj-elisp maj-html maj-javascript maj-kotlin maj-python maj-rust maj-scala maj-sh maj-sql maj-web-mode"
ptom["64"]="maj-ansible subsys-drawing maj-graphviz maj-json maj-plantuml subsys-http"
ptom["70"]="subsys-doc-viewer subsys-ocr"
ptom["80"]="subsys-llm subsys-audio maj-eww subsys-game subsys-gnus subsys-map"
ptom["82"]="subsys-zotero"
ptom["86"]="maj-ibuffer"
ptom["90"]="subsys-readers subsys-treemacs subsys-weather"
ptom["91"]="maj-elfeed"
ptom["98"]="maj-dashboard"
ptom["linux-00"]="linux-gui"

# DO NOT MODIFY BELOW

readonly scriptname="${0##*/}"

function usage() {
  cat <<USAGE >&2
Usage: $scriptname [-dh]

Create or remove symlinks under init.d/.

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
  elif [ "$refresh_mode" = 1 ]; then
    delete_symlinks
    create_symlinks
  else
    create_symlinks
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
