#!/usr/bin/env bash

# Maps priority -> lisp modules
declare -A x
x["00"]="optimizations startup"
x["02"]="consult embark help minibuffer navigation treemacs"
x["03"]="atomic-chrome"
x["04"]="faces themes"
x["10"]="ime projectile"
x["12"]="lsp"
x["13"]="lsp-grammarly lsp-ltex"
x["14"]="org"
x["15"]="org-roam"
x["20"]="dir-locals terminals"
x["40"]="completion editing flycheck git lookup polymode tesseract"
x["50"]="text-mode"
x["52"]="markdown rst yaml"
x["54"]="conf-mode"
x["60"]="prog-mode"
x["62"]="c css docker elisp html javascript kotlin python rust scala shell sql"
x["64"]="ansible excalidraw graphviz json plantuml restclient"
x["70"]="browse-url epub openwith pdf"
x["80"]="ai anki elfeed emms eww games gnus osm"
x["88"]="dashboard"
x["linux-00"]="linux-gui"

for i in "${!x[@]}"; do
  for j in ${x[$i]}; do
    source="../lisp/${j}.el"
    if [[ $i = linux-* ]] && [[ $j = linux-* ]]; then
      target=$i-${j#linux-}.el
    else
      target=$i-$j.el
    fi
    cmd="ln -s $source init.d/$target"
    $cmd
  done
done
