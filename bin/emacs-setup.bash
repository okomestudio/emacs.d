# Source this file in ~/.bashrc

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
  SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
	source "$SCRIPT_DIR/vterm-bash.sh"
  unset SCRIPT_DIR
fi
