xmodmap ~/.Xmodmap
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin/scripts:$PATH"
export WORKON_HOME="$HOME/.venv/"
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -drx
sxhkd &
emacs --daemon
dropbox.py start
vertical.sh
export PATH="$HOME/.cargo/bin:$PATH"
