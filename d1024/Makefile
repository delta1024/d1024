home:
	GUILE_LOAD_PATH=./d1024/  D1024_TARGET=home-config \
	guix home reconfigure ./config.scm
system:
	GUILE_LOAD_PATH=./d1024/ D1024_TARGET=sys-config \
	sudo -E guix system reconfigure ./config.scm
emacs:
	GUILE_LOAD_PATH=./d1024/ D1024_TRAGET=home-config \
	guix home reconfigure ./config.scm && \
        emacs --debug-init
channel:
	guix describe -f channels > ./d1024/channel-lock.scm
home-lock:
	GUILE_LOAD_PATH=./ D1024_TRAGET=home-config \
        guix time-machine -C ./d1024/channel-lock.scm -- \
	home reconfigure ./config.scm
system-lock:
	GUILE_LOAD_PATH=../.config/guix/systems/ D1024_TARGET=sys-config \
	sudo -E guix time-machine -C ./d1024/channel-lock.scm -- \
	system reconfigure ./config.scm
