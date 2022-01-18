home:
	GUILE_LOAD_PATH=./d1024/  D1024_TARGET=home-config \
	guix home reconfigure ./config.scm
system:
	GUILE_LOAD_PATH=./d1024/ D1024_TARGET=sys-config \
	sudo -E guix system reconfigure ./config.scm
emacs:
	GUILE_LOAD_PATH=./d1024/ D1024_TRAGET=home-config \
	guix home reconfigure ./config.scm && \
        emacs --debug-init &\|
channel:
	guix describe -f channels > ./d1024/channel-lock.scm
pull-lock:
	guix time-machine -C ./d1024/channel-lock.scm -- \
	pull
home-lock: d1024/channel-lock.scm
	GUILE_LOAD_PATH=./d1024/ D1024_TRAGET=home-config \
        guix time-machine -C ./d1024/channel-lock.scm -- \
	home reconfigure ./config.scm
system-lock: d1024/chennel-lock.scm
	GUILE_LOAD_PATH=./d1024 D1024_TARGET=sys-config \
	sudo -E guix time-machine -C ./d1024/channel-lock.scm -- \
	system reconfigure ./config.scm
home-build:
	GUILE_LOAD_PATH=./d1024/ D1024_TARGET=home-config \
	guix home --cores=7 build ./config.scm
system-build:
	GUILE_LOAD_PATH=./d1024/ D1024_TARGET=sys-config \
	guix system --cores=7 build ./config.scm
home-build-q:
	GUILE_LOAD_PATH=./d1024/ D1024_TARGET=home-config \
	guix home --cores=7 build -v 1 ./config.scm
system-build-q:
	GUILE_LOAD_PATH=./d1024/ D1024_TARGET=sys-config \
	guix system --cores=7 -v 1 build ./config.scm
home-check:
	GUILE_LOAD_PATH=./d1024/ D1024_TARGET=home-config \
	guix home -n build ./config.scm
system-check:
	GUILE_LOAD_PATH=./d1024/ D1024_TARGET=sys-config \
	guix system -n build ./config.scm
repl:
	GUILE_LOAD_PATH=./d1024 guix repl
