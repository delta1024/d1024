LOAD_PATH = GUILE_LOAD_PATH=./d1024
TARGET = D1024_TARGET=

build:
	${LOAD_PATH} ${TARGET}home \
	guix home --cores=7 build ./config.scm

install:
	${LOAD_PATH}  ${TARGET}home \
	guix home reconfigure ./config.scm

system:
	${LOAD_PATH} ${TARGET}system \
	sudo -E guix system reconfigure ./config.scm
emacs:
	${LOAD_PATH} ${TARGET}home \
	guix home reconfigure ./config.scm && \
        emacs --debug-init &\|
channel:
	guix describe -f channels > ./d1024/d1024/channel.scm

channel-lock:
	guix describe -f channels > ./d1024/channel-lock.scm

pull-lock:
	guix time-machine -C ./d1024/channel-lock.scm -- \
	pull
lock: d1024/channel-lock.scm
	${LOAD_PATH} ${TARGET}home \
        guix time-machine -C ./d1024/channel-lock.scm -- \
	home reconfigure ./config.scm
lock-system: d1024/channel-lock.scm
	${LOAD_PATH} ${TARGET}system \
	sudo -E guix time-machine -C ./d1024/channel-lock.scm -- \
	system reconfigure ./config.scm
sys:
	${LOAD_PATH} ${TARGET}system \
	guix system --cores=7 build ./config.scm
build-q:
	${LOAD_PATH} ${TARGET}home \
	guix home --cores=7 build -v 1 ./config.scm
build-system-q:
	${LOAD_PATH} ${TARGET}system \
	guix system --cores=7 -v 1 build ./config.scm
check:
	${LOAD_PATH} ${TARGET}home \
	guix home -n build ./config.scm
check-system:
	${LOAD_PATH} ${TARGET}system \
	guix system -n build ./config.scm
repl:
	${LOAD_PATH} guix repl
