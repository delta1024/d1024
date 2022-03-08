(define-module (d1024 services shells)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages vim)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define preamble "\
[[ `whoami` == root ]] && source /root/.config/zsh/.zshrc
GPG_TTY=$(tty)
export GPG_TTY
# If not running interactively, don't do anything
[[ $- != *i* ]] && return")

(define tab-complete "\
# Basic auto/tab complete
autoload -Uz compinit promptinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)
promptinit; 
prompt walters")

(define vi-mode "\
#Vi Mode
bindkey -v
export 

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Edit line in vim with ctrl-e
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line")

(define extractor "\
# # ex = Extractor for all kinds of archives
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      ,*.tar.bz2)   tar xjf $1   ;;
      ,*.tar.gz)    tar xzf $1   ;;
      ,*.bz2)       bunzip2 $1   ;;
      ,*.rar)       unrar x $1   ;;
      ,*.gz)        gunzip $1    ;;
      ,*.tar)       tar xf $1    ;;
      ,*.tbz2)      tar xjf $1   ;;
      ,*.tgz)       tar xzf $1   ;;
      ,*.zip)       unzip $1     ;;
      ,*.Z)         uncompress $1;;
      ,*.7z)        7z x $1      ;;
      ,*.deb)       ar x $1      ;;
      ,*.tar.xz)    tar xf $1    ;;
      ,*)           echo \"'$1' cannot be extracted via ex()\" ;;
    esac
  else
    echo \"'$1' is not a valid file\"
  fi
}")

(define lfcd "\
lfcd () {
tmp=\"$(mktemp)\"
lf -last-dir-path=\"$tmp\" \"$@\"
if [ -f \"$tmp\" ]; then
        dir=\"$(cat \"$tmp\")\"
        rm -f \"$tmp\" > /dev/null
        [ -d \"$dir\" ] && [ \"$dir\" != \"$(pwd)\" ] && cd \"$dir\"
fi
}
bindkey -s '^o' lfcd")


(define ls-alias
  '( ( "ls" . "'ls --color=auto -lh'")
     ( "la" . "'/usr/bin/env -S ls --color=auto -a'")
     ( "ll" . "'/usr/bin/env -S ls --color=auto -la'")
     ( "l" . "'/usr/bin/env -S ls --color=auto'")))

(define typos-grep-alias
  '( ( ".." . "'cd ..'")
     ( "pdw" . "pwd")
     ( "grep" . "'grep --color=auto'")
     ( "egrep" . "'egrep --color=auto'")
     ( "fgrep" . "'fgrep --color=auto'")))

(define git-alias
  '( ( "ga" . "\"git add\"")
     ( "gau" . "\"git add -u\"")
     ( "gc" . "\"git commit\"")
     ( "gcm" . "\"git commit -m\"")
     ( "gC" . "\"git checkout\"")
     ( "gp" . "\"git push\"")
     ( "gpu" . "\"git push -u\"")
     ( "gs" . "\"git status\"")))

(define cargo-alias
  '( ( "bcr" . "'RUST_BACKTRACE=1 cargo run'")
     ( "cr" . "'cargo run'")
     ( "ct" . "'cargo test'")
     ( "cbr" . "'cargo build --release'")
     ( "cch" . "'cargo check'")
     ( "ccD" . "'cargo doc --document-private-items'")
     ( "cCD" . "'cargo doc --document-private-items --open'")
     ( "ccd" . "'cargo doc'")
     ( "cCd" . "'cargo doc --open'")
     ( "ccf" . "'cargo fmt'")
     ( "cvm" . "'nvim src/main.rs'")
     ( "cem" . "'emacsclient -nw src/main.rs'" )
     ( "rustdoc" . "'rustup doc&|'")))

(define zalias
  (append
   ls-alias
   typos-grep-alias
   git-alias
   cargo-alias))

(define build-alias-string
  (lambda (alias)
    (string-append "alias " (car alias) "=" (cdr alias) "\n")))

(define aliasrc
  "")
(define append-aliasrc
  (lambda (item)
    (set! aliasrc
	  (string-append aliasrc item))))

(define build-alias-file
  (let ((strings (map build-alias-string zalias)))
    (map append-aliasrc strings)))

(define-public shell-services
  (list
   (service home-zsh-service-type
	    (home-zsh-configuration
	     (zprofile `(,(plain-file "zprofile"
				      "export $(dbus-launch)")))
	     (environment-variables '(( "PATH" .
					"$PATH:$HOME/.scripts/:$HOME/.scripts/web:$HOME/.scripts/status-modules:$HOME/.cargo/bin:$HOME/.bin")
		( "HISTFILE" . "~/.cache/zsh/history")
		( "SAVEHIST" . "10000")
		( "HISTSIZE" . "10000")
		( "KEYTIMEOUT" . "1")))
	     (zshrc `(,(plain-file "zshrc"
				   (string-append
				    preamble "\n"
				    tab-complete "\n"
				    vi-mode "\n"
				    extractor "\n"
				    lfcd "\n"
				    "\
#source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
#source /usr/share/zsh/plugins/zsh-you-should-use/you-should-use.plugin.zsh
autoload autosuggestions" "\n" aliasrc))))))))

